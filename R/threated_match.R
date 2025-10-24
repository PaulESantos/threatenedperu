# ==============================================================================
# MATCHING PIPELINE - Main Orchestrator
# ==============================================================================

#' Match Species Names to Threatened Plant List of Peru
#'
#' @description
#' This function matches given species names against the internal database of
#' threatened plant species in Peru. It uses a hierarchical matching strategy
#' that includes direct matching, genus-level matching, fuzzy matching, and
#' suffix matching to maximize successful matches while maintaining accuracy.

#' @param splist A character vector containing the species names to be matched.
#' @param source Character string specifying which database version to use.
#'   Options are:
#'   \itemize{
#'     \item \code{"original"} (default): Uses the original threatened species
#'       database with support for Rank 4 (quaternomial names)
#'     \item \code{"updated"}: Uses the updated database with current
#'       nomenclature, supporting up to Rank 3 (trinomial names)
#'   }
#' @param quiet Logical, default TRUE
#'
#' @details
#' The matching process follows a hierarchical pipeline with robust handling of
#' infraspecific ranks at two levels (when supported by the database).
#'
#' **Matching Strategy:**
#' 1. Direct exact matching
#' 2. Genus-level matching (exact and fuzzy)
#' 3. Species-level matching within genus
#' 4. Infraspecies-level matching (up to 2 levels for original database)
#'
#' **Rank Validation:**
#' The algorithm implements strict rank validation to prevent false positives.
#' For example, if a user inputs a trinomial name (Rank 3) but only a binomial
#' (Rank 2) exists in the database, the match is rejected.
#'
#' **Ambiguous Matches:**
#' When multiple candidates have identical match scores (string distances), the
#' algorithm automatically selects the first match and issues a warning. To
#' review ambiguous matches for quality control, use
#' \code{\link{get_ambiguous_matches}} on the result object.
#'
#' @return
#' A tibble with detailed matching results including:
#' \describe{
#'   \item{Orig.Name}{Original input name}
#'   \item{Matched.Name}{Matched name from database}
#'   \item{Threat.Status}{IUCN threat category or "Not threatened"}
#'   \item{Rank}{Input taxonomic rank (1-4)}
#'   \item{Matched.Rank}{Matched taxonomic rank}
#'   \item{Comp.Rank}{Logical, whether ranks match exactly}
#'   \item{Match.Level}{Description of match quality}
#'   \item{matched}{Logical, whether a match was found}
#' }
#'
#' The result also includes metadata attributes:
#' \itemize{
#'   \item \code{use_infraspecies_2}: Whether Rank 4 is supported
#'   \item \code{target_database}: Database used ("original" or "updated")
#'   \item \code{matching_date}: Date of matching
#'   \item \code{n_input}: Number of input names
#'   \item \code{n_matched}: Number of successful matches
#'   \item \code{match_rate}: Percentage of successful matches
#' }
#'
#' @seealso
#' \code{\link{is_threatened_peru}} for a simplified interface
#' \code{\link{get_ambiguous_matches}} to retrieve ambiguous match details
#' \code{\link{get_threatened_database}} to access the raw databases
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' species_list <- c("Cattleya maxima", "Polylepis incana")
#' results <- matching_threatenedperu(species_list, source = "original")
#'
#' # Access metadata
#' attr(results, "match_rate")
#'
#' # Check for ambiguous matches
#' ambig <- get_ambiguous_matches(results, type = "all")
#' }
#'
#' @export
matching_threatenedperu <- function(splist, source = c("original", "updated"), quiet = TRUE) {
  source <- match.arg(source)
  use_infraspecies_2 <- identical(source, "original")

  # 1) Validación de inputs y carga de base
  .validate_inputs(splist, quiet)
  target_prepared <- .load_target(source, quiet)
  .validate_target_schema(target_prepared, use_infraspecies_2)

  # 2) Clasificación y preprocesamiento
  splist_class <- .classify_inputs(splist)
  parts <- .split_valid_invalid(splist_class)
  df <- parts$valid
  if (nrow(df) == 0L) {
    res <- .empty_output(splist_class, use_infraspecies_2, source)
    return(.attach_metadata(res, use_infraspecies_2, source, nrow(splist_class), 0L))
  }
  df <- .init_matching_columns(df, use_infraspecies_2, source, quiet)
  .warn_on_rank4_if_unsupported(df, use_infraspecies_2, source, quiet)

  # 3) Pipeline jerárquico (nodos 1–5)
  pipe_1_5 <- .pipeline_nodes_1_to_5(df, target_prepared, source, quiet)

  # 4) Validación por rango y consolidación
  combined <- .combine_nodes(pipe_1_5)
  combined <- .compute_matched_rank_and_validate(combined, use_infraspecies_2)
  lists <- .split_matched_invalid_unmatched(combined, pipe_1_5, quiet)

  # 5) Infraspecies (nodos 6–7)
  infra_out <- .pipeline_nodes_6_7(lists, target_prepared, source, use_infraspecies_2)

  # 6) Join de amenaza + formato
  res_complete <- .join_threat_and_format(
    base_df = dplyr::select(splist_class,
                            "sorter","Orig.Name","Orig.Genus","Orig.Species",
                            "Orig.Infraspecies","Orig.Infraspecies_2",
                            "Rank","Orig.Infra.Rank","Orig.Infra.Rank_2","Author"
    ),
    res = infra_out$res,
    target_prepared = target_prepared,
    use_infraspecies_2 = use_infraspecies_2
  )

  # 7) Nombre formateado, nivel de match, status
  output_f <- .finalize_output(res_complete, use_infraspecies_2)

  # 8) Validaciones finales y metadatos
  .final_assertions(splist_class, output_f)
  output_f <- .cleanup_infrasp2_if_needed(output_f, use_infraspecies_2)
  .attach_metadata(output_f, use_infraspecies_2, source,
                   n_input = nrow(splist_class),
                   n_matched = sum(output_f$matched, na.rm = TRUE))
}
