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
#'
#' @param splist A character vector containing the species names to be matched.
#' @param source Character string specifying which database version to use.
#'   Options are:
#'   \itemize{
#'     \item \code{"original"} (default): Uses the original threatened species
#'       database with support for Rank 4 (quaternomial names)
#'     \item \code{"updated"}: Uses the updated database with current
#'       nomenclature, supporting up to Rank 3 (trinomial names)
#'   }
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
matching_threatenedperu <- function(splist, source = "original") {

  # ==========================================================================
  # STEP 1: Initialize Configuration
  # ==========================================================================
  config <- .initialize_matching_config(source)

  # ==========================================================================
  # STEP 2: Preprocess Input
  # ==========================================================================
  preprocessed <- .preprocess_input(splist, config)

  # Early return if all names filtered out
  if (nrow(preprocessed$df) == 0) {
    return(.create_empty_result(preprocessed$splist_class, config))
  }

  # ==========================================================================
  # STEP 3: Run Matching Pipeline
  # ==========================================================================
  matched_results <- .run_matching_pipeline(
    df = preprocessed$df,
    target_df = config$target_prepared,
    config = config
  )

  # ==========================================================================
  # STEP 4: Validate Rank Matches (CRITICAL - Prevents False Positives)
  # ==========================================================================
  validated_results <- .validate_rank_matches(
    matched_results = matched_results,
    use_infraspecies_2 = config$use_infraspecies_2
  )

  # ==========================================================================
  # STEP 5: Add Threat Information
  # ==========================================================================
  output_with_threat <- .add_threat_information(
    results = validated_results$combined,
    splist_class = preprocessed$splist_class,
    target_df = config$target_prepared,
    config = config
  )

  # ==========================================================================
  # STEP 6: Finalize Output
  # ==========================================================================
  final_output <- .finalize_output(
    output = output_with_threat,
    splist_class = preprocessed$splist_class,
    config = config
  )

  return(final_output)
}
