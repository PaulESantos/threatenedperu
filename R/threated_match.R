#' Match Species Names to Threatened Plant List of Peru
#'
#' @description
#' This function matches given species names against the internal database of threatened
#' plant species in Peru. It uses a hierarchical matching strategy that includes direct
#' matching, genus-level matching, fuzzy matching, and suffix matching to maximize
#' successful matches while maintaining accuracy.
#'
#' @param splist A character vector containing the species names to be matched.
#' @param target_df Character string specifying which database version to use.
#'   Options are:
#'   \itemize{
#'     \item \code{"original"} (default): Uses the original threatened species database
#'     \item \code{"updated"}: Uses the updated database with synonyms
#'   }
#'
#' @details
#' The matching process follows a hierarchical pipeline:
#'
#' \strong{Step 1: Direct Match}
#' Attempts exact matches for genus, species, and infraspecies simultaneously.
#'
#' \strong{Step 2: Genus Match}
#' For unmatched names, matches the genus against the database.
#'
#' \strong{Step 3: Fuzzy Genus Match}
#' Uses string distance algorithms (max distance = 1) to match genus names
#' with minor spelling variations.
#'
#' \strong{Step 4: Species within Genus Match}
#' For matched genera, attempts direct matching of species epithets.
#'
#' \strong{Step 5a: Suffix Match}
#' Handles common Latin suffix variations (e.g., -a, -um, -us, -is).
#'
#' \strong{Step 5b: Fuzzy Species Match}
#' Uses string distance algorithms to match species epithets within matched genera.
#'
#' \strong{Step 6: Infraspecies Fuzzy Match}
#' For matched species with infraspecies, attempts fuzzy matching of
#' infraspecific epithets.
#'
#' The maximum edit distance is intentionally set to one to maintain high
#' matching accuracy while allowing for common typos.
#'
#' @return
#' A tibble with the following key columns:
#' \describe{
#'   \item{sorter}{Original row number for maintaining input order}
#'   \item{Orig.Name}{Original species name as provided}
#'   \item{Matched.Name}{Matched name from the database (properly formatted)}
#'   \item{Threat.Status}{Threat status: "Threatened - [CR|EN|VU|NT]" or "Not threatened"}
#'   \item{threat_category}{IUCN threat category (CR, EN, VU, NT) if threatened}
#'   \item{Matched.Genus}{Matched genus name (uppercase)}
#'   \item{Matched.Species}{Matched species epithet (uppercase)}
#'   \item{Matched.Infraspecies}{Matched infraspecies epithet if applicable}
#'   \item{matched}{Logical indicating if a match was found}
#'   \item{direct_match}{Logical indicating if direct match succeeded}
#'   \item{genus_match}{Logical indicating if genus match succeeded}
#'   \item{fuzzy_match_genus}{Logical indicating if fuzzy genus match was used}
#'   \item{direct_match_species_within_genus}{Logical for direct species match}
#'   \item{suffix_match_species_within_genus}{Logical for suffix match}
#'   \item{fuzzy_match_species_within_genus}{Logical for fuzzy species match}
#'   \item{fuzzy_genus_dist}{String distance for fuzzy genus matches}
#'   \item{fuzzy_species_dist}{String distance for fuzzy species matches}
#' }
#'
#' @note
#' The function performs additional data cleaning at the end to remove duplicate
#' matches and handle specific edge cases in the Cactaceae family.
#'
#' @seealso
#' \code{\link{is_threatened_peru}} for a simplified interface
#'
#' @examples
#' \dontrun{
#' # Basic usage with original database
#' species_list <- c("Cattleya maxima", "Polylepis incana", "Unknown species")
#' results <- matching_threatenedperu(species_list, target_df = "original")
#'
#' # View matching details
#' View(results)
#'
#' # Check which matching methods were used
#' table(results$fuzzy_match_genus, useNA = "ifany")
#'
#' # Using updated database with synonyms
#' results_updated <- matching_threatenedperu(species_list, target_df = "updated")
#' }
#'
#' @export
matching_threatenedperu <- function(splist, target_df = "original"){

  # ========================================================================
  # SECTION 1: Target Database Selection and Validation
  # ========================================================================

  # Validate target_df parameter
  if (!is.character(target_df) || length(target_df) != 1) {
    stop("target_df must be a single character string: 'original' or 'updated'")
  }

  # Select appropriate database
  target_prepared <- switch(
    target_df,
    "original" = {
      if (!exists("threatenedperu", envir = parent.frame())) {
        stop("Database 'threatenedperu' not found. Please load the package data.")
      }
      get("threatenedperu", envir = parent.frame())
    },
    "updated" = {
      if (!exists("threatenedperu_syn", envir = parent.frame())) {
        stop("Database 'threatenedperu_syn' not found. Please load the package data.")
      }
      get("threatenedperu_syn", envir = parent.frame())
    },
    stop("Invalid target_df value. Must be 'original' or 'updated'")
  )

  # Validate database structure
  required_cols <- c("genus", "species", "infraspecies", "threat_category")
  missing_cols <- setdiff(required_cols, names(target_prepared))
  if (length(missing_cols) > 0) {
    stop("Target database missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }


  # ========================================================================
  # SECTION 2: Input Processing and Classification
  # ========================================================================

  # Classify species names into taxonomic components
  splist_class <- .splist_classify(splist) |>
    .transform_split_classify()

  # Check for and handle non-binomial names
  non_binomial <- .check_binomial(splist_class, splist = splist)

  if(length(non_binomial) != 0){
    df <- splist_class[-non_binomial,]
    df$sorter <- 1:nrow(df)
  } else{
    df <- splist_class
  }

  # Initialize matching columns
  if(!all(c('Matched.Genus', 'Matched.Species', 'Matched.Infraspecies') %in%
          colnames(df))){
    df <- df |>
      tibble::add_column(
        Matched.Genus = as.character(NA),
        Matched.Species = as.character(NA),
        Matched.Infraspecies = as.character(NA)
      )
  }

  # ========================================================================
  # SECTION 3: Hierarchical Matching Pipeline
  # ========================================================================

  # ---------------------------------------------------------------
  # Node 1: Direct Match (Exact match for full name)
  # ---------------------------------------------------------------
  Node_1_processed <- df |>
    direct_match(target_df = target_prepared)

  Node_1_TRUE <- Node_1_processed |>
    dplyr::filter(direct_match == TRUE)

  Node_1_FALSE <- Node_1_processed |>
    dplyr::filter(direct_match == FALSE)

  assertthat::assert_that(nrow(df) == (nrow(Node_1_TRUE) + nrow(Node_1_FALSE)),
                          msg = "Row count mismatch after Node 1: Direct Match")

  # ---------------------------------------------------------------
  # Node 2: Genus Match (Exact genus match)
  # ---------------------------------------------------------------
  Node_2_processed <- Node_1_FALSE |>
    genus_match(target_prepared)

  Node_2_TRUE <- Node_2_processed |>
    dplyr::filter(genus_match == TRUE)

  Node_2_FALSE <- Node_2_processed |>
    dplyr::filter(genus_match == FALSE)

  assertthat::assert_that(nrow(Node_2_processed) == (nrow(Node_2_TRUE) + nrow(Node_2_FALSE)),
                          msg = "Row count mismatch after Node 2: Genus Match")

  # ---------------------------------------------------------------
  # Node 3: Fuzzy Match Genus (Approximate genus matching)
  # ---------------------------------------------------------------
  Node_3_processed <- Node_2_FALSE |>
    fuzzy_match_genus(target_prepared)

  Node_3_TRUE <- Node_3_processed |>
    dplyr::filter(fuzzy_match_genus == TRUE)

  Node_3_FALSE <- Node_3_processed |>
    dplyr::filter(fuzzy_match_genus == FALSE)

  assertthat::assert_that(nrow(Node_3_processed) == (nrow(Node_3_TRUE) + nrow(Node_3_FALSE)),
                          msg = "Row count mismatch after Node 3: Fuzzy Genus Match")

  # ---------------------------------------------------------------
  # Node 4: Direct Match Species within Genus
  # ---------------------------------------------------------------
  Node_4_input <- Node_3_TRUE |>
    dplyr::bind_rows(Node_2_TRUE)

  Node_4_processed <- Node_4_input |>
    direct_match_species_within_genus(target_prepared)

  Node_4_TRUE <- Node_4_processed |>
    dplyr::filter(direct_match_species_within_genus == TRUE)

  Node_4_FALSE <- Node_4_processed |>
    dplyr::filter(direct_match_species_within_genus == FALSE)

  assertthat::assert_that(nrow(Node_4_processed) == (nrow(Node_4_TRUE) + nrow(Node_4_FALSE)),
                          msg = "Row count mismatch after Node 4: Species Match")

  # ---------------------------------------------------------------
  # Node 5a: Suffix Match Species within Genus
  # ---------------------------------------------------------------
  Node_5a_processed <- Node_4_FALSE |>
    suffix_match_species_within_genus(target_prepared)

  Node_5a_TRUE <- Node_5a_processed |>
    dplyr::filter(suffix_match_species_within_genus == TRUE)

  Node_5a_FALSE <- Node_5a_processed |>
    dplyr::filter(suffix_match_species_within_genus == FALSE)

  assertthat::assert_that(nrow(Node_4_FALSE) == (nrow(Node_5a_TRUE) + nrow(Node_5a_FALSE)),
                          msg = "Row count mismatch after Node 5a: Suffix Match")

  # ---------------------------------------------------------------
  # Node 5b: Fuzzy Match Species within Genus
  # ---------------------------------------------------------------
  Node_5b_input <- Node_5a_FALSE
  Node_5b_processed <- Node_5b_input |>
    fuzzy_match_species_within_genus(target_prepared)

  Node_5b_TRUE <- Node_5b_processed |>
    dplyr::filter(fuzzy_match_species_within_genus == TRUE)

  Node_5b_FALSE <- Node_5b_processed |>
    dplyr::filter(fuzzy_match_species_within_genus == FALSE)

  assertthat::assert_that(nrow(Node_5b_input) == (nrow(Node_5b_TRUE) + nrow(Node_5b_FALSE)),
                          msg = "Row count mismatch after Node 5b: Fuzzy Species Match")

  # ========================================================================
  # SECTION 4: Combine Results and Handle Infraspecies
  # ========================================================================

  # Combine matched and unmatched results
  matched <- dplyr::bind_rows(Node_1_TRUE, Node_4_TRUE,
                              Node_5a_TRUE, Node_5b_TRUE) |>
    dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies)

  unmatched <- dplyr::bind_rows(Node_3_FALSE, Node_5b_FALSE) |>
    dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies)

  res <- dplyr::bind_rows(matched, unmatched, .id='matched') |>
    dplyr::mutate(matched = (matched == 1))

  # ---------------------------------------------------------------
  # Infraspecies Fuzzy Matching
  # ---------------------------------------------------------------
  infra_input <- res |>
    dplyr::filter(!is.na(Orig.Infraspecies),
                  is.na(Matched.Infraspecies))

  infra_sorter <- as.vector(infra_input$sorter)

  if(nrow(infra_input) != 0){
    infra_matched <- infra_input |>
      fuzzy_match_infraspecies_within_species(target_prepared)
  }

  # ---------------------------------------------------------------
  # Handle Genus-level Names
  # ---------------------------------------------------------------
  if(length(non_binomial) != 0){
    genus_level <- matrix(nrow = length(non_binomial),
                          ncol = length(names(res)))
    colnames(genus_level) <- names(res)
    genus_level <- as.data.frame(genus_level)
    genus_level$sorter <- splist_class[splist_class$Rank == 1, "sorter"]
    genus_level$Orig.Name <- splist_class[splist_class$Rank == 1, "Orig.Name"]
    genus_level$Orig.Genus <- splist_class[splist_class$Rank == 1, "Orig.Genus"]
    genus_level$Rank <- splist_class[splist_class$Rank == 1, "Rank"]
  }

  # ========================================================================
  # SECTION 5: Consolidate All Results
  # ========================================================================

  if(nrow(infra_input) == 0 & length(non_binomial) == 0){
    out <- res
    rm(res)
  } else if (nrow(infra_input) != 0 & length(non_binomial) == 0){
    out <- dplyr::bind_rows(res |>
                              dplyr::filter(!sorter %in% infra_sorter),
                            infra_matched)
    rm(res, infra_matched)
  } else if (nrow(infra_input) == 0 & length(non_binomial) != 0){
    out <- dplyr::bind_rows(res, genus_level)
    rm(res, genus_level)
  } else if (nrow(infra_input) != 0 & length(non_binomial) != 0){
    out <- dplyr::bind_rows(res |>
                              dplyr::filter(!sorter %in% infra_sorter),
                            infra_matched,
                            genus_level)
    rm(res, infra_matched, genus_level)
  }

  # ========================================================================
  # SECTION 6: Add Threat Information and Format Output
  # ========================================================================

  output <- out |>
    dplyr::arrange(sorter) |>

    # Join with threat category information
    dplyr::left_join(
      target_prepared |>
        dplyr::select(genus, species, infraspecies, threat_category),
      by = c("Matched.Genus" = "genus",
             "Matched.Species" = "species",
             "Matched.Infraspecies" = "infraspecies")
    ) |>

    # Create properly formatted matched name
    dplyr::mutate(Matched.Name = dplyr::case_when(
      is.na(Matched.Species) ~ stringr::str_to_sentence(Matched.Genus),
      is.na(Matched.Infraspecies) ~ paste(
        stringr::str_to_sentence(Matched.Genus),
        stringr::str_to_lower(Matched.Species),
        sep = " "
      ),
      !is.na(Matched.Infraspecies) ~ paste(
        stringr::str_to_sentence(Matched.Genus),
        stringr::str_to_lower(Matched.Species),
        stringr::str_to_lower(Infra.Rank),
        stringr::str_to_lower(Matched.Infraspecies),
        sep = " "
      ),
      TRUE ~ "---"
    )) |>

    # Determine matched rank
    dplyr::mutate(Matched.Rank = dplyr::case_when(
      !is.na(Matched.Genus) & !is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ 2,
      !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) ~ 3,
      is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ 1,
      TRUE ~ NA_real_
    )) |>

    # Standardize original name format
    dplyr::mutate(Orig.Name = str_to_simple_cap(Orig.Name)) |>

    # Compare taxonomic ranks
    dplyr::mutate(Comp.Rank = (Rank == Matched.Rank)) |>

    # Assign threat status
    dplyr::mutate(Threat.Status = dplyr::case_when(
      is.na(Matched.Genus) & is.na(Matched.Species) & is.na(Matched.Infraspecies) ~
        'Not threatened',
      !is.na(threat_category) ~ paste('Threatened -', threat_category),
      Comp.Rank == TRUE & matched == TRUE ~ 'Threatened - Unknown category',
      TRUE ~ 'Not threatened'
    )) |>

    # Fix display of unmatched names
    dplyr::mutate(Matched.Name = dplyr::if_else(
      is.na(Matched.Name), "---", Matched.Name
    )) |>

    # Reorder columns for clarity
    dplyr::relocate(c(
      "sorter", "Orig.Name", "Matched.Name", "Threat.Status", "threat_category",
      "Orig.Genus", "Orig.Species", "Orig.Infraspecies",
      "Rank", "Infra.Rank", "Comp.Rank",
      "Matched.Genus", "Matched.Species", "Matched.Infraspecies", "Matched.Rank",
      "matched", "direct_match", "genus_match", "fuzzy_match_genus",
      "direct_match_species_within_genus", "suffix_match_species_within_genus",
      "fuzzy_match_species_within_genus", "fuzzy_genus_dist", "fuzzy_species_dist"
    ))

  # ========================================================================
  # SECTION 7: Data Cleaning and Final Validation
  # ========================================================================

  # Remove specific duplicate matches and edge cases
 if(target_df == "original"){
   output <- output |>
     dplyr::filter(
       !(stringr::str_detect(Matched.Name,
                             "Haageocereus acranthus subsp. olowinskianus") &
           threat_category != "VU")
     ) |>
     dplyr::group_by(Matched.Name) |>
     dplyr::distinct() |>
     dplyr::ungroup()
 } else if(target_df == "updated"){
   output <- output |>
     dplyr::filter(
       !(stringr::str_detect(Matched.Name,
                             "Haageocereus acranthus subsp. acranthus") &
           threat_category != "VU")
     ) |>
     dplyr::group_by(Matched.Name) |>
     dplyr::distinct() |>
     dplyr::ungroup()
 }


  # Final validation
  assertthat::assert_that(
    nrow(splist_class) == nrow(output),
    msg = "Final output row count does not match input"
  )

  return(output)
}

