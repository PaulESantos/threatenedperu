#' Consolidated Matching for DS 043-2006-AG Species
#'
#' @description
#' Performs consolidated matching that searches species names in both the original
#' DS 043-2006-AG list (2006 names) and the updated nomenclature database. This
#' ensures that users with updated names can still identify if their species are
#' protected under the decree, even if the nomenclature has changed.
#'
#' @param splist Character vector of species names to check
#' @param prioritize Character. Which result to prioritize when both databases
#'   match: "original" (default) or "updated"
#' @param return_details Logical. Return detailed matching information
#'
#' @return
#' If return_details = FALSE: Character vector with consolidated threat status.
#' If return_details = TRUE: Tibble with detailed reconciliation information.
#'
#' @details
#' The function performs a two-stage search:
#'
#' 1. Searches in original DS 043-2006-AG (names as listed in 2006)
#' 2. Searches in updated nomenclature database (current accepted names)
#' 3. Consolidates results with clear indication of which database provided the match
#' 4. Identifies if original names are now synonyms
#'
#' This approach handles cases where:
#' - User provides original name from 2006: Found in original database
#' - User provides updated name: Found in updated database and linked to DS 043
#' - Name matches in both: Returns most relevant result based on priority
#' - Original name is now a synonym: Indicated with "(synonym)" marker
#'
#' @examples
#' \dontrun{
#' # Species with nomenclatural changes
#' species <- c(
#'   "Haageocereus acranthus subsp. olowinskianus",  # Original name
#'   "Brassia ocanensis",                            # Updated name (was Ada)
#'   "Ida locusta",                                  # Updated name
#'   "Lycaste locusta",                              # Now a synonym
#'   "Persea americana"                              # Not threatened
#' )
#'
#' # Get consolidated status
#' status <- consolidated_ds043_matching(species)
#'
#' # Get detailed information
#' details <- consolidated_ds043_matching(species, return_details = TRUE)
#' View(details)
#' }
#'
#' @export
consolidated_ds043_matching <- function(splist,
                                        prioritize = "original",
                                        return_details = FALSE) {

  # ========================================================================
  # SECTION 1: Input Validation
  # ========================================================================

  if (!is.character(splist)) {
    stop("splist must be a character vector")
  }

  if (length(splist) == 0) {
    warning("Empty species list provided")
    return(if(return_details) tibble::tibble() else character(0))
  }

  if (!prioritize %in% c("original", "updated")) {
    stop("prioritize must be 'original' or 'updated'")
  }

  # Check required datasets
  if (!exists("threatenedperu")) {
    stop("Dataset 'threatenedperu' not found")
  }

  if (!exists("threatenedperu_syn")) {
    stop("Dataset 'threatenedperu_syn' not found")
  }


  # ========================================================================
  # SECTION 2: Search in Original Database (DS 043-2006-AG 2006)
  # ========================================================================

  res_original <- matching_threatenedperu(
    splist = splist,
    target_df = "original"
  )


  # ========================================================================
  # SECTION 3: Search in Updated Database (Current Nomenclature)
  # ========================================================================

  res_updated <- matching_threatenedperu(
    splist = splist,
    target_df = "updated"
  )

  # ========================================================================
  # SECTION 4: Synonyms detection for original names
  # ========================================================================

  # Check which matched names are synonyms
  synonyms_detected <- threatenedperu |>
    dplyr::filter(
      scientific_name %in% res_original$Matched.Name,
      taxonomic_status == "Synonym"
    ) |>
    dplyr::select(
      scientific_name,
      accepted_name
    )

  # ========================================================================
  # SECTION 5: Consolidate Results
  # ========================================================================


  consolidated <-
    tibble::tibble(
      Input.Name = splist,
      sorter = 1:length(splist)
    ) |>
    dplyr::left_join(
      res_original |>
        dplyr::select(
          sorter,
          Original.Matched = Matched.Name,
          Original.Status = Threat.Status,
          Original.Category = threat_category,
          Original.Match.Type = matched
        ),
      by = "sorter"
    ) |>
    dplyr::left_join(
      res_updated |>
        dplyr::select(
          sorter,
          Updated.Matched = Matched.Name,
          Updated.Status = Threat.Status,
          Updated.Category = threat_category,
          Updated.Match.Type = matched
        ),
      by = "sorter"
    ) |>
    # Add synonym information
    dplyr::left_join(
      synonyms_detected |>
        dplyr::rename(
          Original.Matched = scientific_name,
          Accepted.Name = accepted_name
        ),
      by = "Original.Matched"
    ) |>
    dplyr::mutate(
      # Check if the original matched name is a synonym
      Is.Synonym = !is.na(Accepted.Name),

      # Determine which database found a threatened species
      Found.In.Original = stringr::str_detect(Original.Status, "Threatened"),
      Found.In.Updated = stringr::str_detect(Updated.Status, "Threatened"),

      # Determine matching scenario
      Match.Scenario = dplyr::case_when(
        Found.In.Original & Found.In.Updated ~ "Both databases",
        Found.In.Original & !Found.In.Updated ~ "Original only",
        !Found.In.Original & Found.In.Updated ~ "Updated only",
        TRUE ~ "Not found"
      ),

      # Consolidated matched name
      Consolidated.Name = dplyr::case_when(
        prioritize == "original" & Original.Matched != "---" ~ Original.Matched,
        prioritize == "original" & Updated.Matched != "---" ~ Updated.Matched,
        prioritize == "updated" & Updated.Matched != "---" ~ Updated.Matched,
        prioritize == "updated" & Original.Matched != "---" ~ Original.Matched,
        TRUE ~ "---"
      ),

      # Consolidated threat status with synonym indicator
      Consolidated.Status = dplyr::case_when(
        # If found as threatened in original AND is a synonym
        Found.In.Original & Is.Synonym ~ paste0(Original.Status, " (synonym)"),
        # If found as threatened in original but not a synonym
        Found.In.Original & !Is.Synonym ~ Original.Status,
        # If only found in updated, it's protected under updated nomenclature
        Found.In.Updated ~ paste0(Updated.Status, " (updated name)"),
        # Not found in either
        TRUE ~ "Not threatened"
      ),

      # Consolidated category
      Consolidated.Category = dplyr::case_when(
        Found.In.Original ~ Original.Category,
        Found.In.Updated ~ Updated.Category,
        TRUE ~ NA_character_
      ),

      # Source of final decision
      Final.Source = dplyr::case_when(
        Found.In.Original & Is.Synonym ~ "DS 043-2006-AG (original, now synonym)",
        Found.In.Original & !Is.Synonym ~ "DS 043-2006-AG (original)",
        Found.In.Updated ~ "DS 043-2006-AG (updated nomenclature)",
        TRUE ~ "Not in DS 043-2006-AG"
      ),

      # Is protected under DS 043-2006-AG?
      Protected.DS043 = Found.In.Original | Found.In.Updated,

      # Nomenclatural status
      Nomenclature.Status = dplyr::case_when(
        Found.In.Original & Is.Synonym ~ "Synonym (name updated)",
        Found.In.Original & !Is.Synonym & !Found.In.Updated ~ "Original name (2006)",
        !Found.In.Original & Found.In.Updated ~ "Name updated since 2006",
        Found.In.Original & Found.In.Updated ~ "Found in both",
        TRUE ~ "Not applicable"
      )
    ) |>
    dplyr::select(
      Input.Name,
      Consolidated.Name,
      Consolidated.Status,
      Consolidated.Category,
      Protected.DS043,
      Is.Synonym,
      Accepted.Name,
      Final.Source,
      Match.Scenario,
      Nomenclature.Status,
      Original.Matched,
      Original.Status,
      Updated.Matched,
      Updated.Status
    )

  # ========================================================================
  # SECTION 6: Return Results
  # ========================================================================

  if (return_details) {
    return(consolidated)
  } else {
    return(consolidated$Consolidated.Status)
  }
}

#' Simplified wrapper for consolidated matching
#'
#' @description
#' Simplified interface for checking DS 043-2006-AG status with automatic
#' consolidation of original and updated nomenclature.
#'
#' @param splist Character vector of species names
#' @param return_simple Logical. If TRUE, returns only "Protected" or "Not protected"
#'
#' @return Character vector with protection status
#' @export
#'
#' @examples
#' \dontrun{
#' species <- c("Brassia ocanensis", "Persea americana")
#' check_ds043(species)
#' }
check_ds043 <- function(splist, return_simple = FALSE) {

  results <- consolidated_ds043_matching(
    splist = splist,
    return_details = FALSE
  )

  if (return_simple) {
    return(ifelse(
      stringr::str_detect(results, "Threatened"),
      "Protected by DS 043-2006-AG",
      "Not protected"
    ))
  } else {
    return(results)
  }
}

#' Create comparison table between original and updated results
#'
#' @description
#' Creates a side-by-side comparison table useful for understanding
#' nomenclatural changes and their impact on DS 043-2006-AG status.
#'
#' @param splist Character vector of species names
#'
#' @return Tibble with comparison
#' @export
comparison_table_ds043 <- function(splist) {

  consolidated <- consolidated_ds043_matching(
    splist = splist,
    return_details = TRUE
  )

  comparison <- consolidated |>
    dplyr::select(
      input_species = Input.Name,
      match_2006_list = Original.Matched,
      status_original = Original.Status,
      match_updated_name = Updated.Matched,
      status_updated = Updated.Status,
      protected_ds_043 = Protected.DS043,
      nomenclature_status = Nomenclature.Status
    ) |>
    dplyr::mutate(
      protected_by_ds_043 = ifelse(protected_ds_043, "YES", "NO")
    )

  return(comparison)
}
