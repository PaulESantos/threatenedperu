#' Check if species are threatened in Peru
#'
#' @description
#' This function checks if a list of species names are threatened according to the
#' Peruvian threatened species database. The function allows fuzzy matching for
#' species names with a maximum distance threshold to handle potential typos or
#' variations in species names.
#'
#' @param splist A character vector containing the list of species names to be
#' checked for threatened status in Peru.
#' @param target_type Character string specifying which database version to use.
#'   Options are:
#'   \itemize{
#'     \item \code{"original"} (default): Uses the original threatened species database
#'     \item \code{"updated"}: Uses the updated database with synonyms
#'   }
#' @param return_details Logical. If TRUE, returns detailed matching results.
#' If FALSE (default), returns only the threat status vector.
#'
#' @return If return_details = FALSE: A character vector indicating the threat
#' status of each species ("Not threatened", "Threatened - CR", "Threatened - EN",
#' "Threatened - VU", "Threatened - NT", or "Threatened - Unknown category").
#'
#' If return_details = TRUE: A tibble with detailed matching results including
#' matched names, threat categories, and matching process information.
#'
#' @export
#'
#' @examples
#' \donttest{
#'#' # Simple usage - returns threat status vector
#' species_list <- c("Cattleya maxima", "Polylepis incana", "Fake species")
#' threat_status <- is_threatened_peru(species_list)
#' threat_status
#'
#' # Detailed usage - returns full tibble
#' detailed_results <- is_threatened_peru(species_list,
#'                                        return_details = TRUE)
#' detailed_results
#' }
is_threatened_peru <- function(splist, target_type = "original", return_details = FALSE) {

  # Input validation
  if (!is.character(splist)) {
    stop("splist must be a character vector")
  }

  if (length(splist) == 0) {
    warning("Empty species list provided")
    return(character(0))
  }

  # Remove empty strings and NA values
  splist_clean <- splist[!is.na(splist) & nchar(trimws(splist)) > 0]

  if (length(splist_clean) == 0) {
    warning("All species names are empty or NA")
    return(rep("Not threatened", length(splist)))
  }

  if (length(splist_clean) != length(splist)) {
    warning("Some species names were empty or NA and were treated as 'Not threatened'")
  }

  # Use internal data if target_df not provided
    # This would load internal package data in a real package
    target_df <- threatenedperu

  # Validate target_df
  required_cols <- c("genus", "species", "infraspecies", "threat_category")
  if (!all(required_cols %in% colnames(target_df))) {
    stop("target_df must contain columns: ",
         paste(required_cols, collapse = ", "))
  }

  # Perform detailed matching
  match_df <- matching_threatenedperu(splist_clean, target_df = target_type)
if(target_type == "original"){
  message(check_name_update(match_df, threatenedperu))
}

  # Create result vector for all original inputs
  result_vector <- rep("Not threatened", length(splist))

  # Fill in results for valid species names
  valid_indices <- which(!is.na(splist) & nchar(trimws(splist)) > 0)
  result_vector[valid_indices] <- match_df$Threat.Status

  if (return_details) {
    # Add back the original indices and return full results
    detailed_results <- match_df |>
      dplyr::mutate(Original.Index = valid_indices) |>
      dplyr::relocate(Original.Index, .before = 1)

    return(detailed_results)
  } else {
    # Return simple vector
    return(result_vector)
  }
}

#' Get threat categories summary
#'
#' @description
#' Get a summary of threat categories for a list of species
#'
#' @param splist A character vector containing species names
#' @param target_df A tibble representing the threatened species database
#'
#' @return A tibble with threat category counts
#'
#' @export

get_threat_summary <- function(splist, target_df = NULL) {

  if (is.null(target_df)) {
    stop("target_df must be provided. Use prepare_threatened_data() to prepare your dataset first.")
  }

  threat_status <- is_threatened_peru(splist)
threat_status
  summary_table <- tibble::tibble(Threat_Status = threat_status) |>
    dplyr::count(Threat_Status, name = "Count", sort = TRUE) |>
    dplyr::mutate(
      Percentage = round(100 * Count / sum(Count), 2),
      Category = dplyr::case_when(
        Threat_Status == "Not threatened" ~ "Not threatened",
        stringr::str_detect(Threat_Status, "CR") ~ "Critically Endangered",
        stringr::str_detect(Threat_Status, "EN") ~ "Endangered",
        stringr::str_detect(Threat_Status, "VU") ~ "Vulnerable",
        stringr::str_detect(Threat_Status, "NT") ~ "Near Threatened",
        TRUE ~ "Unknown category"
      )
    ) |>
    dplyr::relocate(Category, .after = Threat_Status)
summary_table
  return(summary_table)
}

#' Check if a single species is threatened
#'
#' @description
#' Convenience function to check the threat status of a single species
#'
#' @param species_name A character string with a single species name
#' @param target_df A tibble representing the threatened species database
#'
#' @return A character string with the threat status
#'
#' @export
#'

is_single_species_threatened <- function(species_name, target_df = NULL) {

  if (!is.character(species_name) || length(species_name) != 1) {
    stop("species_name must be a single character string")
  }

  result <- is_threatened_peru(species_name, target_df)
  return(result[1])
}

#' Find threatened species by family
#'
#' @description
#' Find all threatened species within a specific plant family from your input list
#'
#' @param splist A character vector containing species names
#' @param family_name A character string with the family name to filter by
#' @param target_df A tibble representing the threatened species database
#'
#' @return A tibble with threatened species from the specified family
#'
#' @export
#'

find_threatened_by_family <- function(splist, family_name, target_df = NULL) {

  if (is.null(target_df)) {
    stop("target_df must be provided. Use prepare_threatened_data() to prepare your dataset first.")
  }

  if (!is.character(family_name) || length(family_name) != 1) {
    stop("family_name must be a single character string")
  }

  family_name <- toupper(trimws(family_name))

  # Get detailed results
  detailed_results <- is_threatened_peru(splist, target_df, return_details = TRUE)

  # Filter for threatened species and join with family information
  threatened_in_family <- detailed_results |>
    dplyr::filter(stringr::str_detect(Threat.Status, "Threatened")) |>
    dplyr::left_join(
      target_df |> dplyr::select(genus, species, family, threat_Category),
      by = c("Matched.Genus" = "genus", "Matched.Species" = "species")
    ) |>
    dplyr::filter(Family == family_name) |>
    dplyr::select(Orig.Name, Matched.Name, Family, Threat.Status, threat_category) |>
    dplyr::arrange(Threat_Category, Orig.Name)

  if (nrow(threatened_in_family) == 0) {
    message("No threatened species found in family ", family_name)
  }

  return(threatened_in_family)
}
