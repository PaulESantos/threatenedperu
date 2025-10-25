##' Match Species Names to Threatened Plant List of Peru
##'
##' @description
##' This function matches given species names against the internal database of threatened plant species in Peru.
##'
##' @param splist A vector containing the species list.
##' @param target_df Character string indicating which database version to use.
##'   Options are:
##'   \describe{
##'     \item{"original"}{Uses the original threatened species database (default)}
##'     \item{"updated"}{Uses the updated database with taxonomic synonyms}
##'   }
##'   Alternatively, you can provide a custom tibble with the required structure.
##'
##' @details
##' The function first attempts to directly match species names with exact
##' matches in the database (genus and specific epithet, or genus, specific
##' epithet, and infra species). If no exact match is found, the function
##' performs a fuzzy match using the fuzzyjoin package with an optimal string alignment distance of one, as implemented in stringdist.
##'
##' The maximum edit distance is intentionally set to one.
##'
##' The function matching_threatenedperu returns a tibble with new columns Matched.Genus, Matched.Species, and Matched.Infraspecies, containing the matched names or NA if no match was found.
##'
##' Additionally, a logical column is added for each function called, allowing users to see which functions were applied to each name during the matching process. If a process column shows `NA`, the corresponding function was not called for that name because it was already matched by a preceding function.
##'
##' @return
##' Returns a tibble with the matched names in
##' Matched.Genus, Matched.Species for binomial names,
##' and Matched.Infraspecies for valid infra species names.
##'
##' @export
#matching_threatenedperu <- function(splist, target_df = "original"){
#
#
#  # Validate and prepare target database based on user input
#  if (is.character(target_df)) {
#    target_prepared <- switch(
#      target_df,
#      "original" = {
#        if (!exists("threatenedperu")) {
#          stop("Original threatened species database 'threatenedperu' not found. ",
#               "Please ensure the package data is loaded.")
#        }
#        threatenedperu
#      },
#      "updated" = {
#        if (!exists("threatenedperu_syn")) {
#          stop("Updated threatened species database 'threatenedperu_syn' not found. ",
#               "Please ensure the package data is loaded.")
#        }
#        threatenedperu_syn
#      },
#      # Default case - invalid option
#      stop("Invalid target_df option. Must be 'original', 'updated', or a custom tibble. ",
#           "You provided: '", target_df, "'")
#    )
#  } else if (is.data.frame(target_df)) {
#    # User provided custom database - validate structure
#    required_cols <- c("genus", "species", "infraspecies", "threat_category")
#    missing_cols <- setdiff(required_cols, names(target_df))
#
#    if (length(missing_cols) > 0) {
#      stop("Custom target_df is missing required columns: ",
#           paste(missing_cols, collapse = ", "))
#    }
#
#    target_prepared <- target_df
#  } else {
#    stop("target_df must be either 'original', 'updated', or a data frame/tibble")
#  }
#
#  # Ensure target database has proper structure
#  if (nrow(target_prepared) == 0) {
#    stop("Target database is empty")
#  }
#
#  target_prepared
#
#  # Classify splist
#  splist_class <- .splist_classify(splist) |>
#    .transform_split_classify()
#
#  # Check binomial
#  non_binomial <- .check_binomial(splist_class, splist = splist)
#  if(length(non_binomial) != 0){
#    df <- splist_class[-non_binomial,]
#    df$sorter <- 1:nrow(df)
#  } else{
#    df <- splist_class
#  }
#
#  ### Add two Columns Matched.Genus & Matched.Species and fill with NA's
#  if(!all(c('Matched.Genus', 'Matched.Species',
#            'Matched.Infraspecies') %in% colnames(df))){
#    df <- df |>
#      tibble::add_column(Matched.Genus = as.character(NA),
#                         Matched.Species = as.character(NA),
#                         Matched.Infraspecies = as.character(NA))
#  }
#  df
#  # ---------------------------------------------------------------
#  # Node 1: Direct Match
#  Node_1_processed <- df |>
#    direct_match(target_df = target_prepared )
#
#  Node_1_TRUE <- Node_1_processed |>
#    dplyr::filter(direct_match == TRUE)
#
#  Node_1_FALSE <- Node_1_processed |>
#    dplyr::filter(direct_match == FALSE)
#
#  assertthat::assert_that(nrow(df) == (nrow(Node_1_TRUE) + nrow(Node_1_FALSE)))
#  Node_1_processed
#  # ---------------------------------------------------------------
#  # Node 2: Genus Match
#  Node_2_processed <- Node_1_FALSE |>
#    genus_match(target_prepared)
#
#  Node_2_TRUE <- Node_2_processed |>
#    dplyr::filter(genus_match == TRUE)
#
#  Node_2_FALSE <- Node_2_processed |>
#    dplyr::filter(genus_match == FALSE)
#
#  assertthat::assert_that(nrow(Node_2_processed) == (nrow(Node_2_TRUE) + nrow(Node_2_FALSE)))
#  Node_2_processed
#  # ---------------------------------------------------------------
#  # Node 3: Fuzzy Match Genus
#  Node_3_processed <- Node_2_FALSE |>
#    fuzzy_match_genus(target_prepared)
#
#  Node_3_TRUE <- Node_3_processed |>
#    dplyr::filter(fuzzy_match_genus == TRUE)
#
#  Node_3_FALSE <- Node_3_processed |>
#    dplyr::filter(fuzzy_match_genus == FALSE)
#  Node_3_processed
#  assertthat::assert_that(nrow(Node_3_processed) == (nrow(Node_3_TRUE) + nrow(Node_3_FALSE)))
#
#  # ---------------------------------------------------------------
#  # Node 4: Direct (Exact) Match Species within Genus
#  Node_4_input <- Node_3_TRUE |>
#    dplyr::bind_rows(Node_2_TRUE)
#
#  Node_4_processed <- Node_4_input |>
#    direct_match_species_within_genus(target_prepared)
#
#  Node_4_TRUE <- Node_4_processed |>
#    dplyr::filter(direct_match_species_within_genus == TRUE)
#
#  Node_4_FALSE <- Node_4_processed |>
#    dplyr::filter(direct_match_species_within_genus == FALSE)
#
#  assertthat::assert_that(nrow(Node_4_processed) == (nrow(Node_4_TRUE) + nrow(Node_4_FALSE)))
#  Node_4_processed
#  # ---------------------------------------------------------------
#  # Node 5a: Suffix Match Species within Genus
#  Node_5a_processed <- Node_4_FALSE |>
#    suffix_match_species_within_genus(target_prepared)
#
#  Node_5a_TRUE <- Node_5a_processed |>
#    dplyr::filter(suffix_match_species_within_genus == TRUE)
#
#  Node_5a_FALSE <- Node_5a_processed |>
#    dplyr::filter(suffix_match_species_within_genus == FALSE)
#
#  assertthat::assert_that(nrow(Node_4_FALSE) == (nrow(Node_5a_TRUE) + nrow(Node_5a_FALSE)))
#
#  # Node 5b: Fuzzy Match Species within Genus
#  Node_5b_input <- Node_5a_FALSE
#  Node_5b_processed <- Node_5b_input |>
#    fuzzy_match_species_within_genus(target_prepared)
#
#  Node_5b_TRUE <- Node_5b_processed |>
#    dplyr::filter(fuzzy_match_species_within_genus == TRUE)
#
#  Node_5b_FALSE <- Node_5b_processed |>
#    dplyr::filter(fuzzy_match_species_within_genus == FALSE)
#
#  assertthat::assert_that(nrow(Node_5b_input) == (nrow(Node_5b_TRUE) + nrow(Node_5b_FALSE)))
#
#  # Output
#  # Output A: matched
#  matched <- dplyr::bind_rows(Node_1_TRUE, Node_4_TRUE,
#                              Node_5a_TRUE, Node_5b_TRUE) |>
#    dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies)
#
#  # Output B: unmatched
#  unmatched <- dplyr::bind_rows(Node_3_FALSE, Node_5b_FALSE) |>
#    dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies)
#
#  # Concatenate Output A and Output B
#  res <- dplyr::bind_rows(matched,
#                          unmatched,
#                          .id='matched') |>
#    dplyr::mutate(matched = (matched == 1)) ## convert to Boolean
#
#  # ---------------------------------------------------------------
#  ### Check for infra species fuzzy matching
#  ## Add sorter value
#
#  infra_input <- res |>
#    dplyr::filter(!is.na(Orig.Infraspecies),
#                  is.na(Matched.Infraspecies))
#  infra_sorter <- as.vector(infra_input$sorter)
#  if(nrow(infra_input) != 0){
#    infra_matched <- infra_input |>
#      fuzzy_match_infraspecies_within_species(target_prepared)
#  }
#
#  # ---------------------------------------------------------------
#
#  # Check non binomial names
#  if(length(non_binomial) != 0){
#    genus_level <- matrix(nrow = length(non_binomial),
#                          ncol = length(names(res)))
#    colnames(genus_level) <- names(res)
#    genus_level <- as.data.frame(genus_level)
#    genus_level$sorter <- splist_class[splist_class$Rank == 1, "sorter"]
#    genus_level$Orig.Name <- splist_class[splist_class$Rank == 1, "Orig.Name"]
#    genus_level$Orig.Genus <- splist_class[splist_class$Rank == 1, "Orig.Genus"]
#    genus_level$Rank <- splist_class[splist_class$Rank == 1, "Rank"]
#  }
#
#  # ---------------------------------------------------------------
#  if(nrow(infra_input) == 0 & length(non_binomial) == 0){
#    out <- res
#    rm(res)
#  } else if (nrow(infra_input) != 0 & length(non_binomial) == 0){
#    out <- dplyr::bind_rows(res |>
#                              dplyr::filter(!sorter %in% infra_sorter) ,
#                            infra_matched)
#    rm(res, infra_matched)
#  }else if (nrow(infra_input) == 0 & length(non_binomial) != 0){
#    out <- dplyr::bind_rows(res,
#                            genus_level)
#    rm(res, genus_level)
#  }else if (nrow(infra_input) != 0 & length(non_binomial) != 0){
#    out <- dplyr::bind_rows(res |>
#                              dplyr::filter(!sorter %in% infra_sorter) ,
#                            infra_matched,
#                            genus_level)
#    rm(res, infra_matched, genus_level)
#  }
#
#  # ---------------------------------------------------------------
#  # Create final output with threat information
#  output <- out |>
#    dplyr::arrange(sorter) |>
#
#    # Add threat category information
#    dplyr::left_join(
#      target_prepared |>
#        dplyr::select(genus, species, infraspecies, threat_category),
#      by = c("Matched.Genus" = "genus",
#             "Matched.Species" = "species",
#             "Matched.Infraspecies" = "infraspecies")
#    ) |>
#
#    # Create matched name
#    dplyr::mutate(Matched.Name = dplyr::case_when(
#      is.na(Matched.Species) ~ stringr::str_to_sentence(Matched.Genus),
#      is.na(Matched.Infraspecies) ~ paste(stringr::str_to_sentence(Matched.Genus),
#                                          stringr::str_to_lower(Matched.Species),
#                                          sep = " "),
#      !is.na(Matched.Infraspecies) ~ paste(stringr::str_to_sentence(Matched.Genus),
#                                           stringr::str_to_lower(Matched.Species),
#                                           stringr::str_to_lower(Infra.Rank),
#                                           stringr::str_to_lower(Matched.Infraspecies),
#                                           sep = " "),
#      TRUE ~ "---"
#    )) |>
#
#    # Create matched rank
#    dplyr::mutate(Matched.Rank = dplyr::case_when(
#      !is.na(Matched.Genus) & !is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ 2,
#      !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) ~ 3,
#      is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ 1,
#      TRUE ~ NA_real_
#    )) |>
#
#    # Standardize original name
#    dplyr::mutate(Orig.Name = str_to_simple_cap(Orig.Name)) |>
#
#    # Compare ranks
#    dplyr::mutate(Comp.Rank = (Rank == Matched.Rank)) |>
#
#    # Create threat status
#    dplyr::mutate(Threat.Status = dplyr::case_when(
#      is.na(Matched.Genus) & is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ 'Not threatened',
#      !is.na(threat_category) ~ paste('Threatened -', threat_category),
#      Comp.Rank == TRUE & matched == TRUE ~ 'Threatened - Unknown category',
#      TRUE ~ 'Not threatened'
#    )) |>
#
#    # Fix matched name display
#    dplyr::mutate(Matched.Name = dplyr::if_else(is.na(Matched.Name),
#                                                "---",
#                                                Matched.Name)) |>
#
#    # Reorder columns
#    dplyr::relocate(c("sorter", "Orig.Name", "Matched.Name",
#                      "Threat.Status", "threat_category", "Orig.Genus", "Orig.Species",
#                      "Orig.Infraspecies", "Rank", "Infra.Rank", "Comp.Rank",
#                      "Matched.Genus", "Matched.Species", "Matched.Infraspecies",
#                      "Matched.Rank", "matched", "direct_match", "genus_match",
#                      "fuzzy_match_genus", "direct_match_species_within_genus" ,
#                      "suffix_match_species_within_genus",
#                      "fuzzy_match_species_within_genus", "fuzzy_genus_dist",
#                      "fuzzy_species_dist"))
#  output
#  # Clean
#  output <-
#    output |>
#    dplyr::filter(
#      !(stringr::str_detect(Matched.Name,
#                            "Haageocereus acranthus subsp. olowinskianus") & threat_category != "VU"),
#    ) |>
#    dplyr::group_by(Matched.Name) |>
#    dplyr::distinct() |>
#    dplyr::ungroup()
#
#  assertthat::assert_that(nrow(splist_class) == nrow(output))
#  return(output)
#}

# ---------------------------------------------------------------

#' Check taxonomic name updates
#'
#' This function compares a set of matched names (from the function
#' \code{matching_threatenedperu()}) against the reference dataset
#' \code{threatenedperu}. It determines how many of the queried names
#' are no longer accepted and have been taxonomically updated.
#'
#' @param resultado A tibble or data frame returned by
#'   \code{matching_threatenedperu()}, containing at least the column
#'   \code{Matched.Name}.
#' @param threatenedperu A tibble or data frame with taxonomic reference
#'   information, containing at least the columns \code{scientific_name}
#'   and \code{taxonomic_status}.
#'
#' @return A character string reporting the number and percentage of
#'   names that have been taxonomically updated.
#'
#' @examples
#' \dontrun{
#' resultado <- matching_threatenedperu(c(
#'   "Haageocereus acranthus subsp. olowinskianus",
#'   "Tecoma estans var. velutyno",
#'   "Haageocereus pseudomelanostele subsp. setosus",
#'   "Cleistocactus pachycladus"
#' )) |>
#'   tibble::as_tibble()
#'
#' check_name_update(resultado, threatenedperu)
#' }
#'
# check_name_update <- function(resultado, threatenedperu) {
#
#   # Filter matches
#   check <- threatenedperu:::threatenedperu |>
#     dplyr::semi_join(resultado, by = c("scientific_name" = "Matched.Name"))
#
#   # Calculations
#   total <- nrow(check)
#   updated <- sum(check$taxonomic_status != "Accepted", na.rm = TRUE)
#   percentage <- round(100 * updated / total, 2)
#
#   # Build message in English
#   message <- paste0(
#     "Out of ", total, " names checked, ",
#     updated, " (", percentage, "%) have been taxonomically updated.",
#     "\nTo review these species, use: target_type = \"updated\" ",
#     "based on the WCVP database."
#   )
#
#   return(message)
# }
#-------------------------------------------------------------------------
# matching_threatenedperu <- function(splist, target_df = "original"){

#   # ========================================================================
#   # SECTION 1: Target Database Selection and Validation
#   # ========================================================================
#
#   # Validate target_df parameter
#   if (!is.character(target_df) || length(target_df) != 1) {
#     stop("target_df must be a single character string: 'original' or 'updated'")
#   }
#
#   # Select appropriate database
#   target_prepared <- switch(
#     target_df,
#     "original" = {
#       if (!exists("threatenedperu", envir = parent.frame())) {
#         stop("Database 'threatenedperu' not found. Please load the package data.")
#       }
#       get("threatenedperu", envir = parent.frame())
#     },
#     "updated" = {
#       if (!exists("threatenedperu_syn", envir = parent.frame())) {
#         stop("Database 'threatenedperu_syn' not found. Please load the package data.")
#       }
#       get("threatenedperu_syn", envir = parent.frame())
#     },
#     stop("Invalid target_df value. Must be 'original' or 'updated'")
#   )
#
#   # Validate database structure
#   # Validate database structure
#   if(target_df == "original"){
#     required_cols <- c("genus", "species",
#                        "infraspecies", "infraspecies_2", "threat_category")
#   }else{
#     required_cols <- c("genus", "species",
#                        "infraspecies", "threat_category")
#   }
#
#   missing_cols <- setdiff(required_cols, names(target_prepared))
#   if (length(missing_cols) > 0) {
#     stop("Target database missing required columns: ",
#          paste(missing_cols, collapse = ", "))
#   }
#
#
#   # ========================================================================
#   # SECTION 2: Input Processing and Classification
#   # ========================================================================
#
#   # Classify species names into taxonomic components
#   splist_class <- .splist_classify(splist) |>
#     .transform_split_classify()
#
#   # Check for and handle non-binomial names
#   non_binomial <- .check_binomial(splist_class, splist = splist)
#
#   if(length(non_binomial) != 0){
#     df <- splist_class[-non_binomial,]
#     # df$sorter <- 1:nrow(df)
#   } else{
#     df <- splist_class
#   }
#
#   # Initialize matching columns
#   if(!all(c('Matched.Genus', 'Matched.Species', 'Matched.Infraspecies') %in%
#           colnames(df))){
#     df <- df |>
#       tibble::add_column(
#         Matched.Genus = as.character(NA),
#         Matched.Species = as.character(NA),
#         Matched.Infraspecies = as.character(NA)
#       )
#   }
#   #df
#   # ========================================================================
#   # SECTION 3: Hierarchical Matching Pipeline
#   # ========================================================================
#
#   # ---------------------------------------------------------------
#   # Node 1: Direct Match (Exact match for full name)
#   # ---------------------------------------------------------------
#   Node_1_processed <- df |>
#     direct_match(target_df = target_prepared)
#
#   Node_1_TRUE <- Node_1_processed |>
#     dplyr::filter(direct_match == TRUE)
#
#   Node_1_FALSE <- Node_1_processed |>
#     dplyr::filter(direct_match == FALSE)
#
#   assertthat::assert_that(nrow(df) == (nrow(Node_1_TRUE) + nrow(Node_1_FALSE)),
#                           msg = "Row count mismatch after Node 1: Direct Match")
#
#   # ---------------------------------------------------------------
#   # Node 2: Genus Match (Exact genus match)
#   # ---------------------------------------------------------------
#   Node_2_processed <- Node_1_FALSE |>
#     genus_match(target_prepared)
#
#   Node_2_TRUE <- Node_2_processed |>
#     dplyr::filter(genus_match == TRUE)
#
#   Node_2_FALSE <- Node_2_processed |>
#     dplyr::filter(genus_match == FALSE)
#
#   assertthat::assert_that(nrow(Node_2_processed) == (nrow(Node_2_TRUE) + nrow(Node_2_FALSE)),
#                           msg = "Row count mismatch after Node 2: Genus Match")
#
#   # ---------------------------------------------------------------
#   # Node 3: Fuzzy Match Genus (Approximate genus matching)
#   # ---------------------------------------------------------------
#   Node_3_processed <- Node_2_FALSE |>
#     fuzzy_match_genus(target_prepared)
#
#   Node_3_TRUE <- Node_3_processed |>
#     dplyr::filter(fuzzy_match_genus == TRUE)
#
#   Node_3_FALSE <- Node_3_processed |>
#     dplyr::filter(fuzzy_match_genus == FALSE)
#
#   assertthat::assert_that(nrow(Node_3_processed) == (nrow(Node_3_TRUE) + nrow(Node_3_FALSE)),
#                           msg = "Row count mismatch after Node 3: Fuzzy Genus Match")
#
#   # ---------------------------------------------------------------
#   # Node 4: Direct Match Species within Genus
#   # ---------------------------------------------------------------
#   Node_4_input <- Node_3_TRUE |>
#     dplyr::bind_rows(Node_2_TRUE)
#
#   Node_4_processed <- Node_4_input |>
#     direct_match_species_within_genus(target_prepared)
#
#   Node_4_TRUE <- Node_4_processed |>
#     dplyr::filter(direct_match_species_within_genus == TRUE)
#
#   Node_4_FALSE <- Node_4_processed |>
#     dplyr::filter(direct_match_species_within_genus == FALSE)
#
#   assertthat::assert_that(nrow(Node_4_processed) == (nrow(Node_4_TRUE) + nrow(Node_4_FALSE)),
#                           msg = "Row count mismatch after Node 4: Species Match")
#
#   # ---------------------------------------------------------------
#   # Node 5a: Suffix Match Species within Genus
#   # ---------------------------------------------------------------
#   Node_5a_processed <- Node_4_FALSE |>
#     suffix_match_species_within_genus(target_prepared)
#
#   Node_5a_TRUE <- Node_5a_processed |>
#     dplyr::filter(suffix_match_species_within_genus == TRUE)
#
#   Node_5a_FALSE <- Node_5a_processed |>
#     dplyr::filter(suffix_match_species_within_genus == FALSE)
#
#   assertthat::assert_that(nrow(Node_4_FALSE) == (nrow(Node_5a_TRUE) + nrow(Node_5a_FALSE)),
#                           msg = "Row count mismatch after Node 5a: Suffix Match")
#
#   # ---------------------------------------------------------------
#   # Node 5b: Fuzzy Match Species within Genus
#   # ---------------------------------------------------------------
#   Node_5b_input <- Node_5a_FALSE
#   Node_5b_processed <- Node_5b_input |>
#     fuzzy_match_species_within_genus(target_prepared)
#
#   Node_5b_TRUE <- Node_5b_processed |>
#     dplyr::filter(fuzzy_match_species_within_genus == TRUE)
#
#   Node_5b_FALSE <- Node_5b_processed |>
#     dplyr::filter(fuzzy_match_species_within_genus == FALSE)
#
#   assertthat::assert_that(nrow(Node_5b_input) == (nrow(Node_5b_TRUE) + nrow(Node_5b_FALSE)),
#                           msg = "Row count mismatch after Node 5b: Fuzzy Species Match")
#
#   # ========================================================================
#   # SECTION 4: Combine Results and Handle Infraspecies
#   # ========================================================================
#
#   # Combine matched and unmatched results
#   matched <- dplyr::bind_rows(Node_1_TRUE, Node_4_TRUE,
#                               Node_5a_TRUE, Node_5b_TRUE) |>
#     dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies)
#
#   unmatched <- dplyr::bind_rows(Node_3_FALSE, Node_5b_FALSE) |>
#     dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies)
#
#   res <- dplyr::bind_rows(matched, unmatched, .id='matched') |>
#     dplyr::mutate(matched = (matched == 1))
#
#   # ---------------------------------------------------------------
#   # Infraspecies Fuzzy Matching
#   # ---------------------------------------------------------------
#   infra_input <- res |>
#     dplyr::filter(!is.na(Orig.Infraspecies),
#                   is.na(Matched.Infraspecies))
#
#   infra_sorter <- as.vector(infra_input$sorter)
#
#   if(nrow(infra_input) != 0){
#     infra_matched <- infra_input |>
#       fuzzy_match_infraspecies_within_species(target_prepared)
#   }
#
#   # ---------------------------------------------------------------
#   # Handle Genus-level Names and NA values
#   # ---------------------------------------------------------------
#   if(length(non_binomial) != 0){
#     genus_level <- matrix(nrow = length(non_binomial),
#                           ncol = length(names(res)))
#     colnames(genus_level) <- names(res)
#     genus_level <- as.data.frame(genus_level)
#     genus_level$sorter <- splist_class[splist_class$Rank == 1, "sorter"]
#     genus_level$Orig.Name <- splist_class[splist_class$Rank == 1, "Orig.Name"]
#     genus_level$Orig.Genus <- splist_class[splist_class$Rank == 1, "Orig.Genus"]
#     genus_level$Rank <- splist_class[splist_class$Rank == 1, "Rank"]
#   }
#
#   # ========================================================================
#   # SECTION 5: Consolidate All Results
#   # ========================================================================
#
#   if(nrow(infra_input) == 0 & length(non_binomial) == 0){
#     out <- res
#     rm(res)
#   } else if (nrow(infra_input) != 0 & length(non_binomial) == 0){
#     out <- dplyr::bind_rows(res |>
#                               dplyr::filter(!sorter %in% infra_sorter),
#                             infra_matched)
#     rm(res, infra_matched)
#   } else if (nrow(infra_input) == 0 & length(non_binomial) != 0){
#     out <- dplyr::bind_rows(res, genus_level)
#     rm(res, genus_level)
#   } else if (nrow(infra_input) != 0 & length(non_binomial) != 0){
#     out <- dplyr::bind_rows(res |>
#                               dplyr::filter(!sorter %in% infra_sorter),
#                             infra_matched,
#                             genus_level)
#     rm(res, infra_matched, genus_level)
#   }
#
#   # ========================================================================
#   # SECTION 6: Add Threat Information and Format Output
#   # ========================================================================
#
#   output <- out |>
#     dplyr::arrange(sorter) |>
#
#     # Join with threat category information
#     dplyr::left_join(
#       target_prepared |>
#         dplyr::select(genus, species, infraspecies, threat_category), #|>
#       # dplyr::distinct(genus, species, infraspecies, .keep_all = TRUE),
#       by = c("Matched.Genus" = "genus",
#              "Matched.Species" = "species",
#              "Matched.Infraspecies" = "infraspecies")
#     ) |>
#
#     # Create properly formatted matched name
#     dplyr::mutate(Matched.Name = dplyr::case_when(
#       is.na(Matched.Species) ~ stringr::str_to_sentence(Matched.Genus),
#       is.na(Matched.Infraspecies) ~ paste(
#         stringr::str_to_sentence(Matched.Genus),
#         stringr::str_to_lower(Matched.Species),
#         sep = " "
#       ),
#       !is.na(Matched.Infraspecies) ~ paste(
#         stringr::str_to_sentence(Matched.Genus),
#         stringr::str_to_lower(Matched.Species),
#         stringr::str_to_lower(Infra.Rank),
#         stringr::str_to_lower(Matched.Infraspecies),
#         sep = " "
#       ),
#       TRUE ~ "---"
#     )) |>
#
#     # Determine matched rank
#     dplyr::mutate(Matched.Rank = dplyr::case_when(
#       !is.na(Matched.Genus) & !is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ 2,
#       !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) ~ 3,
#       is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ 1,
#       TRUE ~ NA_real_
#     )) |>
#
#     # Standardize original name format
#     dplyr::mutate(Orig.Name = str_to_simple_cap(Orig.Name)) |>
#
#     # Compare taxonomic ranks
#     dplyr::mutate(Comp.Rank = (Rank == Matched.Rank)) |>
#
#     # Assign threat status
#     dplyr::mutate(Threat.Status = dplyr::case_when(
#       is.na(Matched.Genus) & is.na(Matched.Species) & is.na(Matched.Infraspecies) ~
#         'Not threatened',
#       !is.na(threat_category) ~ threat_category,
#       # Comp.Rank == TRUE & matched == TRUE ~ 'Threatened - Unknown category',
#       TRUE ~ 'Not threatened'
#     )) |>
#
#     # Fix display of unmatched names
#     dplyr::mutate(Matched.Name = dplyr::if_else(
#       is.na(Matched.Name), "---", Matched.Name
#     )) |>
#
#     # Reorder columns for clarity
#     dplyr::relocate(c(
#       "sorter", "Orig.Name", "Matched.Name", "Threat.Status", "threat_category",
#       "Orig.Genus", "Orig.Species", "Orig.Infraspecies",
#       "Rank", "Infra.Rank", "Comp.Rank",
#       "Matched.Genus", "Matched.Species", "Matched.Infraspecies", "Matched.Rank",
#       "matched", "direct_match", "genus_match", "fuzzy_match_genus",
#       "direct_match_species_within_genus", "suffix_match_species_within_genus",
#       "fuzzy_match_species_within_genus", "fuzzy_genus_dist", "fuzzy_species_dist"
#     ))
#
#   # ========================================================================
#   # SECTION 7: Data Cleaning and Final Validation
#   # ========================================================================
#
#   # Remove specific duplicate matches and edge cases
#   if(target_df == "original"){
#     output <- output |>
#       #dplyr::filter(
#       # !(stringr::str_detect(Matched.Name,
#       #                      "Haageocereus acranthus subsp. olowinskianus") &
#       #   threat_category != "VU")
#       # ) |>
#       dplyr::group_by(Matched.Name) |>
#       dplyr::distinct() |>
#       dplyr::ungroup()
#   } else if(target_df == "updated"){
#     output <- output |>
#       dplyr::filter(
#         !(stringr::str_detect(Matched.Name,
#                               "Haageocereus acranthus subsp. acranthus") &
#             threat_category != "VU")
#       ) |>
#       dplyr::group_by(Matched.Name) |>
#       dplyr::distinct() |>
#       dplyr::ungroup()
#   }
#
#
#   # Final validation
#   assertthat::assert_that(
#     nrow(splist_class) == nrow(output),
#     msg = "Final output row count does not match input"
#   )
#
#   return(output)
# }


##################### matching_threatenedperu#########################
#' Match Species Names to Threatened Plant List of Peru
#'
#' @description
#' This function matches given species names against the internal database of threatened
#' plant species in Peru. It uses a hierarchical matching strategy that includes direct
#' matching, genus-level matching, fuzzy matching, and suffix matching to maximize
#' successful matches while maintaining accuracy.
#'
#' @param splist A character vector containing the species names to be matched.
#' @param source Character string specifying which database version to use.
#'   Options are:
#'   \itemize{
#'     \item \code{"original"} (default): Uses the original threatened species database
#'     \item \code{"updated"}: Uses the updated database with synonyms
#'   }
#'
#' @details
#' The matching process follows a hierarchical pipeline with robust handling of
#' infraspecific ranks at two levels (when supported by the database).
#'
#' **Ambiguous Matches:**
#' When multiple candidates have identical match scores (string distances), the
#' algorithm automatically selects the first match and issues a warning. To review
#' ambiguous matches for quality control, use \code{\link{get_ambiguous_matches}}
#' on the result object.
#'
#' @return
#' A tibble with detailed matching results including matched names, threat status,
#' and matching methodology information.
#'
#' @seealso
#' \code{\link{is_threatened_peru}} for a simplified interface
#' \code{\link{get_ambiguous_matches}} to retrieve ambiguous match details
#'
#' @examples
#' \dontrun{
#' species_list <- c("Cattleya maxima", "Polylepis incana")
#' results <- matching_threatenedperu(species_list, source = "original")
#' }
#'
#' @export
###  matching_threatenedperu_2 <- function(splist, source = "original") {
###
###    # ==========================================================================
###    # SECTION 1: Target Database Selection and Validation
###    # ==========================================================================
###
###    # Validate source parameter
###    if (!is.character(source) || length(source) != 1) {
###      stop("source must be a single character string: 'original' or 'updated'",
###           call. = FALSE)
###    }
###
###    if (!source %in% c("original", "updated")) {
###      stop(
###        "Invalid source value: '", source, "'. Must be 'original' or 'updated'",
###        call. = FALSE
###      )
###    }
###
###    # Determine if infraspecies_2 is supported
###    use_infraspecies_2 <- (source == "original")
###
###    # Load database using internal function
###    target_prepared <- tryCatch({
###      get_threatened_data(type = source)
###    }, error = function(e) {
###      stop(
###        "Failed to load database '", source, "'.\n",
###        "Error: ", e$message,
###        call. = FALSE
###      )
###    })
###
###    target_prepared <- target_prepared |>
###      dplyr::mutate(
###        dplyr::across(
###          dplyr::where(is.character),
###          ~stringr::str_squish(.)  # Elimina espacios extra internos y externos
###        )
###      )
###
###    # Validate database structure
###    if (use_infraspecies_2) {
###      required_cols <- c("genus", "species", "tag", "infraspecies",
###                         "infraspecies_2", "threat_category")
###    } else {
###      required_cols <- c("genus", "species", "tag_acc", "infraspecies",
###                         "threat_category")
###    }
###
###    missing_cols <- setdiff(required_cols, names(target_prepared))
###    if (length(missing_cols) > 0) {
###      stop(
###        "Database '", source, "' missing required columns: ",
###        paste(missing_cols, collapse = ", "),
###        call. = FALSE
###      )
###    }
###
###    # ==========================================================================
###    # SECTION 2: Input Processing and Classification
###    # ==========================================================================
###
###    # Classify species names into taxonomic components
###    splist_class <- .splist_classify(splist) |>
###      .transform_split_classify()
###
###    # Check for and handle non-binomial names
###    non_binomial <- .check_binomial(splist_class, splist = splist)
###
###    if (length(non_binomial) != 0) {
###      df <- splist_class[-non_binomial, ]
###    } else {
###      df <- splist_class
###    }
###
###
###    # ==========================================================================
###    # SECTION 2B: Handle Empty Dataframe BEFORE Adding Columns
###    # ==========================================================================
###
###    # CRITICAL: Check if df is empty BEFORE attempting to add columns
###    if (nrow(df) == 0) {
###      message(
###        "All input names were filtered out (genus-level or invalid names).\n",
###        "Returning empty result with 'Not threatened' status."
###      )
###
###      # Create empty result matching the expected structure
###      empty_result <- splist_class |>
###        dplyr::select(sorter, Orig.Name, Orig.Genus, Orig.Species,
###                      Orig.Infraspecies, Orig.Infraspecies_2,
###                      Rank, Orig.Infra.Rank, Orig.Infra.Rank_2, Author) |>
###        dplyr::mutate(
###          Matched.Name = "---",
###          Matched.Genus = NA_character_,
###          Matched.Species = NA_character_,
###          Matched.Infra.Rank = NA_character_,
###          Matched.Infraspecies = NA_character_,
###          Matched.Infra.Rank_2 = NA_character_,
###          Matched.Infraspecies_2 = NA_character_,
###          Matched.Rank = NA_integer_,
###          Matched.Rank.Calculated = NA_integer_,
###          valid_rank = FALSE,
###          matched = FALSE,
###          threat_category = NA_character_,
###          accepted_name_author = "---",
###          Threat.Status = "Not threatened",
###          Comp.Rank = FALSE,
###          Match.Level = "No match"
###        ) |>
###        dplyr::relocate(
###          sorter, Orig.Name, Matched.Name, Threat.Status,
###          Author, accepted_name_author, Matched.Rank,
###          Comp.Rank, Match.Level
###        )
###
###      # Add metadata
###      attr(empty_result, "use_infraspecies_2") <- use_infraspecies_2
###      attr(empty_result, "target_database") <- source
###      attr(empty_result, "matching_date") <- Sys.Date()
###      attr(empty_result, "n_input") <- nrow(splist_class)
###      attr(empty_result, "n_matched") <- 0
###      attr(empty_result, "match_rate") <- 0
###
###      return(empty_result)
###    }
###
###
###
###    # ==========================================================================
###    # SECTION 2C: Initialize ALL Matching Columns (only if df has rows)
###    # ==========================================================================
###
###    all_matching_cols <- c(
###      'Matched.Genus',
###      'Matched.Species',
###      'Matched.Infra.Rank',
###      'Matched.Infraspecies',
###      'Matched.Infra.Rank_2',
###      'Matched.Infraspecies_2'
###    )
###
###    for (col in all_matching_cols) {
###      if (!col %in% colnames(df)) {
###        df[[col]] <- NA_character_  # ← Ahora seguro, df tiene filas
###      }
###    }
###
###    if (!'Orig.Infraspecies' %in% colnames(df)) {
###      df$Orig.Infraspecies <- NA_character_
###    }
###
###    if (!'Orig.Infraspecies_2' %in% colnames(df)) {
###      df$Orig.Infraspecies_2 <- NA_character_
###    }
###
###    attr(df, "use_infraspecies_2") <- use_infraspecies_2
###
###    if (!use_infraspecies_2) {
###      df$Orig.Infraspecies_2 <- NA_character_
###      df$Matched.Infraspecies_2 <- NA_character_
###
###      message(
###        "Note: Using database '", source,
###        "' which supports the updated names of species listed in DS 043-2006-AG."
###      )
###    }
###
###
###
###    # ==========================================================================
###    # SECTION 2C: Validate Rank 4 with Database Compatibility
###    # ==========================================================================
###
###    if (!use_infraspecies_2 && any(df$Rank == 4, na.rm = TRUE)) {
###      rank4_count <- sum(df$Rank == 4, na.rm = TRUE)
###
###      warning(
###        rank4_count, " species with Rank 4 detected. ",
###        "Database '", source, "' does not support infraspecies_2. ",
###        "These species will not be matched.",
###        call. = FALSE,
###        immediate. = TRUE
###      )
###    }
###
###    # ==========================================================================
###    # SECTION 3: Hierarchical Matching Pipeline
###    # ==========================================================================
###
###    # -------------------------------------------------------------------------
###    # Node 1: Direct Match (Exact match for full name)
###    # -------------------------------------------------------------------------
###    Node_1_processed <- df |>
###      direct_match(target_df = target_prepared,
###                   source = source)
###
###    Node_1_TRUE <- Node_1_processed |>
###      dplyr::filter(direct_match == TRUE)
###
###    Node_1_FALSE <- Node_1_processed |>
###      dplyr::filter(direct_match == FALSE)
###
###    assertthat::assert_that(
###      nrow(df) == (nrow(Node_1_TRUE) + nrow(Node_1_FALSE)),
###      msg = "Row count mismatch after Node 1: Direct Match"
###    )
###
###    # -------------------------------------------------------------------------
###    # Node 2: Genus Match (Exact genus match)
###    # -------------------------------------------------------------------------
###    Node_2_processed <- Node_1_FALSE |>
###      genus_match(target_prepared)
###
###    Node_2_TRUE <- Node_2_processed |>
###      dplyr::filter(genus_match == TRUE)
###
###    Node_2_FALSE <- Node_2_processed |>
###      dplyr::filter(genus_match == FALSE)
###
###    assertthat::assert_that(
###      nrow(Node_2_processed) == (nrow(Node_2_TRUE) + nrow(Node_2_FALSE)),
###      msg = "Row count mismatch after Node 2: Genus Match"
###    )
###
###    # -------------------------------------------------------------------------
###    # Node 3: Fuzzy Match Genus (Approximate genus matching)
###    # -------------------------------------------------------------------------
###    Node_3_processed <- Node_2_FALSE |>
###      fuzzy_match_genus(target_prepared)
###
###    Node_3_TRUE <- Node_3_processed |>
###      dplyr::filter(fuzzy_match_genus == TRUE)
###
###    Node_3_FALSE <- Node_3_processed |>
###      dplyr::filter(fuzzy_match_genus == FALSE)
###
###    assertthat::assert_that(
###      nrow(Node_3_processed) == (nrow(Node_3_TRUE) + nrow(Node_3_FALSE)),
###      msg = "Row count mismatch after Node 3: Fuzzy Genus Match"
###    )
###
###    # -------------------------------------------------------------------------
###    # Node 4: Direct Match Species within Genus
###    # -------------------------------------------------------------------------
###    Node_4_input <- Node_3_TRUE |>
###      dplyr::bind_rows(Node_2_TRUE)
###
###    Node_4_processed <- Node_4_input |>
###      direct_match_species_within_genus(target_prepared)
###
###    Node_4_TRUE <- Node_4_processed |>
###      dplyr::filter(direct_match_species_within_genus == TRUE)
###
###    Node_4_FALSE <- Node_4_processed |>
###      dplyr::filter(direct_match_species_within_genus == FALSE)
###
###    assertthat::assert_that(
###      nrow(Node_4_processed) == (nrow(Node_4_TRUE) + nrow(Node_4_FALSE)),
###      msg = "Row count mismatch after Node 4: Species Match"
###    )
###
###    # -------------------------------------------------------------------------
###    # Node 5a: Suffix Match Species within Genus
###    # -------------------------------------------------------------------------
###    Node_5a_processed <- Node_4_FALSE |>
###      suffix_match_species_within_genus(target_prepared)
###
###    Node_5a_TRUE <- Node_5a_processed |>
###      dplyr::filter(suffix_match_species_within_genus == TRUE)
###
###    Node_5a_FALSE <- Node_5a_processed |>
###      dplyr::filter(suffix_match_species_within_genus == FALSE)
###
###    assertthat::assert_that(
###      nrow(Node_4_FALSE) == (nrow(Node_5a_TRUE) + nrow(Node_5a_FALSE)),
###      msg = "Row count mismatch after Node 5a: Suffix Match"
###    )
###
###    # -------------------------------------------------------------------------
###    # Node 5b: Fuzzy Match Species within Genus
###    # -------------------------------------------------------------------------
###    Node_5b_input <- Node_5a_FALSE |>
###      dplyr::filter(!is.na(Orig.Species))
###
###    Node_5b_processed <- Node_5b_input |>
###      fuzzy_match_species_within_genus(target_prepared)
###
###    Node_5b_TRUE <- Node_5b_processed |>
###      dplyr::filter(fuzzy_match_species_within_genus == TRUE)
###
###    Node_5b_FALSE <- Node_5b_processed |>
###      dplyr::filter(fuzzy_match_species_within_genus == FALSE)
###
###    assertthat::assert_that(
###      nrow(Node_5b_input) == (nrow(Node_5b_TRUE) + nrow(Node_5b_FALSE)),
###      msg = "Row count mismatch after Node 5b: Fuzzy Species Match"
###    )
###
###    # ==========================================================================
###    # SECTION 4: Combine Results and Validate Ranks CRITICAL SECTION
###    # ==========================================================================
###
###    combined <- dplyr::bind_rows(
###      Node_1_TRUE,
###      Node_2_TRUE |> dplyr::filter(!is.na(Matched.Genus) & Rank == 1),
###      Node_4_TRUE,
###      Node_5a_TRUE,
###      Node_5b_TRUE
###    )
###
###    # =========================================================================
###    # CRITICAL VALIDATION: Matched Rank vs Declared Rank
###    # =========================================================================
###    # This validation prevents FALSE POSITIVES where:
###    # - User inputs: "Cattleya maxima var. alba" (Rank 3, doesn't exist)
###    # - Database has: "Cattleya maxima" (Rank 2, exists)
###    # - Without validation: Would incorrectly match and return "Threatened"
###    # - With validation: Correctly rejects match, returns "Not threatened"
###    # =========================================================================
###
###    combined <- combined |>
###      dplyr::mutate(
###        # Calculate the actual matched rank based on populated columns
###        Matched.Rank.Calculated = dplyr::case_when(
###          # Rank 1: Only genus matched
###          !is.na(Matched.Genus) &
###            is.na(Matched.Species) ~ 1L,
###
###          # Rank 2: Genus + species matched (binomial), NO infraspecies
###          !is.na(Matched.Genus) &
###            !is.na(Matched.Species) &
###            is.na(Matched.Infra.Rank) &
###            is.na(Matched.Infraspecies) ~ 2L,
###
###          # Rank 3: Genus + species + infraspecies level 1 (trinomial)
###          !is.na(Matched.Genus) &
###            !is.na(Matched.Species) &
###            !is.na(Matched.Infra.Rank) &
###            !is.na(Matched.Infraspecies) &
###            is.na(Matched.Infra.Rank_2) &
###            is.na(Matched.Infraspecies_2) ~ 3L,
###
###          # Rank 4: All levels matched (quaternomial)
###          use_infraspecies_2 &
###            !is.na(Matched.Genus) &
###            !is.na(Matched.Species) &
###            !is.na(Matched.Infra.Rank) &
###            !is.na(Matched.Infraspecies) &
###            !is.na(Matched.Infra.Rank_2) &
###            !is.na(Matched.Infraspecies_2) ~ 4L,
###
###          # No valid match
###          TRUE ~ NA_integer_
###        ),
###
###        # Validate: Declared Rank MUST match Matched Rank
###        valid_rank = (Rank == Matched.Rank.Calculated)
###      )
###
###    # =========================================================================
###    # Filter: Keep ONLY matches with valid rank correspondence
###    # =========================================================================
###
###    matched <- combined |>
###      dplyr::filter(valid_rank == TRUE, !is.na(Matched.Rank.Calculated))
###
###    # Identify INVALID matches (false positives to be rejected)
###    invalid_matches <- combined |>
###      dplyr::filter(valid_rank == FALSE | is.na(Matched.Rank.Calculated))
###
###    # Add diagnostic info for invalid matches
###    if (nrow(invalid_matches) > 0) {
###      message(
###        "Info: ", nrow(invalid_matches), " potential matches were rejected due to rank mismatch.\n",
###        "  (e.g., trinomial input matching with binomial in database)"
###      )
###    }
###
###    # Combine all unmatched records
###    unmatched <- dplyr::bind_rows(
###      invalid_matches,  # ← CRITICAL: Include rejected false positives
###      Node_3_FALSE,
###      Node_5b_FALSE
###    )
###
###    # ==========================================================================
###    # SECTION 5: Infraspecies Matching (Nodes 6-7)
###    # ==========================================================================
###
###    # -------------------------------------------------------------------------
###    # Node 6a: Direct Match Infraspecific Rank
###    # -------------------------------------------------------------------------
###    Node_6_input <- unmatched |>
###      dplyr::filter(
###        !is.na(Orig.Infra.Rank),
###        !is.na(Matched.Genus),
###        !is.na(Matched.Species),
###        is.na(Matched.Infraspecies)
###      )
###
###    Node_6a_processed <- Node_6_input |>
###      direct_match_infra_rank_within_species(target_df = target_prepared,
###                                             source = source)
###
###    Node_6a_TRUE <- Node_6a_processed |>
###      dplyr::filter(direct_match_infra_rank == TRUE)
###
###    Node_6a_FALSE <- Node_6a_processed |>
###      dplyr::filter(direct_match_infra_rank == FALSE)
###
###    # -------------------------------------------------------------------------
###    # Node 6b: Fuzzy Match Infraspecific Epithet
###    # -------------------------------------------------------------------------
###    Node_6b_processed <- Node_6a_TRUE |>
###      fuzzy_match_infraspecies_within_species(target_df = target_prepared,
###                                              source = source) |>
###      dplyr::mutate(
###        # Recalculate matched rank
###        Matched.Rank.Calculated = dplyr::case_when(
###          !is.na(Matched.Genus) &
###            !is.na(Matched.Species) &
###            !is.na(Matched.Infra.Rank) &
###            !is.na(Matched.Infraspecies) &
###            is.na(Matched.Infraspecies_2) ~ 3L,
###          TRUE ~ NA_integer_
###        ),
###        # Validate rank
###        valid_rank = (Rank == Matched.Rank.Calculated)
###      )
###
###    Node_6b_TRUE <- Node_6b_processed |>
###      dplyr::filter(
###        fuzzy_match_infraspecies == TRUE,
###        valid_rank == TRUE,
###        Rank == 3
###      )
###
###    Node_6b_FALSE <- Node_6b_processed |>
###      dplyr::filter(
###        fuzzy_match_infraspecies == FALSE |
###          valid_rank == FALSE |
###          Rank != 3
###      )
###
###    # -------------------------------------------------------------------------
###    # Node 7: Infraspecies Level 2 Fuzzy Matching
###    # -------------------------------------------------------------------------
###    if (use_infraspecies_2) {
###      Node_7_input <- Node_6b_FALSE |>
###        dplyr::filter(
###          Rank == 4,
###          !is.na(Orig.Infraspecies_2),
###          is.na(Matched.Infraspecies_2),
###          !is.na(Matched.Infraspecies)
###        )
###
###      if (nrow(Node_7_input) > 0) {
###        Node_7_processed <- Node_7_input |>
###          fuzzy_match_infraspecies2_within_infraspecies(target_prepared) |>
###          dplyr::mutate(
###            # Recalculate matched rank
###            Matched.Rank.Calculated = dplyr::case_when(
###              !is.na(Matched.Genus) &
###                !is.na(Matched.Species) &
###                !is.na(Matched.Infra.Rank) &
###                !is.na(Matched.Infraspecies) &
###                Orig.Infra.Rank_2 == "F." &
###                !is.na(Matched.Infraspecies_2) ~ 4L,
###              TRUE ~ NA_integer_
###            ),
###            # Validate rank
###            valid_rank = (Rank == Matched.Rank.Calculated)
###          )
###
###        Node_7_TRUE <- Node_7_processed |>
###          dplyr::filter(
###            valid_rank == TRUE,
###            fuzzy_match_infraspecies_2 == TRUE
###          ) |>
###          dplyr::mutate(
###            Matched.Infra.Rank_2 = "F."
###          )
###
###        Node_7_FALSE <- Node_7_processed |>
###          dplyr::filter(
###            valid_rank == FALSE | fuzzy_match_infraspecies_2 == FALSE
###          )
###
###      } else {
###        Node_7_TRUE <- Node_6b_FALSE[0, ]
###        Node_7_FALSE <- Node_6b_FALSE[0, ]
###      }
###
###      # Combine all matches
###      matched_f <- dplyr::bind_rows(
###        matched,
###        Node_6b_TRUE,
###        Node_7_TRUE
###      )
###
###      unmatched_f <- dplyr::bind_rows(
###        Node_6a_FALSE,
###        Node_7_FALSE
###      )
###
###      res <- dplyr::bind_rows(matched_f, unmatched_f, .id = 'matched') |>
###        dplyr::mutate(matched = (matched == 1))
###
###    } else {
###      # Without infraspecies_2, no Node 7
###      matched_f <- dplyr::bind_rows(matched, Node_6b_TRUE)
###
###      res <- dplyr::bind_rows(matched_f, Node_6b_FALSE, .id = 'matched') |>
###        dplyr::mutate(matched = (matched == 1))
###    }
###
###    # ==========================================================================
###    # SECTION 6: Add Threat Information and Format Output
###    # ==========================================================================
###
###    base_df <- splist_class |>
###      dplyr::select(
###        sorter, Orig.Name, Orig.Genus, Orig.Species,
###        Orig.Infraspecies, Orig.Infraspecies_2,
###        Rank, Orig.Infra.Rank, Orig.Infra.Rank_2, Author
###      )
###
###    cols_to_exclude <- c(
###      "Orig.Name", "Orig.Genus", "Orig.Species",
###      "Orig.Infraspecies", "Orig.Infraspecies_2",
###      "Rank", "Orig.Infra.Rank", "Orig.Infra.Rank_2", "Author"
###    )
###
###    res_complete <- base_df |>
###      dplyr::left_join(
###        res |> dplyr::select(-dplyr::any_of(cols_to_exclude)),
###        by = "sorter"
###      )
###
###    res_complete <- res_complete |>
###      dplyr::mutate(
###        matched = tidyr::replace_na(matched, FALSE),
###        Matched.Genus = dplyr::if_else(is.na(Matched.Genus),
###                                       NA_character_, Matched.Genus),
###        Matched.Species = dplyr::if_else(is.na(Matched.Species),
###                                         NA_character_, Matched.Species),
###        Matched.Infraspecies = dplyr::if_else(is.na(Matched.Infraspecies),
###                                              NA_character_, Matched.Infraspecies),
###        Matched.Infraspecies_2 = dplyr::if_else(is.na(Matched.Infraspecies_2),
###                                                NA_character_,
###                                                Matched.Infraspecies_2)
###      )
###
###    # =========================================================================
###    # CRITICAL: Add valid_rank if not present (from SECTION 4)
###    # =========================================================================
###    # valid_rank should have been calculated in SECTION 4, but ensure it exists
###    if (!"valid_rank" %in% colnames(res_complete)) {
###      warning(
###        "Column 'valid_rank' not found in res_complete. ",
###        "This indicates the validation in SECTION 4 was not applied. ",
###        "Calculating it now as a fallback.",
###        call. = FALSE,
###        immediate. = TRUE
###      )
###
###      res_complete <- res_complete |>
###        dplyr::mutate(
###          Matched.Rank.Calculated = dplyr::case_when(
###            !is.na(Matched.Genus) & is.na(Matched.Species) ~ 1L,
###            !is.na(Matched.Genus) & !is.na(Matched.Species) &
###              is.na(Matched.Infra.Rank) & is.na(Matched.Infraspecies) ~ 2L,
###            !is.na(Matched.Genus) & !is.na(Matched.Species) &
###              !is.na(Matched.Infra.Rank) & !is.na(Matched.Infraspecies) &
###              is.na(Matched.Infra.Rank_2) & is.na(Matched.Infraspecies_2) ~ 3L,
###            use_infraspecies_2 &
###              !is.na(Matched.Genus) & !is.na(Matched.Species) &
###              !is.na(Matched.Infra.Rank) & !is.na(Matched.Infraspecies) &
###              !is.na(Matched.Infra.Rank_2) & !is.na(Matched.Infraspecies_2) ~ 4L,
###            TRUE ~ NA_integer_
###          ),
###          valid_rank = (Rank == Matched.Rank.Calculated)
###        )
###    }
###
###    # =========================================================================
###    # Join threat information based on Rank AND valid_rank
###    # =========================================================================
###
###    if (use_infraspecies_2) {
###
###      # -----------------------------------------------------------------------
###      # Prepare target_threat data
###      # -----------------------------------------------------------------------
###      target_threat <- target_prepared |>
###        dplyr::select(
###          genus, species, tag, infraspecies, infraspecies_2,
###          threat_category, accepted_name_author
###        ) |>
###        dplyr::mutate(tag = toupper(tag)) |>
###        dplyr::distinct(genus, species, tag, infraspecies, infraspecies_2,
###                        .keep_all = TRUE)
###
###      # -----------------------------------------------------------------------
###      # RANK 2: Binomial matches with valid rank
###      # -----------------------------------------------------------------------
###      output_rank2 <- res_complete |>
###        dplyr::filter(
###          Rank == 2,
###          matched == TRUE,
###          valid_rank == TRUE  # ← CRITICAL: Only valid matches
###        ) |>
###        dplyr::left_join(
###          target_threat |>
###            dplyr::filter(is.na(tag), is.na(infraspecies), is.na(infraspecies_2)),
###          by = c(
###            "Matched.Genus" = "genus",
###            "Matched.Species" = "species"
###          ),
###          na_matches = "never"
###        )
###
###      # -----------------------------------------------------------------------
###      # RANK 3: Trinomial matches with valid rank
###      # -----------------------------------------------------------------------
###      output_rank3 <- res_complete |>
###        dplyr::filter(
###          Rank == 3,
###          matched == TRUE,
###          valid_rank == TRUE  # ← CRITICAL: Only valid matches
###        ) |>
###        dplyr::left_join(
###          target_threat |>
###            dplyr::filter(!is.na(tag), !is.na(infraspecies), is.na(infraspecies_2)),
###          by = c(
###            "Matched.Genus" = "genus",
###            "Matched.Species" = "species",
###            "Matched.Infra.Rank" = "tag",
###            "Matched.Infraspecies" = "infraspecies"
###          ),
###          na_matches = "never"
###        )
###
###      # -----------------------------------------------------------------------
###      # RANK 4: Quaternomial matches with valid rank
###      # -----------------------------------------------------------------------
###      output_rank4 <- res_complete |>
###        dplyr::filter(
###          Rank == 4,
###          matched == TRUE,
###          valid_rank == TRUE  # ← CRITICAL: Only valid matches
###        ) |>
###        dplyr::left_join(
###          target_threat |>
###            dplyr::filter(!is.na(tag), !is.na(infraspecies), !is.na(infraspecies_2)),
###          by = c(
###            "Matched.Genus" = "genus",
###            "Matched.Species" = "species",
###            "Matched.Infra.Rank" = "tag",
###            "Matched.Infraspecies" = "infraspecies",
###            "Matched.Infraspecies_2" = "infraspecies_2"
###          ),
###          na_matches = "never"
###        )
###
###      # -----------------------------------------------------------------------
###      # INVALID matches (matched == TRUE but valid_rank == FALSE)
###      # These are the FALSE POSITIVES we need to exclude
###      # -----------------------------------------------------------------------
###      output_invalid <- res_complete |>
###        dplyr::filter(
###          matched == TRUE,
###          valid_rank == FALSE  # ← These are invalid matches
###        ) |>
###        dplyr::mutate(
###          tag = NA_character_,
###          infraspecies = NA_character_,
###          infraspecies_2 = NA_character_,
###          threat_category = NA_character_,
###          accepted_name_author = NA_character_
###        )
###
###      # -----------------------------------------------------------------------
###      # Unmatched records (matched == FALSE)
###      # -----------------------------------------------------------------------
###      output_unmatched <- res_complete |>
###        dplyr::filter(matched == FALSE) |>
###        dplyr::mutate(
###          tag = NA_character_,
###          infraspecies = NA_character_,
###          infraspecies_2 = NA_character_,
###          threat_category = NA_character_,
###          accepted_name_author = NA_character_
###        )
###
###      # -----------------------------------------------------------------------
###      # Combine all outputs
###      # -----------------------------------------------------------------------
###      output <- dplyr::bind_rows(
###        output_rank2,
###        output_rank3,
###        output_rank4,
###        output_invalid,    # ← Include invalid matches with NA threat_category
###        output_unmatched
###      ) |>
###        dplyr::arrange(sorter)
###
###    } else {
###
###      # =====================================================================
###      # For updated database (no infraspecies_2 support)
###      # =====================================================================
###
###      target_threat <- target_prepared |>
###        dplyr::select(
###          genus, species, infraspecies, tag_acc,
###          threat_category, accepted_name_author
###        ) |>
###        dplyr::distinct(genus, species, tag_acc, infraspecies, .keep_all = TRUE)
###
###      # -----------------------------------------------------------------------
###      # RANK 2: Binomial matches with valid rank
###      # -----------------------------------------------------------------------
###      output_rank2 <- res_complete |>
###        dplyr::filter(
###          Rank == 2,
###          matched == TRUE,
###          valid_rank == TRUE
###        ) |>
###        dplyr::left_join(
###          target_threat |>
###            dplyr::filter(is.na(tag_acc), is.na(infraspecies)),
###          by = c(
###            "Matched.Genus" = "genus",
###            "Matched.Species" = "species"
###          ),
###          na_matches = "never"
###        )
###
###      # -----------------------------------------------------------------------
###      # RANK 3: Trinomial matches with valid rank
###      # -----------------------------------------------------------------------
###      output_rank3 <- res_complete |>
###        dplyr::filter(
###          Rank == 3,
###          matched == TRUE,
###          valid_rank == TRUE
###        ) |>
###        dplyr::left_join(
###          target_threat |>
###            dplyr::filter(!is.na(tag_acc), !is.na(infraspecies)),
###          by = c(
###            "Matched.Genus" = "genus",
###            "Matched.Species" = "species",
###            "Matched.Infra.Rank" = "tag_acc",
###            "Matched.Infraspecies" = "infraspecies"
###          ),
###          na_matches = "never"
###        )
###
###      # -----------------------------------------------------------------------
###      # INVALID matches (matched == TRUE but valid_rank == FALSE)
###      # -----------------------------------------------------------------------
###      output_invalid <- res_complete |>
###        dplyr::filter(
###          matched == TRUE,
###          valid_rank == FALSE
###        ) |>
###        dplyr::mutate(
###          tag_acc = NA_character_,
###          infraspecies = NA_character_,
###          threat_category = NA_character_,
###          accepted_name_author = NA_character_
###        )
###
###      # -----------------------------------------------------------------------
###      # Unmatched records
###      # -----------------------------------------------------------------------
###      output_unmatched <- res_complete |>
###        dplyr::filter(matched == FALSE) |>
###        dplyr::mutate(
###          tag_acc = NA_character_,
###          infraspecies = NA_character_,
###          threat_category = NA_character_,
###          accepted_name_author = NA_character_
###        )
###
###      # -----------------------------------------------------------------------
###      # Combine all outputs
###      # -----------------------------------------------------------------------
###      output <- dplyr::bind_rows(
###        output_rank2,
###        output_rank3,
###        output_invalid,
###        output_unmatched
###      ) |>
###        dplyr::arrange(sorter)
###    }
###    # ==========================================================================
###    # SECTION 7: Calculate Matched Rank and Create Formatted Names
###    # ==========================================================================
###
###    output_f <- output |>
###      dplyr::mutate(
###        Matched.Rank = dplyr::case_when(
###          # Rank 1: Only genus matched
###          !is.na(Matched.Genus) & is.na(Matched.Species) ~ 1L,
###
###          # Rank 2: Genus + species matched, no infraspecies
###          !is.na(Matched.Genus) &
###            !is.na(Matched.Species) &
###            is.na(Matched.Infra.Rank) &
###            is.na(Matched.Infraspecies) ~ 2L,
###
###          # Rank 3: Genus + species + infraspecies level 1
###          !is.na(Matched.Genus) &
###            !is.na(Matched.Species) &
###            !is.na(Matched.Infra.Rank) &
###            !is.na(Matched.Infraspecies) &
###            is.na(Matched.Infraspecies_2) ~ 3L,
###
###          # Rank 4: All levels (only if use_infraspecies_2 = TRUE)
###          !is.na(Matched.Genus) &
###            !is.na(Matched.Species) &
###            !is.na(Matched.Infra.Rank) &
###            !is.na(Matched.Infraspecies) &
###            !is.na(Matched.Infraspecies_2) ~ 4L,
###
###          # Not matched
###          TRUE ~ NA_integer_
###        )
###      )
###
###    # Validate: If not using infraspecies_2, there should be no Rank 4
###    if (!use_infraspecies_2) {
###      invalid_rank4 <- sum(output_f$Matched.Rank == 4, na.rm = TRUE)
###
###      if (invalid_rank4 > 0) {
###        warning(
###          "Found ", invalid_rank4, " matches with Rank 4, but use_infraspecies_2 = FALSE.\n",
###          "This is a bug in the matching pipeline. Correcting to NA.",
###          call. = FALSE,
###          immediate. = TRUE
###        )
###
###        output_f$Matched.Rank[output_f$Matched.Rank == 4] <- NA_integer_
###        output_f$Matched.Infraspecies_2[!is.na(output_f$Matched.Rank) &
###                                          output_f$Matched.Rank == 4] <- NA_character_
###      }
###    }
###
###    # Create formatted matched name
###    output_f <- output_f |>
###      dplyr::mutate(
###        Matched.Name = dplyr::case_when(
###          # No match
###          is.na(Matched.Genus) ~ "---",
###
###          # Rank 1: Only genus
###          Matched.Rank == 1 ~ str_to_simple_cap(Matched.Genus),
###
###          # Rank 2: Binomial
###          Matched.Rank == 2 ~ paste(
###            str_to_simple_cap(Matched.Genus),
###            stringr::str_to_lower(Matched.Species)
###          ),
###
###          # Rank 3: Trinomial with infraspecific rank
###          Matched.Rank == 3 & !is.na(Matched.Infra.Rank) ~ paste(
###            str_to_simple_cap(Matched.Genus),
###            stringr::str_to_lower(Matched.Species),
###            stringr::str_to_lower(Matched.Infra.Rank),
###            stringr::str_to_lower(Matched.Infraspecies)
###          ),
###
###          # Rank 4: Quaternomial with both ranks
###          Matched.Rank == 4 &
###            !is.na(Matched.Infra.Rank) &
###            !is.na(Matched.Infra.Rank_2) &
###            !is.na(Matched.Infraspecies_2) ~ paste(
###              str_to_simple_cap(Matched.Genus),
###              stringr::str_to_lower(Matched.Species),
###              stringr::str_to_lower(Matched.Infra.Rank),
###              stringr::str_to_lower(Matched.Infraspecies),
###              stringr::str_to_lower(Matched.Infra.Rank_2),
###              stringr::str_to_lower(Matched.Infraspecies_2)
###            ),
###
###          # Unexpected cases
###          TRUE ~ "---"
###        )
###      )
###
###    # Clean multiple spaces
###    output_f <- output_f |>
###      dplyr::mutate(
###        Matched.Name = stringr::str_squish(Matched.Name),
###        Orig.Name = str_to_simple_cap(Orig.Name)
###      )
###
###    # Calculate rank comparison and match level
###    output_f <- output_f |>
###      dplyr::mutate(
###        Comp.Rank = (Rank == Matched.Rank),
###
###        Match.Level = dplyr::case_when(
###          is.na(Matched.Rank) ~ "No match",
###          Rank == Matched.Rank ~ "Exact rank",
###          Rank > Matched.Rank ~ "Matched at higher rank",
###          Rank < Matched.Rank ~ "Matched at lower rank (unexpected)",
###          TRUE ~ "Unknown"
###        ),
###
###        Threat.Status = dplyr::case_when(
###          is.na(Matched.Genus) &
###            is.na(Matched.Species) &
###            is.na(Matched.Infraspecies) ~ "Not threatened",
###          !is.na(threat_category) ~ threat_category,
###          TRUE ~ "Not threatened"
###        )
###      )
###
###    # Format final columns
###    output_f <- output_f |>
###      dplyr::mutate(
###        Matched.Name = dplyr::if_else(is.na(Matched.Name), "---", Matched.Name),
###        accepted_name_author = dplyr::if_else(
###          is.na(accepted_name_author),
###          "---",
###          accepted_name_author
###        )
###      ) |>
###      dplyr::relocate(
###        c("sorter", "Orig.Name", "Matched.Name", "Threat.Status",
###          "Author", "accepted_name_author", "Matched.Rank",
###          "Comp.Rank", "Match.Level")
###      )
###
###    # ==========================================================================
###    # SECTION 8: Final Validation
###    # ==========================================================================
###
###    # Validate all input records are present
###    assertthat::assert_that(
###      nrow(splist_class) == nrow(output_f),
###      msg = paste0(
###        "Final output row count (", nrow(output_f),
###        ") does not match input (", nrow(splist_class), ")"
###      )
###    )
###
###    assertthat::assert_that(
###      all(splist_class$sorter %in% output_f$sorter),
###      msg = "Some input records are missing from output"
###    )
###
###    assertthat::assert_that(
###      all(output_f$sorter == sort(output_f$sorter)),
###      msg = "Output records are not in correct order"
###    )
###
###    # Validate coherence of Infraspecies_2
###    if (!use_infraspecies_2) {
###      has_infrasp2_data <- any(!is.na(output_f$Matched.Infraspecies_2))
###
###      if (has_infrasp2_data) {
###        warning(
###          "Matched.Infraspecies_2 contains data but use_infraspecies_2 = FALSE.\n",
###          "Clearing Matched.Infraspecies_2 column.",
###          call. = FALSE,
###          immediate. = TRUE
###        )
###        output_f$Matched.Infraspecies_2 <- NA_character_
###      }
###    }
###
###    # Add metadata
###    # Add metadata about the process
###    attr(output_f, "use_infraspecies_2") <- use_infraspecies_2
###    attr(output_f, "target_database") <- source
###    attr(output_f, "matching_date") <- Sys.Date()
###    attr(output_f, "n_input") <- nrow(splist_class)
###    attr(output_f, "n_matched") <- sum(output_f$matched, na.rm = TRUE)
###    attr(output_f, "match_rate") <- round(
###      sum(output_f$matched, na.rm = TRUE) / nrow(splist_class) * 100,
###      2
###    )
###
###    return(output_f)
###  }
#======================================================================
# deprecated fuzzymatched

#' Fuzzy Match Genus Name
#'
#' @description
#' This function performs a fuzzy match of genus names against the threatened
#' species database using fuzzyjoin::stringdist() to account for slight
#' variations in spelling.
#'
#' @param df A tibble containing the genus names to be matched.
#' @param target_df A tibble representing the threatened species database
#'   containing the reference list of threatened species.
#'
#' @return
#' A tibble with two additional columns:
#' - fuzzy_match_genus: A logical column indicating whether the genus was
#'   successfully matched (`TRUE`) or not (`FALSE`).
#' - fuzzy_genus_dist: A numeric column representing the distance for each match.
#'
#' @details
#' If multiple genera match with the same string distance (ambiguous matches),
#' a warning is issued and the first match is automatically selected. To
#' examine ambiguous matches in detail, use \code{\link{get_ambiguous_matches}}
#' on the result object.
#'
#' @seealso \code{\link{get_ambiguous_matches}} to retrieve ambiguous match details
#'
#' @keywords internal
fuzzy_match_genus <- function(df, target_df = NULL) {

  # ========================================================================
  # SECTION 1: Input Validation
  # ========================================================================

  assertthat::assert_that(all(c('Orig.Genus',
                                'Orig.Species',
                                'Orig.Infraspecies',
                                'Orig.Infraspecies_2') %in% colnames(df)))

  # Handle empty input tibble
  if (nrow(df) == 0) {
    if (!all(c('fuzzy_match_genus', 'fuzzy_genus_dist') %in% colnames(df))) {
      return(tibble::add_column(df,
                                fuzzy_match_genus = NA,
                                fuzzy_genus_dist = NA))
    } else {
      return(df)
    }
  }

  # Remove existing fuzzy_genus_dist column if present (for sequential matching)
  if ('fuzzy_genus_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_genus_dist = NULL)
  }

  # ========================================================================
  # SECTION 2: Fuzzy Matching
  # ========================================================================

  Threatened.Genera <- target_df |>
    dplyr::distinct(genus)

  # Perform fuzzy match
  matched_temp <- df |>
    fuzzyjoin::stringdist_left_join(Threatened.Genera,
                                    by = c('Orig.Genus' = 'genus'),
                                    max_dist = 1,
                                    distance_col = 'fuzzy_genus_dist') |>
    dplyr::mutate(Matched.Genus = genus) |>
    dplyr::select(-c('genus')) |>
    dplyr::group_by(Orig.Genus, Orig.Species) |>
    dplyr::filter(fuzzy_genus_dist == min(fuzzy_genus_dist))

  # ========================================================================
  # SECTION 3: Handle Ambiguous Matches (CRAN Compliant)
  # ========================================================================

  # Detect ambiguous matches (multiple genera with same distance)
  ambiguous_matches <- matched_temp |>
    dplyr::filter(dplyr::n() > 1)

  if (nrow(ambiguous_matches) > 0) {
    # Count unique genera with ambiguous matches
    n_ambiguous_genera <- ambiguous_matches |>
      dplyr::distinct(Orig.Genus) |>
      nrow()

    # Issue informative warning WITHOUT writing files
    warning(
      "Found ", n_ambiguous_genera, " genera with multiple fuzzy matches ",
      "(tied string distances).\n",
      "  The algorithm will automatically select the first match.\n",
      "  To examine ambiguous matches, use: ",
      "get_ambiguous_matches(result, type = 'genus')\n",
      "  Consider manual curation for critical applications.",
      call. = FALSE,
      immediate. = TRUE
    )

    # Store ambiguous match information as attribute for later retrieval
    attr(matched_temp, "ambiguous_genera") <- ambiguous_matches |>
      dplyr::ungroup() |>
      dplyr::select(Orig.Genus, Orig.Species, Matched.Genus, fuzzy_genus_dist) |>
      dplyr::arrange(Orig.Genus, Matched.Genus)
  }

  # ========================================================================
  # SECTION 4: Select First Match for Ambiguous Cases
  # ========================================================================

  matched <- matched_temp |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0,
              return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup()

  # Preserve ambiguous match attribute if it exists
  if (!is.null(attr(matched_temp, "ambiguous_genera"))) {
    attr(matched, "ambiguous_genera") <- attr(matched_temp, "ambiguous_genera")
  }

  # ========================================================================
  # SECTION 5: Identify Unmatched and Combine Results
  # ========================================================================

  unmatched <- df |>
    fuzzyjoin::stringdist_anti_join(Threatened.Genera,
                                    by = c('Orig.Genus' = 'genus'),
                                    max_dist = 1)

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  res <- dplyr::bind_rows(matched, unmatched,
                          .id = 'fuzzy_match_genus') |>
    dplyr::mutate(fuzzy_match_genus = (fuzzy_match_genus == 1)) |>
    dplyr::arrange(Orig.Genus, Orig.Species) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies'))

  # Preserve ambiguous match attribute in final result
  if (!is.null(attr(matched, "ambiguous_genera"))) {
    attr(res, "ambiguous_genera") <- attr(matched, "ambiguous_genera")
  }

  return(res)
}


# =============================================================================
# FUZZY MATCH SPECIES WITHIN GENUS
# =============================================================================

#' Fuzzy Match Species within Genus
#'
#' @description
#' This function attempts to fuzzy match species names within a genus to the
#' threatened species database using fuzzyjoin::stringdist for fuzzy matching.
#'
#' @param df A tibble containing the species data to be matched.
#' @param target_df A tibble representing the threatened species database
#'   containing the reference list of threatened species.
#'
#' @return
#' A tibble with an additional logical column fuzzy_match_species_within_genus,
#' indicating whether the specific epithet was successfully fuzzy matched within
#' the matched genus (`TRUE`) or not (`FALSE`).
#'
#' @details
#' If multiple species match with the same string distance (ambiguous matches),
#' a warning is issued and the first match is automatically selected. To
#' examine ambiguous matches in detail, use \code{\link{get_ambiguous_matches}}
#' on the result object with \code{type = "species"}.
#'
#' @seealso \code{\link{get_ambiguous_matches}} to retrieve ambiguous match details
#'
#' @keywords internal

fuzzy_match_species_within_genus <- function(df, target_df = NULL) {

  assertthat::assert_that(all(c('Orig.Genus',
                                'Orig.Species',
                                'Orig.Infraspecies',
                                'Matched.Genus') %in% colnames(df)))

  if (nrow(df) == 0) {
    if (!all(c('fuzzy_match_species_within_genus',
               'fuzzy_species_dist') %in% colnames(df))) {
      return(tibble::add_column(df,
                                fuzzy_match_species_within_genus = NA,
                                fuzzy_species_dist = NA))
    } else {
      return(df)
    }
  }

  if ('fuzzy_species_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_species_dist = NULL)
  }

  res_list <- df |>
    dplyr::group_by(Matched.Genus) |>
    dplyr::group_split()

  # =========================================================================
  # CRÍTICO: Capturar atributos ANTES de bind_rows()
  # =========================================================================

  res_with_attrs <- lapply(res_list, function(chunk) {
    fuzzy_match_species_within_genus_helper(chunk, target_df)
  })

  # Extraer atributos
  all_ambiguous <- lapply(res_with_attrs, function(chunk_result) {
    attr(chunk_result, "ambiguous_species")
  })

  all_ambiguous <- Filter(Negate(is.null), all_ambiguous)

  consolidated_ambiguous <- if (length(all_ambiguous) > 0) {
    dplyr::bind_rows(all_ambiguous)
  } else {
    NULL
  }

  # Combinar resultados
  res <- dplyr::bind_rows(res_with_attrs) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies'))

  # Re-adjuntar atributo consolidado
  if (!is.null(consolidated_ambiguous) && nrow(consolidated_ambiguous) > 0) {
    attr(res, "ambiguous_species") <- consolidated_ambiguous
  }

  return(res)
}





#' Fuzzy Match Species within Genus - Helper
#' @keywords internal
fuzzy_match_species_within_genus_helper <- function(df, target_df) {

  genus <- df |>
    dplyr::distinct(Matched.Genus) |>
    unlist()

  database_subset <- memoised_get_threatened_genus(genus, target_df)

  matched <- df |>
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Species' = 'species'),
                                    distance_col = 'fuzzy_species_dist') |>
    dplyr::mutate(Matched.Species = species) |>
    dplyr::select(-c('species', 'genus')) |>
    dplyr::group_by(Orig.Genus, Orig.Species) |>
    dplyr::filter(fuzzy_species_dist == min(fuzzy_species_dist))

  # Handle ambiguous species matches (CRAN compliant)
  ambiguous_matches <- matched |>
    dplyr::filter(dplyr::n() > 1)

  if (nrow(ambiguous_matches) > 0) {
    n_ambiguous_species <- ambiguous_matches |>
      dplyr::distinct(Orig.Genus, Orig.Species) |>
      nrow()

    warning(
      "Found ", n_ambiguous_species, " species with multiple fuzzy matches ",
      "within genus '", genus, "' (tied string distances).\n",
      "  The algorithm will automatically select the first match.\n",
      "  To examine ambiguous matches, use: ",
      "get_ambiguous_matches(result, type = 'species')\n",
      "  Consider manual curation for critical applications.",
      call. = FALSE,
      immediate. = TRUE
    )

    attr(matched, "ambiguous_species") <- ambiguous_matches |>
      dplyr::ungroup() |>
      dplyr::select(Orig.Genus, Orig.Species, Matched.Species,
                    fuzzy_species_dist) |>
      dplyr::arrange(Orig.Genus, Orig.Species, Matched.Species)
  }

  matched_final <- matched |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0,
              return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup()

  if (!is.null(attr(matched, "ambiguous_species"))) {
    attr(matched_final, "ambiguous_species") <- attr(matched, "ambiguous_species")
  }

  unmatched <- fuzzyjoin::stringdist_anti_join(df,
                                               database_subset,
                                               by = c('Orig.Species' = 'species'))

  assertthat::assert_that(nrow(df) == (nrow(matched_final) + nrow(unmatched)))

  combined <- dplyr::bind_rows(matched_final, unmatched,
                               .id = 'fuzzy_match_species_within_genus') |>
    dplyr::mutate(fuzzy_match_species_within_genus =
                    (fuzzy_match_species_within_genus == 1)) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies'))

  if (!is.null(attr(matched_final, "ambiguous_species"))) {
    attr(combined, "ambiguous_species") <- attr(matched_final, "ambiguous_species")
  }

  return(combined)
}


# =============================================================================
# FUZZY MATCH INFRASPECIES WITHIN SPECIES
# =============================================================================

#' Fuzzy Match Infraspecific Epithet within Species
#' @keywords internal
fuzzy_match_infraspecies_within_species <- function(df,
                                                    target_df = NULL,
                                                    source = "original") {

  use_infraspecies_2 <- (source == "original")

  required_cols <- c(
    'Orig.Genus', 'Orig.Species', 'Orig.Infra.Rank', 'Orig.Infraspecies',
    'Orig.Infraspecies_2', 'Matched.Genus', 'Matched.Species'
  )

  assertthat::assert_that(
    all(required_cols %in% colnames(df)),
    msg = paste(
      "Missing required columns:",
      paste(setdiff(required_cols, colnames(df)), collapse = ", ")
    )
  )

  if (nrow(df) == 0) {
    if (!all(c('fuzzy_match_infraspecies', 'fuzzy_infraspecies_dist') %in% colnames(df))) {
      return(
        tibble::add_column(
          df,
          fuzzy_match_infraspecies = logical(0),
          fuzzy_infraspecies_dist = numeric(0)
        )
      )
    } else {
      return(df)
    }
  }

  if ('fuzzy_infraspecies_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_infraspecies_dist = NULL)
  }

  res_list <- df |>
    dplyr::group_by(Matched.Species) |>
    dplyr::group_split()

  # =========================================================================
  # CRÍTICO: Capturar atributos de cada chunk ANTES de bind_rows()
  # =========================================================================

  res_with_attrs <- lapply(res_list, function(chunk) {
    fuzzy_match_infraspecies_within_species_helper(chunk, target_df, source = source)
  })

  # Extraer todos los atributos "ambiguous_infraspecies" de cada chunk
  all_ambiguous <- lapply(res_with_attrs, function(chunk_result) {
    attr(chunk_result, "ambiguous_infraspecies")
  })

  # Filtrar NULLs
  all_ambiguous <- Filter(Negate(is.null), all_ambiguous)

  # Combinar todos los atributos en un solo data frame
  consolidated_ambiguous <- if (length(all_ambiguous) > 0) {
    dplyr::bind_rows(all_ambiguous)
  } else {
    NULL
  }

  # Ahora sí hacer bind_rows() de los resultados
  res <- dplyr::bind_rows(res_with_attrs) |>
    dplyr::relocate(c(
      'Orig.Genus', 'Orig.Species', 'Orig.Infra.Rank',
      'Orig.Infraspecies', 'Orig.Infraspecies_2'
    ))

  # CRÍTICO: Re-adjuntar el atributo consolidado
  if (!is.null(consolidated_ambiguous) && nrow(consolidated_ambiguous) > 0) {
    attr(res, "ambiguous_infraspecies") <- consolidated_ambiguous
  }

  return(res)
}

#' Helper: Fuzzy Match Infraspecific Epithet within Species
#' @keywords internal
fuzzy_match_infraspecies_within_species_helper <- function(df,
                                                           target_df,
                                                           source = "original") {

  use_infraspecies_2 <- (source == "original")

  species_matched <- df |>
    dplyr::distinct(Matched.Species) |>
    dplyr::pull(Matched.Species)

  get_threatened_infraspecies <- function(species_matched,
                                          target_df = NULL,
                                          source = source) {
    use_infraspecies_2 <- (source == "original")

    if (use_infraspecies_2 == TRUE) {
      return(
        target_df |>
          dplyr::filter(species %in% species_matched) |>
          dplyr::select(c('genus', 'species', 'tag', 'infraspecies')) |>
          dplyr::mutate(tag = toupper(tag)) |>
          tidyr::drop_na(tag, infraspecies)
      )
    } else {
      return(
        target_df |>
          dplyr::filter(species %in% species_matched) |>
          dplyr::select(c('genus', 'species', 'tag_acc', 'infraspecies')) |>
          dplyr::mutate(tag_acc = toupper(tag_acc)) |>
          tidyr::drop_na(tag_acc, infraspecies)
      )
    }
  }

  memoised_get_threatened_infrasp <- memoise::memoise(get_threatened_infraspecies)

  database_subset <- memoised_get_threatened_infrasp(species_matched,
                                                     target_df,
                                                     source = source)

  if (nrow(database_subset) == 0) {
    return(
      df |>
        dplyr::mutate(
          fuzzy_match_infraspecies = FALSE,
          fuzzy_infraspecies_dist = NA_real_
        )
    )
  }

  tag_col_to_remove <- if (use_infraspecies_2) "tag" else "tag_acc"

  matched <- df |>
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Infraspecies' = 'infraspecies'),
                                    distance_col = 'fuzzy_infraspecies_dist') |>
    dplyr::mutate(Matched.Infraspecies = infraspecies) |>
    dplyr::select(-c('species', 'genus', 'infraspecies'),
                  -dplyr::all_of(tag_col_to_remove)) |>
    dplyr::group_by(Orig.Genus, Orig.Species, Orig.Infra.Rank, Orig.Infraspecies) |>
    dplyr::filter(fuzzy_infraspecies_dist == min(fuzzy_infraspecies_dist))

  # Handle ambiguous infraspecies matches (CRAN compliant)
  ambiguous_matches <- matched |>
    dplyr::filter(dplyr::n() > 1)

  if (nrow(ambiguous_matches) > 0) {
    n_ambiguous <- ambiguous_matches |>
      dplyr::distinct(Orig.Genus, Orig.Species, Orig.Infraspecies) |>
      nrow()

    warning(
      "Found ", n_ambiguous, " infraspecies with multiple fuzzy matches ",
      "within species '", species_matched, "' (tied string distances).\n",
      "  The algorithm will automatically select the first match.\n",
      "  To examine ambiguous matches, use: ",
      "get_ambiguous_matches(result, type = 'infraspecies')\n",
      "  Consider manual curation for critical applications.",
      call. = FALSE,
      immediate. = TRUE
    )

    attr(matched, "ambiguous_infraspecies") <- ambiguous_matches |>
      dplyr::ungroup() |>
      dplyr::select(
        Orig.Genus, Orig.Species, Orig.Infra.Rank, Orig.Infraspecies,
        Matched.Infraspecies, fuzzy_infraspecies_dist
      ) |>
      dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies, Matched.Infraspecies)
  }

  matched_final <- matched |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup()

  if (!is.null(attr(matched, "ambiguous_infraspecies"))) {
    attr(matched_final, "ambiguous_infraspecies") <- attr(matched, "ambiguous_infraspecies")
  }

  unmatched <- fuzzyjoin::stringdist_anti_join(
    dplyr::filter(df, !is.na(Orig.Infraspecies)),
    database_subset,
    by = c('Orig.Infraspecies' = 'infraspecies')
  )

  assertthat::assert_that(nrow(df) == (nrow(matched_final) + nrow(unmatched)))

  combined <- dplyr::bind_rows(matched_final,
                               unmatched,
                               .id = 'fuzzy_match_infraspecies') |>
    dplyr::mutate(fuzzy_match_infraspecies = (fuzzy_match_infraspecies == "1")) |>
    dplyr::relocate(c('Orig.Genus', 'Orig.Species', 'Orig.Infraspecies',
                      'Orig.Infra.Rank', 'Orig.Infraspecies_2'))

  if (!is.null(attr(matched_final, "ambiguous_infraspecies"))) {
    attr(combined, "ambiguous_infraspecies") <- attr(matched_final, "ambiguous_infraspecies")
  }

  return(combined)
}


# =============================================================================
# FUZZY MATCH INFRASPECIES LEVEL 2
# =============================================================================

#' Fuzzy Match Infraspecies Level 2 within Infraspecies Level 1
#' @keywords internal
fuzzy_match_infraspecies2_within_infraspecies <- function(df, target_df = NULL) {

  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Orig.Infraspecies',
                                'Orig.Infraspecies_2', 'Matched.Genus',
                                'Matched.Species', 'Matched.Infraspecies') %in% colnames(df)))

  if (nrow(df) == 0) {
    if (!all(c('fuzzy_match_infraspecies_2',
               'fuzzy_infraspecies_2_dist') %in% colnames(df))) {
      return(tibble::add_column(df,
                                fuzzy_match_infraspecies_2 = NA,
                                fuzzy_infraspecies_2_dist = NA))
    } else {
      return(df)
    }
  }

  if ('fuzzy_infraspecies_2_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_infraspecies_2_dist = NULL)
  }

  res_list <- df |>
    dplyr::group_by(Matched.Infraspecies) |>
    dplyr::group_split()

  # =========================================================================
  # CRÍTICO: Capturar atributos ANTES de bind_rows()
  # =========================================================================

  res_with_attrs <- lapply(res_list, function(chunk) {
    fuzzy_match_infraspecies2_within_infraspecies_helper(chunk, target_df)
  })

  # Extraer atributos
  all_ambiguous <- lapply(res_with_attrs, function(chunk_result) {
    attr(chunk_result, "ambiguous_infraspecies_2")
  })

  all_ambiguous <- Filter(Negate(is.null), all_ambiguous)

  consolidated_ambiguous <- if (length(all_ambiguous) > 0) {
    dplyr::bind_rows(all_ambiguous)
  } else {
    NULL
  }

  # Combinar resultados
  res <- dplyr::bind_rows(res_with_attrs)

  # Re-adjuntar atributo consolidado
  if (!is.null(consolidated_ambiguous) && nrow(consolidated_ambiguous) > 0) {
    attr(res, "ambiguous_infraspecies_2") <- consolidated_ambiguous
  }

  return(res)
}


#' Helper function for fuzzy matching infraspecies level 2
#' @keywords internal
fuzzy_match_infraspecies2_within_infraspecies_helper <- function(df, target_df) {

  infraspecies1 <- df |>
    dplyr::distinct(Matched.Infraspecies) |>
    unlist()

  get_threatened_infraspecies2 <- function(infraspecies1, target_df = NULL) {
    return(target_df |>
             dplyr::filter(infraspecies %in% infraspecies1) |>
             dplyr::select(c('genus', 'species',
                             'infraspecies', 'infraspecies_2')))
  }

  memoised_get_threatened_infrasp2 <- memoise::memoise(get_threatened_infraspecies2)

  database_subset <- memoised_get_threatened_infrasp2(infraspecies1, target_df) |>
    tidyr::drop_na(infraspecies_2)

  if (nrow(database_subset) == 0) {
    return(df |>
             dplyr::mutate(fuzzy_match_infraspecies_2 = FALSE,
                           fuzzy_infraspecies_2_dist = NA_real_))
  }

  matched <- df |>
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Infraspecies_2' = 'infraspecies_2'),
                                    distance_col = 'fuzzy_infraspecies_2_dist') |>
    dplyr::mutate(Matched.Infraspecies_2 = infraspecies_2) |>
    dplyr::select(-c('species', 'genus', 'infraspecies', 'infraspecies_2')) |>
    dplyr::group_by(Orig.Genus, Orig.Species, Orig.Infraspecies, Orig.Infraspecies_2) |>
    dplyr::filter(fuzzy_infraspecies_2_dist == min(fuzzy_infraspecies_2_dist))

  # Handle ambiguous infraspecies 2 matches (CRAN compliant)
  ambiguous_matches <- matched |>
    dplyr::filter(dplyr::n() > 1)

  if (nrow(ambiguous_matches) > 0) {
    n_ambiguous <- ambiguous_matches |>
      dplyr::distinct(Orig.Genus, Orig.Species, Orig.Infraspecies,
                      Orig.Infraspecies_2) |>
      nrow()

    warning(
      "Found ", n_ambiguous, " infraspecies level 2 with multiple fuzzy matches ",
      "within infraspecies '", infraspecies1, "' (tied string distances).\n",
      "  The algorithm will automatically select the first match.\n",
      "  To examine ambiguous matches, use: ",
      "get_ambiguous_matches(result, type = 'infraspecies')\n",
      "  Consider manual curation for critical applications.",
      call. = FALSE,
      immediate. = TRUE
    )

    attr(matched, "ambiguous_infraspecies_2") <- ambiguous_matches |>
      dplyr::ungroup() |>
      dplyr::select(
        Orig.Genus, Orig.Species, Orig.Infraspecies, Orig.Infraspecies_2,
        Matched.Infraspecies_2, fuzzy_infraspecies_2_dist
      ) |>
      dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies,
                     Orig.Infraspecies_2, Matched.Infraspecies_2)
  }

  matched_final <- matched |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup()

  if (!is.null(attr(matched, "ambiguous_infraspecies_2"))) {
    attr(matched_final, "ambiguous_infraspecies_2") <- attr(matched, "ambiguous_infraspecies_2")
  }

  unmatched <- fuzzyjoin::stringdist_anti_join(
    dplyr::filter(df, !is.na(Orig.Infraspecies_2)),
    database_subset,
    by = c('Orig.Infraspecies_2' = 'infraspecies_2')
  )

  assertthat::assert_that(
    nrow(df) == (nrow(matched_final) + nrow(unmatched)),
    msg = "Row count mismatch in fuzzy_match_infraspecies2"
  )

  combined <- dplyr::bind_rows(matched_final,
                               unmatched,
                               .id = 'fuzzy_match_infraspecies_2') |>
    dplyr::mutate(fuzzy_match_infraspecies_2 = (fuzzy_match_infraspecies_2 == "1")) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies',
                      'Orig.Infraspecies_2'))

  if (!is.null(attr(matched_final, "ambiguous_infraspecies_2"))) {
    attr(combined, "ambiguous_infraspecies_2") <- attr(matched_final, "ambiguous_infraspecies_2")
  }

  return(combined)
}


# =============================================================================
# GET AMBIGUOUS MATCHES - COMPANION FUNCTION
# =============================================================================

#' Retrieve Ambiguous Match Information
#'
#' @description
#' Extracts information about ambiguous matches (multiple candidates with
#' tied distances) from matching results. This is useful for quality control
#' and manual curation of uncertain matches.
#'
#' @param match_result A tibble returned by matching functions such as
#'   \code{\link{matching_threatenedperu}} or internal matching functions.
#' @param type Character. Type of ambiguous matches to retrieve:
#'   \itemize{
#'     \item \code{"genus"} (default): Ambiguous genus-level matches
#'     \item \code{"species"}: Ambiguous species-level matches
#'     \item \code{"infraspecies"}: Ambiguous infraspecies-level matches (includes level 2)
#'     \item \code{"all"}: All types of ambiguous matches
#'   }
#' @param save_to_file Logical. If TRUE, saves results to a CSV file.
#'   Default is FALSE (CRAN compliant - no automatic file writing).
#' @param output_dir Character. Directory to save the file if save_to_file = TRUE.
#'   Defaults to \code{tempdir()} for safe file operations.
#'
#' @return
#' A tibble with ambiguous match details, or NULL if no ambiguous matches exist.
#' Columns depend on the match type but typically include original names,
#' matched names, and distance metrics.
#'
#' @details
#' During fuzzy matching, multiple candidates may have identical string distances,
#' making the choice of match ambiguous. The matching algorithm automatically
#' selects the first candidate, but this function allows you to:
#' \itemize{
#'   \item Review all ambiguous matches for quality control
#'   \item Export them for manual curation
#'   \item Make informed decisions about match quality
#' }
#'
#' @section File Output:
#' When \code{save_to_file = TRUE}, a timestamped CSV file is created:
#' \itemize{
#'   \item Filename format: "threatenedperu_ambiguous_[type]_[timestamp].csv"
#'   \item Location: \code{output_dir} (defaults to tempdir())
#'   \item Contains all ambiguous matches with metadata
#' }
#'
#' @export
#' @examples
#' \donttest{
#' # Basic usage after matching
#' species_list <- c("Catleya maxima", "Polylepis incana")  # Note typo in Cattleya
#' result <- is_threatened_peru(species_list, return_details = TRUE)
#'
#' # Check for ambiguous genus matches
#' ambig_genera <- get_ambiguous_matches(result, type = "genus")
#' if (!is.null(ambig_genera)) {
#'   print(ambig_genera)
#' }
#'
#' # Get all types of ambiguous matches
#' all_ambig <- get_ambiguous_matches(result, type = "all")
#'
#' # Save to file for manual review (optional)
#' ambig_genera <- get_ambiguous_matches(
#'   result,
#'   type = "genus",
#'   save_to_file = TRUE,
#'   output_dir = tempdir()
#' )
#' }
get_ambiguous_matches <- function(match_result,
                                  type = c("genus", "species", "infraspecies", "all"),
                                  save_to_file = FALSE,
                                  output_dir = tempdir()) {

  type <- match.arg(type)

  # ========================================================================
  # SECTION 1: Validate Input
  # ========================================================================

  if (!inherits(match_result, "data.frame")) {
    stop(
      "match_result must be a data frame or tibble returned by a matching function.",
      call. = FALSE
    )
  }

  # ========================================================================
  # SECTION 2: Extract Ambiguous Match Attributes
  # ========================================================================

  ambiguous_data <- list()

  # Check for genus-level ambiguous matches
  if (type %in% c("genus", "all") && !is.null(attr(match_result, "ambiguous_genera"))) {
    ambiguous_data$genus <- attr(match_result, "ambiguous_genera") |>
      dplyr::mutate(Match_Type = "Genus", .before = 1)
  }

  # Check for species-level ambiguous matches
  if (type %in% c("species", "all") && !is.null(attr(match_result, "ambiguous_species"))) {
    ambiguous_data$species <- attr(match_result, "ambiguous_species") |>
      dplyr::mutate(Match_Type = "Species", .before = 1)
  }

  # Check for infraspecies-level ambiguous matches (includes both level 1 and 2)
  if (type %in% c("infraspecies", "all")) {
    # Check for level 1 infraspecies
    if (!is.null(attr(match_result, "ambiguous_infraspecies"))) {
      ambiguous_data$infraspecies <- attr(match_result, "ambiguous_infraspecies") |>
        dplyr::mutate(Match_Type = "Infraspecies", .before = 1)
    }

    # Check for level 2 infraspecies (quaternomial names)
    if (!is.null(attr(match_result, "ambiguous_infraspecies_2"))) {
      infrasp2_data <- attr(match_result, "ambiguous_infraspecies_2") |>
        dplyr::mutate(Match_Type = "Infraspecies_2", .before = 1)

      # Combine with level 1 if both exist
      if ("infraspecies" %in% names(ambiguous_data)) {
        ambiguous_data$infraspecies <- dplyr::bind_rows(
          ambiguous_data$infraspecies,
          infrasp2_data
        )
      } else {
        ambiguous_data$infraspecies <- infrasp2_data
      }
    }
  }

  # ========================================================================
  # SECTION 3: Handle No Ambiguous Matches
  # ========================================================================

  if (length(ambiguous_data) == 0) {
    message(
      "No ambiguous ", type, " matches found in the result.\n",
      "This is good news - all matches were unambiguous!"
    )
    return(invisible(NULL))
  }

  # ========================================================================
  # SECTION 4: Combine Results
  # ========================================================================

  if (type == "all") {
    result <- dplyr::bind_rows(ambiguous_data)
  } else {
    result <- ambiguous_data[[type]]
  }

  # Add summary information
  n_matches <- nrow(result)
  n_original <- result |>
    dplyr::distinct(dplyr::across(dplyr::starts_with("Orig."))) |>
    nrow()

  message(
    "Found ", n_matches, " ambiguous match(es) for ",
    n_original, " original name(s).\n"
  )

  # ========================================================================
  # SECTION 5: Optional File Export
  # ========================================================================

  if (save_to_file) {
    # Validate output directory
    if (!dir.exists(output_dir)) {
      tryCatch({
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      }, error = function(e) {
        stop(
          "Cannot create output directory: ", output_dir, "\n",
          "Error: ", e$message,
          call. = FALSE
        )
      })
    }

    # Create timestamped filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0(
      "threatenedperu_ambiguous_",
      type,
      "_",
      timestamp,
      ".csv"
    )
    filepath <- file.path(output_dir, filename)

    # Write file
    tryCatch({
      readr::write_csv(result, filepath)
      message("Ambiguous matches saved to: ", filepath)
    }, error = function(e) {
      warning(
        "Failed to write file: ", filepath, "\n",
        "Error: ", e$message,
        call. = FALSE,
        immediate. = TRUE
      )
    })
  }

  # ========================================================================
  # SECTION 6: Return Results
  # ========================================================================

  return(result)
}

