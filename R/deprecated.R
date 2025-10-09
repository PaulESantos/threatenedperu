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



