# ==============================================================================
# MATCHING CONFIGURATION
# ==============================================================================

#' Initialize Matching Configuration
#'
#' @description
#' Sets up the configuration for the matching pipeline, including loading and
#' validating the target database.
#'
#' @param source Character. Database to use: "original" or "updated"
#'
#' @return List with configuration:
#'   \item{source}{Database source}
#'   \item{use_infraspecies_2}{Whether Rank 4 is supported}
#'   \item{target_prepared}{Prepared target database}
#'
#' @keywords internal
.initialize_matching_config <- function(source) {

  # ========================================================================
  # Validate Source Parameter
  # ========================================================================
  if (!is.character(source) || length(source) != 1) {
    stop(
      "source must be a single character string: 'original' or 'updated'",
      call. = FALSE
    )
  }

  if (!source %in% c("original", "updated")) {
    stop(
      "Invalid source value: '", source, "'. Must be 'original' or 'updated'",
      call. = FALSE
    )
  }

  # ========================================================================
  # Determine Database Capabilities
  # ========================================================================
  use_infraspecies_2 <- (source == "original")

  # ========================================================================
  # Load and Prepare Database
  # ========================================================================
  target_prepared <- tryCatch({
    get_threatened_data(type = source)
  }, error = function(e) {
    stop(
      "Failed to load database '", source, "'.\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })

  # Clean whitespace from all character columns
  target_prepared <- target_prepared |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~stringr::str_squish(.)
      )
    )

  # ========================================================================
  # Validate Database Structure
  # ========================================================================
  required_cols <- if (use_infraspecies_2) {
    c("genus", "species", "tag", "infraspecies",
      "infraspecies_2", "threat_category")
  } else {
    c("genus", "species", "tag_acc", "infraspecies",
      "threat_category")
  }

  missing_cols <- setdiff(required_cols, names(target_prepared))
  if (length(missing_cols) > 0) {
    stop(
      "Database '", source, "' missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ========================================================================
  # User Information
  # ========================================================================
  if (!use_infraspecies_2) {
    message(
      "Note: Using database '", source,
      "' which supports the updated names of species listed in DS 043-2006-AG."
    )
  }

  # ========================================================================
  # Return Configuration
  # ========================================================================
  list(
    source = source,
    use_infraspecies_2 = use_infraspecies_2,
    target_prepared = target_prepared
  )
}




# ==============================================================================
# INPUT PREPROCESSING
# ==============================================================================

#' Preprocess Input Species List
#'
#' @description
#' Classifies and validates input species names, handling edge cases like
#' genus-level names, NA values, and invalid formats.
#'
#' @param splist Character vector of species names
#' @param config Configuration list from .initialize_matching_config()
#'
#' @return List with:
#'   \item{splist_class}{Classified species data (all inputs)}
#'   \item{df}{Filtered data ready for matching (valid binomials only)}
#'
#' @keywords internal
.preprocess_input <- function(splist, config) {

  # ========================================================================
  # Classify Species Names
  # ========================================================================
  splist_class <- .splist_classify(splist) |>
    .transform_split_classify()

  # ========================================================================
  # Check for Non-Binomial Names
  # ========================================================================
  non_binomial <- .check_binomial(splist_class, splist = splist)

  # Filter out problematic names
  df <- if (length(non_binomial) != 0) {
    splist_class[-non_binomial, ]
  } else {
    splist_class
  }

  # ========================================================================
  # Initialize Matching Columns (only if df has rows)
  # ========================================================================
  if (nrow(df) > 0) {
    df <- .initialize_matching_columns(df, config$use_infraspecies_2)

    # Validate Rank 4 compatibility
    if (!config$use_infraspecies_2 && any(df$Rank == 4, na.rm = TRUE)) {
      rank4_count <- sum(df$Rank == 4, na.rm = TRUE)
      warning(
        rank4_count, " species with Rank 4 detected. ",
        "Database '", config$source, "' does not support infraspecies_2. ",
        "These species will not be matched.",
        call. = FALSE,
        immediate. = TRUE
      )
    }
  }

  # ========================================================================
  # Return Preprocessed Data
  # ========================================================================
  list(
    splist_class = splist_class,
    df = df
  )
}


#' Initialize Matching Columns
#'
#' @description
#' Adds all necessary matching columns to the dataframe with NA defaults.
#'
#' @param df Dataframe with classified species
#' @param use_infraspecies_2 Logical, whether Rank 4 is supported
#'
#' @return Dataframe with initialized matching columns
#'
#' @keywords internal
.initialize_matching_columns <- function(df, use_infraspecies_2) {

  # All matching columns that will be populated during pipeline
  matching_cols <- c(
    'Matched.Genus',
    'Matched.Species',
    'Matched.Infra.Rank',
    'Matched.Infraspecies',
    'Matched.Infra.Rank_2',
    'Matched.Infraspecies_2'
  )

  # Add columns if missing
  for (col in matching_cols) {
    if (!col %in% colnames(df)) {
      df[[col]] <- NA_character_
    }
  }

  # Ensure Orig.Infraspecies columns exist
  if (!'Orig.Infraspecies' %in% colnames(df)) {
    df$Orig.Infraspecies <- NA_character_
  }

  if (!'Orig.Infraspecies_2' %in% colnames(df)) {
    df$Orig.Infraspecies_2 <- NA_character_
  }

  # Set infraspecies_2 to NA if not supported
  if (!use_infraspecies_2) {
    df$Orig.Infraspecies_2 <- NA_character_
    df$Matched.Infraspecies_2 <- NA_character_
  }

  # Add metadata attribute
  attr(df, "use_infraspecies_2") <- use_infraspecies_2

  return(df)
}


#' Create Empty Result for Edge Case
#'
#' @description
#' Creates properly structured empty result when all input names are invalid.
#'
#' @param splist_class Classified species data
#' @param config Configuration list
#'
#' @return Empty tibble with correct structure and metadata
#'
#' @keywords internal
.create_empty_result <- function(splist_class, config) {

  message(
    "All input names were filtered out (genus-level or invalid names).\n",
    "Returning empty result with 'Not threatened' status."
  )

  empty_result <- splist_class |>
    dplyr::select(
      sorter, Orig.Name, Orig.Genus, Orig.Species,
      Orig.Infraspecies, Orig.Infraspecies_2,
      Rank, Orig.Infra.Rank, Orig.Infra.Rank_2, Author
    ) |>
    dplyr::mutate(
      Matched.Name = "---",
      Matched.Genus = NA_character_,
      Matched.Species = NA_character_,
      Matched.Infra.Rank = NA_character_,
      Matched.Infraspecies = NA_character_,
      Matched.Infra.Rank_2 = NA_character_,
      Matched.Infraspecies_2 = NA_character_,
      Matched.Rank = NA_integer_,
      Matched.Rank.Calculated = NA_integer_,
      valid_rank = FALSE,
      matched = FALSE,
      threat_category = NA_character_,
      accepted_name_author = "---",
      Threat.Status = "Not threatened",
      Comp.Rank = FALSE,
      Match.Level = "No match"
    ) |>
    dplyr::relocate(
      sorter, Orig.Name, Matched.Name, Threat.Status,
      Author, accepted_name_author, Matched.Rank,
      Comp.Rank, Match.Level
    )

  # Add metadata
  attr(empty_result, "use_infraspecies_2") <- config$use_infraspecies_2
  attr(empty_result, "target_database") <- config$source
  attr(empty_result, "matching_date") <- Sys.Date()
  attr(empty_result, "n_input") <- nrow(splist_class)
  attr(empty_result, "n_matched") <- 0
  attr(empty_result, "match_rate") <- 0

  return(empty_result)
}


# ==============================================================================
# MATCHING PIPELINE NODES
# ==============================================================================

#' Run Complete Matching Pipeline
#'
#' @description
#' Coordinates all matching nodes in hierarchical sequence.
#'
#' @param df Preprocessed dataframe ready for matching
#' @param target_df Target database
#' @param config Configuration list
#'
#' @return List with matched and unmatched records
#'
#' @keywords internal
.run_matching_pipeline <- function(df, target_df, config) {

  # ========================================================================
  # Phase 1: Genus Matching
  # ========================================================================
  genus_results <- .run_genus_matching(df, target_df, config)

  # ========================================================================
  # Phase 2: Species Matching (within matched genera)
  # ========================================================================
  species_results <- .run_species_matching(
    genus_results$matched,
    genus_results$unmatched_no_genus,
    target_df,
    config
  )

  # ========================================================================
  # Phase 3: Infraspecies Matching (within matched species)
  # ========================================================================
  infraspecies_results <- .run_infraspecies_matching(
    species_results$matched,
    species_results$unmatched,
    target_df,
    config
  )

  # ========================================================================
  # CRITICAL FIX: Remove Duplicates and Add 'matched' Column
  # ========================================================================
  matched_combined <- dplyr::bind_rows(
    species_results$matched,
    infraspecies_results$matched
  ) |>
    dplyr::distinct(sorter, .keep_all = TRUE) |>  # ← REMOVE DUPLICATES
    dplyr::mutate(matched = TRUE)                  # ← ADD matched column

  unmatched_combined <- dplyr::bind_rows(
    genus_results$unmatched_no_genus,
    infraspecies_results$unmatched
  ) |>
    dplyr::distinct(sorter, .keep_all = TRUE) |>  # ← REMOVE DUPLICATES
    dplyr::mutate(matched = FALSE)                 # ← ADD matched column

  # ========================================================================
  # Validate Output Structure
  # ========================================================================
  if (nrow(matched_combined) == 0) {
    # Initialize empty tibble with required columns
    matched_combined <- df[0, ] |>
      dplyr::mutate(
        matched = logical(0),
        Matched.Genus = character(0),
        Matched.Species = character(0),
        Matched.Infra.Rank = character(0),
        Matched.Infraspecies = character(0),
        Matched.Infra.Rank_2 = character(0),
        Matched.Infraspecies_2 = character(0)
      )
  }

  result <- dplyr::bind_rows(
    matched_combined,
    unmatched_combined
  ) |>
    dplyr::arrange(sorter)

  # Validate structure
  assertthat::assert_that(
    is.list(result),
    msg = ".run_matching_pipeline() must return a list"
  )
  assertthat::assert_that(
    "matched" %in% names(result),
    msg = ".run_matching_pipeline() result must have 'matched' element"
  )
  assertthat::assert_that(
    "unmatched" %in% names(result),
    msg = ".run_matching_pipeline() result must have 'unmatched' element"
  )
  assertthat::assert_that(
    "matched" %in% colnames(result$matched),
    msg = ".run_matching_pipeline() matched tibble must have 'matched' column"
  )

  return(result)
}


#' Run Genus-Level Matching
#'
#' @description
#' Nodes 1-3: Direct match, Genus match, Fuzzy genus match
#'
#' @keywords internal
.run_genus_matching <- function(df, target_df, config) {

  # Node 1: Direct exact match (full name)
  node1 <- df |>
    direct_match(target_df = target_df, source = config$source)

  node1_matched <- node1 |> dplyr::filter(direct_match == TRUE)
  node1_unmatched <- node1 |> dplyr::filter(direct_match == FALSE)

  # Node 2: Exact genus match
  node2 <- node1_unmatched |>
    genus_match(target_df)

  node2_matched <- node2 |> dplyr::filter(genus_match == TRUE)
  node2_unmatched <- node2 |> dplyr::filter(genus_match == FALSE)

  # Node 3: Fuzzy genus match
  node3 <- node2_unmatched |>
    fuzzy_match_genus(target_df)

  node3_matched <- node3 |> dplyr::filter(fuzzy_match_genus == TRUE)
  node3_unmatched <- node3 |> dplyr::filter(fuzzy_match_genus == FALSE)

  # ========================================================================
  # CRITICAL FIX: Use distinct() to avoid duplicates
  # ========================================================================
  matched <- dplyr::bind_rows(
    node1_matched,
    node2_matched |> dplyr::filter(!is.na(Matched.Genus) & Rank == 1),
    node3_matched
  ) |>
    dplyr::distinct(sorter, .keep_all = TRUE)  # ← REMOVE DUPLICATES

  # Return results
  list(
    matched = matched,
    unmatched_no_genus = node3_unmatched
  )
}


#' Run Species-Level Matching
#'
#' @description
#' Nodes 4-5: Direct species match, Suffix match, Fuzzy species match
#'
#' @keywords internal
.run_species_matching <- function(genus_matched,
                                  genus_unmatched,
                                  target_df,
                                  config) {

  # Node 4: Direct species match within genus
  node4 <- genus_matched |>
    direct_match_species_within_genus(target_df)

  node4_matched <- node4 |>
    dplyr::filter(direct_match_species_within_genus == TRUE)
  node4_unmatched <- node4 |>
    dplyr::filter(direct_match_species_within_genus == FALSE)

  # Node 5a: Suffix match
  node5a <- node4_unmatched |>
    suffix_match_species_within_genus(target_df)

  node5a_matched <- node5a |>
    dplyr::filter(suffix_match_species_within_genus == TRUE)
  node5a_unmatched <- node5a |>
    dplyr::filter(suffix_match_species_within_genus == FALSE)

  # Node 5b: Fuzzy species match
  node5b_input <- node5a_unmatched |>
    dplyr::filter(!is.na(Orig.Species))

  node5b <- node5b_input |>
    fuzzy_match_species_within_genus(target_df)

  node5b_matched <- node5b |>
    dplyr::filter(fuzzy_match_species_within_genus == TRUE)
  node5b_unmatched <- node5b |>
    dplyr::filter(fuzzy_match_species_within_genus == FALSE)

  # ========================================================================
  # CRITICAL FIX: Use distinct() to avoid duplicates
  # ========================================================================
  matched <- dplyr::bind_rows(
    node4_matched,
    node5a_matched,
    node5b_matched
  ) |>
    dplyr::distinct(sorter, .keep_all = TRUE)  # ← REMOVE DUPLICATES

  unmatched <- dplyr::bind_rows(
    genus_unmatched,
    node5b_unmatched
  ) |>
    dplyr::distinct(sorter, .keep_all = TRUE)  # ← REMOVE DUPLICATES

  # Return results
  list(
    matched = matched,
    unmatched = unmatched
  )
}


#' Run Infraspecies-Level Matching
#'
#' @description
#' Nodes 6-7: Infraspecies rank and epithet matching (2 levels if supported)
#'
#' @keywords internal
.run_infraspecies_matching <- function(species_matched,
                                       species_unmatched,
                                       target_df,
                                       config) {

  # Node 6a: Direct infraspecific rank match
  node6_input <- species_unmatched |>
    dplyr::filter(
      !is.na(Orig.Infra.Rank),
      !is.na(Matched.Genus),
      !is.na(Matched.Species),
      is.na(Matched.Infraspecies)
    )

  node6a <- node6_input |>
    direct_match_infra_rank_within_species(
      target_df = target_df,
      source = config$source
    )

  node6a_matched <- node6a |> dplyr::filter(direct_match_infra_rank == TRUE)
  node6a_unmatched <- node6a |> dplyr::filter(direct_match_infra_rank == FALSE)

  # Node 6b: Fuzzy infraspecies epithet match
  node6b <- node6a_matched |>
    fuzzy_match_infraspecies_within_species(
      target_df = target_df,
      source = config$source
    ) |>
    .recalculate_rank_infrasp1()

  node6b_matched <- node6b |>
    dplyr::filter(
      fuzzy_match_infraspecies == TRUE,
      valid_rank == TRUE,
      Rank == 3
    )

  node6b_unmatched <- node6b |>
    dplyr::filter(
      fuzzy_match_infraspecies == FALSE |
        valid_rank == FALSE |
        Rank != 3
    )

  # Node 7: Infraspecies level 2 (only if supported)
  if (config$use_infraspecies_2) {
    infrasp2_results <- .match_infraspecies_level2(
      node6b_unmatched,
      target_df
    )

    all_matched <- dplyr::bind_rows(
      species_matched,
      node6b_matched,
      infrasp2_results$matched
    ) |>
      dplyr::distinct(sorter, .keep_all = TRUE)  # ← REMOVE DUPLICATES

    all_unmatched <- dplyr::bind_rows(
      node6a_unmatched,
      infrasp2_results$unmatched
    ) |>
      dplyr::distinct(sorter, .keep_all = TRUE)  # ← REMOVE DUPLICATES
  } else {
    all_matched <- dplyr::bind_rows(
      species_matched,
      node6b_matched
    ) |>
      dplyr::distinct(sorter, .keep_all = TRUE)  # ← REMOVE DUPLICATES

    all_unmatched <- dplyr::bind_rows(
      node6a_unmatched,
      node6b_unmatched
    ) |>
      dplyr::distinct(sorter, .keep_all = TRUE)  # ← REMOVE DUPLICATES
  }

  list(
    matched = all_matched,
    unmatched = all_unmatched
  )
}


#' Match Infraspecies Level 2 (Rank 4)
#'
#' @description
#' Node 7: Fuzzy match for second infraspecific level
#'
#' @keywords internal
.match_infraspecies_level2 <- function(unmatched, target_df) {

  node7_input <- unmatched |>
    dplyr::filter(
      Rank == 4,
      !is.na(Orig.Infraspecies_2),
      is.na(Matched.Infraspecies_2),
      !is.na(Matched.Infraspecies)
    )

  if (nrow(node7_input) == 0) {
    return(list(
      matched = node7_input,
      unmatched = unmatched
    ))
  }

  node7 <- node7_input |>
    fuzzy_match_infraspecies2_within_infraspecies(target_df) |>
    .recalculate_rank_infrasp2()

  node7_matched <- node7 |>
    dplyr::filter(
      valid_rank == TRUE,
      fuzzy_match_infraspecies_2 == TRUE
    ) |>
    dplyr::mutate(Matched.Infra.Rank_2 = "F.")

  node7_unmatched <- node7 |>
    dplyr::filter(
      valid_rank == FALSE | fuzzy_match_infraspecies_2 == FALSE
    )

  list(
    matched = node7_matched,
    unmatched = node7_unmatched
  )
}


#' Recalculate Rank After Infraspecies Level 1 Match
#' @keywords internal
.recalculate_rank_infrasp1 <- function(df) {
  df |>
    dplyr::mutate(
      Matched.Rank.Calculated = dplyr::case_when(
        !is.na(Matched.Genus) &
          !is.na(Matched.Species) &
          !is.na(Matched.Infra.Rank) &
          !is.na(Matched.Infraspecies) &
          is.na(Matched.Infraspecies_2) ~ 3L,
        TRUE ~ NA_integer_
      ),
      valid_rank = (Rank == Matched.Rank.Calculated)
    )
}


#' Recalculate Rank After Infraspecies Level 2 Match
#' @keywords internal
.recalculate_rank_infrasp2 <- function(df) {
  df |>
    dplyr::mutate(
      Matched.Rank.Calculated = dplyr::case_when(
        !is.na(Matched.Genus) &
          !is.na(Matched.Species) &
          !is.na(Matched.Infra.Rank) &
          !is.na(Matched.Infraspecies) &
          Orig.Infra.Rank_2 == "F." &
          !is.na(Matched.Infraspecies_2) ~ 4L,
        TRUE ~ NA_integer_
      ),
      valid_rank = (Rank == Matched.Rank.Calculated)
    )
}



# ==============================================================================
# RANK VALIDATION - CRITICAL FOR FALSE POSITIVE PREVENTION
# ==============================================================================

#' Validate Rank Matches to Prevent False Positives
#'
#' @description
#' **CRITICAL FUNCTION:** Validates that the declared rank (user input) matches
#' the matched rank (database). This prevents false positives where:
#'
#' - User inputs: "Cattleya maxima var. alba" (Rank 3, doesn't exist)
#' - Database has: "Cattleya maxima" (Rank 2, exists)
#' - Without validation: Would incorrectly return "Threatened"
#' - With validation: Correctly rejects match, returns "Not threatened"
#'
#' @param matched_results List with matched and unmatched records from pipeline
#' @param use_infraspecies_2 Logical, whether Rank 4 is supported
#'
#' @return List with:
#'   \item{combined}{All records with valid_rank column}
#'   \item{valid_matches}{Only records with valid rank correspondence}
#'   \item{invalid_matches}{Records rejected due to rank mismatch}
#'   \item{unmatched}{Records that never matched}
#'
#' @keywords internal
.validate_rank_matches <- function(matched_results, use_infraspecies_2) {

  # ========================================================================
  # Calculate Matched Rank Based on Populated Columns
  # ========================================================================
  combined <- matched_results$matched |>
    dplyr::mutate(
      Matched.Rank.Calculated = .calculate_matched_rank(
        Matched.Genus,
        Matched.Species,
        Matched.Infra.Rank,
        Matched.Infraspecies,
        Matched.Infra.Rank_2,
        Matched.Infraspecies_2,
        use_infraspecies_2
      ),

      # CRITICAL: Declared Rank MUST match Matched Rank
      valid_rank = (Rank == Matched.Rank.Calculated)
    )

  # ========================================================================
  # Separate Valid from Invalid Matches
  # ========================================================================
  valid_matches <- combined |>
    dplyr::filter(valid_rank == TRUE, !is.na(Matched.Rank.Calculated))

  invalid_matches <- combined |>
    dplyr::filter(valid_rank == FALSE | is.na(Matched.Rank.Calculated))

  # ========================================================================
  # Report Invalid Matches (False Positives Rejected)
  # ========================================================================
  if (nrow(invalid_matches) > 0) {
    message(
      "Info: ", nrow(invalid_matches),
      " potential matches were rejected due to rank mismatch.\n",
      "  (e.g., trinomial input matching with binomial in database)"
    )
  }

  # ========================================================================
  # Return Validated Results
  # ========================================================================
  list(
    combined = combined,
    valid_matches = valid_matches,
    invalid_matches = invalid_matches,
    unmatched = matched_results$unmatched
  )
}


#' Calculate Matched Rank Based on Populated Columns
#'
#' @description
#' Determines the actual taxonomic rank of a match based on which columns
#' contain non-NA values.
#'
#' @param genus Matched genus
#' @param species Matched species
#' @param infra_rank Matched infraspecific rank tag
#' @param infraspecies Matched infraspecific epithet (level 1)
#' @param infra_rank_2 Matched infraspecific rank tag (level 2)
#' @param infraspecies_2 Matched infraspecific epithet (level 2)
#' @param use_infraspecies_2 Whether Rank 4 is supported
#'
#' @return Integer rank (1-4) or NA
#'
#' @keywords internal
.calculate_matched_rank <- function(genus,
                                    species,
                                    infra_rank,
                                    infraspecies,
                                    infra_rank_2,
                                    infraspecies_2,
                                    use_infraspecies_2) {

  dplyr::case_when(
    # Rank 1: Only genus matched
    !is.na(genus) & is.na(species) ~ 1L,

    # Rank 2: Genus + species, NO infraspecies
    !is.na(genus) &
      !is.na(species) &
      is.na(infra_rank) &
      is.na(infraspecies) ~ 2L,

    # Rank 3: Genus + species + infraspecies level 1, NO level 2
    !is.na(genus) &
      !is.na(species) &
      !is.na(infra_rank) &
      !is.na(infraspecies) &
      is.na(infra_rank_2) &
      is.na(infraspecies_2) ~ 3L,

    # Rank 4: All levels (only if supported)
    use_infraspecies_2 &
      !is.na(genus) &
      !is.na(species) &
      !is.na(infra_rank) &
      !is.na(infraspecies) &
      !is.na(infra_rank_2) &
      !is.na(infraspecies_2) ~ 4L,

    # No valid match
    TRUE ~ NA_integer_
  )
}



# ==============================================================================
# THREAT INFORMATION ADDITION
# ==============================================================================

#' Add Threat Information to Matched Results
#'
#' @description
#' Joins threat category and author information from the target database
#' based on validated matches. Handles different database structures
#' (original vs updated).
#'
#' @param results Validated results with combined matches
#' @param splist_class Original classified species data
#' @param target_df Target database
#' @param config Configuration list
#'
#' @return Tibble with threat information added
#'
#' @keywords internal
.add_threat_information <- function(results,
                                    splist_class,
                                    target_df,
                                    config) {

  # ========================================================================
  # Prepare Base Dataframe (All Original Inputs)
  # ========================================================================
  base_df <- splist_class |>
    dplyr::select(
      sorter, Orig.Name, Orig.Genus, Orig.Species,
      Orig.Infraspecies, Orig.Infraspecies_2,
      Rank, Orig.Infra.Rank, Orig.Infra.Rank_2, Author
    )

  # ========================================================================
  # Merge Results with Base
  # ========================================================================
  cols_to_exclude <- c(
    "Orig.Name", "Orig.Genus", "Orig.Species",
    "Orig.Infraspecies", "Orig.Infraspecies_2",
    "Rank", "Orig.Infra.Rank", "Orig.Infra.Rank_2", "Author"
  )

  res_complete <- base_df |>
    dplyr::left_join(
      results |> # results$combined
        dplyr::select(-dplyr::any_of(cols_to_exclude)),
      by = "sorter"
    ) |>
    dplyr::mutate(
      matched = tidyr::replace_na(matched, FALSE),
      Matched.Genus = dplyr::if_else(
        is.na(Matched.Genus),
        NA_character_,
        Matched.Genus
      ),
      Matched.Species = dplyr::if_else(
        is.na(Matched.Species),
        NA_character_,
        Matched.Species
      ),
      Matched.Infraspecies = dplyr::if_else(
        is.na(Matched.Infraspecies),
        NA_character_,
        Matched.Infraspecies
      ),
      Matched.Infraspecies_2 = dplyr::if_else(
        is.na(Matched.Infraspecies_2),
        NA_character_,
        Matched.Infraspecies_2
      )
    )

  # ========================================================================
  # Ensure valid_rank Column Exists
  # ========================================================================
  if (!"valid_rank" %in% colnames(res_complete)) {
    warning(
      "Column 'valid_rank' not found. Calculating as fallback.",
      call. = FALSE,
      immediate. = TRUE
    )

    res_complete <- res_complete |>
      dplyr::mutate(
        Matched.Rank.Calculated = .calculate_matched_rank(
          Matched.Genus,
          Matched.Species,
          Matched.Infra.Rank,
          Matched.Infraspecies,
          Matched.Infra.Rank_2,
          Matched.Infraspecies_2,
          config$use_infraspecies_2
        ),
        valid_rank = (Rank == Matched.Rank.Calculated)
      )
  }

  # ========================================================================
  # Join Threat Information (Database-Specific)
  # ========================================================================
  if (config$use_infraspecies_2) {
    output <- .join_threat_info_original(res_complete, target_df)
  } else {
    output <- .join_threat_info_updated(res_complete, target_df)
  }

  return(output)
}


#' Join Threat Info for Original Database (Supports Rank 4)
#'
#' @keywords internal
.join_threat_info_original <- function(res_complete, target_df) {

  # Prepare target threat data
  target_threat <- target_df |>
    dplyr::select(
      genus, species, tag, infraspecies, infraspecies_2,
      threat_category, accepted_name_author
    ) |>
    dplyr::mutate(tag = toupper(tag)) |>
    dplyr::distinct(
      genus, species, tag, infraspecies, infraspecies_2,
      .keep_all = TRUE
    )

  # ========================================================================
  # Join by Rank
  # ========================================================================

  # Rank 2: Binomial matches with valid rank
  output_rank2 <- res_complete |>
    dplyr::filter(Rank == 2, matched == TRUE, valid_rank == TRUE) |>
    dplyr::left_join(
      target_threat |>
        dplyr::filter(
          is.na(tag),
          is.na(infraspecies),
          is.na(infraspecies_2)
        ),
      by = c("Matched.Genus" = "genus", "Matched.Species" = "species"),
      na_matches = "never"
    )

  # Rank 3: Trinomial matches with valid rank
  output_rank3 <- res_complete |>
    dplyr::filter(Rank == 3, matched == TRUE, valid_rank == TRUE) |>
    dplyr::left_join(
      target_threat |>
        dplyr::filter(
          !is.na(tag),
          !is.na(infraspecies),
          is.na(infraspecies_2)
        ),
      by = c(
        "Matched.Genus" = "genus",
        "Matched.Species" = "species",
        "Matched.Infra.Rank" = "tag",
        "Matched.Infraspecies" = "infraspecies"
      ),
      na_matches = "never"
    )

  # Rank 4: Quaternomial matches with valid rank
  output_rank4 <- res_complete |>
    dplyr::filter(Rank == 4, matched == TRUE, valid_rank == TRUE) |>
    dplyr::left_join(
      target_threat |>
        dplyr::filter(
          !is.na(tag),
          !is.na(infraspecies),
          !is.na(infraspecies_2)
        ),
      by = c(
        "Matched.Genus" = "genus",
        "Matched.Species" = "species",
        "Matched.Infra.Rank" = "tag",
        "Matched.Infraspecies" = "infraspecies",
        "Matched.Infraspecies_2" = "infraspecies_2"
      ),
      na_matches = "never"
    )

  # Invalid matches (matched but wrong rank)
  output_invalid <- res_complete |>
    dplyr::filter(matched == TRUE, valid_rank == FALSE) |>
    dplyr::mutate(
      tag = NA_character_,
      infraspecies = NA_character_,
      infraspecies_2 = NA_character_,
      threat_category = NA_character_,
      accepted_name_author = NA_character_
    )

  # Unmatched
  output_unmatched <- res_complete |>
    dplyr::filter(matched == FALSE) |>
    dplyr::mutate(
      tag = NA_character_,
      infraspecies = NA_character_,
      infraspecies_2 = NA_character_,
      threat_category = NA_character_,
      accepted_name_author = NA_character_
    )

  # Combine all
  dplyr::bind_rows(
    output_rank2,
    output_rank3,
    output_rank4,
    output_invalid,
    output_unmatched
  ) |>
    dplyr::arrange(sorter)
}


#' Join Threat Info for Updated Database (Rank 3 max)
#'
#' @keywords internal
.join_threat_info_updated <- function(res_complete, target_df) {

  # Prepare target threat data
  target_threat <- target_df |>
    dplyr::select(
      genus, species, infraspecies, tag_acc,
      threat_category, accepted_name_author
    ) |>
    dplyr::distinct(genus, species, tag_acc, infraspecies, .keep_all = TRUE)

  # ========================================================================
  # Join by Rank
  # ========================================================================

  # Rank 2: Binomial
  output_rank2 <- res_complete |>
    dplyr::filter(Rank == 2, matched == TRUE, valid_rank == TRUE) |>
    dplyr::left_join(
      target_threat |>
        dplyr::filter(is.na(tag_acc), is.na(infraspecies)),
      by = c("Matched.Genus" = "genus", "Matched.Species" = "species"),
      na_matches = "never"
    )

  # Rank 3: Trinomial
  output_rank3 <- res_complete |>
    dplyr::filter(Rank == 3, matched == TRUE, valid_rank == TRUE) |>
    dplyr::left_join(
      target_threat |>
        dplyr::filter(!is.na(tag_acc), !is.na(infraspecies)),
      by = c(
        "Matched.Genus" = "genus",
        "Matched.Species" = "species",
        "Matched.Infra.Rank" = "tag_acc",
        "Matched.Infraspecies" = "infraspecies"
      ),
      na_matches = "never"
    )

  # Invalid matches
  output_invalid <- res_complete |>
    dplyr::filter(matched == TRUE, valid_rank == FALSE) |>
    dplyr::mutate(
      tag_acc = NA_character_,
      infraspecies = NA_character_,
      threat_category = NA_character_,
      accepted_name_author = NA_character_
    )

  # Unmatched
  output_unmatched <- res_complete |>
    dplyr::filter(matched == FALSE) |>
    dplyr::mutate(
      tag_acc = NA_character_,
      infraspecies = NA_character_,
      threat_category = NA_character_,
      accepted_name_author = NA_character_
    )

  # Combine all
  dplyr::bind_rows(
    output_rank2,
    output_rank3,
    output_invalid,
    output_unmatched
  ) |>
    dplyr::arrange(sorter)
}



# ==============================================================================
# OUTPUT FINALIZATION
# ==============================================================================

#' Finalize Matching Output
#'
#' @description
#' Creates formatted names, calculates match levels, validates output,
#' and adds metadata.
#'
#' @param output Output with threat information
#' @param splist_class Original classified species data
#' @param config Configuration list
#'
#' @return Final formatted tibble with metadata
#'
#' @keywords internal
.finalize_output <- function(output, splist_class, config) {

  # ========================================================================
  # Calculate Matched Rank
  # ========================================================================
  output_f <- output |>
    dplyr::mutate(
      Matched.Rank = .calculate_matched_rank(
        Matched.Genus,
        Matched.Species,
        Matched.Infra.Rank,
        Matched.Infraspecies,
        Matched.Infra.Rank_2,
        Matched.Infraspecies_2,
        config$use_infraspecies_2
      )
    )

  # ========================================================================
  # Validate Rank 4 Coherence (if database doesn't support it)
  # ========================================================================
  if (!config$use_infraspecies_2) {
    output_f <- .validate_no_rank4(output_f)
  }

  # ========================================================================
  # Create Formatted Matched Names
  # ========================================================================
  output_f <- output_f |>
    dplyr::mutate(
      Matched.Name = .create_formatted_name(
        Matched.Genus,
        Matched.Species,
        Matched.Infra.Rank,
        Matched.Infraspecies,
        Matched.Infra.Rank_2,
        Matched.Infraspecies_2,
        Matched.Rank
      ),
      Orig.Name = str_to_simple_cap(Orig.Name)
    ) |>
    dplyr::mutate(
      Matched.Name = stringr::str_squish(Matched.Name)
    )

  # ========================================================================
  # Calculate Match Levels and Threat Status
  # ========================================================================
  output_f <- output_f |>
    dplyr::mutate(
      Comp.Rank = (Rank == Matched.Rank),

      Match.Level = dplyr::case_when(
        is.na(Matched.Rank) ~ "No match",
        Rank == Matched.Rank ~ "Exact rank",
        Rank > Matched.Rank ~ "Matched at higher rank",
        Rank < Matched.Rank ~ "Matched at lower rank (unexpected)",
        TRUE ~ "Unknown"
      ),

      Threat.Status = dplyr::case_when(
        is.na(Matched.Genus) &
          is.na(Matched.Species) &
          is.na(Matched.Infraspecies) ~ "Not threatened",
        !is.na(threat_category) ~ threat_category,
        TRUE ~ "Not threatened"
      )
    )

  # ========================================================================
  # Format Final Columns
  # ========================================================================
  output_f <- output_f |>
    dplyr::mutate(
      Matched.Name = dplyr::if_else(
        is.na(Matched.Name),
        "---",
        Matched.Name
      ),
      accepted_name_author = dplyr::if_else(
        is.na(accepted_name_author),
        "---",
        accepted_name_author
      )
    ) |>
    dplyr::relocate(
      c("sorter", "Orig.Name", "Matched.Name", "Threat.Status",
        "Author", "accepted_name_author", "Matched.Rank",
        "Comp.Rank", "Match.Level")
    )

  # ========================================================================
  # Final Validations
  # ========================================================================
  .validate_final_output(output_f, splist_class)

  # ========================================================================
  # Add Metadata
  # ========================================================================
  output_f <- .add_metadata(output_f, splist_class, config)

  return(output_f)
}


#' Validate No Rank 4 in Output (Updated Database)
#'
#' @keywords internal
.validate_no_rank4 <- function(output) {

  invalid_rank4 <- sum(output$Matched.Rank == 4, na.rm = TRUE)

  if (invalid_rank4 > 0) {
    warning(
      "Found ", invalid_rank4,
      " matches with Rank 4, but use_infraspecies_2 = FALSE.\n",
      "This is a bug in the matching pipeline. Correcting to NA.",
      call. = FALSE,
      immediate. = TRUE
    )

    output$Matched.Rank[output$Matched.Rank == 4] <- NA_integer_
    output$Matched.Infraspecies_2[
      !is.na(output$Matched.Rank) & output$Matched.Rank == 4
    ] <- NA_character_
  }

  return(output)
}


#' Create Formatted Scientific Name
#'
#' @description
#' Formats matched components into a proper scientific name with correct
#' capitalization.
#'
#' @keywords internal
.create_formatted_name <- function(genus,
                                   species,
                                   infra_rank,
                                   infraspecies,
                                   infra_rank_2,
                                   infraspecies_2,
                                   rank) {

  dplyr::case_when(
    # No match
    is.na(genus) ~ "---",

    # Rank 1: Only genus
    rank == 1 ~ str_to_simple_cap(genus),

    # Rank 2: Binomial
    rank == 2 ~ paste(
      str_to_simple_cap(genus),
      stringr::str_to_lower(species)
    ),

    # Rank 3: Trinomial with infraspecific rank
    rank == 3 & !is.na(infra_rank) ~ paste(
      str_to_simple_cap(genus),
      stringr::str_to_lower(species),
      stringr::str_to_lower(infra_rank),
      stringr::str_to_lower(infraspecies)
    ),

    # Rank 4: Quaternomial with both ranks
    rank == 4 &
      !is.na(infra_rank) &
      !is.na(infra_rank_2) &
      !is.na(infraspecies_2) ~ paste(
        str_to_simple_cap(genus),
        stringr::str_to_lower(species),
        stringr::str_to_lower(infra_rank),
        stringr::str_to_lower(infraspecies),
        stringr::str_to_lower(infra_rank_2),
        stringr::str_to_lower(infraspecies_2)
      ),

    # Unexpected cases
    TRUE ~ "---"
  )
}


#' Validate Final Output Structure
#'
#' @keywords internal
.validate_final_output <- function(output, splist_class) {

  # Validate row count
  assertthat::assert_that(
    nrow(splist_class) == nrow(output),
    msg = paste0(
      "Final output row count (", nrow(output),
      ") does not match input (", nrow(splist_class), ")"
    )
  )

  # Validate all sorter values present
  assertthat::assert_that(
    all(splist_class$sorter %in% output$sorter),
    msg = "Some input records are missing from output"
  )

  # Validate sort order
  assertthat::assert_that(
    all(output$sorter == sort(output$sorter)),
    msg = "Output records are not in correct order"
  )

  invisible(TRUE)
}


#' Add Metadata to Final Output
#'
#' @keywords internal
.add_metadata <- function(output, splist_class, config) {

  attr(output, "use_infraspecies_2") <- config$use_infraspecies_2
  attr(output, "target_database") <- config$source
  attr(output, "matching_date") <- Sys.Date()
  attr(output, "n_input") <- nrow(splist_class)
  attr(output, "n_matched") <- sum(output$matched, na.rm = TRUE)
  attr(output, "match_rate") <- round(
    sum(output$matched, na.rm = TRUE) / nrow(splist_class) * 100,
    2
  )

  return(output)
}
