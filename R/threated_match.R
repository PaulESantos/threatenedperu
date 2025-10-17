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
#' @return
#' A tibble with detailed matching results including matched names, threat status,
#' and matching methodology information.
#'
#' @seealso
#' \code{\link{is_threatened_peru}} for a simplified interface
#'
#' @examples
#' \dontrun{
#' species_list <- c("Cattleya maxima", "Polylepis incana")
#' results <- matching_threatenedperu(species_list, source = "original")
#' }
#'
#' @export
matching_threatenedperu <- function(splist, source = "original") {

  # ==========================================================================
  # SECTION 1: Target Database Selection and Validation
  # ==========================================================================

  # Validate source parameter
  if (!is.character(source) || length(source) != 1) {
    stop("source must be a single character string: 'original' or 'updated'",
         call. = FALSE)
  }

  if (!source %in% c("original", "updated")) {
    stop(
      "Invalid source value: '", source, "'. Must be 'original' or 'updated'",
      call. = FALSE
    )
  }

  # Determine if infraspecies_2 is supported
  use_infraspecies_2 <- (source == "original")

  # Load database using internal function
  target_prepared <- tryCatch({
    get_threatened_data(type = source)
  }, error = function(e) {
    stop(
      "Failed to load database '", source, "'.\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })

  # Validate database structure
  if (use_infraspecies_2) {
    required_cols <- c("genus", "species", "tag", "infraspecies",
                       "infraspecies_2", "threat_category")
  } else {
    required_cols <- c("genus", "species", "tag_acc", "infraspecies",
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

  # ==========================================================================
  # SECTION 2: Input Processing and Classification
  # ==========================================================================

  # Classify species names into taxonomic components
  splist_class <- .splist_classify(splist) |>
    .transform_split_classify()

  # Check for and handle non-binomial names
  non_binomial <- .check_binomial(splist_class, splist = splist)

  if (length(non_binomial) != 0) {
    df <- splist_class[-non_binomial, ]
  } else {
    df <- splist_class
  }

  # ==========================================================================
  # SECTION 2B: Initialize ALL Matching Columns
  # ==========================================================================

  all_matching_cols <- c(
    'Matched.Genus',
    'Matched.Species',
    'Matched.Infra.Rank',
    'Matched.Infraspecies',
    'Matched.Infra.Rank_2',
    'Matched.Infraspecies_2'
  )

  for (col in all_matching_cols) {
    if (!col %in% colnames(df)) {
      df[[col]] <- NA_character_
    }
  }

  if (!'Orig.Infraspecies' %in% colnames(df)) {
    df$Orig.Infraspecies <- NA_character_
  }

  if (!'Orig.Infraspecies_2' %in% colnames(df)) {
    df$Orig.Infraspecies_2 <- NA_character_
  }

  attr(df, "use_infraspecies_2") <- use_infraspecies_2

  if (!use_infraspecies_2) {
    df$Orig.Infraspecies_2 <- NA_character_
    df$Matched.Infraspecies_2 <- NA_character_

    message(
      "Note: Using database '", source,
      "' which supports the updated names of species listed in DS 043-2006-AG."
    )
  }

  # ==========================================================================
  # SECTION 2C: Validate Rank 4 with Database Compatibility
  # ==========================================================================

  if (!use_infraspecies_2 && any(df$Rank == 4, na.rm = TRUE)) {
    rank4_count <- sum(df$Rank == 4, na.rm = TRUE)

    warning(
      rank4_count, " species with Rank 4 detected. ",
      "Database '", source, "' does not support infraspecies_2. ",
      "These species will not be matched.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  # ==========================================================================
  # SECTION 3: Hierarchical Matching Pipeline
  # ==========================================================================

  # -------------------------------------------------------------------------
  # Node 1: Direct Match (Exact match for full name)
  # -------------------------------------------------------------------------
  Node_1_processed <- df |>
    direct_match(target_df = target_prepared,
                 source = source)

  Node_1_TRUE <- Node_1_processed |>
    dplyr::filter(direct_match == TRUE)

  Node_1_FALSE <- Node_1_processed |>
    dplyr::filter(direct_match == FALSE)

  assertthat::assert_that(
    nrow(df) == (nrow(Node_1_TRUE) + nrow(Node_1_FALSE)),
    msg = "Row count mismatch after Node 1: Direct Match"
  )

  # -------------------------------------------------------------------------
  # Node 2: Genus Match (Exact genus match)
  # -------------------------------------------------------------------------
  Node_2_processed <- Node_1_FALSE |>
    genus_match(target_prepared)

  Node_2_TRUE <- Node_2_processed |>
    dplyr::filter(genus_match == TRUE)

  Node_2_FALSE <- Node_2_processed |>
    dplyr::filter(genus_match == FALSE)

  assertthat::assert_that(
    nrow(Node_2_processed) == (nrow(Node_2_TRUE) + nrow(Node_2_FALSE)),
    msg = "Row count mismatch after Node 2: Genus Match"
  )

  # -------------------------------------------------------------------------
  # Node 3: Fuzzy Match Genus (Approximate genus matching)
  # -------------------------------------------------------------------------
  Node_3_processed <- Node_2_FALSE |>
    fuzzy_match_genus(target_prepared)

  Node_3_TRUE <- Node_3_processed |>
    dplyr::filter(fuzzy_match_genus == TRUE)

  Node_3_FALSE <- Node_3_processed |>
    dplyr::filter(fuzzy_match_genus == FALSE)

  assertthat::assert_that(
    nrow(Node_3_processed) == (nrow(Node_3_TRUE) + nrow(Node_3_FALSE)),
    msg = "Row count mismatch after Node 3: Fuzzy Genus Match"
  )

  # -------------------------------------------------------------------------
  # Node 4: Direct Match Species within Genus
  # -------------------------------------------------------------------------
  Node_4_input <- Node_3_TRUE |>
    dplyr::bind_rows(Node_2_TRUE)

  Node_4_processed <- Node_4_input |>
    direct_match_species_within_genus(target_prepared)

  Node_4_TRUE <- Node_4_processed |>
    dplyr::filter(direct_match_species_within_genus == TRUE)

  Node_4_FALSE <- Node_4_processed |>
    dplyr::filter(direct_match_species_within_genus == FALSE)

  assertthat::assert_that(
    nrow(Node_4_processed) == (nrow(Node_4_TRUE) + nrow(Node_4_FALSE)),
    msg = "Row count mismatch after Node 4: Species Match"
  )

  # -------------------------------------------------------------------------
  # Node 5a: Suffix Match Species within Genus
  # -------------------------------------------------------------------------
  Node_5a_processed <- Node_4_FALSE |>
    suffix_match_species_within_genus(target_prepared)

  Node_5a_TRUE <- Node_5a_processed |>
    dplyr::filter(suffix_match_species_within_genus == TRUE)

  Node_5a_FALSE <- Node_5a_processed |>
    dplyr::filter(suffix_match_species_within_genus == FALSE)

  assertthat::assert_that(
    nrow(Node_4_FALSE) == (nrow(Node_5a_TRUE) + nrow(Node_5a_FALSE)),
    msg = "Row count mismatch after Node 5a: Suffix Match"
  )

  # -------------------------------------------------------------------------
  # Node 5b: Fuzzy Match Species within Genus
  # -------------------------------------------------------------------------
  Node_5b_input <- Node_5a_FALSE |>
    dplyr::filter(!is.na(Orig.Species))

  Node_5b_processed <- Node_5b_input |>
    fuzzy_match_species_within_genus(target_prepared)

  Node_5b_TRUE <- Node_5b_processed |>
    dplyr::filter(fuzzy_match_species_within_genus == TRUE)

  Node_5b_FALSE <- Node_5b_processed |>
    dplyr::filter(fuzzy_match_species_within_genus == FALSE)

  assertthat::assert_that(
    nrow(Node_5b_input) == (nrow(Node_5b_TRUE) + nrow(Node_5b_FALSE)),
    msg = "Row count mismatch after Node 5b: Fuzzy Species Match"
  )

  # ==========================================================================
  # SECTION 4: Combine Results and Validate Ranks CRITICAL SECTION
  # ==========================================================================

  combined <- dplyr::bind_rows(
    Node_1_TRUE,
    Node_2_TRUE |> dplyr::filter(!is.na(Matched.Genus) & Rank == 1),
    Node_4_TRUE,
    Node_5a_TRUE,
    Node_5b_TRUE
  )

  # =========================================================================
  # CRITICAL VALIDATION: Matched Rank vs Declared Rank
  # =========================================================================
  # This validation prevents FALSE POSITIVES where:
  # - User inputs: "Cattleya maxima var. alba" (Rank 3, doesn't exist)
  # - Database has: "Cattleya maxima" (Rank 2, exists)
  # - Without validation: Would incorrectly match and return "Threatened"
  # - With validation: Correctly rejects match, returns "Not threatened"
  # =========================================================================

  combined <- combined |>
    dplyr::mutate(
      # Calculate the actual matched rank based on populated columns
      Matched.Rank.Calculated = dplyr::case_when(
        # Rank 1: Only genus matched
        !is.na(Matched.Genus) &
          is.na(Matched.Species) ~ 1L,

        # Rank 2: Genus + species matched (binomial), NO infraspecies
        !is.na(Matched.Genus) &
          !is.na(Matched.Species) &
          is.na(Matched.Infra.Rank) &
          is.na(Matched.Infraspecies) ~ 2L,

        # Rank 3: Genus + species + infraspecies level 1 (trinomial)
        !is.na(Matched.Genus) &
          !is.na(Matched.Species) &
          !is.na(Matched.Infra.Rank) &
          !is.na(Matched.Infraspecies) &
          is.na(Matched.Infra.Rank_2) &
          is.na(Matched.Infraspecies_2) ~ 3L,

        # Rank 4: All levels matched (quaternomial)
        use_infraspecies_2 &
          !is.na(Matched.Genus) &
          !is.na(Matched.Species) &
          !is.na(Matched.Infra.Rank) &
          !is.na(Matched.Infraspecies) &
          !is.na(Matched.Infra.Rank_2) &
          !is.na(Matched.Infraspecies_2) ~ 4L,

        # No valid match
        TRUE ~ NA_integer_
      ),

      # Validate: Declared Rank MUST match Matched Rank
      valid_rank = (Rank == Matched.Rank.Calculated)
    )

  # =========================================================================
  # Filter: Keep ONLY matches with valid rank correspondence
  # =========================================================================

  matched <- combined |>
    dplyr::filter(valid_rank == TRUE, !is.na(Matched.Rank.Calculated))

  # Identify INVALID matches (false positives to be rejected)
  invalid_matches <- combined |>
    dplyr::filter(valid_rank == FALSE | is.na(Matched.Rank.Calculated))

  # Add diagnostic info for invalid matches
  if (nrow(invalid_matches) > 0) {
    message(
      "Info: ", nrow(invalid_matches), " potential matches were rejected due to rank mismatch.\n",
      "  (e.g., trinomial input matching with binomial in database)"
    )
  }

  # Combine all unmatched records
  unmatched <- dplyr::bind_rows(
    invalid_matches,  # ← CRITICAL: Include rejected false positives
    Node_3_FALSE,
    Node_5b_FALSE
  )

  # ==========================================================================
  # SECTION 5: Infraspecies Matching (Nodes 6-7)
  # ==========================================================================

  # -------------------------------------------------------------------------
  # Node 6a: Direct Match Infraspecific Rank
  # -------------------------------------------------------------------------
  Node_6_input <- unmatched |>
    dplyr::filter(
      !is.na(Orig.Infra.Rank),
      !is.na(Matched.Genus),
      !is.na(Matched.Species),
      is.na(Matched.Infraspecies)
    )

  Node_6a_processed <- Node_6_input |>
    direct_match_infra_rank_within_species(target_df = target_prepared,
                                           source = source)

  Node_6a_TRUE <- Node_6a_processed |>
    dplyr::filter(direct_match_infra_rank == TRUE)

  Node_6a_FALSE <- Node_6a_processed |>
    dplyr::filter(direct_match_infra_rank == FALSE)

  # -------------------------------------------------------------------------
  # Node 6b: Fuzzy Match Infraspecific Epithet
  # -------------------------------------------------------------------------
  Node_6b_processed <- Node_6a_TRUE |>
    fuzzy_match_infraspecies_within_species(target_df = target_prepared,
                                            source = source) |>
    dplyr::mutate(
      # Recalculate matched rank
      Matched.Rank.Calculated = dplyr::case_when(
        !is.na(Matched.Genus) &
          !is.na(Matched.Species) &
          !is.na(Matched.Infra.Rank) &
          !is.na(Matched.Infraspecies) &
          is.na(Matched.Infraspecies_2) ~ 3L,
        TRUE ~ NA_integer_
      ),
      # Validate rank
      valid_rank = (Rank == Matched.Rank.Calculated)
    )

  Node_6b_TRUE <- Node_6b_processed |>
    dplyr::filter(
      fuzzy_match_infraspecies == TRUE,
      valid_rank == TRUE,
      Rank == 3
    )

  Node_6b_FALSE <- Node_6b_processed |>
    dplyr::filter(
      fuzzy_match_infraspecies == FALSE |
        valid_rank == FALSE |
        Rank != 3
    )

  # -------------------------------------------------------------------------
  # Node 7: Infraspecies Level 2 Fuzzy Matching
  # -------------------------------------------------------------------------
  if (use_infraspecies_2) {
    Node_7_input <- Node_6b_FALSE |>
      dplyr::filter(
        Rank == 4,
        !is.na(Orig.Infraspecies_2),
        is.na(Matched.Infraspecies_2),
        !is.na(Matched.Infraspecies)
      )

    if (nrow(Node_7_input) > 0) {
      Node_7_processed <- Node_7_input |>
        fuzzy_match_infraspecies2_within_infraspecies(target_prepared) |>
        dplyr::mutate(
          # Recalculate matched rank
          Matched.Rank.Calculated = dplyr::case_when(
            !is.na(Matched.Genus) &
              !is.na(Matched.Species) &
              !is.na(Matched.Infra.Rank) &
              !is.na(Matched.Infraspecies) &
              Orig.Infra.Rank_2 == "F." &
              !is.na(Matched.Infraspecies_2) ~ 4L,
            TRUE ~ NA_integer_
          ),
          # Validate rank
          valid_rank = (Rank == Matched.Rank.Calculated)
        )

      Node_7_TRUE <- Node_7_processed |>
        dplyr::filter(
          valid_rank == TRUE,
          fuzzy_match_infraspecies_2 == TRUE
        ) |>
        dplyr::mutate(
          Matched.Infra.Rank_2 = "F."
        )

      Node_7_FALSE <- Node_7_processed |>
        dplyr::filter(
          valid_rank == FALSE | fuzzy_match_infraspecies_2 == FALSE
        )

    } else {
      Node_7_TRUE <- Node_6b_FALSE[0, ]
      Node_7_FALSE <- Node_6b_FALSE[0, ]
    }

    # Combine all matches
    matched_f <- dplyr::bind_rows(
      matched,
      Node_6b_TRUE,
      Node_7_TRUE
    )

    unmatched_f <- dplyr::bind_rows(
      Node_6a_FALSE,
      Node_7_FALSE
    )

    res <- dplyr::bind_rows(matched_f, unmatched_f, .id = 'matched') |>
      dplyr::mutate(matched = (matched == 1))

  } else {
    # Without infraspecies_2, no Node 7
    matched_f <- dplyr::bind_rows(matched, Node_6b_TRUE)

    res <- dplyr::bind_rows(matched_f, Node_6b_FALSE, .id = 'matched') |>
      dplyr::mutate(matched = (matched == 1))
  }

  # ==========================================================================
  # SECTION 6: Add Threat Information and Format Output
  # ==========================================================================

  base_df <- splist_class |>
    dplyr::select(
      sorter, Orig.Name, Orig.Genus, Orig.Species,
      Orig.Infraspecies, Orig.Infraspecies_2,
      Rank, Orig.Infra.Rank, Orig.Infra.Rank_2, Author
    )

  cols_to_exclude <- c(
    "Orig.Name", "Orig.Genus", "Orig.Species",
    "Orig.Infraspecies", "Orig.Infraspecies_2",
    "Rank", "Orig.Infra.Rank", "Orig.Infra.Rank_2", "Author"
  )

  res_complete <- base_df |>
    dplyr::left_join(
      res |> dplyr::select(-dplyr::any_of(cols_to_exclude)),
      by = "sorter"
    )

  res_complete <- res_complete |>
    dplyr::mutate(
      matched = tidyr::replace_na(matched, FALSE),
      Matched.Genus = dplyr::if_else(is.na(Matched.Genus),
                                     NA_character_, Matched.Genus),
      Matched.Species = dplyr::if_else(is.na(Matched.Species),
                                       NA_character_, Matched.Species),
      Matched.Infraspecies = dplyr::if_else(is.na(Matched.Infraspecies),
                                            NA_character_, Matched.Infraspecies),
      Matched.Infraspecies_2 = dplyr::if_else(is.na(Matched.Infraspecies_2),
                                              NA_character_,
                                              Matched.Infraspecies_2)
    )

  # =========================================================================
  # CRITICAL: Add valid_rank if not present (from SECTION 4)
  # =========================================================================
  # valid_rank should have been calculated in SECTION 4, but ensure it exists
  if (!"valid_rank" %in% colnames(res_complete)) {
    warning(
      "Column 'valid_rank' not found in res_complete. ",
      "This indicates the validation in SECTION 4 was not applied. ",
      "Calculating it now as a fallback.",
      call. = FALSE,
      immediate. = TRUE
    )

    res_complete <- res_complete |>
      dplyr::mutate(
        Matched.Rank.Calculated = dplyr::case_when(
          !is.na(Matched.Genus) & is.na(Matched.Species) ~ 1L,
          !is.na(Matched.Genus) & !is.na(Matched.Species) &
            is.na(Matched.Infra.Rank) & is.na(Matched.Infraspecies) ~ 2L,
          !is.na(Matched.Genus) & !is.na(Matched.Species) &
            !is.na(Matched.Infra.Rank) & !is.na(Matched.Infraspecies) &
            is.na(Matched.Infra.Rank_2) & is.na(Matched.Infraspecies_2) ~ 3L,
          use_infraspecies_2 &
            !is.na(Matched.Genus) & !is.na(Matched.Species) &
            !is.na(Matched.Infra.Rank) & !is.na(Matched.Infraspecies) &
            !is.na(Matched.Infra.Rank_2) & !is.na(Matched.Infraspecies_2) ~ 4L,
          TRUE ~ NA_integer_
        ),
        valid_rank = (Rank == Matched.Rank.Calculated)
      )
  }

  # =========================================================================
  # Join threat information based on Rank AND valid_rank
  # =========================================================================

  if (use_infraspecies_2) {

    # -----------------------------------------------------------------------
    # Prepare target_threat data
    # -----------------------------------------------------------------------
    target_threat <- target_prepared |>
      dplyr::select(
        genus, species, tag, infraspecies, infraspecies_2,
        threat_category, accepted_name_author
      ) |>
      dplyr::mutate(tag = toupper(tag)) |>
      dplyr::distinct(genus, species, tag, infraspecies, infraspecies_2,
                      .keep_all = TRUE)

    # -----------------------------------------------------------------------
    # RANK 2: Binomial matches with valid rank
    # -----------------------------------------------------------------------
    output_rank2 <- res_complete |>
      dplyr::filter(
        Rank == 2,
        matched == TRUE,
        valid_rank == TRUE  # ← CRITICAL: Only valid matches
      ) |>
      dplyr::left_join(
        target_threat |>
          dplyr::filter(is.na(tag), is.na(infraspecies), is.na(infraspecies_2)),
        by = c(
          "Matched.Genus" = "genus",
          "Matched.Species" = "species"
        ),
        na_matches = "never"
      )

    # -----------------------------------------------------------------------
    # RANK 3: Trinomial matches with valid rank
    # -----------------------------------------------------------------------
    output_rank3 <- res_complete |>
      dplyr::filter(
        Rank == 3,
        matched == TRUE,
        valid_rank == TRUE  # ← CRITICAL: Only valid matches
      ) |>
      dplyr::left_join(
        target_threat |>
          dplyr::filter(!is.na(tag), !is.na(infraspecies), is.na(infraspecies_2)),
        by = c(
          "Matched.Genus" = "genus",
          "Matched.Species" = "species",
          "Matched.Infra.Rank" = "tag",
          "Matched.Infraspecies" = "infraspecies"
        ),
        na_matches = "never"
      )

    # -----------------------------------------------------------------------
    # RANK 4: Quaternomial matches with valid rank
    # -----------------------------------------------------------------------
    output_rank4 <- res_complete |>
      dplyr::filter(
        Rank == 4,
        matched == TRUE,
        valid_rank == TRUE  # ← CRITICAL: Only valid matches
      ) |>
      dplyr::left_join(
        target_threat |>
          dplyr::filter(!is.na(tag), !is.na(infraspecies), !is.na(infraspecies_2)),
        by = c(
          "Matched.Genus" = "genus",
          "Matched.Species" = "species",
          "Matched.Infra.Rank" = "tag",
          "Matched.Infraspecies" = "infraspecies",
          "Matched.Infraspecies_2" = "infraspecies_2"
        ),
        na_matches = "never"
      )

    # -----------------------------------------------------------------------
    # INVALID matches (matched == TRUE but valid_rank == FALSE)
    # These are the FALSE POSITIVES we need to exclude
    # -----------------------------------------------------------------------
    output_invalid <- res_complete |>
      dplyr::filter(
        matched == TRUE,
        valid_rank == FALSE  # ← These are invalid matches
      ) |>
      dplyr::mutate(
        tag = NA_character_,
        infraspecies = NA_character_,
        infraspecies_2 = NA_character_,
        threat_category = NA_character_,
        accepted_name_author = NA_character_
      )

    # -----------------------------------------------------------------------
    # Unmatched records (matched == FALSE)
    # -----------------------------------------------------------------------
    output_unmatched <- res_complete |>
      dplyr::filter(matched == FALSE) |>
      dplyr::mutate(
        tag = NA_character_,
        infraspecies = NA_character_,
        infraspecies_2 = NA_character_,
        threat_category = NA_character_,
        accepted_name_author = NA_character_
      )

    # -----------------------------------------------------------------------
    # Combine all outputs
    # -----------------------------------------------------------------------
    output <- dplyr::bind_rows(
      output_rank2,
      output_rank3,
      output_rank4,
      output_invalid,    # ← Include invalid matches with NA threat_category
      output_unmatched
    ) |>
      dplyr::arrange(sorter)

  } else {

    # =====================================================================
    # For updated database (no infraspecies_2 support)
    # =====================================================================

    target_threat <- target_prepared |>
      dplyr::select(
        genus, species, infraspecies, tag_acc,
        threat_category, accepted_name_author
      ) |>
      dplyr::distinct(genus, species, tag_acc, infraspecies, .keep_all = TRUE)

    # -----------------------------------------------------------------------
    # RANK 2: Binomial matches with valid rank
    # -----------------------------------------------------------------------
    output_rank2 <- res_complete |>
      dplyr::filter(
        Rank == 2,
        matched == TRUE,
        valid_rank == TRUE
      ) |>
      dplyr::left_join(
        target_threat |>
          dplyr::filter(is.na(tag_acc), is.na(infraspecies)),
        by = c(
          "Matched.Genus" = "genus",
          "Matched.Species" = "species"
        ),
        na_matches = "never"
      )

    # -----------------------------------------------------------------------
    # RANK 3: Trinomial matches with valid rank
    # -----------------------------------------------------------------------
    output_rank3 <- res_complete |>
      dplyr::filter(
        Rank == 3,
        matched == TRUE,
        valid_rank == TRUE
      ) |>
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

    # -----------------------------------------------------------------------
    # INVALID matches (matched == TRUE but valid_rank == FALSE)
    # -----------------------------------------------------------------------
    output_invalid <- res_complete |>
      dplyr::filter(
        matched == TRUE,
        valid_rank == FALSE
      ) |>
      dplyr::mutate(
        tag_acc = NA_character_,
        infraspecies = NA_character_,
        threat_category = NA_character_,
        accepted_name_author = NA_character_
      )

    # -----------------------------------------------------------------------
    # Unmatched records
    # -----------------------------------------------------------------------
    output_unmatched <- res_complete |>
      dplyr::filter(matched == FALSE) |>
      dplyr::mutate(
        tag_acc = NA_character_,
        infraspecies = NA_character_,
        threat_category = NA_character_,
        accepted_name_author = NA_character_
      )

    # -----------------------------------------------------------------------
    # Combine all outputs
    # -----------------------------------------------------------------------
    output <- dplyr::bind_rows(
      output_rank2,
      output_rank3,
      output_invalid,
      output_unmatched
    ) |>
      dplyr::arrange(sorter)
  }
  # ==========================================================================
  # SECTION 7: Calculate Matched Rank and Create Formatted Names
  # ==========================================================================

  output_f <- output |>
    dplyr::mutate(
      Matched.Rank = dplyr::case_when(
        # Rank 1: Only genus matched
        !is.na(Matched.Genus) & is.na(Matched.Species) ~ 1L,

        # Rank 2: Genus + species matched, no infraspecies
        !is.na(Matched.Genus) &
          !is.na(Matched.Species) &
          is.na(Matched.Infra.Rank) &
          is.na(Matched.Infraspecies) ~ 2L,

        # Rank 3: Genus + species + infraspecies level 1
        !is.na(Matched.Genus) &
          !is.na(Matched.Species) &
          !is.na(Matched.Infra.Rank) &
          !is.na(Matched.Infraspecies) &
          is.na(Matched.Infraspecies_2) ~ 3L,

        # Rank 4: All levels (only if use_infraspecies_2 = TRUE)
        !is.na(Matched.Genus) &
          !is.na(Matched.Species) &
          !is.na(Matched.Infra.Rank) &
          !is.na(Matched.Infraspecies) &
          !is.na(Matched.Infraspecies_2) ~ 4L,

        # Not matched
        TRUE ~ NA_integer_
      )
    )

  # Validate: If not using infraspecies_2, there should be no Rank 4
  if (!use_infraspecies_2) {
    invalid_rank4 <- sum(output_f$Matched.Rank == 4, na.rm = TRUE)

    if (invalid_rank4 > 0) {
      warning(
        "Found ", invalid_rank4, " matches with Rank 4, but use_infraspecies_2 = FALSE.\n",
        "This is a bug in the matching pipeline. Correcting to NA.",
        call. = FALSE,
        immediate. = TRUE
      )

      output_f$Matched.Rank[output_f$Matched.Rank == 4] <- NA_integer_
      output_f$Matched.Infraspecies_2[!is.na(output_f$Matched.Rank) &
                                        output_f$Matched.Rank == 4] <- NA_character_
    }
  }

  # Create formatted matched name
  output_f <- output_f |>
    dplyr::mutate(
      Matched.Name = dplyr::case_when(
        # No match
        is.na(Matched.Genus) ~ "---",

        # Rank 1: Only genus
        Matched.Rank == 1 ~ str_to_simple_cap(Matched.Genus),

        # Rank 2: Binomial
        Matched.Rank == 2 ~ paste(
          str_to_simple_cap(Matched.Genus),
          stringr::str_to_lower(Matched.Species)
        ),

        # Rank 3: Trinomial with infraspecific rank
        Matched.Rank == 3 & !is.na(Matched.Infra.Rank) ~ paste(
          str_to_simple_cap(Matched.Genus),
          stringr::str_to_lower(Matched.Species),
          stringr::str_to_lower(Matched.Infra.Rank),
          stringr::str_to_lower(Matched.Infraspecies)
        ),

        # Rank 4: Quaternomial with both ranks
        Matched.Rank == 4 &
          !is.na(Matched.Infra.Rank) &
          !is.na(Matched.Infra.Rank_2) &
          !is.na(Matched.Infraspecies_2) ~ paste(
            str_to_simple_cap(Matched.Genus),
            stringr::str_to_lower(Matched.Species),
            stringr::str_to_lower(Matched.Infra.Rank),
            stringr::str_to_lower(Matched.Infraspecies),
            stringr::str_to_lower(Matched.Infra.Rank_2),
            stringr::str_to_lower(Matched.Infraspecies_2)
          ),

        # Unexpected cases
        TRUE ~ "---"
      )
    )

  # Clean multiple spaces
  output_f <- output_f |>
    dplyr::mutate(
      Matched.Name = stringr::str_squish(Matched.Name),
      Orig.Name = str_to_simple_cap(Orig.Name)
    )

  # Calculate rank comparison and match level
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

  # Format final columns
  output_f <- output_f |>
    dplyr::mutate(
      Matched.Name = dplyr::if_else(is.na(Matched.Name), "---", Matched.Name),
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

  # ==========================================================================
  # SECTION 8: Final Validation
  # ==========================================================================

  # Validate all input records are present
  assertthat::assert_that(
    nrow(splist_class) == nrow(output_f),
    msg = paste0(
      "Final output row count (", nrow(output_f),
      ") does not match input (", nrow(splist_class), ")"
    )
  )

  assertthat::assert_that(
    all(splist_class$sorter %in% output_f$sorter),
    msg = "Some input records are missing from output"
  )

  assertthat::assert_that(
    all(output_f$sorter == sort(output_f$sorter)),
    msg = "Output records are not in correct order"
  )

  # Validate coherence of Infraspecies_2
  if (!use_infraspecies_2) {
    has_infrasp2_data <- any(!is.na(output_f$Matched.Infraspecies_2))

    if (has_infrasp2_data) {
      warning(
        "Matched.Infraspecies_2 contains data but use_infraspecies_2 = FALSE.\n",
        "Clearing Matched.Infraspecies_2 column.",
        call. = FALSE,
        immediate. = TRUE
      )
      output_f$Matched.Infraspecies_2 <- NA_character_
    }
  }

  # Add metadata
  # Add metadata about the process
  attr(output_f, "use_infraspecies_2") <- use_infraspecies_2
  attr(output_f, "target_database") <- source
  attr(output_f, "matching_date") <- Sys.Date()
  attr(output_f, "n_input") <- nrow(splist_class)
  attr(output_f, "n_matched") <- sum(output_f$matched, na.rm = TRUE)
  attr(output_f, "match_rate") <- round(
    sum(output_f$matched, na.rm = TRUE) / nrow(splist_class) * 100,
    2
  )

  return(output_f)
}
