#' Direct Match Species Names
#'
#' @description
#' Performs direct matching of species names against the threatened species database.
#' Matches binomial names (genus + species), trinomial names (+ infraspecies level 1),
#' and quaternomial names (+ infraspecies level 2) when applicable.
#'
#' @param df A tibble containing the species data to be matched.
#' @param target_df A tibble representing the threatened species database containing
#'   the reference list of threatened species.
#' @param use_infraspecies_2 Logical. If TRUE (default), attempts to match quaternomial
#'   names with infraspecies level 2. If FALSE, only matches up to infraspecies level 1.
#'   This should match the database capabilities (original = TRUE, updated = FALSE).
#'
#' @return
#' A tibble with an additional logical column `direct_match` indicating whether
#' the name was successfully matched (`TRUE`) or not (`FALSE`).
#'
#' @keywords internal
direct_match <- function(df, target_df = NULL, use_infraspecies_2 = TRUE) {

  # ==========================================================================
  # SECTION 1: Validate Input Columns
  # ==========================================================================

  # Basic required columns (always needed)
  required_cols <- c(
    'Orig.Genus',
    'Orig.Species',
    'Orig.Infra.Rank',
    'Orig.Infraspecies'
  )

  # Add infraspecies level 2 columns if needed
  if (use_infraspecies_2) {
    required_cols <- c(
      required_cols,
      'Orig.Infra.Rank_2',
      'Orig.Infraspecies_2'
    )
  }

  # Check for missing columns
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      "direct_match() requires columns: ",
      paste(missing_cols, collapse = ", "),
      "\nProvided dataframe is missing these columns.",
      "\nThis is likely a bug in the pipeline. Please report this issue.",
      call. = FALSE
    )
  }

  # Handle empty dataframe
  if (nrow(df) == 0) {
    if (!'direct_match' %in% colnames(df)) {
      df <- tibble::add_column(df, direct_match = logical(0))
    }
    return(df)
  }

  # ==========================================================================
  # SECTION 2: Initialize Matched Columns
  # ==========================================================================

  # Ensure all Matched columns exist (needed for binding later)
  matched_cols <- c(
    'Matched.Genus',
    'Matched.Species',
    'Matched.Infra.Rank',
    'Matched.Infraspecies',
    'Matched.Infra.Rank_2',
    'Matched.Infraspecies_2'
  )

  for (col in matched_cols) {
    if (!col %in% colnames(df)) {
      df[[col]] <- NA_character_
    }
  }

  # ==========================================================================
  # SECTION 3: Prepare Target Database
  # ==========================================================================

  # Standardize infraspecific rank tags to uppercase for matching

  if (use_infraspecies_2) {
    target_prepared <-
    target_df |>
      dplyr::mutate(
        tag = toupper(tag),
        tag_2 = dplyr::if_else(!is.na(infraspecies_2), "F.", NA_character_)
      )
  }  else{
    target_prepared <-
    target_df |>
      dplyr::mutate(
        tag_acc = toupper(tag_acc)
      )
  }

  # ==========================================================================
  # SECTION 4: Match Binomial Names (Rank 2)
  # ==========================================================================

  matched_bino <- df |>
    dplyr::filter(Rank == 2) |>
    dplyr::semi_join(
      target_prepared,
      by = c('Orig.Genus' = 'genus', 'Orig.Species' = 'species')
    ) |>
    dplyr::mutate(
      Matched.Genus = Orig.Genus,
      Matched.Species = Orig.Species
    )

  # ==========================================================================
  # SECTION 5: Match Trinomial Names (Rank 3 - Infraspecies Level 1)
  # ==========================================================================

  matched_infra_1 <- df |>
    dplyr::filter(Rank == 3) |>
    dplyr::semi_join(
      target_prepared,
      by = c(
        'Orig.Genus' = 'genus',
        'Orig.Species' = 'species',
        'Orig.Infra.Rank' = 'tag',
        'Orig.Infraspecies' = 'infraspecies'
      )
    ) |>
    dplyr::mutate(
      Matched.Genus = Orig.Genus,
      Matched.Species = Orig.Species,
      Matched.Infra.Rank = Orig.Infra.Rank,
      Matched.Infraspecies = Orig.Infraspecies
    )

  # ==========================================================================
  # SECTION 6: Match Quaternomial Names (Rank 4 - Infraspecies Level 2)
  # ==========================================================================

  if (use_infraspecies_2) {
    matched_infra_2 <- df |>
      dplyr::filter(Rank == 4) |>
      dplyr::semi_join(
        target_prepared,
        by = c(
          'Orig.Genus' = 'genus',
          'Orig.Species' = 'species',
          'Orig.Infra.Rank' = 'tag',
          'Orig.Infraspecies' = 'infraspecies',
          'Orig.Infra.Rank_2' = 'tag_2',
          'Orig.Infraspecies_2' = 'infraspecies_2'
        )
      ) |>
      dplyr::mutate(
        Matched.Genus = Orig.Genus,
        Matched.Species = Orig.Species,
        Matched.Infra.Rank = Orig.Infra.Rank,
        Matched.Infraspecies = Orig.Infraspecies,
        Matched.Infra.Rank_2 = Orig.Infra.Rank_2,
        Matched.Infraspecies_2 = Orig.Infraspecies_2
      )
  }

  # ==========================================================================
  # SECTION 7: Combine Matched Records
  # ==========================================================================

  if (use_infraspecies_2) {
    matched <- dplyr::bind_rows(
      matched_bino,
      matched_infra_1,
      matched_infra_2
    ) |>
      dplyr::filter(
        dplyr::case_when(
          # Valid Rank 2: Genus + Species
          !is.na(Matched.Genus) & !is.na(Matched.Species) & Rank == 2 ~ TRUE,

          # Valid Rank 3: Genus + Species + Infraspecies level 1
          !is.na(Matched.Genus) & !is.na(Matched.Species) &
            !is.na(Matched.Infra.Rank) & !is.na(Matched.Infraspecies) &
            Rank == 3 ~ TRUE,

          # Valid Rank 4: Genus + Species + Infraspecies level 1 + level 2
          !is.na(Matched.Genus) & !is.na(Matched.Species) &
            !is.na(Matched.Infra.Rank) & !is.na(Matched.Infraspecies) &
            !is.na(Matched.Infra.Rank_2) & !is.na(Matched.Infraspecies_2) &
            Rank == 4 ~ TRUE,

          TRUE ~ FALSE
        )
      )
  } else {
    matched <- dplyr::bind_rows(matched_bino, matched_infra_1) |>
      dplyr::filter(
        dplyr::case_when(
          # Valid Rank 2: Genus + Species
          !is.na(Matched.Genus) & !is.na(Matched.Species) & Rank == 2 ~ TRUE,

          # Valid Rank 3: Genus + Species + Infraspecies level 1
          !is.na(Matched.Genus) & !is.na(Matched.Species) &
            !is.na(Matched.Infra.Rank) & !is.na(Matched.Infraspecies) &
            Rank == 3 ~ TRUE,

          TRUE ~ FALSE
        )
      )
  }

  # ==========================================================================
  # SECTION 8: Identify Unmatched Records
  # ==========================================================================

  if (use_infraspecies_2) {
    # Unmatched Rank 2: Binomial names that didn't match
    unmatched_bino <- df |>
      dplyr::filter(Rank == 2) |>
      dplyr::anti_join(
        target_prepared,
        by = c('Orig.Genus' = 'genus', 'Orig.Species' = 'species')
      )

    # Unmatched Rank 3: Infraspecies level 1 that didn't match
    unmatched_infra_1 <- df |>
      dplyr::filter(Rank == 3) |>
      dplyr::anti_join(
        target_prepared,
        by = c(
          'Orig.Genus' = 'genus',
          'Orig.Species' = 'species',
          'Orig.Infra.Rank' = 'tag',
          'Orig.Infraspecies' = 'infraspecies'
        )
      )

    # Unmatched Rank 4: Infraspecies level 2 that didn't match
    unmatched_infra_2 <- df |>
      dplyr::filter(Rank == 4) |>
      dplyr::anti_join(
        target_prepared,
        by = c(
          'Orig.Genus' = 'genus',
          'Orig.Species' = 'species',
          'Orig.Infra.Rank' = 'tag',
          'Orig.Infraspecies' = 'infraspecies',
          'Orig.Infra.Rank_2' = 'tag_2',
          'Orig.Infraspecies_2' = 'infraspecies_2'
        )
      )

    unmatched <- dplyr::bind_rows(
      unmatched_bino,
      unmatched_infra_1,
      unmatched_infra_2
    )

  } else {
    # When not using infraspecies_2, combine unmatched from Rank 2 and 3
    unmatched <- df |>
      dplyr::anti_join(
        target_prepared,
        by = c(
          'Orig.Genus' = 'genus',
          'Orig.Species' = 'species',
          'Orig.Infra.Rank' = 'tag',
          'Orig.Infraspecies' = 'infraspecies'
        )
      ) |>
      dplyr::filter(!sorter %in% matched$sorter)
  }

  # ==========================================================================
  # SECTION 9: Validate Row Counts
  # ==========================================================================

  assertthat::assert_that(
    nrow(df) == (nrow(matched) + nrow(unmatched)),
    msg = paste0(
      "Row count mismatch in direct_match():\n",
      "Input: ", nrow(df), " rows\n",
      "Matched: ", nrow(matched), " rows\n",
      "Unmatched: ", nrow(unmatched), " rows"
    )
  )

  # ==========================================================================
  # SECTION 10: Combine and Return Results
  # ==========================================================================

  combined <- dplyr::bind_rows(matched, unmatched, .id = 'direct_match') |>
    dplyr::mutate(direct_match = (direct_match == 1))

  # Reorder columns based on whether infraspecies_2 is used
  if (use_infraspecies_2) {
    combined <- combined |>
      dplyr::relocate(c(
        'Orig.Genus',
        'Orig.Species',
        'Orig.Infra.Rank',
        'Orig.Infraspecies',
        'Orig.Infra.Rank_2',
        'Orig.Infraspecies_2'
      ))
  } else {
    combined <- combined |>
      dplyr::relocate(c(
        'Orig.Genus',
        'Orig.Species',
        'Orig.Infra.Rank',
        'Orig.Infraspecies'
      ))
  }

  return(combined)
}

#' Match Genus Name
#'
#' @description
#' This function performs a direct match of genus names against the genus names listed in the threatened species database.
#'
#' @param df A tibble containing the genus names to be matched.
#' @param target_df A tibble representing the threatened species database containing the reference list of threatened species.
#'
#' @return
#' A tibble with an additional logical column genus_match indicating whether the genus was successfully matched (`TRUE`) or not (`FALSE`).
#' @keywords internal
genus_match <- function(df, target_df = NULL){
  assertthat::assert_that(all(c('Orig.Genus',
                                'Orig.Species',
                                'Orig.Infraspecies',
                                'Orig.Infraspecies_2')
                              %in% colnames(df)))

  ## solve issue of empty input tibble and needed to ensure compatilbility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('genus_match') %in% colnames(df))){
      return(tibble::add_column(df, genus_match = NA))
    }
    else{
      return(df)
    }
  }
  matched <-
   df |>
    dplyr::semi_join( target_df,
                      by = c('Orig.Genus' = 'genus')) |>
    dplyr::mutate(Matched.Genus = Orig.Genus) #|>
    # Revisar la siguiente seccion podria generar un error
    #dplyr::filter(dplyr::case_when(
    #!is.na(Matched.Genus) & Rank == 1 ~ TRUE,
    #TRUE ~ FALSE
    #))
  unmatched <-
   df |>
    dplyr::anti_join(matched,
                     by = c('Orig.Genus' = 'Orig.Genus'))

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched,
                                .id = 'genus_match') |>
    dplyr::mutate(genus_match = (genus_match == 1)) |>  ## convert to Boolean
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies',
                      'Orig.Infraspecies_2'))
  ## Genus & Species column at the beginning of tibble

  return(combined)
}

#' Direct Match Species within Genus
#'
#' @description
#' This function performs a direct match of specific epithets within an already matched genus from the list of threatened species in the database.
#'
#' @param df A tibble containing the species data to be matched.
#' @param target_df A tibble representing the threatened species database containing the reference list of threatened species.
#'
#' @return
#' A tibble with an additional logical column indicating whether the specific epithet was successfully matched within the matched genus (`TRUE`) or not (`FALSE`).
#'
#' @keywords internal
direct_match_species_within_genus_helper <- function(df, target_df){
  # subset database
  genus_sub <- df |>
    dplyr::distinct(Matched.Genus) |>
    dplyr::pull(Matched.Genus)

  database_subset <- memoised_get_threatened_genus(genus_sub, target_df)

  # match specific epithet within genus
  matched <- df |>
    dplyr::semi_join(database_subset,
                     by = c('Orig.Species' = 'species')) |>
    dplyr::mutate(Matched.Species = Orig.Species)
#matched |> as.data.frame()
  unmatched <- df |>
    dplyr::anti_join(database_subset,
                     by = c('Orig.Species' = 'species'))
#unmatched |> as.data.frame()
  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched,
                                .id = 'direct_match_species_within_genus') |>
    dplyr::mutate(direct_match_species_within_genus = (direct_match_species_within_genus == 1)) |>  ## convert to Boolean
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies')) ## Genus & Species column at the beginning of tibble

  return(combined)
}


direct_match_species_within_genus <- function(df, target_df = NULL){

  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species',
                                'Orig.Infraspecies',
                                'Matched.Genus') %in% colnames(df)))

  ## solve issue of empty input tibble, and needed to ensure compatilbility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('direct_match_species_within_genus') %in% colnames(df))){
      return(tibble::add_column(df,
                                direct_match_species_within_genus = NA))
    }
    else{
      return(df)
    }
  }

  res <- df |>
    dplyr::group_by(Matched.Genus) |>
    dplyr::group_split() |>
    map_dfr_progress(direct_match_species_within_genus_helper,
                     target_df)

  return(res)
}


#' Direct Match Infraspecific Rank within Species
#'
#' @description
#' Performs direct matching of infraspecific rank (VAR., SUBSP., F., etc.) within
#' an already matched species. This is a prerequisite before fuzzy matching the
#' infraspecific epithet, as the rank category must match exactly.
#'
#' @param df A tibble containing the species data to be matched.
#' @param target_df A tibble representing the threatened species database.
#'
#' @return
#' A tibble with an additional logical column `direct_match_infra_rank` indicating
#' whether the infraspecific rank was successfully matched (`TRUE`) or not (`FALSE`).
#'
#' @details
#' This function ensures that the infraspecific category (e.g., VAR., SUBSP., F.)
#' matches exactly before attempting fuzzy matching on the infraspecific epithet.
#' This prevents inappropriate matches like "var. alba" matching with "subsp. alba"
#' which, despite having similar epithets, are taxonomically different entities.
#'
#' @keywords internal
direct_match_infra_rank_within_species <- function(df, target_df = NULL) {

  # ==========================================================================
  # SECTION 1: Validate Input
  # ==========================================================================

  assertthat::assert_that(
    all(c(
      'Orig.Genus',
      'Orig.Species',
      'Orig.Infra.Rank',
      'Orig.Infraspecies',
      'Matched.Genus',
      'Matched.Species'
    ) %in% colnames(df))
  )

  # Handle empty input
  if (nrow(df) == 0) {
    if (!'direct_match_infra_rank' %in% colnames(df)) {
      return(tibble::add_column(df, direct_match_infra_rank = NA))
    } else {
      return(df)
    }
  }

  # ==========================================================================
  # SECTION 2: Process by Matched Species
  # ==========================================================================

  res <- df |>
    dplyr::group_by(Matched.Species) |>
    dplyr::group_split() |>
    map_dfr_progress(
      direct_match_infra_rank_within_species_helper,
      target_df
    ) |>
    dplyr::relocate(c(
      'Orig.Genus',
      'Orig.Species',
      'Orig.Infra.Rank',
      'Orig.Infraspecies'
    ))

  return(res)
}


#' Helper: Direct Match Infraspecific Rank within Species
#'
#' @description
#' Helper function that performs the actual matching of infraspecific ranks
#' for a single matched species.
#'
#' @param df A tibble containing data for a single matched species.
#' @param target_df A tibble representing the threatened species database.
#'
#' @return A tibble with match results.
#'
#' @keywords internal
direct_match_infra_rank_within_species_helper <- function(df, target_df) {

  # ==========================================================================
  # SECTION 1: Get Matched Species
  # ==========================================================================

  species_matched <- df |>
    dplyr::distinct(Matched.Species) |>
    dplyr::pull(Matched.Species)
  # ==========================================================================
  # SECTION 2: Define Helper Function for Database Subset
  # ==========================================================================

  get_threatened_infra_ranks <- function(species, target_df = NULL) {
    return(
      target_df |>
        dplyr::filter(species %in% species_matched) |>
        dplyr::select(c(
          'genus',
          'species',
          'tag',           # Infraspecific rank category
          'infraspecies'
        )) |>
        dplyr::mutate(tag = toupper(tag)) |> # Standardize to uppercase
        tidyr::drop_na(tag, infraspecies)     # Only infraspecific taxa
    )
  }

  # Memoize for performance
  memoised_get_infra_ranks <- memoise::memoise(get_threatened_infra_ranks)

  # ==========================================================================
  # SECTION 3: Get Database Subset
  # ==========================================================================

  database_subset <- memoised_get_infra_ranks(species_matched, target_df)

  # If no infraspecific taxa exist for this species, all are unmatched
  if (nrow(database_subset) == 0) {
    return(
      df |>
        dplyr::mutate(direct_match_infra_rank = FALSE)
    )
  }

  # ==========================================================================
  # SECTION 4: Match Infraspecific Rank
  # ==========================================================================
  # Match only if BOTH rank category AND epithet match exactly
  # This is strict matching - rank must be identical (VAR. != SUBSP.)
  # ==========================================================================

  matched <-
  df |>
    dplyr::semi_join(
      database_subset,
      by = c(
        'Orig.Infra.Rank' = 'tag')
    ) |>
    dplyr::mutate(
      Matched.Infra.Rank = Orig.Infra.Rank
    )

  # ==========================================================================
  # SECTION 5: Identify Unmatched
  # ==========================================================================

  unmatched <- df |>
    dplyr::anti_join(
      database_subset,
      by = c(
        'Orig.Infra.Rank' = 'tag'))

  # ==========================================================================
  # SECTION 6: Validate and Combine
  # ==========================================================================

  assertthat::assert_that(
    nrow(df) == (nrow(matched) + nrow(unmatched)),
    msg = "Row count mismatch in direct_match_infra_rank_within_species_helper"
  )

  combined <- dplyr::bind_rows(
    matched,
    unmatched,
    .id = 'direct_match_infra_rank'
  ) |>
    dplyr::mutate(
      direct_match_infra_rank = (direct_match_infra_rank == 1)
    ) |>
    dplyr::relocate(c(
      'Orig.Genus',
      'Orig.Species',
      'Orig.Infra.Rank',
      'Orig.Infraspecies'
    ))

  return(combined)
}
