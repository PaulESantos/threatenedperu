#' Fuzzy Match Genus Name
#'
#' @description
#' This function performs a fuzzy match of genus names against the threatened species database using fuzzyjoin::stringdist() to account for slight variations in spelling.
#'
#' @param df A tibble containing the genus names to be matched.
#' @param target_df A tibble representing the threatened species database containing the reference list of threatened species.
#'
#' @return
#' A tibble with two additional columns:
#' - fuzzy_match_genus: A logical column indicating whether the genus was successfully matched (`TRUE`) or not (`FALSE`).
#' - fuzzy_genus_dist: A numeric column representing the distance for each match.
#'
#' @keywords internal
fuzzy_match_genus <- function(df, target_df = NULL){
  assertthat::assert_that(all(c('Orig.Genus',
                                'Orig.Species',
                                'Orig.Infraspecies',
                                'Orig.Infraspecies_2') %in% colnames(df)))

  ## solve issue of empty input tibble, and needed to ensure compatilbility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('fuzzy_match_genus', 'fuzzy_genus_dist') %in% colnames(df))){
      return(tibble::add_column(df,
                                fuzzy_match_genus = NA,
                                fuzzy_genus_dist = NA))
    }
    else{
      return(df)
    }
  }
  ## solve issue in second iteration of sequential_matching: necessary to remove fuzzy_species_dist column: otherwise 2 columns are generated 'fuzzy_species_dist...1, fuzzy_species_dist...2'
  if('fuzzy_genus_dist' %in% colnames(df)){
    df <- df |>
      dplyr::mutate(fuzzy_genus_dist = NULL)
  } ## TODO: can potentially be removed again????

  Threatened.Genera <- target_df |>
    dplyr::distinct(genus)
  # fuzzy match
  matched_temp <- df |>
    fuzzyjoin::stringdist_left_join(Threatened.Genera,
                                    by = c('Orig.Genus' = 'genus'),
                                    max_dist = 1,
                                    distance_col = 'fuzzy_genus_dist') |>
    # save matched Genus name to Matched.Genus
    dplyr::mutate(Matched.Genus = genus) |>
    dplyr::select(-c('genus')) |>
    dplyr::group_by(Orig.Genus, Orig.Species) |>
    dplyr::filter(fuzzy_genus_dist == min(fuzzy_genus_dist))


  ## If there are multiple matches for the same genus: raise warning and advise for manual checking
  if(matched_temp |>
     dplyr::filter(dplyr::n() > 1) |>
     nrow() > 0){
    message("Multiple fuzzy matches for genera with similar string distance:
            Please consider curating the ambiguous entries by hand and re-run the pipeline.
            The ambiguous matched genera were saved to 'threatenedperu_ambiguous_genera.csv' in the current working directory.
             The algorithm will choose the first genus to continue.")
    #Do you want save a list of the ambiguous matched genera current working directory in 'threatenedperu_ambiguous_genera.csv'?")
    ## Save ambiguous genera for manual curation:
    matched_temp |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::select(Orig.Genus, Orig.Species, Matched.Genus) |>
      readr::write_csv(file = 'threatenedperu_ambiguous_genera.csv') ##
    ## Alternative Idea: prompt the user to insert the correct name. Caution here however because this might cause trouble with unit testing
  }

  ## continue selecting first genus if more than one match
  matched <- matched_temp |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0,
              return(.x),
              return(dplyr::slice_head(.x,n = 1)))
      ## In cases of multiple matches: we choose first match.
      ## Alternatively could use something more sophisticated here:
      ## like for instance choosing the one with more support (present
      ## in more databases)
    ) |>
    dplyr::ungroup()


  unmatched <- df |>
    fuzzyjoin::stringdist_anti_join(Threatened.Genera,
                                    by = c('Orig.Genus' = 'genus'),
                                    max_dist = 1)
  #unmatched
  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  res <-  dplyr::bind_rows(matched, unmatched,
                           .id = 'fuzzy_match_genus') |>
    dplyr::mutate(fuzzy_match_genus = (fuzzy_match_genus == 1)) |>  ## convert to Boolean
    dplyr::arrange(Orig.Genus, Orig.Species) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies')) ## Genus & Species column at the beginning of tibble
  #res
  return(res)
}

#=====================================================================
#' Fuzzy Match Species within Genus
#'
#' @description
#' This function attempts to fuzzy match species names within a genus to the threatened species database using fuzzyjoin::stringdist for fuzzy matching.
#'
#' @param df A tibble containing the species data to be matched.
#' @param target_df A tibble representing the threatened species database containing the reference list of threatened species.
#'
#' @return
#' A tibble with an additional logical column fuzzy_match_species_within_genus, indicating whether the specific epithet was successfully fuzzy matched within the matched genus (`TRUE`) or not (`FALSE`).
#' @keywords internal
fuzzy_match_species_within_genus_helper <- function(df, target_df){

  # subset database
  genus <- df |>
    dplyr::distinct(Matched.Genus) |>
    unlist()

  database_subset <- memoised_get_threatened_genus(genus, target_df)

  # fuzzy match
  matched <-
    df |>
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Species' = 'species'),
                                    distance_col = 'fuzzy_species_dist') |>
    dplyr::mutate(Matched.Species = species) |>
    dplyr::select(-c('species', 'genus')) |>
    dplyr::group_by(Orig.Genus, Orig.Species) |>
    dplyr::filter(fuzzy_species_dist == min(fuzzy_species_dist)) |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x),
              return(dplyr::slice_head(.x,n=1))) ## In cases of multiple matches: we choose first match. Alternatively could use something more sophisticated here: like for instance choosing the one with more support (present in more databases)
    ) |>
    dplyr::ungroup()

  unmatched <-
    fuzzyjoin::stringdist_anti_join(df,
                                    database_subset,
                                    by = c('Orig.Species' = 'species'))


  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched,
                                .id = 'fuzzy_match_species_within_genus') |>
    dplyr::mutate(fuzzy_match_species_within_genus = (fuzzy_match_species_within_genus == 1)) |>  ## convert to Boolean
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies')) ## Genus & Species column at the beginning of tibble

  return(combined)
}

fuzzy_match_species_within_genus <- function(df, target_df = NULL){
  assertthat::assert_that(all(c('Orig.Genus',
                                'Orig.Species',
                                'Orig.Infraspecies',
                                'Matched.Genus') %in% colnames(df)))

  ## solve issue of empty input tibble and needed to ensure compatibility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('fuzzy_match_species_within_genus',
              'fuzzy_species_dist') %in% colnames(df))){
      return(tibble::add_column(df,
                                fuzzy_match_species_within_genus = NA,
                                fuzzy_species_dist = NA))
    }
    else{
      return(df)
    }
  }

  ## solve issue in second iteration of sequential_matching: necessary to remove fuzzy_species_dist column: otherwise 2 columns are generated 'fuzzy_species_dist...1, fuzzy_species_dist...2'
  if('fuzzy_species_dist' %in% colnames(df)){
    df <- df |>
      dplyr::mutate(fuzzy_species_dist = NULL)
  }

  res <- df |>
    dplyr::group_by(Matched.Genus) |>
    dplyr::group_split() |>  ## TODO: change to dplyr::group_map to be able to omit dplyr::group_split() stage
    map_dfr_progress(fuzzy_match_species_within_genus_helper,
                     target_df) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies'))

  return(res)
}

#=====================================================================
#' Fuzzy Match Infraspecific Epithet within Species
#'
#' @description
#' Performs fuzzy matching of infraspecific epithets within an already matched
#' species where the infraspecific rank category (VAR., SUBSP., F., etc.) has
#' already been validated via direct_match_infra_rank.
#'
#' @param df A tibble containing the species data to be matched. Must have
#'   already passed through direct_match_infra_rank validation.
#' @param target_df A tibble representing the threatened species database.
#'
#' @return
#' A tibble with two additional columns:
#' - `fuzzy_match_infraspecies`: Logical indicating whether the infraspecific
#'   epithet was successfully fuzzy matched (`TRUE`) or not (`FALSE`)
#' - `fuzzy_infraspecies_dist`: Numeric indicating the string distance of the match
#'
#' @details
#' This function assumes that infraspecific rank categories have already been
#' validated. It only performs fuzzy matching on the infraspecific epithet
#' (e.g., "ovatoformes" vs "ovatoformis") within the same rank category.
#'
#' @keywords internal
fuzzy_match_infraspecies_within_species <- function(df, target_df = NULL) {

  # ==========================================================================
  # SECTION 1: Validate Required Columns
  # ==========================================================================

  required_cols <- c(
    'Orig.Genus',
    'Orig.Species',
    'Orig.Infra.Rank',
    'Orig.Infraspecies',
    'Orig.Infraspecies_2',
    'Matched.Genus',
    'Matched.Species'
  )

  assertthat::assert_that(
    all(required_cols %in% colnames(df)),
    msg = paste(
      "Missing required columns:",
      paste(setdiff(required_cols, colnames(df)), collapse = ", ")
    )
  )

  # ==========================================================================
  # SECTION 2: Handle Empty Input
  # ==========================================================================

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

  # ==========================================================================
  # SECTION 3: Remove Existing Distance Column (Sequential Matching)
  # ==========================================================================

  if ('fuzzy_infraspecies_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_infraspecies_dist = NULL)
  }

  # ==========================================================================
  # SECTION 4: Process Each Matched Species Separately
  # ==========================================================================

  res <- df |>
    dplyr::group_by(Matched.Species) |>
    dplyr::group_split() |>
    map_dfr_progress(
      fuzzy_match_infraspecies_within_species_helper,
      target_df
    ) |>
    dplyr::relocate(c(
      'Orig.Genus',
      'Orig.Species',
      'Orig.Infra.Rank',
      'Orig.Infraspecies',
      'Orig.Infraspecies_2'
    ))

  return(res)
}


#' Helper: Fuzzy Match Infraspecific Epithet within Species
#'
#' @description
#' Helper function that performs fuzzy matching of infraspecific epithets
#' for a single matched species. The infraspecific rank category must already
#' be validated.
#'
#' @param df A tibble containing data for a single matched species.
#' @param target_df A tibble representing the threatened species database.
#'
#' @return A tibble with fuzzy match results and logical indicator.
#'
#' @keywords internal
fuzzy_match_infraspecies_within_species_helper <- function(df, target_df) {

  # ==========================================================================
  # SECTION 1: Extract Matched Species
  # ==========================================================================

  species_matched <- df|>
    dplyr::distinct(Matched.Species) |>
    dplyr::pull(Matched.Species)

  # ==========================================================================
  # SECTION 2: Define Database Subset Function
  # ==========================================================================

  get_threatened_infraspecies <- function(species_matched, target_df = NULL) {
    return(
      target_df |>
        dplyr::filter(species %in% species_matched) |>
        dplyr::select(c(
          'genus',
          'species',
          'tag',
          'infraspecies'
        )) |>
        dplyr::mutate(tag = toupper(tag)) |>  # Standardize to uppercase
        tidyr::drop_na(tag, infraspecies)      # Only complete infraspecific taxa
    )
  }

  # Memoize for performance
  memoised_get_threatened_infrasp <- memoise::memoise(get_threatened_infraspecies)

  # ==========================================================================
  # SECTION 3: Get Database Subset for Current Species
  # ==========================================================================

  database_subset <- memoised_get_threatened_infrasp(species_matched,
                                                     target_df)

  # If no infraspecific taxa in database, mark all as unmatched
  if (nrow(database_subset) == 0) {
    return(
      df |>
        dplyr::mutate(
          fuzzy_match_infraspecies = FALSE,
          fuzzy_infraspecies_dist = NA_real_
        )
    )
  }

  # ==========================================================================
  # SECTION 4: Fuzzy Match with Rank Category Filter
  # ==========================================================================
  # IMPORTANT: Only fuzzy match epithets within the SAME rank category
  # This ensures "SUBSP. ovatoformes" only matches with "SUBSP. X" entries
  # ==========================================================================

  matched <-
    df |>
    # Join by exact rank to filter candidates
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Infraspecies' = 'infraspecies'),
                                    distance_col = 'fuzzy_infraspecies_dist') |>
    # Assign matched values
    dplyr::mutate(Matched.Infraspecies = infraspecies) |>
    dplyr::select(-c('species', 'genus', 'infraspecies', 'tag')) |>
    dplyr::group_by(Orig.Genus, Orig.Species, Orig.Infra.Rank, Orig.Infraspecies) |>
    dplyr::filter(fuzzy_infraspecies_dist == min(fuzzy_infraspecies_dist)) |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup() |>
    as.data.frame()

  # ==========================================================================
  # SECTION 5: Identify Unmatched Records
  # ==========================================================================
  # Records where no matching rank category exists in database,
  # or epithet distance is too large
  # ==========================================================================

  unmatched <-
  fuzzyjoin::stringdist_anti_join(
    dplyr::filter(df,
                 !is.na(Orig.Infraspecies)),
                 database_subset,
                 by = c('Orig.Infraspecies' = 'infraspecies'))

  # ==========================================================================
  # SECTION 6: Validate Row Counts
  # ==========================================================================

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  combined <-
    dplyr::bind_rows(matched,
                    unmatched,
                  .id = 'fuzzy_match_infraspecies') |>
    dplyr::mutate(fuzzy_match_infraspecies = (fuzzy_match_infraspecies == "1")) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies',
                      'Orig.Infra.Rank',
                      'Orig.Infraspecies_2'))

  return(combined)
}


#=====================================================================


#' Fuzzy Match Infraspecies Level 2 within Infraspecies Level 1
#'
#' @description
#' This function performs a fuzzy match of the second infraspecific level within
#' an already matched first infraspecific level from the list of threatened
#' species in the database. Follows the same pattern as
#' fuzzy_match_infraspecies_within_species.
#'
#' @param df A tibble containing the species data to be matched.
#' @param target_df A tibble representing the threatened species database
#'   containing the reference list of threatened species.
#'
#' @return
#' A tibble with an additional logical column fuzzy_match_infraspecies2,
#' indicating whether the second infraspecific level was successfully fuzzy
#' matched within the matched first infraspecific level (`TRUE`) or not (`FALSE`).
#'
#' @keywords internal
fuzzy_match_infraspecies2_within_infraspecies <- function(df, target_df = NULL) {
  # df <- Node_6_FALSE
  #  df
  # Validar columnas requeridas
  assertthat::assert_that(all(c('Orig.Genus',
                                'Orig.Species',
                                'Orig.Infraspecies',
                                'Orig.Infraspecies_2',
                                'Matched.Genus',
                                'Matched.Species',
                                'Matched.Infraspecies') %in% colnames(df)))

  # Manejar caso de tibble vacío
  if(nrow(df) == 0) {
    if(!all(c('fuzzy_match_infraspecies_2',
              'fuzzy_infraspecies_2_dist') %in% colnames(df))) {
      return(tibble::add_column(df,
                                fuzzy_match_infraspecies_2 = NA,
                                fuzzy_infraspecies_2_dist = NA))
    } else {
      return(df)
    }
  }

  # Eliminar columna de distancia previa si existe (para evitar duplicados)
  if('fuzzy_infraspecies_2_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_infraspecies_2_dist = NULL)
  }

  # Procesar por grupos de Matched.Infraspecies usando helper
  res <- df |>
    dplyr::group_by(Matched.Infraspecies) |>
    dplyr::group_split() |>
    map_dfr_progress(fuzzy_match_infraspecies2_within_infraspecies_helper,
                     target_df)

  return(res)
}

#' Helper function for fuzzy matching infraspecies level 2
#' @keywords internal
fuzzy_match_infraspecies2_within_infraspecies_helper <- function(df, target_df) {

  # Obtener el infraspecies nivel 1 para este grupo
  infraspecies1 <- df |>
    dplyr::distinct(Matched.Infraspecies) |>
    unlist()

  # Función para obtener infraspecies nivel 2 de la base de datos
  get_threatened_infraspecies2 <- function(infraspecies1, target_df = NULL) {
    return(target_df |>
             dplyr::filter(infraspecies %in% infraspecies1) |>
             dplyr::select(c('genus', 'species',
                             'infraspecies', 'infraspecies_2')))
  }

  # Memoizar para mejor performance
  memoised_get_threatened_infrasp2 <- memoise::memoise(get_threatened_infraspecies2)

  # Obtener subset de la base de datos y eliminar NAs
  database_subset <- memoised_get_threatened_infrasp2(infraspecies1, target_df) |>
    tidyr::drop_na(infraspecies_2)  # Solo mantener registros con nivel 2

  # Si no hay registros con nivel 2 en la BD, retornar sin match
  if(nrow(database_subset) == 0) {
    return(df |>
             dplyr::mutate(fuzzy_match_infraspecies_2 = FALSE,
                           fuzzy_infraspecies_2_dist = NA_real_))
  }

  # Fuzzy match usando stringdist
  matched <- df |>
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Infraspecies_2' = 'infraspecies_2'),
                                    distance_col = 'fuzzy_infraspecies_2_dist') |>
    dplyr::mutate(Matched.Infraspecies_2 = infraspecies_2) |>
    dplyr::select(-c('species', 'genus', 'infraspecies', 'infraspecies_2')) |>
    dplyr::group_by(Orig.Genus, Orig.Species, Orig.Infraspecies, Orig.Infraspecies_2) |>
    dplyr::filter(fuzzy_infraspecies_2_dist == min(fuzzy_infraspecies_2_dist)) |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup()

  # Anti-join para encontrar los no matcheados
  unmatched <- fuzzyjoin::stringdist_anti_join(
    dplyr::filter(df, !is.na(Orig.Infraspecies_2)),
    database_subset,
    by = c('Orig.Infraspecies_2' = 'infraspecies_2')
  )

  # Verificación de integridad
  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)),
                          msg = "Row count mismatch in fuzzy_match_infraspecies2")

  # Combinar matched y unmatched con indicador booleano
  combined <- dplyr::bind_rows(matched,
                               unmatched,
                               .id = 'fuzzy_match_infraspecies_2') |>
    dplyr::mutate(fuzzy_match_infraspecies_2 = (fuzzy_match_infraspecies_2 == "1")) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies',
                      'Orig.Infraspecies_2'))

  return(combined)
}
