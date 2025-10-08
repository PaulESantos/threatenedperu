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
                                'Orig.Infraspecies') %in% colnames(df)))

  ## solve issue of empty input tibble, and needed to ensure compatilbility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('fuzzy_match_genus', 'fuzzy_genus_dist') %in% colnames(df))){
      return(tibble::add_column(df, fuzzy_match_genus = NA, fuzzy_genus_dist = NA))
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
  matched <- df |>
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

#' Fuzzy Match Infraspecies within Species
#'
#' @description
#' This function performs a fuzzy match of specific infraspecies within an already matched epithet from the list of threatened species in the database.
#'
#' @param df A tibble containing the species data to be matched.
#' @param target_df A tibble representing the threatened species database containing the reference list of threatened species.
#'
#' @return
#' A tibble with an additional logical column fuzzy_match_infraspecies_within_species, indicating whether the specific infraspecies was successfully fuzzy matched within the matched species (`TRUE`) or not (`FALSE`).
#' @keywords internal
fuzzy_match_infraspecies_within_species <- function(df, target_df = NULL){
  assertthat::assert_that(all(c('Orig.Genus',
                                'Orig.Species',
                                'Orig.Infraspecies',
                                'Matched.Genus') %in% colnames(df)))

  if(nrow(df) == 0){
    if(!all(c('fuzzy_match_infraspecies_within_species',
              'fuzzy_infraspecies_dist') %in% colnames(df))){
      return(tibble::add_column(df,
                                fuzzy_match_infraspecies_within_species = NA,
                                fuzzy_infraspecies_dist = NA))
    } else {
      return(df)
    }
  }

  if('fuzzy_infraspecies_dist' %in% colnames(df)){
    df <- df |>
      dplyr::mutate(fuzzy_infraspecies_dist = NULL)
  }

  res <- df |>
    dplyr::group_by(Matched.Species) |>
    dplyr::group_split() |>
    map_dfr_progress(fuzzy_match_infraspecies_within_species_helper,
                     target_df)

  return(res)
}

fuzzy_match_infraspecies_within_species_helper <- function(df, target_df){
  species <- df |>
    dplyr::distinct(Matched.Species) |>
    unlist()

  get_threatened_infraspecies <- function(species, target_df = NULL){
    return(target_df |>
             dplyr::filter(species %in% species) |>
             dplyr::select(c('genus', 'species', 'infraspecies')))
  }

  memoised_get_threatened_infrasp <- memoise::memoise(get_threatened_infraspecies)

  database_subset <- memoised_get_threatened_infrasp(species, target_df) |>
    tidyr::drop_na()

  matched <-
    df |>
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Infraspecies' = 'infraspecies'),
                                    distance_col = 'fuzzy_infraspecies_dist') |>
    dplyr::mutate(Matched.Infraspecies = infraspecies) |>
    dplyr::select(-c('species', 'genus', 'infraspecies')) |>
    dplyr::group_by(Orig.Genus, Orig.Species, Orig.Infraspecies) |>
    dplyr::filter(fuzzy_infraspecies_dist == min(fuzzy_infraspecies_dist)) |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup()

  unmatched <- fuzzyjoin::stringdist_anti_join(dplyr::filter(df,
                                                             !is.na(Orig.Infraspecies)),
                                               database_subset,
                                               by = c('Orig.Infraspecies' = 'infraspecies'))

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  combined <-  dplyr::bind_rows(matched,
                                unmatched,
                                .id = 'fuzzy_match_infraspecies_within_species') |>
    dplyr::mutate(fuzzy_match_infraspecies_within_species = (fuzzy_match_infraspecies_within_species == 1)) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies'))

  return(combined)
}
