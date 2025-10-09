#' Direct Match
#'
#' @description
#' This function performs a direct match of species names. It matches the genus and species if the name is binomial, and matches the genus, species, and infra species if the name includes a subspecies.
#'
#' @param df A tibble containing the species data to be matched.
#' @param target_df A tibble representing the threatened species database containing the reference list of threatened species.
#'
#' @return
#' A tibble with an additional logical column direct_match indicating whether the binomial or trinomial name was successfully matched (`TRUE`) or not (`FALSE`).
#'
#' @keywords internal
direct_match <- function(df, target_df = NULL) {

  assertthat::assert_that(all(c('Orig.Genus',
                                'Orig.Species',
                                'Orig.Infraspecies',
                                'Orig.Infraspecies_2') %in%
                                colnames(df)))

  if(!all(c('direct_match') %in% colnames(df))) {
    if(nrow(df) == 0) {
      return(tibble::add_column(df,
                                direct_match = NA))
    }
  }

  # Match completo (género + especie + infrasp1 + infrasp2)
  matched_bino <- df |>
    dplyr::filter(Rank == 2) |>
    dplyr::semi_join(target_df,
                     by = c('Orig.Genus' = 'genus',
                            'Orig.Species' = 'species')) |>
    dplyr::mutate(Matched.Genus = Orig.Genus,
                  Matched.Species = Orig.Species)

  matched_infra_1 <- df |>
    dplyr::filter(Rank == 3) |>
    dplyr::semi_join(target_df,
                     by = c('Orig.Genus' = 'genus',
                            'Orig.Species' = 'species',
                            'Orig.Infraspecies' = 'infraspecies')) |>
    dplyr::mutate(Matched.Genus = Orig.Genus,
                  Matched.Species = Orig.Species,
                  Matched.Infraspecies = Orig.Infraspecies)
  matched_infra_2 <- df |>
    dplyr::filter(Rank == 4) |>
    dplyr::semi_join(target_df,
                     by = c('Orig.Genus' = 'genus',
                            'Orig.Species' = 'species',
                            'Orig.Infraspecies' = 'infraspecies',
                            'Orig.Infraspecies_2' = 'infraspecies_2')) |>
    dplyr::mutate(Matched.Genus = Orig.Genus,
                  Matched.Species = Orig.Species,
                  Matched.Infraspecies = Orig.Infraspecies,
                  Matched.Infraspecies_2 = Orig.Infraspecies_2)

  matched <- dplyr::bind_rows(matched_bino, matched_infra_1, matched_infra_2) |>
    dplyr::filter(
      dplyr::case_when(
        # Nivel especie
        !is.na(Matched.Genus) & !is.na(Matched.Species) & Rank == 2 ~ TRUE,
        # Nivel infraspecie 1
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & Rank == 3 ~ TRUE,
        # Nivel infraspecie 2
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & !is.na(Matched.Infraspecies_2) & Rank == 4 ~ TRUE,
        # Todo lo demás queda fuera
        TRUE ~ FALSE
      )
    )

  unmatched <- df |>
    dplyr::anti_join(target_df,
                     c('Orig.Genus' = 'genus',
                       'Orig.Species' = 'species',
                       'Orig.Infraspecies' = 'infraspecies',
                       'Orig.Infraspecies_2' = 'infraspecies_2'))

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  combined <- dplyr::bind_rows(matched, unmatched, .id = 'direct_match') |>
    dplyr::mutate(direct_match = (direct_match == 1)) |>
    dplyr::relocate(c('Orig.Genus', 'Orig.Species', 'Orig.Infraspecies',
                      'Orig.Infraspecies_2'))

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
