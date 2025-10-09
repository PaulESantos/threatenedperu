#' Match Species Names to Threatened Plant List of Peru
#'
#' @description
#' This function matches given species names against the internal database of threatened
#' plant species in Peru. It uses a hierarchical matching strategy that includes direct
#' matching, genus-level matching, fuzzy matching, and suffix matching to maximize
#' successful matches while maintaining accuracy.
#'
#' @param splist A character vector containing the species names to be matched.
#' @param target_df Character string specifying which database version to use.
#'   Options are:
#'   \itemize{
#'     \item \code{"original"} (default): Uses the original threatened species database
#'     \item \code{"updated"}: Uses the updated database with synonyms
#'   }
#'
#' @details
#' The matching process follows a hierarchical pipeline:
#'
#' \strong{Step 1: Direct Match}
#' Attempts exact matches for genus, species, and infraspecies simultaneously.
#'
#' \strong{Step 2: Genus Match}
#' For unmatched names, matches the genus against the database.
#'
#' \strong{Step 3: Fuzzy Genus Match}
#' Uses string distance algorithms (max distance = 1) to match genus names
#' with minor spelling variations.
#'
#' \strong{Step 4: Species within Genus Match}
#' For matched genera, attempts direct matching of species epithets.
#'
#' \strong{Step 5a: Suffix Match}
#' Handles common Latin suffix variations (e.g., -a, -um, -us, -is).
#'
#' \strong{Step 5b: Fuzzy Species Match}
#' Uses string distance algorithms to match species epithets within matched genera.
#'
#' \strong{Step 6: Infraspecies Fuzzy Match}
#' For matched species with infraspecies, attempts fuzzy matching of
#' infraspecific epithets.
#'
#' The maximum edit distance is intentionally set to one to maintain high
#' matching accuracy while allowing for common typos.
#'
#' @return
#' A tibble with the following key columns:
#' \describe{
#'   \item{sorter}{Original row number for maintaining input order}
#'   \item{Orig.Name}{Original species name as provided}
#'   \item{Matched.Name}{Matched name from the database (properly formatted)}
#'   \item{Threat.Status}{Threat status: "Threatened - [CR|EN|VU|NT]" or "Not threatened"}
#'   \item{threat_category}{IUCN threat category (CR, EN, VU, NT) if threatened}
#'   \item{Matched.Genus}{Matched genus name (uppercase)}
#'   \item{Matched.Species}{Matched species epithet (uppercase)}
#'   \item{Matched.Infraspecies}{Matched infraspecies epithet if applicable}
#'   \item{matched}{Logical indicating if a match was found}
#'   \item{direct_match}{Logical indicating if direct match succeeded}
#'   \item{genus_match}{Logical indicating if genus match succeeded}
#'   \item{fuzzy_match_genus}{Logical indicating if fuzzy genus match was used}
#'   \item{direct_match_species_within_genus}{Logical for direct species match}
#'   \item{suffix_match_species_within_genus}{Logical for suffix match}
#'   \item{fuzzy_match_species_within_genus}{Logical for fuzzy species match}
#'   \item{fuzzy_genus_dist}{String distance for fuzzy genus matches}
#'   \item{fuzzy_species_dist}{String distance for fuzzy species matches}
#' }
#'
#' @note
#' The function performs additional data cleaning at the end to remove duplicate
#' matches and handle specific edge cases in the Cactaceae family.
#'
#' @seealso
#' \code{\link{is_threatened_peru}} for a simplified interface
#'
#' @examples
#' \dontrun{
#' # Basic usage with original database
#' species_list <- c("Cattleya maxima", "Polylepis incana", "Unknown species")
#' results <- matching_threatenedperu(species_list, target_df = "original")
#'
#' # View matching details
#' View(results)
#'
#' # Check which matching methods were used
#' table(results$fuzzy_match_genus, useNA = "ifany")
#'
#' # Using updated database with synonyms
#' results_updated <- matching_threatenedperu(species_list, target_df = "updated")
#' }
#'
#' @export
matching_threatenedperu <- function(splist, target_df = "original"){

  # ========================================================================
  # SECTION 1: Target Database Selection and Validation
  # ========================================================================
  # Validate target_df parameter
  if (!is.character(target_df) || length(target_df) != 1) {
    stop("target_df must be a single character string: 'original' or 'updated'")
  }

  # Determine if infraspecies_2 is supported
  use_infraspecies_2 <- (target_df == "original")

  # Select appropriate database
  target_prepared <- switch(
    target_df,
    "original" = {
      if (!exists("threatenedperu", envir = parent.frame())) {
        stop("Database 'threatenedperu' not found. Please load the package data.")
      }
      get("threatenedperu", envir = parent.frame())
    },
    "updated" = {
      if (!exists("threatenedperu_syn", envir = parent.frame())) {
        stop("Database 'threatenedperu_syn' not found. Please load the package data.")
      }
      get("threatenedperu_syn", envir = parent.frame())
    },
    stop("Invalid target_df value. Must be 'original' or 'updated'")
  )

  # Validate database structure
  if(use_infraspecies_2){
    required_cols <- c("genus", "species", "infraspecies", "infraspecies_2", "threat_category")
  } else {
    required_cols <- c("genus", "species", "infraspecies", "threat_category")
  }

  missing_cols <- setdiff(required_cols, names(target_prepared))
  if (length(missing_cols) > 0) {
    stop("Target database missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # ========================================================================
  # SECTION 2: Input Processing and Classification
  # ========================================================================

  # Classify species names into taxonomic components
  splist_class <- .splist_classify(splist) |>
    .transform_split_classify()

  # Check for and handle non-binomial names
  non_binomial <- .check_binomial(splist_class, splist = splist)
  if(length(non_binomial) != 0){
    df <- splist_class[-non_binomial,]
  } else{
    df <- splist_class
  }

  # Initialize matching columns - SIEMPRE inicializar para compatibilidad
  matching_cols_to_add <- c('Matched.Genus', 'Matched.Species', 'Matched.Infraspecies')

  if(use_infraspecies_2) {
    matching_cols_to_add <- c(matching_cols_to_add, 'Matched.Infraspecies_2')
  }

  cols_missing <- setdiff(matching_cols_to_add, colnames(df))

  if(length(cols_missing) > 0) {
    for(col in cols_missing) {
      df[[col]] <- as.character(NA)
    }
  }

  # Si NO usamos infraspecies_2 pero la columna existe en df, mantenerla como NA
  if(!use_infraspecies_2 && 'Orig.Infraspecies_2' %in% colnames(df)) {
    df$Orig.Infraspecies_2 <- NA_character_
    if(!'Matched.Infraspecies_2' %in% colnames(df)) {
      df$Matched.Infraspecies_2 <- NA_character_
    }
  }

  # ========================================================================
  # SECTION 3: Hierarchical Matching Pipeline
  # ========================================================================

  # ---------------------------------------------------------------
  # Node 1: Direct Match (Exact match for full name)
  # ---------------------------------------------------------------
  Node_1_processed <- df |>
    direct_match(target_df = target_prepared,
                 use_infraspecies_2 = use_infraspecies_2)

  Node_1_TRUE <- Node_1_processed |>
    dplyr::filter(direct_match == TRUE)

  Node_1_FALSE <- Node_1_processed |>
    dplyr::filter(direct_match == FALSE)

  assertthat::assert_that(nrow(df) == (nrow(Node_1_TRUE) + nrow(Node_1_FALSE)),
                          msg = "Row count mismatch after Node 1: Direct Match")

  # ---------------------------------------------------------------
  # Node 2: Genus Match (Exact genus match)
  # ---------------------------------------------------------------
  Node_2_processed <- Node_1_FALSE |>
    genus_match(target_prepared)

  Node_2_TRUE <- Node_2_processed |>
    dplyr::filter(genus_match == TRUE)

  Node_2_FALSE <- Node_2_processed |>
    dplyr::filter(genus_match == FALSE)

  assertthat::assert_that(nrow(Node_2_processed) == (nrow(Node_2_TRUE) + nrow(Node_2_FALSE)),
                          msg = "Row count mismatch after Node 2: Genus Match")

  # ---------------------------------------------------------------
  # Node 3: Fuzzy Match Genus (Approximate genus matching)
  # ---------------------------------------------------------------
  Node_3_processed <- Node_2_FALSE |>
    fuzzy_match_genus(target_prepared)

  Node_3_TRUE <- Node_3_processed |>
    dplyr::filter(fuzzy_match_genus == TRUE)

  Node_3_FALSE <- Node_3_processed |>
    dplyr::filter(fuzzy_match_genus == FALSE)

  assertthat::assert_that(nrow(Node_3_processed) == (nrow(Node_3_TRUE) + nrow(Node_3_FALSE)),
                          msg = "Row count mismatch after Node 3: Fuzzy Genus Match")

  # ---------------------------------------------------------------
  # Node 4: Direct Match Species within Genus
  # ---------------------------------------------------------------
  Node_4_input <- Node_3_TRUE |>
    dplyr::bind_rows(Node_2_TRUE)

  Node_4_processed <- Node_4_input |>
    direct_match_species_within_genus(target_prepared)

  Node_4_TRUE <- Node_4_processed |>
    dplyr::filter(direct_match_species_within_genus == TRUE)

  Node_4_FALSE <- Node_4_processed |>
    dplyr::filter(direct_match_species_within_genus == FALSE)

  assertthat::assert_that(nrow(Node_4_processed) == (nrow(Node_4_TRUE) + nrow(Node_4_FALSE)),
                          msg = "Row count mismatch after Node 4: Species Match")

  # ---------------------------------------------------------------
  # Node 5a: Suffix Match Species within Genus
  # ---------------------------------------------------------------
  Node_5a_processed <- Node_4_FALSE |>
    suffix_match_species_within_genus(target_prepared)

  Node_5a_TRUE <- Node_5a_processed |>
    dplyr::filter(suffix_match_species_within_genus == TRUE)

  Node_5a_FALSE <- Node_5a_processed |>
    dplyr::filter(suffix_match_species_within_genus == FALSE)

  assertthat::assert_that(nrow(Node_4_FALSE) == (nrow(Node_5a_TRUE) + nrow(Node_5a_FALSE)),
                          msg = "Row count mismatch after Node 5a: Suffix Match")

  # ---------------------------------------------------------------
  # Node 5b: Fuzzy Match Species within Genus
  # ---------------------------------------------------------------
  Node_5b_input <- Node_5a_FALSE |>
    dplyr::filter(!is.na(Orig.Species))

  Node_5b_processed <- Node_5b_input |>
    fuzzy_match_species_within_genus(target_prepared)

  Node_5b_TRUE <- Node_5b_processed |>
    dplyr::filter(fuzzy_match_species_within_genus == TRUE)

  Node_5b_FALSE <- Node_5b_processed |>
    dplyr::filter(fuzzy_match_species_within_genus == FALSE)

  assertthat::assert_that(nrow(Node_5b_input) == (nrow(Node_5b_TRUE) + nrow(Node_5b_FALSE)),
                          msg = "Row count mismatch after Node 5b: Fuzzy Species Match")

  # ========================================================================
  # SECTION 4: Combine Results and Handle Infraspecies
  # ========================================================================

  # Combine matched and unmatched results
  if(use_infraspecies_2) {
    combined <- dplyr::bind_rows(Node_1_TRUE,
                                 Node_2_TRUE |>
                                   dplyr::filter(!is.na(Matched.Genus) & Rank == 1),
                                 Node_4_TRUE,
                                 Node_5a_TRUE,
                                 Node_5b_TRUE) |>
      dplyr::mutate(val_rank_declred = dplyr::case_when(
        !is.na(Matched.Genus) & Rank == 1 ~ TRUE,
        !is.na(Matched.Genus) & !is.na(Matched.Species) & Rank == 2 ~ TRUE,
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & Rank == 3 ~ TRUE,
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & !is.na(Matched.Infraspecies_2) & Rank == 4 ~ TRUE,
        TRUE ~ FALSE
      ))
  } else {
    # Sin infraspecies_2, Rank 4 no es válido
    combined <- dplyr::bind_rows(Node_1_TRUE,
                                 Node_2_TRUE |>
                                   dplyr::filter(!is.na(Matched.Genus) & Rank == 1),
                                 Node_4_TRUE,
                                 Node_5a_TRUE,
                                 Node_5b_TRUE) |>
      dplyr::mutate(val_rank_declred = dplyr::case_when(
        !is.na(Matched.Genus) & Rank == 1 ~ TRUE,
        !is.na(Matched.Genus) & !is.na(Matched.Species) & Rank == 2 ~ TRUE,
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & Rank == 3 ~ TRUE,
        TRUE ~ FALSE
      ))
  }

  matched <- combined |>
    dplyr::filter(val_rank_declred == TRUE)

  unmatched_nd <- combined |>
    dplyr::filter(val_rank_declred == FALSE)

  unmatched <- dplyr::bind_rows(unmatched_nd,
                                Node_3_FALSE,
                                Node_5b_FALSE)

  # ---------------------------------------------------------------
  # NODE 6: Infraspecies Fuzzy Matching
  Node_6_input <- unmatched |>
    dplyr::filter(!is.na(Orig.Infraspecies),
                  is.na(Matched.Infraspecies))
  Node_6_processed <- Node_6_input |>
    fuzzy_match_infraspecies_within_species(target_prepared) |>
    as.data.frame()


  if(use_infraspecies_2) {
    Node_6_processed <- Node_6_processed |>
      dplyr::mutate(val_rank_declred = dplyr::case_when(
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & Rank == 3 ~ TRUE,
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & !is.na(Matched.Infraspecies_2) & Rank == 4 ~ TRUE,
        TRUE ~ FALSE
      ))
  } else {
    Node_6_processed <- Node_6_processed |>
      dplyr::mutate(val_rank_declred = dplyr::case_when(
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & Rank == 3 ~ TRUE,
        TRUE ~ FALSE
      ))
  }

  Node_6_TRUE <- Node_6_processed |>
    dplyr::filter(val_rank_declred == TRUE & fuzzy_match_infraspecies_within_species == TRUE)
  Node_6_FALSE <- Node_6_processed |>
    dplyr::filter(val_rank_declred == FALSE & fuzzy_match_infraspecies_within_species == TRUE)

  # ---------------------------------------------------------------
  # NODE 7: Infraspecies 2 Fuzzy Matching (solo si use_infraspecies_2 = TRUE)
  # ---------------------------------------------------------------
  if(use_infraspecies_2) {
    Node_7_processed <- Node_6_FALSE |>
      fuzzy_match_infraspecies2_within_infraspecies(target_prepared) |>
      as.data.frame() |>
      dplyr::mutate(val_rank_declred = dplyr::case_when(
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & !is.na(Matched.Infraspecies_2) & Rank == 4 ~ TRUE,
        TRUE ~ FALSE
      ))

    Node_7_TRUE <- Node_7_processed |>
      dplyr::filter(val_rank_declred == TRUE & fuzzy_match_infraspecies_2 == TRUE)
    Node_7_FALSE <- Node_7_processed |>
      dplyr::filter(val_rank_declred == FALSE & fuzzy_match_infraspecies_2 == TRUE)

    matched_f <- dplyr::bind_rows(matched, Node_6_TRUE, Node_7_TRUE)
    res <- dplyr::bind_rows(matched_f, Node_7_FALSE, .id = 'matched') |>
      dplyr::mutate(matched = (matched == 1))
  } else {
    # Sin infraspecies_2, no hay Node 7
    matched_f <- dplyr::bind_rows(matched, Node_6_TRUE)
    res <- dplyr::bind_rows(matched_f, Node_6_FALSE, .id = 'matched') |>
      dplyr::mutate(matched = (matched == 1))
  }

  # ========================================================================
  # SECTION 6: Add Threat Information and Format Output
  # ========================================================================

  # CRÍTICO: Asegurar que todos los registros de entrada estén en el resultado
  # Crear un data frame base con todos los sorters originales
  base_df <- splist_class |>
    dplyr::select(sorter, Orig.Name, Orig.Genus, Orig.Species,
                  Orig.Infraspecies, Orig.Infraspecies_2,
                  Rank, Infra.Rank, Infra.Rank_2, Author)

  # Hacer left_join excluyendo TODAS las columnas que ya existen en base_df
  # para evitar duplicados (.x y .y)
  cols_to_exclude <- c("Orig.Name", "Orig.Genus", "Orig.Species",
                       "Orig.Infraspecies", "Orig.Infraspecies_2",
                       "Rank", "Infra.Rank", "Infra.Rank_2", "Author")

  res_complete <- base_df |>
    dplyr::left_join(
      res |>
        dplyr::select(-dplyr::any_of(cols_to_exclude)),
      by = "sorter"
    )

  # Si hay registros que no están en res, inicializar sus columnas de matching
  if(use_infraspecies_2) {
    res_complete <- res_complete |>
      dplyr::mutate(
        matched = tidyr::replace_na(matched, FALSE),
        Matched.Genus = dplyr::if_else(is.na(Matched.Genus), NA_character_, Matched.Genus),
        Matched.Species = dplyr::if_else(is.na(Matched.Species), NA_character_, Matched.Species),
        Matched.Infraspecies = dplyr::if_else(is.na(Matched.Infraspecies), NA_character_, Matched.Infraspecies),
        Matched.Infraspecies_2 = dplyr::if_else(is.na(Matched.Infraspecies_2), NA_character_, Matched.Infraspecies_2)
      )
  } else {
    res_complete <- res_complete |>
      dplyr::mutate(
        matched = tidyr::replace_na(matched, FALSE),
        Matched.Genus = dplyr::if_else(is.na(Matched.Genus), NA_character_, Matched.Genus),
        Matched.Species = dplyr::if_else(is.na(Matched.Species), NA_character_, Matched.Species),
        Matched.Infraspecies = dplyr::if_else(is.na(Matched.Infraspecies), NA_character_, Matched.Infraspecies)
      )
  }

  # Ahora hacer el join con la información de amenaza
  if(use_infraspecies_2) {
    target_threat <- target_prepared |>
      dplyr::select(genus, species, infraspecies, infraspecies_2,
                    threat_category, Accepted_name_author = accepted_name_author) |>
      dplyr::distinct(genus, species, infraspecies, infraspecies_2, .keep_all = TRUE)

    output <- res_complete |>
      dplyr::arrange(sorter) |>
      dplyr::left_join(
        target_threat,
        by = c("Matched.Genus" = "genus",
               "Matched.Species" = "species",
               "Matched.Infraspecies" = "infraspecies",
               "Matched.Infraspecies_2" = "infraspecies_2")
      )
  } else {
    target_threat <- target_prepared |>
      dplyr::select(genus, species, infraspecies,
                    threat_category, Accepted_name_author = accepted_name_author) |>
      dplyr::distinct(genus, species, infraspecies, .keep_all = TRUE)

    output <- res_complete |>
      dplyr::arrange(sorter) |>
      dplyr::left_join(
        target_threat,
        by = c("Matched.Genus" = "genus",
               "Matched.Species" = "species",
               "Matched.Infraspecies" = "infraspecies")
      )
  }

  # Create properly formatted matched name
  if(use_infraspecies_2) {
    output_f <- output |>
      dplyr::mutate(Matched.Name = dplyr::case_when(
        is.na(Matched.Species) & Rank == 1 ~ stringr::str_to_sentence(Matched.Genus),
        is.na(Matched.Infraspecies) & Rank == 2 ~ paste(
          stringr::str_to_sentence(Matched.Genus),
          stringr::str_to_lower(Matched.Species), sep = " "
        ),
        !is.na(Matched.Infraspecies) & is.na(Matched.Infraspecies_2) & Rank == 3 ~ paste(
          stringr::str_to_sentence(Matched.Genus),
          stringr::str_to_lower(Matched.Species),
          stringr::str_to_lower(Infra.Rank),
          stringr::str_to_lower(Matched.Infraspecies), sep = " "
        ),
        !is.na(Matched.Infraspecies_2) & Rank == 4 ~ paste(
          stringr::str_to_sentence(Matched.Genus),
          stringr::str_to_lower(Matched.Species),
          stringr::str_to_lower(Infra.Rank),
          stringr::str_to_lower(Matched.Infraspecies),
          stringr::str_to_lower(Infra.Rank_2),
          stringr::str_to_lower(Matched.Infraspecies_2), sep = " "
        ),
        TRUE ~ "---"
      ))
  } else {
    output_f <- output |>
      dplyr::mutate(Matched.Name = dplyr::case_when(
        is.na(Matched.Species) & Rank == 1 ~ stringr::str_to_sentence(Matched.Genus),
        is.na(Matched.Infraspecies) & Rank == 2 ~ paste(
          stringr::str_to_sentence(Matched.Genus),
          stringr::str_to_lower(Matched.Species), sep = " "
        ),
        !is.na(Matched.Infraspecies) & Rank == 3 ~ paste(
          stringr::str_to_sentence(Matched.Genus),
          stringr::str_to_lower(Matched.Species),
          stringr::str_to_lower(Infra.Rank),
          stringr::str_to_lower(Matched.Infraspecies), sep = " "
        ),
        TRUE ~ "---"
      ))
  }

  # Continue with rest of formatting
  output_f <- output_f |>
    dplyr::mutate(Orig.Name = str_to_simple_cap(Orig.Name))

  if(use_infraspecies_2) {
    output_f <- output_f |>
      dplyr::mutate(Matched.Rank = dplyr::case_when(
        !is.na(Matched.Genus) & !is.na(Matched.Species) & is.na(Matched.Infraspecies) & is.na(Matched.Infraspecies_2) ~ 2,
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & is.na(Matched.Infraspecies_2) ~ 3,
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) & !is.na(Matched.Infraspecies_2) ~ 4,
        is.na(Matched.Species) & is.na(Matched.Infraspecies) & is.na(Matched.Infraspecies_2) ~ 1,
        TRUE ~ NA_real_
      ))
  } else {
    output_f <- output_f |>
      dplyr::mutate(Matched.Rank = dplyr::case_when(
        !is.na(Matched.Genus) & !is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ 2,
        !is.na(Matched.Genus) & !is.na(Matched.Species) & !is.na(Matched.Infraspecies) ~ 3,
        is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ 1,
        TRUE ~ NA_real_
      ))
  }

  output_f <- output_f |>
    dplyr::mutate(Comp.Rank = (Rank == Matched.Rank)) |>
    dplyr::mutate(Threat.Status = dplyr::case_when(
      is.na(Matched.Genus) & is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ "Not threatened",
      !is.na(threat_category) ~ threat_category,
      TRUE ~ "Not threatened"
    )) |>
    dplyr::mutate(Matched.Name = dplyr::if_else(is.na(Matched.Name), "---", Matched.Name)) |>
    dplyr::mutate(Accepted_name_author = dplyr::if_else(is.na(Accepted_name_author), "---", Accepted_name_author)) |>
    dplyr::relocate(c("sorter", "Orig.Name", "Matched.Name", "Threat.Status",
                      "Author", "Accepted_name_author"))

  # ========================================================================
  # SECTION 7: Data Cleaning and Final Validation
  # ========================================================================

  # Final validation: TODOS los registros de entrada deben estar presentes
  assertthat::assert_that(
    nrow(splist_class) == nrow(output_f),
    msg = paste0("Final output row count (", nrow(output_f),
                 ") does not match input (", nrow(splist_class), ")")
  )

  # Verificar que todos los sorters originales estén presentes
  assertthat::assert_that(
    all(splist_class$sorter %in% output_f$sorter),
    msg = "Some input records are missing from output"
  )

  # Verificar que los sorters estén en el orden correcto
  assertthat::assert_that(
    all(output_f$sorter == sort(output_f$sorter)),
    msg = "Output records are not in correct order"
  )

  return(output_f)

}
