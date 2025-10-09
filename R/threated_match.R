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
#' results <- matching_threatenedperu(species_list, target_df = "original")
#' }
#'
#' @export
matching_threatenedperu <- function(splist, target_df = "original"){

  # ========================================================================
  # SECTION 1: Target Database Selection and Validation
  # ========================================================================

  # Validate target_df parameter
  if (!is.character(target_df) || length(target_df) != 1) {
    stop("target_df must be a single character string: 'original' or 'updated'",
         call. = FALSE)
  }

  if (!target_df %in% c("original", "updated")) {
    stop(
      "Invalid target_df value: '", target_df, "'. Must be 'original' or 'updated'",
      call. = FALSE
    )
  }

  # Determine if infraspecies_2 is supported
  use_infraspecies_2 <- (target_df == "original")

  # Load database using internal function
  target_prepared <- tryCatch({
    get_threatened_data(type = target_df)
  }, error = function(e) {
    stop(
      "Failed to load database '", target_df, "'.\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })

  # Validate database structure
  if (use_infraspecies_2) {
    required_cols <- c("genus", "species", "infraspecies", "infraspecies_2", "threat_category")
  } else {
    required_cols <- c("genus", "species", "infraspecies", "threat_category")
  }

  missing_cols <- setdiff(required_cols, names(target_prepared))
  if (length(missing_cols) > 0) {
    stop(
      "Database '", target_df, "' missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
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

  # ========================================================================
  # SECTION 2B: Initialize ALL Matching Columns (Standardized)
  # ========================================================================

  # ESTRATEGIA: Siempre inicializar TODAS las columnas posibles para evitar
  # errores de columnas faltantes en cualquier parte del pipeline.

  # Lista completa de columnas de matching (TODAS se inicializan)
  all_matching_cols <- c(
    'Matched.Genus',
    'Matched.Species',
    'Matched.Infraspecies',
    'Matched.Infraspecies_2'
  )

  # Inicializar TODAS las columnas de matching como NA si no existen
  for (col in all_matching_cols) {
    if (!col %in% colnames(df)) {
      df[[col]] <- NA_character_
    }
  }

  # Asegurar que las columnas originales de infraspecies también existan
  if (!'Orig.Infraspecies' %in% colnames(df)) {
    df$Orig.Infraspecies <- NA_character_
  }

  if (!'Orig.Infraspecies_2' %in% colnames(df)) {
    df$Orig.Infraspecies_2 <- NA_character_
  }

  # Marcar explícitamente si usaremos infraspecies_2 en este proceso
  attr(df, "use_infraspecies_2") <- use_infraspecies_2

  # Si NO usamos infraspecies_2, asegurar que esas columnas permanezcan NA
  if (!use_infraspecies_2) {
    df$Orig.Infraspecies_2 <- NA_character_
    df$Matched.Infraspecies_2 <- NA_character_

    message(
      "Note: Using database '", target_df, "' which does not support Rank 4 level.\n",
      "Any subspecific rank beyond the first level will be ignored."
    )
  }

  # Validar que Rank esté correctamente calculado
  if (!'Rank' %in% colnames(df)) {
    stop("Column 'Rank' missing from processed data. Check .transform_split_classify()",
         call. = FALSE)
  }

  # Validación: Si use_infraspecies_2 = FALSE, no debe haber Rank 4
  if (!use_infraspecies_2 && any(df$Rank == 4, na.rm = TRUE)) {
    rank4_count <- sum(df$Rank == 4, na.rm = TRUE)
    warning(
      rank4_count, " input names have Rank 4 (two infraspecific levels), ",
      "but database '", target_df, "' does not support infraspecies_2.\n",
      "These will be matched only at Rank 3 level.",
      call. = FALSE,
      immediate. = TRUE
    )

    # Degradar Rank 4 a Rank 3 para este database
    df$Rank[df$Rank == 4] <- 3
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

  # Combine matched and unmatched results with UNIFIED validation logic
  combined <- dplyr::bind_rows(
    Node_1_TRUE,
    Node_2_TRUE |> dplyr::filter(!is.na(Matched.Genus) & Rank == 1),
    Node_4_TRUE,
    Node_5a_TRUE,
    Node_5b_TRUE
  ) |>
    dplyr::mutate(
      val_rank_declred = dplyr::case_when(
        # Rank 1: Solo género
        !is.na(Matched.Genus) & is.na(Matched.Species) & Rank == 1 ~ TRUE,

        # Rank 2: Género + especie
        !is.na(Matched.Genus) & !is.na(Matched.Species) &
          is.na(Matched.Infraspecies) & Rank == 2 ~ TRUE,

        # Rank 3: Género + especie + infraspecies nivel 1
        !is.na(Matched.Genus) & !is.na(Matched.Species) &
          !is.na(Matched.Infraspecies) & is.na(Matched.Infraspecies_2) & Rank == 3 ~ TRUE,

        # Rank 4: Todos los niveles (solo si use_infraspecies_2 = TRUE)
        use_infraspecies_2 & !is.na(Matched.Genus) & !is.na(Matched.Species) &
          !is.na(Matched.Infraspecies) & !is.na(Matched.Infraspecies_2) & Rank == 4 ~ TRUE,

        # Resto: no válido
        TRUE ~ FALSE
      )
    )

  matched <- combined |>
    dplyr::filter(val_rank_declred == TRUE)

  unmatched_nd <- combined |>
    dplyr::filter(val_rank_declred == FALSE)

  unmatched <- dplyr::bind_rows(
    unmatched_nd,
    Node_3_FALSE,
    Node_5b_FALSE
  )

  # ---------------------------------------------------------------
  # NODE 6: Infraspecies Fuzzy Matching
  # ---------------------------------------------------------------

  Node_6_input <- unmatched |>
    dplyr::filter(
      !is.na(Orig.Infraspecies),
      is.na(Matched.Infraspecies),
      !is.na(Matched.Species)  # Debe tener especie matcheada
    )

  Node_6_processed <- Node_6_input |>
    fuzzy_match_infraspecies_within_species(target_prepared) |>
    as.data.frame() |>
    dplyr::mutate(
      val_rank_declred = dplyr::case_when(
        # Rank 3 válido
        !is.na(Matched.Genus) & !is.na(Matched.Species) &
          !is.na(Matched.Infraspecies) & is.na(Matched.Infraspecies_2) & Rank == 3 ~ TRUE,

        # Rank 4 válido (solo si use_infraspecies_2)
        use_infraspecies_2 & !is.na(Matched.Genus) & !is.na(Matched.Species) &
          !is.na(Matched.Infraspecies) & !is.na(Matched.Infraspecies_2) & Rank == 4 ~ TRUE,

        TRUE ~ FALSE
      )
    )

  Node_6_TRUE <- Node_6_processed |>
    dplyr::filter(
      val_rank_declred == TRUE,
      fuzzy_match_infraspecies_within_species == TRUE
    )

  Node_6_FALSE <- Node_6_processed |>
    dplyr::filter(
      val_rank_declred == FALSE | fuzzy_match_infraspecies_within_species == FALSE
    )

  # ---------------------------------------------------------------
  # NODE 7: Infraspecies 2 Fuzzy Matching (solo si use_infraspecies_2 = TRUE)
  # ---------------------------------------------------------------

  if (use_infraspecies_2) {
    # Solo procesar registros que tienen Rank 4 y que no matchearon en Node 6
    Node_7_input <- Node_6_FALSE |>
      dplyr::filter(
        Rank == 4,
        !is.na(Orig.Infraspecies_2),
        is.na(Matched.Infraspecies_2),
        !is.na(Matched.Infraspecies)  # Debe tener infrasp. nivel 1 matcheada
      )

    if (nrow(Node_7_input) > 0) {
      Node_7_processed <- Node_7_input |>
        fuzzy_match_infraspecies2_within_infraspecies(target_prepared) |>
        as.data.frame() |>
        dplyr::mutate(
          val_rank_declred = dplyr::case_when(
            !is.na(Matched.Genus) & !is.na(Matched.Species) &
              !is.na(Matched.Infraspecies) & !is.na(Matched.Infraspecies_2) &
              Rank == 4 ~ TRUE,
            TRUE ~ FALSE
          )
        )

      Node_7_TRUE <- Node_7_processed |>
        dplyr::filter(
          val_rank_declred == TRUE,
          fuzzy_match_infraspecies_2 == TRUE
        )

      Node_7_FALSE <- Node_7_processed |>
        dplyr::filter(
          val_rank_declred == FALSE | fuzzy_match_infraspecies_2 == FALSE
        )
    } else {
      # Si no hay datos para procesar, crear tibbles vacíos
      Node_7_TRUE <- Node_6_FALSE[0, ]
      Node_7_FALSE <- Node_6_FALSE[0, ]
    }

    # Combinar todos los matches
    matched_f <- dplyr::bind_rows(matched, Node_6_TRUE, Node_7_TRUE)

    # Unir con no-matches
    res <- dplyr::bind_rows(matched_f, Node_7_FALSE, .id = 'matched') |>
      dplyr::mutate(matched = (matched == 1))

  } else {
    # Sin infraspecies_2, no hay Node 7
    matched_f <- dplyr::bind_rows(matched, Node_6_TRUE)
    res <- dplyr::bind_rows(matched_f, Node_6_FALSE, .id = 'matched') |>
      dplyr::mutate(matched = (matched == 1))
  }

  # ========================================================================
  # SECTION 5: Add Threat Information and Format Output
  # ========================================================================

  # CRÍTICO: Asegurar que todos los registros de entrada estén en el resultado
  base_df <- splist_class |>
    dplyr::select(
      sorter, Orig.Name, Orig.Genus, Orig.Species,
      Orig.Infraspecies, Orig.Infraspecies_2,
      Rank, Infra.Rank, Infra.Rank_2, Author
    )

  # Excluir columnas que ya existen en base_df para evitar duplicados
  cols_to_exclude <- c(
    "Orig.Name", "Orig.Genus", "Orig.Species",
    "Orig.Infraspecies", "Orig.Infraspecies_2",
    "Rank", "Infra.Rank", "Infra.Rank_2", "Author"
  )

  res_complete <- base_df |>
    dplyr::left_join(
      res |> dplyr::select(-dplyr::any_of(cols_to_exclude)),
      by = "sorter"
    )

  # Inicializar columnas de matching para registros sin match
  res_complete <- res_complete |>
    dplyr::mutate(
      matched = tidyr::replace_na(matched, FALSE),
      Matched.Genus = dplyr::if_else(is.na(Matched.Genus), NA_character_, Matched.Genus),
      Matched.Species = dplyr::if_else(is.na(Matched.Species), NA_character_, Matched.Species),
      Matched.Infraspecies = dplyr::if_else(is.na(Matched.Infraspecies), NA_character_, Matched.Infraspecies),
      Matched.Infraspecies_2 = dplyr::if_else(is.na(Matched.Infraspecies_2), NA_character_, Matched.Infraspecies_2)
    )

  # Join con información de amenaza
  if (use_infraspecies_2) {
    target_threat <- target_prepared |>
      dplyr::select(
        genus, species, infraspecies, infraspecies_2,
        threat_category, accepted_name_author
      ) |>
      dplyr::distinct(genus, species, infraspecies, infraspecies_2, .keep_all = TRUE)

    output <- res_complete |>
      dplyr::arrange(sorter) |>
      dplyr::left_join(
        target_threat,
        by = c(
          "Matched.Genus" = "genus",
          "Matched.Species" = "species",
          "Matched.Infraspecies" = "infraspecies",
          "Matched.Infraspecies_2" = "infraspecies_2"
        )
      )
  } else {
    target_threat <- target_prepared |>
      dplyr::select(
        genus, species, infraspecies,
        threat_category, accepted_name_author
      ) |>
      dplyr::distinct(genus, species, infraspecies, .keep_all = TRUE)

    output <- res_complete |>
      dplyr::arrange(sorter) |>
      dplyr::left_join(
        target_threat,
        by = c(
          "Matched.Genus" = "genus",
          "Matched.Species" = "species",
          "Matched.Infraspecies" = "infraspecies"
        )
      )
  }

  # ========================================================================
  # SECTION 6: Calculate Matched Rank and Create Formatted Names
  # ========================================================================

  # Calcular Matched.Rank usando lógica unificada
  output_f <- output |>
    dplyr::mutate(
      Matched.Rank = dplyr::case_when(
        # Rank 1: Solo género matcheado
        !is.na(Matched.Genus) & is.na(Matched.Species) ~ 1L,

        # Rank 2: Género + especie matcheados, sin infraspecies
        !is.na(Matched.Genus) & !is.na(Matched.Species) &
          is.na(Matched.Infraspecies) ~ 2L,

        # Rank 3: Genus + especie + infraspecies nivel 1
        !is.na(Matched.Genus) & !is.na(Matched.Species) &
          !is.na(Matched.Infraspecies) & is.na(Matched.Infraspecies_2) ~ 3L,

        # Rank 4: Todos los niveles (solo si use_infraspecies_2 = TRUE)
        !is.na(Matched.Genus) & !is.na(Matched.Species) &
          !is.na(Matched.Infraspecies) & !is.na(Matched.Infraspecies_2) ~ 4L,

        # No matcheado
        TRUE ~ NA_integer_
      )
    )

  # Validación: Si no usamos infraspecies_2, no debe haber Rank 4
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

  # Crear nombre matcheado formateado
  output_f <- output_f |>
    dplyr::mutate(
      Matched.Name = dplyr::case_when(
        # No match
        is.na(Matched.Genus) ~ "---",

        # Rank 1: Solo género
        Matched.Rank == 1 ~ stringr::str_to_sentence(Matched.Genus),

        # Rank 2: Binomial
        Matched.Rank == 2 ~ paste(
          stringr::str_to_sentence(Matched.Genus),
          stringr::str_to_lower(Matched.Species)
        ),

        # Rank 3: Trinomial con rank infraespecífico
        Matched.Rank == 3 & !is.na(Infra.Rank) ~ paste(
          stringr::str_to_sentence(Matched.Genus),
          stringr::str_to_lower(Matched.Species),
          stringr::str_to_lower(Infra.Rank),
          stringr::str_to_lower(Matched.Infraspecies)
        ),

        # Rank 3 sin Infra.Rank (fallback)
        Matched.Rank == 3 & is.na(Infra.Rank) ~ paste(
          stringr::str_to_sentence(Matched.Genus),
          stringr::str_to_lower(Matched.Species),
          "subsp.",
          stringr::str_to_lower(Matched.Infraspecies)
        ),

        # Rank 4: Cuatrinomial con ambos ranks
        Matched.Rank == 4 & !is.na(Infra.Rank) & !is.na(Infra.Rank_2) ~ paste(
          stringr::str_to_sentence(Matched.Genus),
          stringr::str_to_lower(Matched.Species),
          stringr::str_to_lower(Infra.Rank),
          stringr::str_to_lower(Matched.Infraspecies),
          stringr::str_to_lower(Infra.Rank_2),
          stringr::str_to_lower(Matched.Infraspecies_2)
        ),

        # Rank 4 con ranks faltantes (fallback)
        Matched.Rank == 4 ~ paste(
          stringr::str_to_sentence(Matched.Genus),
          stringr::str_to_lower(Matched.Species),
          ifelse(!is.na(Infra.Rank), stringr::str_to_lower(Infra.Rank), "subsp."),
          stringr::str_to_lower(Matched.Infraspecies),
          ifelse(!is.na(Infra.Rank_2), stringr::str_to_lower(Infra.Rank_2), "var."),
          stringr::str_to_lower(Matched.Infraspecies_2)
        ),

        # Casos inesperados
        TRUE ~ "---"
      )
    )

  # Limpiar espacios múltiples
  output_f <- output_f |>
    dplyr::mutate(
      Matched.Name = stringr::str_squish(Matched.Name),
      Orig.Name = str_to_simple_cap(Orig.Name)
    )

  # Calcular comparación de ranks y nivel de matching
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
        is.na(Matched.Genus) & is.na(Matched.Species) & is.na(Matched.Infraspecies) ~ "Not threatened",
        !is.na(threat_category) ~ threat_category,
        TRUE ~ "Not threatened"
      )
    )

  # Formatear columnas finales
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
        "Author", "accepted_name_author", "Matched.Rank", "Comp.Rank", "Match.Level")
    )

  # ========================================================================
  # SECTION 7: Final Validation
  # ========================================================================

  # Validar que todos los registros de entrada estén presentes
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

  # Validación de coherencia Infraspecies_2
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

  # Validar coherencia entre Matched.Rank y columnas pobladas
  validation <- output_f |>
    dplyr::mutate(
      valid_rank = dplyr::case_when(
        is.na(Matched.Rank) ~ TRUE,  # No match es válido
        Matched.Rank == 1 ~ !is.na(Matched.Genus) & is.na(Matched.Species),
        Matched.Rank == 2 ~ !is.na(Matched.Genus) & !is.na(Matched.Species) &
          is.na(Matched.Infraspecies),
        Matched.Rank == 3 ~ !is.na(Matched.Genus) & !is.na(Matched.Species) &
          !is.na(Matched.Infraspecies) & is.na(Matched.Infraspecies_2),
        Matched.Rank == 4 ~ !is.na(Matched.Genus) & !is.na(Matched.Species) &
          !is.na(Matched.Infraspecies) & !is.na(Matched.Infraspecies_2),
        TRUE ~ FALSE
      )
    )

  invalid_ranks <- sum(!validation$valid_rank, na.rm = TRUE)

  if (invalid_ranks > 0) {
    warning(
      invalid_ranks, " records have inconsistent Matched.Rank values.\n",
      "Rank values do not match the populated taxonomic columns.\n",
      "This indicates a bug in the matching pipeline.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  # Añadir metadata sobre el proceso
  attr(output_f, "use_infraspecies_2") <- use_infraspecies_2
  attr(output_f, "target_database") <- target_df
  attr(output_f, "matching_date") <- Sys.Date()
  attr(output_f, "n_input") <- nrow(splist_class)
  attr(output_f, "n_matched") <- sum(output_f$matched, na.rm = TRUE)
  attr(output_f, "match_rate") <- round(
    sum(output_f$matched, na.rm = TRUE) / nrow(splist_class) * 100,
    2
  )

  return(output_f)
}
