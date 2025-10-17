#' @keywords internal
.names_standardize <- function(splist) {

  # Identificar NAs desde el inicio
  na_positions <- is.na(splist)

  # Trabajar solo con valores no-NA
  splist_clean <- splist[!na_positions]

  # Si todo es NA, retornar el vector original
  if (length(splist_clean) == 0) {
    return(splist)
  }

  # Convertir todo a mayúsculas
  fixed1 <- toupper(splist_clean)

  # Eliminar 'CF.' y 'AFF.'
  fixed2 <- gsub("CF\\.", "", fixed1)
  fixed3 <- gsub("AFF\\.", "", fixed2)

  # Eliminar espacios en blanco al inicio y al final
  fixed4 <- trimws(fixed3)

  # Cambiar guiones bajos por espacios
  fixed5 <- gsub("_", " ", fixed4)

  # Estandarizar categorías infraespecíficas
  # 1. VAR y VAR.
  fixed6 <- gsub(" VAR ", " VAR. ", fixed5)
  fixed6 <- gsub(" VAR\\. ", " VAR. ", fixed6)  # Asegurar solo un punto

  # 2. FORMA, F, F.
  fixed7 <- gsub(" (F|FO|FO\\.|FORM|FORM\\.|FORMA|FORMA\\.) ", " F. ", fixed6)

  # 3. SUBSP, SSP, SPP, etc.
  fixed8 <- gsub(" (SSP|SSP\\.|SPP|SPP\\.|SUBSP|SUBSP\\.|SP|SP\\.) ", " SUBSP. ", fixed7)

  # 4. SUBVAR y SUBVAR. (IMPORTANTE: debe ir después de VAR)
  fixed9 <- gsub(" SUBVAR ", " SUBVAR. ", fixed8)
  fixed9 <- gsub(" SUBVAR\\. ", " SUBVAR. ", fixed9)  # Asegurar solo un punto

  # 5. SUBF y SUBF. (IMPORTANTE: debe ir después de F)
  fixed9 <- gsub(" SUBF ", " SUBF. ", fixed9)
  fixed9 <- gsub(" SUBF\\. ", " SUBF. ", fixed9)  # Asegurar solo un punto

  # Manejar híbridos (eliminar 'X' y '\u00d7')
  fixed10 <- gsub("(^X )|( X$)|( X )|(^\u00d7 )|( \u00d7$)|( \u00d7 )", " ", fixed9)
  hybrids <- fixed9 == fixed10

  # Verificar híbridos (excluyendo NAs en la comparación)
  if (!all(hybrids, na.rm = TRUE)) {
    sp_hybrids <- splist_clean[!hybrids]
    warning(paste("The 'X' sign indicating hybrids have been removed in the",
                  "following names before search:",
                  paste(paste0("'", sp_hybrids, "'"), collapse = ", ")),
            immediate. = TRUE, call. = FALSE)
  }

  # Eliminar múltiples espacios
  fixed11 <- gsub(" +", " ", fixed10)

  # Eliminar símbolos no alfabéticos al inicio (CORREGIDO: usar fixed11 en ambos lados)
  for(j in 1:100) {
    whichs <- which(grepl("^[^A-Z]", fixed11))
    if(length(whichs) > 0)
      fixed11[whichs] <- gsub("^[^A-Z]", "", fixed11[whichs])  # CORREGIDO
    whichs <- which(grepl("^[^A-Z]", fixed11))
    if(length(whichs) == 0) break
  }

  # Reconstruir el vector completo manteniendo NAs en sus posiciones originales
  result <- character(length(splist))
  result[na_positions] <- NA_character_
  result[!na_positions] <- fixed11  # CORREGIDO: usar fixed11

  return(result)
}


#------------------------------------------------
#' @keywords internal
# Function wrap of .classify_algo for multiple species
.splist_classify <- function(x) {

  x <- .names_standardize(x)

  ##################
  infrasp <- c("subsp.", "ssp.", "var.", "subvar.",
               "forma", "f.", "subf.")
  Infrasp_cat <- toupper(infrasp)
  # Regular expression to make sure, infra code is between names
  Infrasp_cat_reg <- paste("[[:alpha:]]",
                           gsub("\\.",
                                "\\\\.",
                                Infrasp_cat),
                           "[[:alpha:]]")
  Infrasp_cat_reg |>  length()
  # Split names
  x_split <- strsplit(x, " ")

  # Aply the algorithm
  result <- lapply(x_split,
                   .classify_algo,
                   Infrasp_cat_reg)
  # Combine result list into a matrix
  result <- do.call(rbind, result)
  result <- cbind(x, result)
  # Combine categories and remove
  result[, 5] <- paste0(result[, 5], result[, 6])
  result[, 9] <- paste0(result[, 9], result[, 10])
  result <- result[, -c(6, 10), drop = FALSE]

  # Give the colnames of the matrix
  colnames(result) <- c(
    "Orig.Name",
    "Orig.Genus",
    "Orig.Species",
    "Author",
    "Subspecies",
    "Variety",
    "Subvariety",
    "Forma",
    "Subforma"
  )
  result
  return(result)
}

#------------------------------------------------
# The algorithm for one name
.classify_algo <- function(x_split_i,
                           Infrasp_cat_reg) {

  # Base output
  output <- character(10)

  # Count the number of names
  n <- length(x_split_i)

  # Genus and epithet
  output[1:2] <- x_split_i[1:2]


  # Check for infrataxa
  if (n > 2) {
    # Connect previous and next name to check for infras
    x_split_i_paste <- x_split_i
    x_split_i_paste[2:n] <- paste(substr(x_split_i[1:(n - 1)], 1, 1),
                                  x_split_i[2:n],
                                  substr(x_split_i[3:n],1 , 1))

    infra_check <- sapply(as.list(Infrasp_cat_reg),
                          function(x, y) {
                            regexpr(x, y) == 1
                          },
                          x_split_i_paste)
    infra_id <- rowSums(infra_check) > 0



    # if there is none get only the author name
    if (!any(infra_id)) {
      output[3] <- paste(x_split_i[3:n],
                         collapse = " ")
    } else {
      # If it has infra categories, get them

      n_infra <- sum(infra_id) # Number of infra categories
      pos <- which(infra_id)
      for (i in 1:n_infra) {
        # do it for all infra names
        # Get the position of the infra
        pos_1 <- pos[i] + 1
        pos_out <- which(infra_check[pos[i], ]) + 3
        output[pos_out] <- x_split_i[pos_1]
      }
      if (n > pos_1) {
        # get the author
        output[3] <- paste(x_split_i[(pos_1 + 1):n],
                           collapse = " ")
      }
      if (pos[1] > 3) { # Author names before infras
        output[3] <- paste(x_split_i[3:(pos[1] - 1)],
                           collapse = " ")
      }
    }
  }
  return(output)
}


# ---------------------------------------------------------------
#' @keywords internal
# Definir la función para transformar el data frame
.transform_split_classify <- function(df) {
  df <- as.data.frame(df)
  df$sorter <- 1:nrow(df)

  infra_cols <- c("Subspecies", "Variety", "Subvariety", "Forma", "Subforma")
  infra_ranks <- c("SUBSP.", "VAR.", "SUBVAR.", "F.", "SUBF.")

  # Siempre inicializar ambos niveles
  df$Orig.Infraspecies <- NA_character_
  df$Orig.Infra.Rank <- NA_character_
  df$Orig.Infraspecies_2 <- NA_character_
  df$Orig.Infra.Rank_2 <- NA_character_

  for (i in 1:nrow(df)) {
    non_empty <- which(df[i, infra_cols] != "")

    if (length(non_empty) >= 1) {
      first_idx <- non_empty[1]
      df$Orig.Infraspecies[i] <- df[i, infra_cols[first_idx]]
      df$Orig.Infra.Rank[i] <- infra_ranks[first_idx]
    }

    if (length(non_empty) >= 2) {
      second_idx <- non_empty[2]
      df$Orig.Infraspecies_2[i] <- df[i, infra_cols[second_idx]]
      df$Orig.Infra.Rank_2[i] <- infra_ranks[second_idx]
    }
  }

  # Calcular Rank de forma clara y explícita
  # Rank 1: Solo género
  # Rank 2: Género + especie (binomial)
  # Rank 3: Género + especie + infraspecies nivel 1 (trinomial)
  # Rank 4: Género + especie + infraspecies nivel 1 + nivel 2 (cuatrinomial)

  df$Rank <- dplyr::case_when(
    # Rank 1: Solo género válido, sin especie
    !is.na(df$Orig.Genus) & is.na(df$Orig.Species) ~ 1L,

    # Rank 2: Género + especie, sin infraspecies
    !is.na(df$Orig.Genus) & !is.na(df$Orig.Species) &
      is.na(df$Orig.Infraspecies) ~ 2L,

    # Rank 3: Género + especie + infraspecies nivel 1, sin nivel 2
    !is.na(df$Orig.Genus) & !is.na(df$Orig.Species) & !is.na(df$Orig.Infra.Rank) &
      !is.na(df$Orig.Infraspecies) & is.na(df$Orig.Infraspecies_2) ~ 3L,

    # Rank 4: Género + especie + infraspecies nivel 1 + nivel 2
    !is.na(df$Orig.Genus) & !is.na(df$Orig.Species) & !is.na(df$Orig.Infra.Rank_2) &
      !is.na(df$Orig.Infraspecies) & !is.na(df$Orig.Infra.Rank_2) & !is.na(df$Orig.Infraspecies_2) ~ 4L,

    # Casos inválidos
    TRUE ~ NA_integer_
  )

  # Validar que no haya NAs inesperados en Rank
  if (any(is.na(df$Rank))) {
    na_ranks <- sum(is.na(df$Rank))
    warning(
      na_ranks, " names could not be assigned a taxonomic rank.\n",
      "This may indicate malformed names in the input.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  # Reportar distribución de ranks para debugging (solo si hay muchos nombres)
  #if (nrow(df) > 10) {
  #  rank_dist <- table(df$Rank, useNA = "ifany")
  #  message(
  #    "Rank distribution: ",
  #    paste(names(rank_dist), "=", rank_dist, collapse = ", ")
  #  )
  #}

  # ========================================================================
  # Reorder Columns and Return
  # ========================================================================

  column_order <- c(
    "sorter", "Orig.Name", "Orig.Genus", "Orig.Species", "Author",
    "Orig.Infraspecies", "Orig.Infra.Rank",
    "Orig.Infraspecies_2", "Orig.Infra.Rank_2",
    "Rank"
  )

  # Verificar que todas las columnas existan antes de reordenar
  missing_order_cols <- setdiff(column_order, colnames(df))
  if (length(missing_order_cols) > 0) {
    stop(
      "Cannot reorder columns. Missing: ",
      paste(missing_order_cols, collapse = ", "),
      call. = FALSE
    )
  }

  df <- df[, column_order]

  return(df)

}


# ---------------------------------------------------------------
#' @keywords internal
map_dfr_progress <- function(.x, .f, ..., .id = NULL) {
  function_name <- stringr::str_remove(toString(substitute(.f)), '_helper')
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x),
                                   force = TRUE,
                                   format = paste(paste0(eval(...), collapse = ' '), ": ",
                                                  function_name, "[:bar] :percent", collapse = ''))

  f <- function(...) {
    pb$tick()
    .f(...)
  }

  purrr::map_dfr(.x, f, ..., .id = .id)
}

# ---------------------------------------------------------------
#' @keywords internal
get_threatened_genus <- function(genus_sub, target_df = NULL){
  return(target_df |>
           dplyr::filter(genus %in% genus_sub) |>
           dplyr::select(c('genus', 'species')))
}

memoised_get_threatened_genus <- memoise::memoise(get_threatened_genus)

# ---------------------------------------------------------------
#' @keywords internal
simple_cap <- function (x) {
  words <- sapply(strsplit(x, " "),
                  function(words) paste(tolower(words),
                                        collapse = " "))
  capitalized <- sapply(strsplit(words, ""), function(word) {
    if (length(word) > 0) {
      word[1] <- toupper(word[1])
    }
    paste(word, collapse = "")
  })
  return(capitalized)
}

# ---------------------------------------------------------------
#' @keywords internal
str_to_simple_cap <- function(text) {
  # Convertir todo el texto a minúsculas
  text <- tolower(text)

  # Obtener la primera letra y convertirla a mayúscula
  first_letter <- toupper(substr(text, 1, 1))

  # Obtener el resto del texto desde la segunda letra en adelante
  rest_text <- substr(text, 2, nchar(text))

  # Combinar la primera letra en mayúscula con el resto del texto en minúsculas
  result <- paste0(first_letter, rest_text)

  return(result)
}

# ---------------------------------------------------------------
#' @keywords internal
.check_binomial <- function(splist_class, splist) {

  # Identificar posiciones con NA en la lista original
  na_positions <- which(is.na(splist))
  #na_positions

  # Identificar nombres que solo tienen género (especies sin epíteto específico)
  # Excluir las filas que corresponden a NA en la lista original
  missing_species <- which(apply(splist_class[, 3:4, drop = FALSE],
                                 1,
                                 function(x) {any(is.na(x))}))

  #missing_species
  # Separar NAs de nombres incompletos
  genuine_missing <- setdiff(missing_species, na_positions)
  #genuine_missing
  # Reportar nombres a nivel de género (excluyendo NAs)
  if (length(genuine_missing) > 0) {
    genus_level_names <- splist[genuine_missing]
    message(paste0("The species list (splist) should only include binomial names. ",
                   "The following names were submitted at the genus level: ",
                   paste(paste0("'", genus_level_names, "'"),
                         collapse = ", ")))
  }

  # Reportar NAs si existen
  if (length(na_positions) > 0) {
    message(paste0("The species list (splist) contains ",
                   length(na_positions),
                   " NA value(s) at position(s): ",
                   paste(na_positions, collapse = ", "),
                   ". \n These will be excluded from matching."))
  }

  # Retornar todas las posiciones problemáticas
  all_problematic <- sort(c(genuine_missing, na_positions))

  return(all_problematic)
}


#' Access Internal Package Data
#'
#' @keywords internal
#' @noRd
get_threatened_data <- function(type = c("original", "updated")) {
  type <- match.arg(type)

  # Los datos deben estar en R/sysdata.rda
  data_name <- switch(
    type,
    "original" = "threatenedperu",
    "updated" = "threatenedperu_syn"
  )

  # Acceder desde el namespace del paquete
  ns <- asNamespace("threatenedperu")

  if (!exists(data_name, envir = ns, inherits = FALSE)) {
    stop(
      "Internal dataset '", data_name, "' not found in package namespace.\n",
      "This indicates a package installation problem.",
      call. = FALSE
    )
  }

  get(data_name, envir = ns, inherits = FALSE)
}

# ---------------------------------------------------------------
# ---------------------------------------------------------------
utils::globalVariables(c(
  # ============================================================
  # Columnas de nombres originales (Orig.*)
  # ============================================================
  "Orig.Genus",
  "Orig.Infra.Rank",
  "Orig.Infra.Rank_2",
  "Orig.Infraspecies",
  "Orig.Infraspecies_2",
  "Orig.Name",
  "Orig.Species",

  # ============================================================
  # Columnas de nombres matched (Matched.*)
  # ============================================================
  "Matched.Genus",
  "Matched.Infra.Rank",
  "Matched.Infra.Rank_2",
  "Matched.Infraspecies",
  "Matched.Infraspecies_2",
  "Matched.Name",
  "Matched.Rank",
  "Matched.Rank.Calculated",
  "Matched.Species",

  # ============================================================
  # Columnas de ranks y niveles taxonómicos
  # ============================================================
  "Comp.Rank",
  "Match.Level",
  "Rank",

  # ============================================================
  # Columnas de threat status
  # ============================================================
  "Threat.Status",
  "Threat_Category",
  "Threat_Status",
  "threat_category",

  # ============================================================
  # Columnas de nomenclatura y taxonomía
  # ============================================================
  "accepted_name",
  "accepted_name_author",
  "scientific_name",
  "taxonomic_status",

  # ============================================================
  # Columnas de métodos de matching (booleanos)
  # ============================================================
  "direct_match",
  "direct_match_infra_rank",
  "direct_match_species_within_genus",
  "fuzzy_match_genus",
  "fuzzy_match_infraspecies",
  "fuzzy_match_infraspecies_2",
  "fuzzy_match_infraspecies_within_species",
  "fuzzy_match_species_within_genus",
  "genus_match",
  "matched",
  "suffix_match_species_within_genus",
  "valid_rank",

  # ============================================================
  # Columnas de distancias (fuzzy matching)
  # ============================================================
  "fuzzy_genus_dist",
  "fuzzy_infraspecies_2_dist",
  "fuzzy_infraspecies_dist",
  "fuzzy_species_dist",

  # ============================================================
  # Columnas de base de datos (target_df)
  # ============================================================
  "family",
  "Family",
  "genus",
  "infraspecies",
  "infraspecies_2",
  "species",
  "tag",
  "tag_2",
  "tag_acc",

  # ============================================================
  # Columnas de consolidación (is_ds043_2006_ag)
  # ============================================================
  "Accepted.Name",
  "Consolidated.Category",
  "Consolidated.Name",
  "Consolidated.Status",
  "Final.Source",
  "Found.In.Original",
  "Found.In.Updated",
  "Input.Name",
  "Is.Synonym",
  "Match.Scenario",
  "Nomenclature.Status",
  "Original.Index",
  "Original.Matched",
  "Original.Status",
  "Protected.DS043",
  "protected_ds_043",
  "Updated.Matched",
  "Updated.Status",

  # ============================================================
  # Columnas auxiliares y de control
  # ============================================================
  "Author",
  "Category",
  "Count",
  "data",
  "sorter",
  "val_rank_declred",
  "source",

  # ============================================================
  # Operadores
  # ============================================================
  "%>%"
))
