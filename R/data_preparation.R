# #' Prepare threatened species database for matching
# #'
# #' @description
# #' Prepares the threatened species database of Peru for use in matching functions
# #' by standardizing column names, handling infraspecies data, and cleaning entries.
# #'
# #' @param file_path Path to the raw data file (TSV format)
# #' @param verbose Logical. Whether to print processing information.
# #'
# #' @return A cleaned and standardized tibble ready for matching operations
# #'
# #' @keywords internal
#  prepare_threatened_data <- function(file_path ) {
#  #file_path <- "D:\\peruflorads43\\ds_043_2006_092025_f1.xlsx"
#    if (!file.exists(file_path)) {
#      stop("File not found: ", file_path)
#    }
#    # Read raw data with proper encoding
#    raw_data <- readxl::read_xlsx(
#      file_path
#    )
#    names(raw_data)
#    # Data cleaning and standardization
#    clean_data <- raw_data |>
#          # Standardize column names
#      dplyr::rename(
#        threat_category = categoria,
#        family = familia,
#        genus = genero,
#        species = specific_epithet,
#        scientific_name = scientific_name,
#        common_name = nombre_comun,
#        author = author,
#        tag = tag,
#        infraspecies = infraespecie,
#        form = forma,
#        accepted_family = Accepted_family
#      ) |>
#      # Clean threat categories
#      dplyr::mutate(
#        threat_category = dplyr::case_when(
#          stringr::str_detect(threat_category, "CRITICO|CR") ~ "CR",
#          stringr::str_detect(threat_category, "EN PELIGRO.*\\(EN\\)") ~ "EN",
#          stringr::str_detect(threat_category, "VULNERABLE|Vu") ~ "VU",
#          stringr::str_detect(threat_category, "CASI AMENAZADO|NT") ~ "NT",
#          TRUE ~ "Unknown"
#        )
#      ) |>
#      # Standardize taxonomic fields to uppercase for matching
#      dplyr::mutate(
#        family = toupper(trimws(family)),
#        genus = toupper(trimws(genus)),
#        species = toupper(trimws(species)),
#        # Handle infraspecies properly
#        infraspecies = dplyr::case_when(
#          !is.na(infraspecies) & nchar(trimws(infraspecies)) > 0 ~
#            toupper(trimws(infraspecies)),
#          !is.na(form) & nchar(trimws(form)) > 0 ~
#            toupper(trimws(form)),
#          TRUE ~ NA_character_
#        ),
#        # Clean infraspecific rank
#        infra_rank = dplyr::case_when(
#          !is.na(tag) & tag %in% c("subsp.", "var.", "f.") ~ toupper(tag),
#          !is.na(infraspecies) & !is.na(tag) ~ toupper(tag),
#          TRUE ~ NA_character_
#        ),
#        # Clean common names (remove special characters)
#        common_name = iconv(common_name, from = "UTF-8", to = "ASCII//TRANSLIT")
#      ) |>
#      dplyr::mutate(family = stringr::str_to_sentence(family))
#    return(clean_data)
#  }
#
#
#  threatenedperu <- prepare_threatened_data(file_path = "D:\\peruflorads43\\ds_043_2006_092025_f1.xlsx"
#  )
#
#  threatenedperu <-
#  threatenedperu |>
#    dplyr::select(scientific_name,
#                  author,
#                  family,
#                  tag,
#                  form,
#                  accepted_name,
#                  accepted_name_author,
#                  accepted_family,
#                  taxonomic_status,
#                  threat_category,
#                  ) |>
#    dplyr::mutate(genus = stringr::word(scientific_name, 1,1) |>
#                    toupper(),
#                  species = stringr::word(scientific_name, 2, 2) |>
#                    toupper(),
#                  infraspecies = stringr::word(scientific_name, 4, 4) |>
#                    toupper())
#  threatenedperu
#
# # ---------------------------------------------------------------
# # 1 Dictyocaryum ptariense  Dictyocaryum
# # 2 Ligeophila spp          Microchilus
# # 3 Epidendrum pardothyrsus Epidendrum
# # 4 Prosthechea cyperifolia Prosthechea
# # 5 Eriosyce omasensis      Eriosyce
# # para estas especies se asignan sinonimos revisar blok de notas para comprender
# # el contexto de la decicion
# threatenedperu_syn <-
#  threatenedperu |>
#    dplyr::select(-c(genus:infraspecies)) |>
#    #mutate(nword= stringi::stri_count_words(accepted_name)) |>
#    dplyr::mutate(genus = stringr::word(accepted_name, 1, 1) |> toupper(),
#           species = stringr::word(accepted_name, 2, 2) |> toupper(),
#           tag_acc = stringr::word(accepted_name, 3, 3),
#           infraspecies = stringr::word(accepted_name, 4, 4) |> toupper()) |>
#    dplyr::filter(taxonomic_status == "Synonym")
#
#  threatenedperu_syn
#
#
# # Al final, después de crear ambos datasets
# usethis::use_data(
#   threatenedperu,
#   threatenedperu_syn,
#   internal = TRUE,
#   overwrite = TRUE,
#   compress = "xz"
# )


