#' @keywords internal
.validate_inputs <- function(splist, quiet) {
  if (!is.character(splist)) {
    stop("`splist` debe ser un vector de caracteres.", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` debe ser TRUE/FALSE de longitud 1.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
.load_target <- function(source, quiet) {
  out <- tryCatch(
    get_threatened_data(type = source),
    error = function(e) {
      stop(sprintf("No se pudo cargar la base '%s': %s", source, e$message), call. = FALSE)
    }
  )
  out |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(is.character),
      .fns  = ~ stringr::str_squish(.x)
    ))
}

#' @keywords internal
.validate_target_schema <- function(target_prepared, use_infraspecies_2) {
  req <- if (use_infraspecies_2) {
    c("genus","species","tag","infraspecies","infraspecies_2","threat_category")
  } else {
    c("genus","species","tag_acc","infraspecies","threat_category")
  }
  miss <- setdiff(req, names(target_prepared))
  if (length(miss)) {
    stop(sprintf("La base carece de columnas requeridas: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
.classify_inputs <- function(splist) {
  # Encapsula tu .splist_classify() + .transform_split_classify()
  .splist_classify(splist) |>
    .transform_split_classify()
}

#' @keywords internal
.split_valid_invalid <- function(splist_class) {
  non_binomial <- .check_binomial(splist_class, splist = splist_class$Orig.Name)
  if (length(non_binomial)) {
    list(valid = splist_class[-non_binomial, , drop = FALSE],
         all   = splist_class)
  } else {
    list(valid = splist_class, all = splist_class)
  }
}

#' @keywords internal
.empty_output <- function(splist_class, use_infraspecies_2, source) {
  tib <- splist_class |>
    dplyr::select("sorter","Orig.Name","Orig.Genus","Orig.Species",
                  "Orig.Infraspecies","Orig.Infraspecies_2",
                  "Rank","Orig.Infra.Rank","Orig.Infra.Rank_2","Author") |>
    dplyr::mutate(
      Matched.Name           = "---",
      Matched.Genus          = NA_character_,
      Matched.Species        = NA_character_,
      Matched.Infra.Rank     = NA_character_,
      Matched.Infraspecies   = NA_character_,
      Matched.Infra.Rank_2   = NA_character_,
      Matched.Infraspecies_2 = NA_character_,
      Matched.Rank           = NA_integer_,
      Matched.Rank.Calculated= NA_integer_,
      valid_rank             = FALSE,
      matched                = FALSE,
      threat_category        = NA_character_,
      accepted_name_author   = "---",
      Threat.Status          = "Not threatened",
      Comp.Rank              = FALSE,
      Match.Level            = "No match"
    ) |>
    dplyr::relocate("sorter","Orig.Name","Matched.Name","Threat.Status",
                    "Author","accepted_name_author","Matched.Rank",
                    "Comp.Rank","Match.Level")
  tib
}

#' @keywords internal
.init_matching_columns <- function(df, use_infraspecies_2, source, quiet) {
  cols <- c("Matched.Genus","Matched.Species","Matched.Infra.Rank",
            "Matched.Infraspecies","Matched.Infra.Rank_2","Matched.Infraspecies_2")
  for (nm in cols) if (!nm %in% names(df)) df[[nm]] <- NA_character_
  if (!"Orig.Infraspecies" %in% names(df)) df$Orig.Infraspecies <- NA_character_
  if (!"Orig.Infraspecies_2" %in% names(df)) df$Orig.Infraspecies_2 <- NA_character_

  if (!use_infraspecies_2) {
    df$Orig.Infraspecies_2   <- NA_character_
    df$Matched.Infraspecies_2 <- NA_character_
    if (!quiet) message("Nota: usando base '", source, "' (nombres actualizados, sin infraspecies_2).")
  }
  df
}

#' @keywords internal
.warn_on_rank4_if_unsupported <- function(df, use_infraspecies_2, source, quiet) {
  if (!use_infraspecies_2 && any(df$Rank == 4L, na.rm = TRUE)) {
    warning(sum(df$Rank == 4L, na.rm = TRUE),
            " nombres Rank 4 detectados; la base '", source,
            "' no soporta infraspecies_2; no serán emparejados.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
.pipeline_nodes_1_to_5 <- function(df, target_prepared, quiet) {
  n1 <- df |> direct_match(target_df = target_prepared, source = attr(df, "target_database", exact = TRUE))
  n1_true  <- dplyr::filter(n1, .data$direct_match)
  n1_false <- dplyr::filter(n1, !.data$direct_match)

  n2 <- genus_match(n1_false, target_prepared)
  n2_true  <- dplyr::filter(n2, .data$genus_match)
  n2_false <- dplyr::filter(n2, !.data$genus_match)

  n3 <- fuzzy_match_genus(n2_false, target_prepared)
  n3_true  <- dplyr::filter(n3, .data$fuzzy_match_genus)
  n3_false <- dplyr::filter(n3, !.data$fuzzy_match_genus)

  n4_input <- dplyr::bind_rows(n3_true, n2_true)
  n4 <- direct_match_species_within_genus(n4_input, target_prepared)
  n4_true  <- dplyr::filter(n4, .data$direct_match_species_within_genus)
  n4_false <- dplyr::filter(n4, !.data$direct_match_species_within_genus)

  n5a <- suffix_match_species_within_genus(n4_false, target_prepared)
  n5a_true  <- dplyr::filter(n5a, .data$suffix_match_species_within_genus)
  n5a_false <- dplyr::filter(n5a, !.data$suffix_match_species_within_genus)

  n5b_input <- dplyr::filter(n5a_false, !is.na(.data$Orig.Species))
  n5b <- fuzzy_match_species_within_genus(n5b_input, target_prepared)
  n5b_true  <- dplyr::filter(n5b, .data$fuzzy_match_species_within_genus)
  n5b_false <- dplyr::filter(n5b, !.data$fuzzy_match_species_within_genus)

  list(
    n1_true = n1_true, n2_true = n2_true, n3_true = n3_true,
    n4_true = n4_true, n5a_true = n5a_true, n5b_true = n5b_true,
    n3_false = n3_false, n5b_false = n5b_false
  )
}

#' @keywords internal
.combine_nodes <- function(pipe) {
  dplyr::bind_rows(
    pipe$n1_true,
    dplyr::filter(pipe$n2_true, !is.na(.data$Matched.Genus) & .data$Rank == 1L),
    pipe$n4_true,
    pipe$n5a_true,
    pipe$n5b_true
  )
}

#' @keywords internal
.compute_matched_rank_and_validate <- function(combined, use_infraspecies_2) {
  dplyr::mutate(combined,
                Matched.Rank.Calculated = dplyr::case_when(
                  !is.na(.data$Matched.Genus) &  is.na(.data$Matched.Species) ~ 1L,
                  !is.na(.data$Matched.Genus) & !is.na(.data$Matched.Species) &
                    is.na(.data$Matched.Infra.Rank) & is.na(.data$Matched.Infraspecies) ~ 2L,
                  !is.na(.data$Matched.Genus) & !is.na(.data$Matched.Species) &
                    !is.na(.data$Matched.Infra.Rank) & !is.na(.data$Matched.Infraspecies) &
                    is.na(.data$Matched.Infra.Rank_2) & is.na(.data$Matched.Infraspecies_2) ~ 3L,
                  use_infraspecies_2 &
                    !is.na(.data$Matched.Genus) & !is.na(.data$Matched.Species) &
                    !is.na(.data$Matched.Infra.Rank) & !is.na(.data$Matched.Infraspecies) &
                    !is.na(.data$Matched.Infra.Rank_2) & !is.na(.data$Matched.Infraspecies_2) ~ 4L,
                  TRUE ~ NA_integer_
                ),
                valid_rank = (.data$Rank == .data$Matched.Rank.Calculated)
  )
}

#' @keywords internal
.split_matched_invalid_unmatched <- function(combined, pipe, quiet) {
  matched <- dplyr::filter(combined, .data$valid_rank, !is.na(.data$Matched.Rank.Calculated))
  invalid <- dplyr::filter(combined, !.data$valid_rank | is.na(.data$Matched.Rank.Calculated))
  if (!quiet && nrow(invalid) > 0L) {
    message("Info: ", nrow(invalid), " candidatos rechazados por discordancia de rango.")
  }
  unmatched <- dplyr::bind_rows(invalid, pipe$n3_false, pipe$n5b_false)
  list(matched = matched, unmatched = unmatched)
}

#' @keywords internal
.pipeline_nodes_6_7 <- function(lists, target_prepared, source, use_infraspecies_2) {
  # Node 6a
  n6_in <- lists$unmatched |>
    dplyr::filter(!is.na(.data$Orig.Infra.Rank),
                  !is.na(.data$Matched.Genus),
                  !is.na(.data$Matched.Species),
                  is.na(.data$Matched.Infraspecies))
  n6a <- direct_match_infra_rank_within_species(n6_in, target_df = target_prepared, source = source)
  n6a_true  <- dplyr::filter(n6a, .data$direct_match_infra_rank)
  n6a_false <- dplyr::filter(n6a, !.data$direct_match_infra_rank)

  # Node 6b
  n6b <- fuzzy_match_infraspecies_within_species(n6a_true, target_df = target_prepared, source = source) |>
    dplyr::mutate(
      Matched.Rank.Calculated = dplyr::case_when(
        !is.na(.data$Matched.Genus) & !is.na(.data$Matched.Species) &
          !is.na(.data$Matched.Infra.Rank) & !is.na(.data$Matched.Infraspecies) &
          is.na(.data$Matched.Infraspecies_2) ~ 3L,
        TRUE ~ NA_integer_
      ),
      valid_rank = (.data$Rank == .data$Matched.Rank.Calculated)
    )
  n6b_true  <- dplyr::filter(n6b, .data$fuzzy_match_infraspecies, .data$valid_rank, .data$Rank == 3L)
  n6b_false <- dplyr::filter(n6b, !.data$fuzzy_match_infraspecies | !.data$valid_rank | .data$Rank != 3L)

  if (use_infraspecies_2) {
    n7_in <- dplyr::filter(n6b_false,
                           .data$Rank == 4L,
                           !is.na(.data$Orig.Infraspecies_2),
                           is.na(.data$Matched.Infraspecies_2),
                           !is.na(.data$Matched.Infraspecies))
    if (nrow(n7_in) > 0L) {
      n7 <- fuzzy_match_infraspecies2_within_infraspecies(n7_in, target_prepared) |>
        dplyr::mutate(
          Matched.Rank.Calculated = dplyr::case_when(
            !is.na(.data$Matched.Genus) & !is.na(.data$Matched.Species) &
              !is.na(.data$Matched.Infra.Rank) & !is.na(.data$Matched.Infraspecies) &
              .data$Orig.Infra.Rank_2 == "F." & !is.na(.data$Matched.Infraspecies_2) ~ 4L,
            TRUE ~ NA_integer_
          ),
          valid_rank = (.data$Rank == .data$Matched.Rank.Calculated)
        )
      n7_true  <- dplyr::filter(n7, .data$valid_rank, .data$fuzzy_match_infraspecies_2) |>
        dplyr::mutate(Matched.Infra.Rank_2 = "F.")
      n7_false <- dplyr::filter(n7, !.data$valid_rank | !.data$fuzzy_match_infraspecies_2)
    } else {
      n7_true  <- n6b_false[0, ]
      n7_false <- n6b_false[0, ]
    }

    matched_f   <- dplyr::bind_rows(lists$matched, n6b_true, n7_true)
    unmatched_f <- dplyr::bind_rows(n6a_false, n7_false)
    res <- dplyr::bind_rows(matched_f, unmatched_f, .id = "matched") |>
      dplyr::mutate(matched = (.data$matched == "1"))
  } else {
    matched_f <- dplyr::bind_rows(lists$matched, n6b_true)
    res <- dplyr::bind_rows(matched_f, n6b_false, .id = "matched") |>
      dplyr::mutate(matched = (.data$matched == "1"))
  }

  list(res = res)
}

#' @keywords internal
.join_threat_and_format <- function(base_df, res, target_prepared, use_infraspecies_2) {
  cols_off <- c("Orig.Name","Orig.Genus","Orig.Species",
                "Orig.Infraspecies","Orig.Infraspecies_2",
                "Rank","Orig.Infra.Rank","Orig.Infra.Rank_2","Author")

  res_complete <- dplyr::left_join(
    base_df,
    dplyr::select(res, -dplyr::any_of(cols_off)),
    by = "sorter"
  ) |>
    dplyr::mutate(
      matched                = tidyr::replace_na(.data$matched, FALSE),
      Matched.Genus          = dplyr::if_else(is.na(.data$Matched.Genus), NA_character_, .data$Matched.Genus),
      Matched.Species        = dplyr::if_else(is.na(.data$Matched.Species), NA_character_, .data$Matched.Species),
      Matched.Infraspecies   = dplyr::if_else(is.na(.data$Matched.Infraspecies), NA_character_, .data$Matched.Infraspecies),
      Matched.Infraspecies_2 = dplyr::if_else(is.na(.data$Matched.Infraspecies_2), NA_character_, .data$Matched.Infraspecies_2)
    )

  if (use_infraspecies_2) {
    target_threat <- target_prepared |>
      dplyr::select("genus","species","tag","infraspecies","infraspecies_2",
                    "threat_category","accepted_name_author") |>
      dplyr::mutate(tag = toupper(.data$tag)) |>
      dplyr::distinct(.data$genus, .data$species, .data$tag, .data$infraspecies, .data$infraspecies_2, .keep_all = TRUE)

    out2 <- dplyr::left_join(
      dplyr::filter(res_complete, .data$Rank == 2L, .data$matched, .data$valid_rank),
      dplyr::filter(target_threat, is.na(.data$tag), is.na(.data$infraspecies), is.na(.data$infraspecies_2)),
      by = c("Matched.Genus" = "genus", "Matched.Species" = "species"),
      na_matches = "never"
    )
    out3 <- dplyr::left_join(
      dplyr::filter(res_complete, .data$Rank == 3L, .data$matched, .data$valid_rank),
      dplyr::filter(target_threat, !is.na(.data$tag), !is.na(.data$infraspecies), is.na(.data$infraspecies_2)),
      by = c("Matched.Genus"="genus","Matched.Species"="species",
             "Matched.Infra.Rank"="tag","Matched.Infraspecies"="infraspecies"),
      na_matches = "never"
    )
    out4 <- dplyr::left_join(
      dplyr::filter(res_complete, .data$Rank == 4L, .data$matched, .data$valid_rank),
      dplyr::filter(target_threat, !is.na(.data$tag), !is.na(.data$infraspecies), !is.na(.data$infraspecies_2)),
      by = c("Matched.Genus"="genus","Matched.Species"="species",
             "Matched.Infra.Rank"="tag","Matched.Infraspecies"="infraspecies",
             "Matched.Infraspecies_2"="infraspecies_2"),
      na_matches = "never"
    )
    invalid <- dplyr::filter(res_complete, .data$matched, !.data$valid_rank) |>
      dplyr::mutate(tag = NA_character_, infraspecies = NA_character_, infraspecies_2 = NA_character_,
                    threat_category = NA_character_, accepted_name_author = NA_character_)
    unmatched <- dplyr::filter(res_complete, !.data$matched) |>
      dplyr::mutate(tag = NA_character_, infraspecies = NA_character_, infraspecies_2 = NA_character_,
                    threat_category = NA_character_, accepted_name_author = NA_character_)

    dplyr::bind_rows(out2, out3, out4, invalid, unmatched) |>
      dplyr::arrange(.data$sorter)
  } else {
    target_threat <- target_prepared |>
      dplyr::select("genus","species","infraspecies","tag_acc",
                    "threat_category","accepted_name_author") |>
      dplyr::distinct(.data$genus,.data$species,.data$tag_acc,.data$infraspecies, .keep_all = TRUE)

    out2 <- dplyr::left_join(
      dplyr::filter(res_complete, .data$Rank == 2L, .data$matched, .data$valid_rank),
      dplyr::filter(target_threat, is.na(.data$tag_acc), is.na(.data$infraspecies)),
      by = c("Matched.Genus"="genus","Matched.Species"="species"),
      na_matches = "never"
    )
    out3 <- dplyr::left_join(
      dplyr::filter(res_complete, .data$Rank == 3L, .data$matched, .data$valid_rank),
      dplyr::filter(target_threat, !is.na(.data$tag_acc), !is.na(.data$infraspecies)),
      by = c("Matched.Genus"="genus","Matched.Species"="species",
             "Matched.Infra.Rank"="tag_acc","Matched.Infraspecies"="infraspecies"),
      na_matches = "never"
    )
    invalid <- dplyr::filter(res_complete, .data$matched, !.data$valid_rank) |>
      dplyr::mutate(tag_acc = NA_character_, infraspecies = NA_character_,
                    threat_category = NA_character_, accepted_name_author = NA_character_)
    unmatched <- dplyr::filter(res_complete, !.data$matched) |>
      dplyr::mutate(tag_acc = NA_character_, infraspecies = NA_character_,
                    threat_category = NA_character_, accepted_name_author = NA_character_)

    dplyr::bind_rows(out2, out3, invalid, unmatched) |>
      dplyr::arrange(.data$sorter)
  }
}

#' @keywords internal
.finalize_output <- function(output, use_infraspecies_2) {
  out <- dplyr::mutate(output,
                       Matched.Rank = dplyr::case_when(
                         !is.na(.data$Matched.Genus) &  is.na(.data$Matched.Species) ~ 1L,
                         !is.na(.data$Matched.Genus) & !is.na(.data$Matched.Species) &
                           is.na(.data$Matched.Infra.Rank) & is.na(.data$Matched.Infraspecies) ~ 2L,
                         !is.na(.data$Matched.Genus) & !is.na(.data$Matched.Species) &
                           !is.na(.data$Matched.Infra.Rank) & !is.na(.data$Matched.Infraspecies) &
                           is.na(.data$Matched.Infraspecies_2) ~ 3L,
                         !is.na(.data$Matched.Genus) & !is.na(.data$Matched.Species) &
                           !is.na(.data$Matched.Infra.Rank) & !is.na(.data$Matched.Infraspecies) &
                           !is.na(.data$Matched.Infraspecies_2) ~ 4L,
                         TRUE ~ NA_integer_
                       )
  )
  if (!use_infraspecies_2 && any(out$Matched.Rank == 4L, na.rm = TRUE)) {
    warning("Se detectaron matches Rank 4 con base sin infraspecies_2; corrigiendo a NA.", call. = FALSE)
    out$Matched.Rank[out$Matched.Rank == 4L] <- NA_integer_
    out$Matched.Infraspecies_2[which(out$Matched.Rank == 4L)] <- NA_character_
  }
  out |>
    dplyr::mutate(
      Matched.Name = dplyr::case_when(
        is.na(.data$Matched.Genus) ~ "---",
        .data$Matched.Rank == 1L ~ str_to_simple_cap(.data$Matched.Genus),
        .data$Matched.Rank == 2L ~ paste(str_to_simple_cap(.data$Matched.Genus), stringr::str_to_lower(.data$Matched.Species)),
        .data$Matched.Rank == 3L & !is.na(.data$Matched.Infra.Rank) ~ paste(
          str_to_simple_cap(.data$Matched.Genus),
          stringr::str_to_lower(.data$Matched.Species),
          stringr::str_to_lower(.data$Matched.Infra.Rank),
          stringr::str_to_lower(.data$Matched.Infraspecies)
        ),
        .data$Matched.Rank == 4L & !is.na(.data$Matched.Infra.Rank) &
          !is.na(.data$Matched.Infra.Rank_2) & !is.na(.data$Matched.Infraspecies_2) ~ paste(
            str_to_simple_cap(.data$Matched.Genus),
            stringr::str_to_lower(.data$Matched.Species),
            stringr::str_to_lower(.data$Matched.Infra.Rank),
            stringr::str_to_lower(.data$Matched.Infraspecies),
            stringr::str_to_lower(.data$Matched.Infra.Rank_2),
            stringr::str_to_lower(.data$Matched.Infraspecies_2)
          ),
        TRUE ~ "---"
      ),
      Matched.Name = stringr::str_squish(.data$Matched.Name),
      Orig.Name    = str_to_simple_cap(.data$Orig.Name),
      Comp.Rank    = (.data$Rank == .data$Matched.Rank),
      Match.Level  = dplyr::case_when(
        is.na(.data$Matched.Rank) ~ "No match",
        .data$Rank == .data$Matched.Rank ~ "Exact rank",
        .data$Rank  > .data$Matched.Rank ~ "Matched at higher rank",
        .data$Rank  < .data$Matched.Rank ~ "Matched at lower rank (unexpected)",
        TRUE ~ "Unknown"
      ),
      Threat.Status = dplyr::case_when(
        is.na(.data$Matched.Genus) & is.na(.data$Matched.Species) & is.na(.data$Matched.Infraspecies) ~ "Not threatened",
        !is.na(.data$threat_category) ~ .data$threat_category,
        TRUE ~ "Not threatened"
      ),
      Matched.Name = dplyr::if_else(is.na(.data$Matched.Name), "---", .data$Matched.Name),
      accepted_name_author = dplyr::if_else(is.na(.data$accepted_name_author), "---", .data$accepted_name_author)
    ) |>
    dplyr::relocate("sorter","Orig.Name","Matched.Name","Threat.Status",
                    "Author","accepted_name_author","Matched.Rank",
                    "Comp.Rank","Match.Level")
}

#' @keywords internal
.final_assertions <- function(splist_class, output_f) {
  if (nrow(splist_class) != nrow(output_f)) {
    stop(sprintf("Conteo final (%d) difiere del input (%d).",
                 nrow(output_f), nrow(splist_class)), call. = FALSE)
  }
  if (!all(splist_class$sorter %in% output_f$sorter)) {
    stop("Faltan registros de entrada en la salida.", call. = FALSE)
  }
  if (!all(output_f$sorter == sort(output_f$sorter))) {
    stop("El orden de salida por 'sorter' no es estable.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
.cleanup_infrasp2_if_needed <- function(output_f, use_infraspecies_2) {
  if (!use_infraspecies_2 && any(!is.na(output_f$Matched.Infraspecies_2))) {
    warning("Se limpiará 'Matched.Infraspecies_2' (base sin soporte infraspecies_2).", call. = FALSE)
    output_f$Matched.Infraspecies_2 <- NA_character_
  }
  output_f
}

#' @keywords internal
.attach_metadata <- function(tbl, use_infraspecies_2, source, n_input, n_matched) {
  attr(tbl, "use_infraspecies_2") <- use_infraspecies_2
  attr(tbl, "target_database")    <- source
  attr(tbl, "matching_date")      <- Sys.Date()
  attr(tbl, "n_input")            <- n_input
  attr(tbl, "n_matched")          <- n_matched
  attr(tbl, "match_rate")         <- round(100 * n_matched / max(1, n_input), 2)
  tbl
}
