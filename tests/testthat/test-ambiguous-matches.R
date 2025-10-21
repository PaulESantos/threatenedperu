# =============================================================================
# Tests for Ambiguous Match Handling
# =============================================================================

test_that("get_ambiguous_matches returns NULL when no ambiguous matches", {
  skip_on_cran()

  # Use species that match unambiguously
  species_list <- c("Cattleya maxima", "Polylepis incana")
  result <- is_threatened_peru(species_list, return_details = TRUE)

  # Should return NULL and message
  expect_message(
    ambig <- get_ambiguous_matches(result, type = "genus"),
    "No ambiguous.*found"
  )
  expect_null(ambig)
})

test_that("get_ambiguous_matches retrieves genus-level ambiguities", {
  skip_on_cran()
  skip_if_not_installed("threatenedperu")

  # This test assumes there might be ambiguous genus matches
  # Adjust species names based on actual database content

  # Create test data that would cause ambiguous matches
  # (This might need adjustment based on actual database)
  test_species <- c("Catleya maxima")  # Typo might match multiple genera

  result <- suppressWarnings(
    is_threatened_peru(test_species, return_details = TRUE)
  )

  # Try to get ambiguous matches
  ambig <- get_ambiguous_matches(result, type = "genus")

  # If ambiguous matches exist, check structure
  if (!is.null(ambig)) {
    expect_s3_class(ambig, "data.frame")
    expect_true("Match_Type" %in% names(ambig))
    expect_true(all(ambig$Match_Type == "Genus"))
  }
})

test_that("get_ambiguous_matches does not write files by default", {
  skip_on_cran()

  species_list <- c("Cattleya maxima")
  result <- suppressWarnings(
    is_threatened_peru(species_list, return_details = TRUE)
  )

  # Create temporary directory for test
  temp_dir <- withr::local_tempdir()
  withr::local_dir(temp_dir)

  # Call function without save_to_file
  get_ambiguous_matches(result, type = "genus", save_to_file = FALSE)

  # Verify no CSV files were created
  csv_files <- list.files(pattern = "\\.csv$")
  expect_length(csv_files, 0)
})

test_that("get_ambiguous_matches saves file when requested", {
  skip_on_cran()

  species_list <- c("Catleya maxima")  # Intentional typo

  result <- suppressWarnings(
    is_threatened_peru(species_list, return_details = TRUE)
  )

  # Only test file saving if ambiguous matches exist
  ambig_check <- get_ambiguous_matches(result, type = "genus")

  if (!is.null(ambig_check)) {
    temp_dir <- tempdir()

    # Save to file
    ambig <- get_ambiguous_matches(
      result,
      type = "genus",
      save_to_file = TRUE,
      output_dir = temp_dir
    )

    # Check file was created
    csv_files <- list.files(temp_dir, pattern = "threatenedperu_ambiguous_genus.*\\.csv$")
    expect_true(length(csv_files) > 0)

    # Cleanup
    unlink(file.path(temp_dir, csv_files))
  } else {
    skip("No ambiguous matches in test data")
  }
})

test_that("get_ambiguous_matches validates input", {
  # Test with invalid input
  expect_error(
    get_ambiguous_matches("not a dataframe"),
    "must be a data frame"
  )

  expect_error(
    get_ambiguous_matches(list(a = 1, b = 2)),
    "must be a data frame"
  )
})

test_that("get_ambiguous_matches handles invalid type parameter", {
  species_list <- c("Cattleya maxima")
  result <- is_threatened_peru(species_list, return_details = TRUE)

  # Invalid type should error via match.arg
  expect_error(
    get_ambiguous_matches(result, type = "invalid"),
    "should be one of"
  )
})

test_that("get_ambiguous_matches returns all types when type = 'all'", {
  skip_on_cran()

  # Create test with potential ambiguities at multiple levels
  species_list <- c(
    "Catleya maxima",           # Genus ambiguity
    "Cattleya maxma"            # Species ambiguity
  )

  result <- suppressWarnings(
    is_threatened_peru(species_list, return_details = TRUE)
  )

  # Get all ambiguous matches
  ambig_all <- get_ambiguous_matches(result, type = "all")

  if (!is.null(ambig_all)) {
    expect_s3_class(ambig_all, "data.frame")
    expect_true("Match_Type" %in% names(ambig_all))

    # Check that Match_Type contains appropriate values
    valid_types <- c("Genus", "Species", "Infraspecies")
    expect_true(all(ambig_all$Match_Type %in% valid_types))
  }
})

test_that("Ambiguous match attributes are preserved through pipeline", {
  skip_on_cran()

  # This tests that attributes survive the matching pipeline
  species_with_typo <- c("Catleya maxima")

  result <- suppressWarnings(
    matching_threatenedperu(species_with_typo, source = "original")
  )

  # Check if ambiguous_genera attribute exists
  # (only if there were actually ambiguous matches)
  if (!is.null(attr(result, "ambiguous_genera"))) {
    ambig_attr <- attr(result, "ambiguous_genera")
    expect_s3_class(ambig_attr, "data.frame")
    expect_true(nrow(ambig_attr) > 0)
  }
})

test_that("get_ambiguous_matches handles missing output directory", {
  skip_on_cran()

  species_list <- c("Cattleya maxima")
  result <- is_threatened_peru(species_list, return_details = TRUE)

  # Non-existent directory should be created
  temp_base <- tempdir()
  new_dir <- file.path(temp_base, "new_test_dir", "nested")

  # Ensure directory doesn't exist
  if (dir.exists(new_dir)) {
    unlink(new_dir, recursive = TRUE)
  }

  # Should create directory and not error
  expect_no_error({
    get_ambiguous_matches(
      result,
      type = "genus",
      save_to_file = TRUE,
      output_dir = new_dir
    )
  })

  # Cleanup
  if (dir.exists(new_dir)) {
    unlink(new_dir, recursive = TRUE)
  }
})

test_that("Timestamp format in filename is correct", {
  skip_on_cran()

  species_list <- c("Catleya maxima")
  result <- suppressWarnings(
    is_threatened_peru(species_list, return_details = TRUE)
  )

  ambig_check <- get_ambiguous_matches(result, type = "genus")

  if (!is.null(ambig_check)) {
    temp_dir <- tempdir()

    # Save file
    get_ambiguous_matches(
      result,
      type = "genus",
      save_to_file = TRUE,
      output_dir = temp_dir
    )

    # Check filename format
    csv_files <- list.files(temp_dir, pattern = "threatenedperu_ambiguous_genus_.*\\.csv$")

    if (length(csv_files) > 0) {
      # Extract timestamp from filename
      # Format should be: threatenedperu_ambiguous_genus_YYYYMMDD_HHMMSS.csv
      timestamp_pattern <- "\\d{8}_\\d{6}"
      expect_true(any(grepl(timestamp_pattern, csv_files)))

      # Cleanup
      unlink(file.path(temp_dir, csv_files))
    }
  } else {
    skip("No ambiguous matches in test data")
  }
})
