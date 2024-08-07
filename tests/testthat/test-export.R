test_that("Read and Write Triple Omics", {
  if (dir.exists("/tmp")) {
    export_tomic_as_triple(brauer_2008_triple, "/tmp", "brauer", verbose = FALSE)

    features_path <- file.path("/tmp", "brauer_features.tsv")
    measurements_path <- file.path("/tmp", "brauer_measurements.tsv")
    samples_path <- file.path("/tmp", "brauer_samples.tsv")

    features <- readr::read_tsv(features_path, show_col_types = FALSE)
    measurements <- readr::read_tsv(measurements_path, show_col_types = FALSE)
    samples <- readr::read_tsv(samples_path, show_col_types = FALSE)

    # cleanup
    unlink(features_path)
    stopifnot(!file.exists(features_path))
    unlink(measurements_path)
    stopifnot(!file.exists(measurements_path))
    unlink(samples_path)
    stopifnot(!file.exists(samples_path))

    tomic <- romic::create_triple_omic(
      measurement_df = measurements,
      feature_df = features,
      sample_df = samples,
      feature_pk = "name",
      sample_pk = "sample"
    )

    expect_equal(
      tomic$features %>%
        dplyr::mutate(
          MF = stringr::str_replace_na(MF, ""),
          BP = stringr::str_replace_na(BP, "")
        ),
      brauer_2008_triple$features
    )

    expect_equal(
      tomic$samples,
      brauer_2008_triple$samples
    )

    expect_equal(
      tomic$measurements,
      brauer_2008_triple$measurements
    )
  }
})

test_that("Read and Write Tidy Omics", {
  if (dir.exists("/tmp")) {
    export_tomic_as_tidy(brauer_2008_tidy, "/tmp", "brauer", verbose = FALSE)

    tidy_path <- file.path("/tmp", "brauer_tidy.tsv")
    tidy_data <- readr::read_tsv(tidy_path, show_col_types = FALSE)

    # cleanup
    unlink(tidy_path)
    stopifnot(!file.exists(tidy_path))

    tomic <- romic::create_tidy_omic(
      df = tidy_data,
      feature_pk = "name",
      feature_vars = c("BP", "MF", "systematic_name"),
      sample_pk = "sample",
      sample_vars = c("nutrient", "DR"),
      verbose = FALSE
    )

    tomic_fill_nas <- tomic$data %>%
      dplyr::mutate(
        MF = stringr::str_replace_na(MF, ""),
        BP = stringr::str_replace_na(BP, "")
      )

    expect_true(
      all(tomic_fill_nas == brauer_2008_tidy$data, na.rm = TRUE)
    )
  }
})

test_that("Read and Write Wide Data", {
  if (dir.exists("/tmp")) {
    export_tomic_as_wide(brauer_2008_triple, "/tmp", "brauer", verbose = FALSE)

    wide_path <- file.path("/tmp", "brauer_wide.tsv")
    # the wide data importer is currently incomplete and doesn't
    # support sample attributes
    wide_data <- suppressMessages(suppressWarnings(readr::read_tsv(
      wide_path,
      show_col_types = FALSE
      )))

    expect_s3_class(wide_data, "tbl")

    # cleanup
    unlink(wide_path)
    stopifnot(!file.exists(wide_path))
  }
})

test_that("Format results as a matrix", {
  expect_equal(dim(tomic_to_matrix(brauer_2008_triple)), c(500, 36))
})
