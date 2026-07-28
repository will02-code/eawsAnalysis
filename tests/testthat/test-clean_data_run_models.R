test_that("clean_data_run_models returns a list of 3 elements", {
  result <- clean_data_run_models(
    dataset = data_clean,
    programs = "eaws",
    metric = "abundance",
    wetlands = NULL,
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = NULL
  )
  expect_length(result, 3)
})

test_that("clean_data_run_models first element is a data frame", {
  result <- clean_data_run_models(
    dataset = data_clean,
    programs = "eaws",
    metric = "abundance",
    wetlands = NULL,
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = NULL
  )
  expect_s3_class(result[[1]], "data.frame")
})

test_that("clean_data_run_models second element is a ggplot", {
  result <- clean_data_run_models(
    dataset = data_clean,
    programs = "eaws",
    metric = "abundance",
    wetlands = NULL,
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = NULL
  )
  expect_s3_class(result[[2]], "ggplot")
})

test_that("clean_data_run_models summary contains mk columns", {
  result <- clean_data_run_models(
    dataset = data_clean,
    programs = "eaws",
    metric = "abundance",
    wetlands = NULL,
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = NULL
  )
  summary <- result[[3]]
  expect_true(all(
    c("mk_tau", "mk_p_value", "long_term_avg") %in% names(summary)
  ))
})

test_that("clean_data_run_models grouping_cols appear in cleaned output", {
  result <- clean_data_run_models(
    dataset = data_clean,
    programs = "eaws",
    metric = "abundance",
    wetlands = NULL,
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = "Wetland"
  )
  expect_true("Wetland" %in% names(result[[1]]))
  expect_true("Wetland" %in% names(result[[3]]))
})
