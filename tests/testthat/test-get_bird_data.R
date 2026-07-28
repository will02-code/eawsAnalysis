test_that("get_bird_data returns a data frame with surv_year and value columns", {
  result <- get_bird_data(
    dataset = data_clean,
    programs = "eaws",
    wetlands = NULL,
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = NULL,
    metric = "abundance"
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("surv_year", "value") %in% names(result)))
})

test_that("get_bird_data abundance values are non-negative", {
  result <- get_bird_data(
    dataset = data_clean,
    programs = "eaws",
    wetlands = NULL,
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = NULL,
    metric = "abundance"
  )
  expect_true(all(result$value >= 0, na.rm = TRUE))
})

test_that("get_bird_data richness values are non-negative integers", {
  result <- get_bird_data(
    dataset = data_clean,
    programs = "eaws",
    wetlands = NULL,
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = NULL,
    metric = "richness"
  )
  expect_true(all(result$value >= 0, na.rm = TRUE))
  expect_true(all(result$value == floor(result$value), na.rm = TRUE))
})

test_that("get_bird_data grouping_cols appears in output", {
  result <- get_bird_data(
    dataset = data_clean,
    programs = "eaws",
    wetlands = NULL,
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = "Wetland",
    metric = "abundance"
  )
  expect_true("Wetland" %in% names(result))
})

test_that("get_bird_data stops on invalid program", {
  expect_snapshot(
    error = TRUE,
    get_bird_data(
      dataset = data_clean,
      programs = "not_a_program",
      wetlands = NULL,
      valleys = NULL,
      basinDiv = NULL,
      grouping_cols = NULL,
      metric = "abundance"
    )
  )
})

test_that("get_bird_data stops on invalid metric", {
  expect_snapshot(
    error = TRUE,
    get_bird_data(
      dataset = data_clean,
      programs = "eaws",
      wetlands = NULL,
      valleys = NULL,
      basinDiv = NULL,
      grouping_cols = NULL,
      metric = "not_a_metric"
    )
  )
})

test_that("get_bird_data stops on invalid wetland", {
  expect_snapshot(
    error = TRUE,
    get_bird_data(
      dataset = data_clean,
      programs = "eaws",
      wetlands = "not_a_wetland",
      valleys = NULL,
      basinDiv = NULL,
      grouping_cols = NULL,
      metric = "abundance"
    )
  )
})

test_that("get_bird_data wetland filter reduces rows vs no filter", {
  result_all <- get_bird_data(
    dataset = data_clean,
    programs = "mdb combined",
    wetlands = NULL,
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = "Wetland",
    metric = "abundance"
  )
  result_one <- get_bird_data(
    dataset = data_clean,
    programs = "mdb combined",
    wetlands = "barmah-millewa",
    valleys = NULL,
    basinDiv = NULL,
    grouping_cols = "Wetland",
    metric = "abundance"
  )
  expect_lt(nrow(result_one), nrow(result_all))
})
