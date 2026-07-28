test_that("fit_mk_model returns a list with expected names", {
  data <- tibble::tibble(
    surv_year = 1983:1995,
    value = seq(10, 100, length.out = 13)
  )
  result <- fit_mk_model(data, decade_analysis = FALSE)
  expect_named(
    result,
    c(
      "model",
      "tau",
      "sl",
      "log_slope",
      "intercept",
      "percent_slope",
      "warnings",
      "has_warnings"
    )
  )
})

test_that("fit_mk_model detects a positive trend", {
  data <- tibble::tibble(
    surv_year = 1983:1995,
    value = seq(10, 100, length.out = 13)
  )
  result <- fit_mk_model(data, decade_analysis = FALSE)
  expect_gt(result$tau[1], 0)
})

test_that("fit_mk_model detects a negative trend", {
  data <- tibble::tibble(
    surv_year = 1983:1995,
    value = seq(100, 10, length.out = 13)
  )
  result <- fit_mk_model(data, decade_analysis = FALSE)
  expect_lt(result$tau[1], 0)
})

test_that("fit_mk_model returns NA values with insufficient data", {
  data <- tibble::tibble(surv_year = 1983L, value = 50)
  expect_snapshot(result <- fit_mk_model(data, decade_analysis = FALSE))
  expect_true(is.na(result$tau))
  expect_true(is.na(result$log_slope))
})

test_that("fit_mk_model has_warnings is FALSE for clean data", {
  data <- tibble::tibble(
    surv_year = 1983:1995,
    value = seq(10, 100, length.out = 13)
  )
  result <- fit_mk_model(data, decade_analysis = FALSE)
  expect_false(result$has_warnings)
})

test_that("fit_mk_model works with decade_analysis = TRUE", {
  data <- tibble::tibble(
    surv_year_in_decade = 1:10,
    value = seq(5, 50, length.out = 10)
  )
  result <- fit_mk_model(data, decade_analysis = TRUE)
  expect_named(
    result,
    c(
      "model",
      "tau",
      "sl",
      "log_slope",
      "intercept",
      "percent_slope",
      "warnings",
      "has_warnings"
    )
  )
  expect_gt(result$tau[1], 0)
})

test_that("fit_mk_model ignores NA values in input", {
  data_with_na <- tibble::tibble(
    surv_year = 1983:1992,
    value = c(10, NA, 30, 40, NA, 60, 70, 80, 90, 100)
  )
  data_clean_vals <- tibble::tibble(
    surv_year = c(1983, 1985, 1986, 1988:1992),
    value = c(10, 30, 40, 60, 70, 80, 90, 100)
  )
  result_na <- fit_mk_model(data_with_na, decade_analysis = FALSE)
  result_clean <- fit_mk_model(data_clean_vals, decade_analysis = FALSE)
  expect_equal(result_na$tau, result_clean$tau)
})
