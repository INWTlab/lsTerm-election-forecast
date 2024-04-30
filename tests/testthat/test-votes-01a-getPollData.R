test_that("getPollData runs without errors", {
  expect_no_error(getPollData())
})

test_that("getPollData's result has roughly the correct format", {
  res <- getPollData()
  expect_type(res, "list")
  expect_true(nrow(res) > 5000)
  expect_length(res, 11) # number of columns
  expect_named(
    res,
    c(
      "Institut",
      "Datum",
      "CDU/CSU",
      "SPD",
      "GRÜNE",
      "FDP",
      "LINKE",
      "AfD",
      "BSW",
      "Befragte",
      "Sonstige"
    )
  )
})


test_that("asNumericWithNA returns correct results", {
  expect_equal(asNumericWithNA(c("10", "0", NA)), c(10, 0, NA))
  expect_equal(asNumericWithNA(c("10", "0")), c(10, 0))
  expect_equal(asNumericWithNA(c(NA, NA)), c(NA_real_, NA_real_))
  expect_no_warning(asNumericWithNA(c("9", NA)))
})
