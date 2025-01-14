test_that("Package code is in line with our style conventions", {
  linters <- INWTUtils::selectLinters(excludeLinters = "sapply_linter")
  lintr::expect_lint_free(linters = linters)
})
