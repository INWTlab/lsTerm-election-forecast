test_that("getDateIdInTimeseq returns correct IDs", {
  
  # There are 0 weeks between "1970-01-04" and "1970-01-04", so it's the first index
  expect_equal(getDateIdInTimeseq(0:10, "1970-01-04"), 1)
  
  # Floor
  expect_equal(getDateIdInTimeseq(0:10, "1970-01-10"), 1)
  
  expect_equal(getDateIdInTimeseq(0:10, "1970-01-11"), 2)
  
  expect_equal(getDateIdInTimeseq(2000:3000, "2025-02-23"), 878)
})


test_that("getDateIdInTimeseq returns correct IDs", {
  
  # There are 0 weeks between "1970-01-04" and "1970-01-04"
  expect_equal(weeksBetweenOriginAndDate("1970-01-04"), 0)
  
  # Floor
  expect_equal(weeksBetweenOriginAndDate("1970-01-10"), 0)
  
  expect_equal(weeksBetweenOriginAndDate("1970-01-11"), 1)
  
  expect_equal(weeksBetweenOriginAndDate("2025-02-23"), 2877)
})
