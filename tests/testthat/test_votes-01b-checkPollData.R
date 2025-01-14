test_that("stopIfNewPollster is happy if there are no new pollsters", {
  expect_no_error(stopIfNewPollster(
    pollstersWeb = c("A", "B", "C"),
    pollstersDb = c("B", "A", "C")
  ))
  
  # Additional pollsters in the database have no effect:
  expect_no_error(stopIfNewPollster(
    pollstersWeb = c("A", "B", "C"),
    pollstersDb = c("B", "A", "C", "D")
  ))
  
})


test_that("stopIfNewPollster detects new pollsters", {
  expect_error(
    stopIfNewPollster(
      pollstersWeb = c("A", "B", "C", "I_am_new"),
      pollstersDb = c("B", "A", "C", "D")
    ),
    regexp = paste0(
      "There are new pollsters in the data: I_am_new\n",
      "Please check if an institute has been renamed or a new institute has been added."
    )
  )
  
})


test_that("checkForNewPolls returns FALSE if there are no new polls", {
  latest <- data.frame(Institut = c("pollster A", "pollster B"),
                       latestPoll = as.Date(c("2022-01-01", "2022-12-12")))
  latestDb <- data.frame(Institut = c("pollster B", "pollster A"),
                         latestPollDb = as.Date(c("2022-12-12", "2022-01-01")))
  expect_false(checkForNewPolls(latest, latestDb))
})


test_that("checkForNewPolls detects new polls", {
  latest <- data.frame(Institut = c("pollster A", "pollster B"),
                       latestPoll = as.Date(c("2022-01-01", "2022-12-24")))
  latestDb <- data.frame(Institut = c("pollster B", "pollster A"),
                         latestPollDb = as.Date(c("2022-12-12", "2022-01-01")))
  expect_true(checkForNewPolls(latest, latestDb))
})
