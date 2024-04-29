test_that("preparePollData runs without errors", {
  load(system.file("testdata", "dataDE.RData", package = "lsTermElectionForecast"))
  # This RData file contains the output of getDataDE()
  # It can be updated using:
  # dataDE <- getDataDE(Sys.Date())
  # save(dataDE, file = "inst/testdata/dataDE.RData")
  expect_no_error(
    preparePollData(
      pollData = dataDE$pollData,
      elections = dataDE$Elections,
      predDate = Sys.Date()
    )
  )
})
