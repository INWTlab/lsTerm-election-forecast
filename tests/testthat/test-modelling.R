test_that("compileRunModel runs without errors", {
  load(system.file("testdata", "dataPrep.RData", package = "lsTermElectionForecast"))
  
  # This RData file contains the output of preparePollData()
  # It can be updated using:
  # dataDE <- getDataDE(Sys.Date())
  # fact_survey <- getFactSurvey(dataDE$pollData)
  # dataPrep <-
  #   preparePollData(dataDE$pollData,
  #                   dataDE$Elections,
  #                   predDate,
  #                   minDate = predDate - 365 * 20.1)
  # save(dataPrep, file = "inst/testdata/dataPrep.RData")
  
  expect_no_error(compileRunModel(data = dataPrep$modelData, dev = TRUE))
})
