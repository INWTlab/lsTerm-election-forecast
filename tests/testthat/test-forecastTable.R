test_that("Event probabilites calculated correctly given model results", {
  # This test compares the probabilities with given values.
  # It ensures that the results don't change in an unintended way during refactoring.
  
  # How to prepare the test data:
  # predDate <- as.Date("2024-05-03")
  # dataDE <- getDataDE(predDate)
  # fact_survey <- getFactSurvey(dataDE$pollData)
  # dataPrep <-
  #   preparePollData(dataDE$pollData,
  #                   dataDE$Elections,
  #                   predDate,
  #                   minDate = predDate - 365 * 20.1)
  # modelResults <-
  #   compileRunModel(data = dataPrep$modelData, dev = TRUE)
  # save(modelResults, dataPrep, predDate, file = "inst/testdata/modelResults.RData")
  load(
    system.file("testdata", "modelResults.RData", package = "lsTermElectionForecast")
  )
  eventProbs <- eventsDE(modelResults, dataPrep, predDate)
  expect_true(all(eventProbs$date_forecast == as.Date("2024-05-03")))
  expect_equal(
    eventProbs$event_id,
    c(3, 5, 6, 7, 8, 10, 17, 20, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)
  )
  expect_equal(
    eventProbs$estimate,
    c(
      0.06875, # ID 3
      0.225, # ID 5
      0.00625, # ID 6
      0.55625, # ID 7
      0.81875, # ID 8
      0, # ID 10
      0.10625, # ID 17
      0.2375, # ID 20
      0.29375, # ID 23
      0.04375, # ID 24
      0.0625, # ID 25
      0.6875, # ID 26
      0.56875, # ID 27
      1.000, # ID 28
      0.73125, # ID 29
      0.9375, # ID 30
      0.79375, # ID 31
      0.875 # ID 32
    )
  )
})
