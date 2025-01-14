# This test checks if `fillDateGapsPerEvent` works.

# There are only some date gaps:
test_that(
  "fillDateGapsPerEvent returns correct results if input has some date gaps",
  {
    df_test <- data.frame(
      date_forecast = as.Date(
        c(
          "2024-04-25",
          "2024-04-29",
          "2024-04-30",
          "2024-05-01",
          "2024-05-02",
          "2024-04-26",
          "2024-04-30",
          "2024-05-03"
          )
      ),
      event_id = c(2, 2, 2, 2, 2, 3, 3, 3),
      estimate = c(0.024, 0.027, 0.025, 0.025, 0.025, 0.024, 0.027, 0.026)
    )
   
    expect_equal(
      fillDateGapsPerEvent(df_test)$date_forecast,
      c(
        "2024-04-25",
        "2024-04-26",
        "2024-04-27",
        "2024-04-28",
        "2024-04-29",
        "2024-04-30",
        "2024-05-01",
        "2024-05-02",
        "2024-04-26",
        "2024-04-27",
        "2024-04-28",
        "2024-04-29",
        "2024-04-30",
        "2024-05-01",
        "2024-05-02",
        "2024-05-03"
      )
    )
    expect_equal(
      fillDateGapsPerEvent(df_test)$estimate,
      c(
        0.024,
        0.024,
        0.024,
        0.024,
        0.027,
        0.025,
        0.025,
        0.025,
        0.024,
        0.024,
        0.024,
        0.024,
        0.027,
        0.027,
        0.027,
        0.026
      )
    )
  }
)


# There are some date gaps with mixed up dates:
test_that(
  "fillDateGapsPerEvent returns correct results if input has some date gaps and 
  dates are mixed up",
  {
    df_test <- data.frame(
      date_forecast = as.Date(
        c(
          "2024-04-30",
          "2024-04-25",
          "2024-04-29",
          "2024-05-01",
          "2024-05-02",
          "2024-05-03",
          "2024-04-30",
          "2024-04-26"
        )
      ),
      event_id = c(2, 2, 2, 2, 2, 3, 3, 3),
      estimate = c(0.025, 0.024, 0.027, 0.025, 0.025, 0.026, 0.027, 0.024)
    )
    
    expect_equal(
      fillDateGapsPerEvent(df_test)$date_forecast,
      c(
        "2024-04-25",
        "2024-04-26",
        "2024-04-27",
        "2024-04-28",
        "2024-04-29",
        "2024-04-30",
        "2024-05-01",
        "2024-05-02",
        "2024-04-26",
        "2024-04-27",
        "2024-04-28",
        "2024-04-29",
        "2024-04-30",
        "2024-05-01",
        "2024-05-02",
        "2024-05-03"
      )
    )
    
    expect_equal(
      fillDateGapsPerEvent(df_test)$estimate,
      c(
        0.024,
        0.024,
        0.024,
        0.024,
        0.027,
        0.025,
        0.025,
        0.025,
        0.024,
        0.024,
        0.024,
        0.024,
        0.027,
        0.027,
        0.027,
        0.026
      )
    )
  }
)


# This dataframe is as expected
test_that(
  "fillDateGapsPerEvent returns correct results if input has no date gaps at all",
  {
    df_test <- data.frame(
      date_forecast = as.Date(
        c(
          "2024-04-25",
          "2024-04-26",
          "2024-04-27",
          "2024-04-28",
          "2024-04-29",
          "2024-04-26",
          "2024-04-27"
        )
      ),
      event_id = c(2, 2, 2, 2, 2, 3, 3),
      estimate = c(0.024, 0.027, 0.025, 0.025, 0.025, 0.028, 0.021)
    )
    
    expect_equal(
      fillDateGapsPerEvent(df_test)$date_forecast,
      c(
        "2024-04-25",
        "2024-04-26",
        "2024-04-27",
        "2024-04-28",
        "2024-04-29",
        "2024-04-26",
        "2024-04-27"
      )
    )
    
    expect_equal(
      fillDateGapsPerEvent(df_test)$estimate,
      c(
        0.024,
        0.027,
        0.025,
        0.025,
        0.025,
        0.028,
        0.021
      )
    )
  }
)
