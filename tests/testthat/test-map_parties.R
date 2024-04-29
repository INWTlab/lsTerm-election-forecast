# these tests check if `mapPartyNamesToNumbers()` works

test_that(
  "mapPartyNamesToNumbers returns correct results if input contains all parties,
  some of them multiple times",
  {
    expect_equal(mapPartyNamesToNumbers(
      c("SPD",
        "GR\u00dcNE",
        "CDU/CSU",
        "FDP",
        "LINKE",
        "AfD",
        "SPD",
        "LINKE",
        "BSW"
      )
    ),
    c(2, 4, 1, 6, 5, 3, 2, 5, 8))
  }
)

test_that(
  "mapPartyNamesToNumbers returns correct results if input contains only some parties",
  {
    expect_equal(mapPartyNamesToNumbers(
      c("CDU/CSU",
        "LINKE",
        "SPD"
      )
    ),
    c(1, 5, 2))
  }
)

test_that(
  "mapPartyNamesToNumbers returns a zero vector if input is an empty vector",
  {
    expect_equal(mapPartyNamesToNumbers(
      c(
      )
    ),
    numeric(
      0
    ))
  }
)

# these tests check if there is an error in `mapsPartyNamesToNumbers()`

test_that(
  "mapPartyNamesToNumbers throws an error if the input contains an unknown party", {
    expect_error(
      mapPartyNamesToNumbers(c("SPD", "INWT")),
      regexp = "Unknown party\\(ies\\) found:.*Please add mapping in mapPartyNamesToNumbers")
  }
  )

test_that(
  "mapPartyNamesToNumbers throws an error if the input contains at least one unknown party", {
    expect_error(
      mapPartyNamesToNumbers(c("SPD", "INWT", "INWT")),
      regexp = "Unknown party\\(ies\\) found:.*Please add mapping in mapPartyNamesToNumbers")
  }
)

test_that(
  "mapPartyNamesToNumbers throws an error if the input contains at least one unknown party", {
    expect_error(
      mapPartyNamesToNumbers(c("SPD", "INWT1", "INWT2")),
      regexp = "Unknown party\\(ies\\) found:.*Please add mapping in mapPartyNamesToNumbers")
  }
)
