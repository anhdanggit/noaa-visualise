library(testthat)

## Testing the eq_create_label
test_that("Correct format", {

  ## Data Setup

  test_dataframe = data.frame(LOCATION_NAME = c("Bab-A-Daraa,Al-Karak", "Ugarit",
                                                "Thera Island (Santorini)"),
                              COUNTRY = c("JORDAN","SYRIA","GREECE"),
                              YEAR = c(2000, 2000, 2000),
                              MONTH = c(NA,1,2),
                              DAY = c(NA,NA,3),
                              LATITUDE = c(31.1,35.6,36.4),
                              LONGITUDE = c(35.5,35.3,35.5),
                              DEATHS = c(NA,1,2),
                              TOTAL_DEATHS = c(NA,1,NA),
                              EQ_PRIMARY = c("3", "4","5"),
                              stringsAsFactors = F)

  annotations = c("<b>Location: </b>Bab-A-Daraa,Al-Karak<br/><b>Magnitude: </b>3<br/>",
                  "<b>Location: </b>Ugarit<br/><b>Magnitude: </b>4<br/><b>Total deaths: </b>1",
                  "<b>Location: </b>Thera Island (Santorini)<br/><b>Magnitude: </b>5<br/>")

  ## Function Call

  m = test_dataframe
  m$popup_text = eq_create_label(m)

  ## Testing

  expect_identical(m$popup_text, annotations)
})
