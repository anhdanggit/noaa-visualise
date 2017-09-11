library(testthat)

# Testing eq_location_clean

test_that("the input is not a dataframe",
  {expect_that(eq_location_clean("abc"), throws_error())
    })

# Testing eq_clean_data

test_that("The output has date column",
          {data ("raw_data")
           expect_that(eq_clean_data(raw_data)$date, is_a("Date"))
          })

test_that("The output has numeric LONGITUDE",
          {data ("raw_data")
            expect_that(eq_clean_data(raw_data)$LONGITUDE, is_a("numeric"))
          })

test_that("The output has numeric LATITUDE",
          {data ("raw_data")
            expect_that(eq_clean_data(raw_data)$LATITUDE, is_a("numeric"))
          })

test_that("The output is data.frame",
          {data ("raw_data")
          expect_that(eq_clean_data(raw_data), is_a("data.frame"))
          })
