library(testthat)

# Testing eq_location_clean

test_that("the input is not a dataframe",
          {expect_that(eq_location_clean("abc"), throws_error())
          })
