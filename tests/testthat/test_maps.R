library(testthat)

test_that("The output is leaflet",
          {data("raw_data")
            data <- eq_clean_data(raw_data)
            input_data <- dplyr::filter(data, COUNTRY == "MEXICO" &
                                          lubridate::year(date) >= 2000)
            leaf <- eq_map(input_data, annot_col = "date")
            expect_that(leaf, is_a("leaflet"))
          })


