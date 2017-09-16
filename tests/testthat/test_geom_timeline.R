library(testthat)

# Testing eq_country_filter and also test geom_timeline

test_that("The output is a ggplot",
          {data("raw_data")
            data <- eq_clean_data(raw_data)
            input_data <- eq_country_filter(data, countries = c("MEXICO", "IRAN"),
                                            xmin = as.Date("1000-01-01"),
                                            xmax = as.Date("2010-01-01"))

            plot <- ggplot(input_data, aes (x = date, y = COUNTRY, color = as.numeric(DEATHS),
                                            size = as.numeric(EQ_PRIMARY))) +
              geom_timeline(aes(xmin = as.Date("1900-01-01"), xmax = as.Date("1925-01-01"))) +
              labs(size = "Richter Scale Values", color = "Number of Deaths" )+
              theme_timeline()

            expect_that(plot, is_a("gg"))
          })
