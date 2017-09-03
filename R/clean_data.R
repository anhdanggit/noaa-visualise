#' @name eq_clean_data
#' @title Cleaning the NOAA Earthquake Dataset
#'
#' @description \code{eq_clean_data()} takes the raw NOAA data frame and returns a clean data frame,
#' which would be well-behaved with visualise functions
#' in this package.
#'
#' @param raw_data A raw data frame obtained from NOAA
#' @return A cleaned data frame with \code{date} is created by uniting \code{YEAR},
#' \code{MONTH}, \code{DAY}, \code{LONGITUDE} and \code{LATITUDE} are converted to numeric
#'
#' @note The dataset is included in the package and could be called by
#' \code{data(raw_data)}. Observations with \code{NA} \code{DAY} or/and \code{MONTH}
#' would take the value of \code{01}. The dataset includes dates BEC (with negative years).
#'
#' @import dplyr
#' @import magrittr
#'
#' @examples \dontrun{use("raw_data")
#' data <- eq_clean_data(raw_data)}
#'
#' @export

eq_clean_data <- function(raw_data){

  raw_data$MONTH[is.na(raw_data$MONTH)] <- "01" ## replace NA by 01
  raw_data$DAY[is.na(raw_data$DAY)] <- "01"

  clean_data <- raw_data %>%

    ### use ISOdate to get the years <1000
    dplyr::mutate(date = as.Date(ISOdate(year = raw_data$YEAR, month = raw_data$MONTH,
                                         day = raw_data$DAY))) %>%

    ### create the mirror date for negative years
    dplyr::mutate(mirror_date = as.Date(ISOdate(year = -raw_data$YEAR, month = raw_data$MONTH,
                                                day = raw_data$DAY))) %>%

    ### create the as.numeric() - number of days from the origin (try to catch leap year)
    dplyr::mutate(date_numeric = as.numeric(difftime("0000-01-01", mirror_date)))

  ### replace the NA date by the converted date (from date_numeric)
  clean_data$date[is.na(clean_data$date)] <- as.Date(clean_data$date_numeric[is.na(clean_data$date)],
                                                     origin = "0000-01-01")

  clean_data <- clean_data %>%
    dplyr::select(-mirror_date, -date_numeric) %>% ## drop unecessary columns

    ### change type of lon and lat

    dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE))
  clean_data
}

#' @title Cleaning the LOCATION_NAME
#'
#' @description \code{eq_location_clean()} cleans \code{LOCATION_NAME} column in the raw NOAA data frame
#' by returning only the names of locations without \code{COUNTRY} (avoid redundant) in
#' the formate of title case, which could be used as pop-up text in leaflet.
#'
#' @param raw_data A data frame with uncleaned \code{LOCATION_NAME}
#'
#' @return A data frame with cleaned \code{LOCATION_NAME}
#'
#' @examples \dontrun{data <- eq_location_clean(eq_clean_data(raw_data))}
#'
#' @importFrom dplyr mutate %>%
#' @importFrom stringr str_replace str_trim str_to_title
#' @export
eq_location_clean <- function(raw_data){

  ### change the LOCATION_NAME
  clean_data <- raw_data%>%
    dplyr::mutate_(LOCATION_NAME = ~LOCATION_NAME %>%
                     stringr:: str_replace(pattern = paste0(COUNTRY, ":"),
                                           replacement = "") %>%
                     stringr:: str_trim(side = "both") %>%
                     stringr:: str_to_title())
  clean_data
}

#' @title Filter the NOAA dataset by countries and time range
#' @description \code{eq_country_filter()} returns
#' the data specifically for called \code{countries},
#' in the timeline from \code{xmin} to \code{xmax}
#'
#' @param data A data frame with relevant columns
#' @param countries A character vectors with the names of considered countries
#' @param xmin The beginning of considered timeline, as a date object
#' @param xmax The ending of considered timeline, as a date object
#'
#' @note The \code{xmin} and \code{xmax} must be date object
#' (see: \code{lubridate::ymd()})
#' @return A subset of the inputed data frame
#'
#' @importFrom dplyr filter %>%
#' @importFrom lubridate ymd
#' @examples \dontrun{input_data <- eq_country_filter(data,
#' c("INDIA", "THAILAND"),
#' xmin = as.Date("1800-01-01"),
#' xmax = as.Date("2010-01-01"))}
#' @export

eq_country_filter <- function(data, countries, xmin, xmax) {
  out <- data %>%
    dplyr::filter(COUNTRY %in% countries) %>%
    dplyr::filter(date < xmax) %>%
    dplyr::filter(date > xmin)
  out}

