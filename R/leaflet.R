#' @name eq_map
#' @title Mapping Tools for the NOAA Earthquake Dataset
#'
#' @description  Function \code{eq_map()} takes an argument data containing the filtered
#' data frame with earthquakes to visualise in the geographical maps bu \code{LONGITUDE}
#' and \code{LATITUDE}, with pop-up windows to provide the further information.
#'
#' @param data A filtered data frame with earthquakes to visualize
#' @param annot_col The name of column is used for the annotation
#'
#' @examples \dontrun{
#' eq_country_filter(data, c("INDIA"),
#' xmin = as.Date("1850-01-01"),
#' xmax = as.Date("1920-01-01")) %>%
#' eq_map(annot_col = "date")}
#'
#' @import leaflet
#' @export
eq_map <- function(data, annot_col){
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addProviderTiles("Esri.WorldStreetMap")%>%
    leaflet::addCircleMarkers(data = data,
                     radius = ~ EQ_PRIMARY,
                     lng = ~ LONGITUDE,
                     lat = ~ LATITUDE,
                     weight = 2,
                     popup = data[[annot_col]])

}


#' @title Creating the pop-up annotation
#' @description Function \code{eq_create_label()} takes the dataset as an argument and creates an HTML label
#' that can be used as the annotation text in the leaflet map.
#'
#' @param data The inputed data frame
#'
#' @examples \dontrun{
#' eq_country_filter(data, c("INDIA"),
#' xmin = as.Date("1850-01-01"),
#' xmax = as.Date("1920-01-01"))%>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
#' }
#' @export
eq_create_label <- function(data){
  data <- eq_location_clean(data)
  out1 <- ifelse (!is.na(data$LOCATION_NAME),
                  paste0("<b>Location: </b>", data$LOCATION_NAME, "<br/>"),
                  paste0(""))

  out2 <- ifelse (!is.na(data$EQ_PRIMARY),
                  paste0("<b>Magnitude: </b>", data$EQ_PRIMARY, "<br/>"),
                  paste0(""))

  out3 <- ifelse(!is.na(data$TOTAL_DEATHS),
                 paste0("<b>Total deaths: </b>", data$TOTAL_DEATHS),
                 paste0(""))
  paste0(out1, out2, out3)
}



