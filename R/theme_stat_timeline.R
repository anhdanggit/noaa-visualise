#' @title Stat of geom_timeline
#' @description \code{stat_timeline()} is used together with \code{geom_timeline}
#' and \code{geom_timeline_label}. It takes the aes the \code{xmin} and \code{xmax}.
#' to return the dataset within this range.
#'
#' @param xmin The minimum date in the range of timeline
#' @param xmax The maximum date in the range of timeline
#' @param x The date of earthquakes
#' @inheritParams ggplot2::geom_point
#'
#' @note The default of \code{xmin} and \code{xmax} are set as the min and max of
#' the whole timeline.
#' @import dplyr
#' @import ggplot2
#'
#' @export
stat_timeline <- function(mapping = NULL, data = NULL, geom = "timeline",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE,
                          n_max = NULL,
                          ...) {
  ggplot2::layer(
    stat = StatTimeline,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}

StatTimeline <- ggplot2::ggproto("StatTimeline", ggplot2::Stat,
                                 compute_group = function(data, scales){
                                   data %>%
                                     dplyr::filter(x > xmin) %>%
                                     dplyr::filter(x < xmax)
                                 } ,
                                 default_aes = ggplot2::aes(xmin = min(data$x),
                                                            xmax = max(data$x)),
                                 required_aes = c("x"))


#' @title Theme of Timeline
#' @description \code{theme_timeline()} is used with \code{geom_timeline()}
#' and \code{geom_timeline_label()}
#'
#' @note The background and y axis is set as blank, when only the line axis x
#' is presented
#' @import ggplot2
#'
#' @examples \dontrun{
#'
#' input_data <- eq_country_filter(data, c("INDIA", "IRAN"),
#' xmin = as.Date("0-01-01"),
#' xmax = as.Date("2010-01-01"))
#'
#' ggplot(input_data,
#' aes (x = date, y = COUNTRY, color = as.numeric(DEATHS),
#' size = as.numeric(EQ_PRIMARY))) +
#' geom_timeline(aes(xmin = as.Date("1800-01-01"), xmax = as.Date("2000-01-01"))) +
#' geom_timeline_label(aes(label = LOCATION_NAME,
#'                        xmin = as.Date("1800-01-01"), xmax = as.Date("2000-01-01")),
#'                                           n_max = 5) +
#'                                           labs(size = "Richter Scale Values", color = "Number of Deaths" ) +
#'                                           theme_timeline()}
#' @export
theme_timeline <- function(){
  ggplot2::theme(title = element_text(),
                 legend.position = "bottom",
                 legend.direction = "horizontal",
                 panel.background = ggplot2::element_blank(),
                 plot.background = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.line.y = ggplot2::element_blank(),
                 axis.line.x = ggplot2::element_line(color = "black", size = 1),
                 legend.key = ggplot2::element_blank()
  )}








