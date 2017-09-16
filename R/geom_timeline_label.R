#' @title Add Labels to Timeline
#'
#' @description \code{geom_timeline_label()} is used together with \code{geom_timeline}
#' for adding the annotations to the timeline. The subset is taken
#' the \code{n_max} number of earthquakes. Aesthetics are x, as the date of
#' earthquakes and \code{label} would be taken from the column name.
#'
#' @inheritParams  stat_timeline
#' @param n_max A integer which indicates the number of the largest earthquakes to have
#' the label
#' @param stat The default set is \code{"timeline"}
#'
#' @note The aes \code{xmin} and \code{xmax} as the starting and ending
#' points of the timeline, which should be clarified.
#' @note The aes \code{x} is the \code{date} of the earthquakes
#' @note The optiona aes \code{y} is the a factor indicating some stratification
#' in which case multiple time lines will be plotted for each level of
#' the factor (e.g. \code{COUNTRY})
#' @note The aes \code{label} is the column in the data frame to obtain
#' the text annotation
#'
#' @import ggplot2
#' @import grid
#' @import dplyr
#'
#' @examples \dontrun{
#' ggplot(input_data, aes (x = date, y = COUNTRY, color = as.numeric(DEATHS), size = as.numeric(EQ_PRIMARY))) +
#' geom_timeline(aes(xmin = as.Date("1800-01-01"), xmax = as.Date("2000-01-01"))) +
#' geom_timeline_label(aes(label = LOCATION_NAME,
#'            xmin = as.Date("1800-01-01"), xmax = as.Date("2000-01-01")),n_max = 5) +
#'            labs(size = "Richter Scale Values", color = "Number of Deaths" ) +
#'            theme_timeline()}
#'
#' @export

geom_timeline_label <- function(mapping = NULL, data = NULL, n_max = NULL, ## add n_max
                                stat = "timeline", position = "identity", ## type stat
                                na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...) ## add n_max
  )
}

GeomTimelineLabel <-
  ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,

                   required_aes = c("x","label"), ## adding label
                   default_aes = ggplot2::aes(xmin = min(data$x),
                                              xmax = max(data$x),
                                              colour = "grey", size = 1.5, alpha = 0.5,
                                              shape = 19, fill = "grey", stroke = 0.5),
                   default_aes = ggplot2::aes(xmin = min(data$x),
                                              xmax = max(data$x)),

                   draw_key = ggplot2::draw_key_blank, ## no points in x-axis



                   ## set n_max by magnitude of earthquakes (which would be size in the input)
                   setup_data = function(data, params){

                     ## optional y aesthetic no exist, set y = 0
                     if (!("y" %in% colnames(data))) {
                       data$y <- 0
                     }

                     data <- data %>%
                       dplyr::group_by(y) %>%
                       dplyr::arrange(desc(size)) %>%
                       dplyr::slice(1:params$n_max) %>% ## take from params$n_max
                       dplyr::ungroup()

                     data ## output: modified data (subset via setup_data)
                   },

                   draw_panel = function(data, panel_scales, coord, n_max) {


                     coords <- coord$transform(data, panel_scales)

                     ## create vertical lines
                     gap <- rep(0.1, length(levels))
                     ver_lines <- grid::polylineGrob(

                       # for each levels in y get both x = 0, x = 1 (end points of lines)
                       x = c(coords$x, coords$x),
                       y = grid::unit(c(coords$y, coords$y+gap), "npc"),

                       # group into seperate lines by levels
                       id = rep(c(1:length(coords$x)), 2),
                       gp = grid::gpar(col = "grey")
                     )

                     ## create labels (inheritparams: geom_text)
                     labels <- grid::textGrob(label = coords$label,
                                              x = unit(coords$x, "npc"),
                                              y = unit(coords$y + gap, "npc"),
                                              rot = 30,
                                              just = c("left","top"),
                                              check.overlap = TRUE,
                                              gp = grid::gpar(fontsize = 12))
                     ## combine
                     grid::gList(ver_lines, labels)
                   }

  )
