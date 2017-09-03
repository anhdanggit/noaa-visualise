#' @title Visualising Timeline for the NOAA Earthquake Dataset
#'
#' @description \code{geom_timeline()} plots the significant earthquake,
#' ranging from \code{xmin} to \code{xmax}
#' The optional aes include \code{color}, \code{size}, \code{alpha}.
#' The xaesthetic is a date and an optional yaesthetic is a factor of levels.
#'
#' @inheritParams stat_timeline
#' @param xmin The minimum date in the range of timeline
#' @param xmax The maximum date in the range of timeline
#' @param x The date of earthquakes
#' @param y The factor of levels (e.g: \code{COUNTRY})
#'
#' @note The aes \code{xmin} and \code{xmax} as date object should be clarified.
#' @note The geom is set: \code{stat = "timeline"}
#'
#' @import ggplot2
#' @import grid
#' @import scales
#'
#' @examples \dontrun{ggplot(input_data, aes (x = date, y = COUNTRY,
#' color = as.numeric(DEATHS),
#' size = as.numeric(EQ_PRIMARY))) +
#' geom_timeline(aes(xmin = as.Date("1900-01-01"), xmax = as.Date("1925-01-01"))) +
#' labs(size = "Richter Scale Values", color = "Number of Deaths" )+
#' theme_timeline()}
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "timeline",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE,...) {

  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomTimeline <-
  ggplot2::ggproto(
    "GeomTimeline", ggplot2::Geom,
    required_aes = c("x"),
    default_aes = ggplot2::aes(colour = "grey", size = 2, alpha = 0.5,
                               shape = 19, fill = "grey", stroke = 0.5),
    draw_key = ggplot2::draw_key_point,
    draw_panel = function(data, panel_scales, coord) {

      ## optional y aesthetic no exist, set y = 0
      if (!("y" %in% colnames(data))) {
        data$y <- 0
      }

      coords <- coord$transform(data, panel_scales)

      points <- grid::pointsGrob(

        ## take (x,y) position from aes
        coords$x, coords$y,

        ## take the shape of points from the aes
        pch = coords$shape,

        ##take from aes, trans to unit, modify. size
        size = grid::unit(coords$size*0.40, "char"),

        ## colours and alphas change by aes (color, size, alpha)
        gp = grid::gpar(
          col = scales::alpha(coords$colour, coords$alpha),
          fill = scales::alpha(coords$colour, coords$alpha)
        )
      )
      levels <- unique(coords$y) ## takes the lines by factors (eg: COUNTRY)

      lines <- grid::polylineGrob(

        # for each levels in y get both x = 0, x = 1 (end points of lines)
        x = grid::unit(rep(c(0,1), each = length(levels)), "npc"),
        y = grid::unit(c(levels,levels), "npc"),

        # group into seperate lines by levels
        id = rep(c(1:length(levels)), 2),
        gp = grid::gpar(col = "grey")
      )

      grid::gList(lines, points)
    }
  )
