#' Theme for TDOE plots
#'
#' @inheritParams ggplot2::theme_minimal
#' @family themes tdoe
#' @export
#' @example theme_tdoe()
#' @importFrom grid unit
theme_tdoe <- function(base_size = 12, base_family = "sans") {
  colors <- c(Red = '#d22630', Navy = '#002d72', Gray = '#75787b', Teal = '#2dccd3',
              Yellow = '#d2d755', Orange = '#e87722', `Dark Green` = '#5d7975', `Dark Gray` = "#3C3C3C")
  (theme_minimal(base_size = base_size, base_family = base_family)
    + theme(
      line = element_line(colour = "black"),
      rect = element_rect(fill = colors["Gray"],
                          linetype = 0, colour = NA),
      text = element_text(colour = colors["Dark Gray"]),
      axis.title = element_blank(),
      axis.text = element_text(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      legend.background = element_rect(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      panel.grid = element_line(colour = NULL),
      panel.grid.major =
        element_line(colour = colors["Gray"]),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      strip.background = element_rect()))
}

#' TDOE color palette
#'
#' The standard three-color TDOE palette for line plots comprises
#' red, navy, and gray
#'
#' @family colour tdoe
#' @export
tdoe_pal <- function() {
  colors <- c(Red = '#d22630', Navy = '#002d72', Gray = '#75787b', Teal = '#2dccd3',
              Yellow = '#d2d755', Orange = '#e87722', `Dark Green` = '#5d7975', `Dark Gray` = "#3C3C3C")
  values <- unname(colors[c("Red", "Navy", "Gray")])
  max_n <- length(values)
  f <- manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

#' TDOE color scales
#'
#' Color scales using the colors in the TDOE graphics.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @family colour tdoe
#' @rdname scale_tdoe
#' @export
scale_colour_tdoe <- function(...) {
  discrete_scale("colour", "economist", tdoe_pal(), ...)
}

#' @rdname scale_tdoe
#' @export
scale_color_tdoe <- scale_colour_tdoe

#' @rdname scale_tdoe
#' @export
scale_fill_tdoe <- function(...) {
  discrete_scale("fill", "economist", tdoe_pal(), ...)
}
