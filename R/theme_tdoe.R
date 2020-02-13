#' Theme for TDOE plots
#'
#' @inheritParams ggplot2::theme_minimal
#' @family themes tdoe
#' @export
theme_tdoe <- function(base_size = 9, base_family = "sans") {
  colors <- c(Red = '#d22630', Navy = '#002d72', Gray = '#75787b', Teal = '#2dccd3',
              Yellow = '#d2d755', Orange = '#e87722', `Dark Green` = '#5d7975', `Dark Gray` = "#3C3C3C")
  (ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
    + ggplot2::theme(
      line = ggplot2::element_line(colour = "black"),
      rect = ggplot2::element_rect(fill = NA,
                          linetype = 0, colour = NA),
      text = ggplot2::element_text(colour = colors["Dark Gray"]),
      # axis.title = element_blank(),
      axis.text = ggplot2::element_text(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = colors["Gray"], size = 0.5), # element_blank(),
      legend.background = ggplot2::element_rect(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      panel.grid = ggplot2::element_line(colour = NULL),
      panel.grid.major.y = ggplot2::element_line(colour = colors["Gray"], size = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0, size = rel(1.5), face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = rel(1.25)),
      plot.margin = ggplot2::unit(c(1, 1, 2, 1), "lines"),
      strip.text = ggplot2::element_text(size = rel(1.2)),
      strip.background = ggplot2::element_rect()))
}

#' Save plot with TDOE Logo
#' Default is the size of image determined to work best with PowerPoint slides
#'
#' @param plot_object ggplot object to save
#' @param file_path Path to save the image, ending in .png
#' @param fig_height Height of image. Default of 4.95
#' @param fig_width Width of image. Default of 9.17
#' @param fig_unit Default of 'in' for inches. Can use 'px' if units are pixels
#' @param fig_res Resolution of image. Default of 1200.
#' @export
save_with_logo <- function(file_path, plot_object = ggplot2::last_plot(), fig_height = 4.95, fig_width = 9.17, fig_unit = 'in', fig_res = 1200) {
  logo <- magick::image_read("N:/ORP_accountability/projects/Andrew/Crosswalks/logo.png")
  grDevices::png(file_path, height = fig_height, width = fig_width, units = fig_unit, res = fig_res)
  print(plot_object)
  grid::grid.raster(logo, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.5, 'inches'))
  grDevices::dev.off()
}


#' @export
add_tdoe_logo <- function(xmin, xmax, ymin, ymax, ...) {
  logo <- png::readPNG(source = "N:/ORP_accountability/projects/Andrew/Crosswalks/logo.png")
  ggplot2::annotation_custom(grid::rasterGrob(logo, interpolate=TRUE), xmin= xmin, xmax=xmax, ymin= ymin, ymax=ymax, ...)
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
  values <- unname(colors[c("Red", "Navy", "Gray", 'Teal', 'Yellow', 'Orange', 'Dark Green')])
  max_n <- length(values)
  f <- scales::manual_pal(values)
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
  ggplot2::discrete_scale("colour", "economist", andrewacct::tdoe_pal(), ...)
}

#' @rdname scale_tdoe
#' @export
scale_color_tdoe <- scale_colour_tdoe

#' @rdname scale_tdoe
#' @export
scale_fill_tdoe <- function(...) {
  ggplot2::discrete_scale("fill", "economist", andrewacct::tdoe_pal(), ...)
}

