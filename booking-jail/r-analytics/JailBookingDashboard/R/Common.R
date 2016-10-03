# R script of functions shared across the package

allRollupID <- -1
JailCapacity <- 150

#' Get the theme for graphics on the dashboard
#'
#' @import ggthemes
getTheme <- function() {
  ggthemes::theme_gdocs()
}

#' Get the color scheme for graphics on the dashboard
#'
#' @import ggthemes
getColorScheme <- function() {
  ggthemes::scale_colour_gdocs()
}

#' Get the fill scheme for graphics on the dashboard
#'
#' @import ggthemes
getFillScheme <- function() {
  ggthemes::scale_fill_gdocs()
}

#' Print a plot to an SVG file
#'
#' @import svglite
svgPrint <- function(plot, filename, width, height) {
  w <- width
  h <- height
  svglite(filename, width=w, height=h)
  print(plot)
  dev.off()
  invisible()
}
