# R script of functions shared across the package

#' Get the theme for graphics on the dashboard
#'
#' @import ggthemes
getTheme <- function() {
  ggthemes::theme_hc()
}

#' Get the color scheme for graphics on the dashboard
#'
#' @import ggthemes
getColorScheme <- function() {
  ggthemes::scale_colour_hc()
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
svgPrint <- function(plot, filename) {
  svglite(filename, width=5, height=3.1)
  print(plot)
  dev.off()
  invisible()
}

AllOriginatingAgenciesLabel <- "All Agencies"
AllCourtsLabel <- "All Courts"
TargetPopulationLabel <- "Target Population"

DefaultOriginatingAgenciesLabel <- AllOriginatingAgenciesLabel
DefaultJurisdictionLabel <- AllCourtsLabel
