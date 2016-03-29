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
svgPrint <- function(plot, filename, width, height) {
  w <- width
  h <- height
  svglite(filename, width=w, height=h)
  print(plot)
  dev.off()
  invisible()
}

AllOriginatingAgenciesLabel <- "All Agencies"
AllCourtsLabel <- "All Jurisdictions"
TargetPopulationLabel <- "Target Population"

DefaultOriginatingAgenciesLabel <- AllOriginatingAgenciesLabel
DefaultJurisdictionLabel <- AllCourtsLabel

JailCapacity <- 2005
