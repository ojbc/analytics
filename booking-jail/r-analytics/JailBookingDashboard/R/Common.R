# Unless explicitly acquired and licensed from Licensor under another license, the contents of
# this file are subject to the Reciprocal Public License ("RPL") Version 1.5, or subsequent
# versions as allowed by the RPL, and You may not copy or use this file in either source code
# or executable form, except in compliance with the terms and conditions of the RPL
# All software distributed under the RPL is provided strictly on an "AS IS" basis, WITHOUT
# WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY DISCLAIMS ALL SUCH
# WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT. See the RPL for specific language
# governing rights and limitations under the RPL.
#
# http://opensource.org/licenses/RPL-1.5
#
# Copyright 2012-2016 Open Justice Broker Consortium

# R script of functions shared across the package

allRollupID <- -1
JailCapacity <- 150

#' Get the theme for graphics on the dashboard
#'
#' @import ggthemes
getTheme <- function() {
  ggthemes::theme_hc()
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
