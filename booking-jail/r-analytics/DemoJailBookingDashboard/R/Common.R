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

# pointers to data frame lists (typically loaded from a Data package)

summaryDataFrameList <- DemoJailBookingDashboardData::SummaryDataFrameList
codeTableDataFrameList <- DemoJailBookingDashboardData::CodeTableDataFrameList
smiSummaryDataFrame <- DemoJailBookingDashboardData::SMISummaryDataFrame

#' @export
getLastLoadTime <- function() {
  DemoJailBookingDashboardData::DimensionalMetadata$LastLoadTime
}

# R script of functions shared across the package

allRollupID <- DemoJailBookingDashboardData::allRollupID
JailCapacity <- 150

#' Get the theme for graphics on the dashboard
#'
#' @import ggthemes
getTheme <- function() {
  ggthemes::theme_hc()
}

filterDimensionList <- list()
filterDimension <- list()
filterDimension$AllLabel <- 'All Jurisdictions'
filterDimension$FactTableFK <- 'JurisdictionTypeID'
filterDimensionList$JurisdictionType <- filterDimension
filterDimension <- list()
filterDimension$AllLabel <- 'All Agencies'
filterDimension$FactTableFK <- 'ArrestAgencyID'
filterDimensionList$Agency <- filterDimension
filterDimension <- list()
filterDimension$AllLabel <- 'All Populations'
filterDimension$FactTableFK <- 'PopulationTypeID'
filterDimensionList$PopulationType <- filterDimension

getPopulationTypeLabelFromBoolean <- function(targetPopulationOnlyBoolean) {
  ifelse(targetPopulationOnlyBoolean, 'Target Population', 'All Populations')
}
