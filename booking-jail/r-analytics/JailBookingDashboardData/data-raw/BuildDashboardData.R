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

# This script populates the data/ directory with demo summary dashboard data and rebuilds the package

library(RMySQL)
library(dplyr)
library(devtools)

recidivismIndicatorField <- 'OneYearRebooking'
allRollupID <- -1

adsConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")

rollupTables <- list(
  Agency=c('AgencyCategory', 'ArrestAgencyID'),
  JurisdictionType=c('JurisdictionCategory', 'JurisdictionTypeID'),
  PopulationType=c('PopulationTypeDescription', 'PopulationTypeID')
)

dimensionTables <- list(
  PersonSexType=c("PersonSexTypeDescription", "JailEpisodeID"),
  PersonEthnicityType=c("PersonEthnicityTypeDescription", "JailEpisodeID"),
  LanguageType=c("LanguageTypeDescription", "JailEpisodeID"),
  SexOffenderStatusType=c("SexOffenderStatusTypeDescription", "JailEpisodeID"),
  MilitaryServiceStatusType=c("MilitaryServiceStatusTypeDescription", "JailEpisodeID"),
  WorkReleaseStatusType=c("WorkReleaseStatusTypeDescription", "JailEpisodeID"),
  EducationLevelType=c("EducationLevelTypeDescription", "JailEpisodeID"),
  CaseStatusType=c('CaseStatusTypeCategory', 'JailEpisodeID'),
  ChargeClassType=c('ChargeClassTypeDescription', 'JailEpisodeChargeID'),
  JurisdictionType=c('JurisdictionCategory', 'JailEpisodeChargeID'),
  ChargeDispositionType=c('ChargeDispositionTypeCategory', 'JailEpisodeChargeID'),
  BehavioralHealthEvaluationType=c('BehavioralHealthEvaluationTypeDescription', 'JailEpisodeID'),
  PersonAgeRangeType=c("AgeRange", "JailEpisodeID"),
  Agency=c('AgencyCategory', 'JailEpisodeArrestID', 'ArrestAgencyID'),
  BondType=c('BondTypeCategory', 'JailEpisodeChargeID'),
  PersonRaceType=c("PersonRaceTypeDescription", "JailEpisodeID")
)

DashboardDataFrameLists <- OJBCJailBookingLoad::buildDashboardData(adsConnection, rollupTables, dimensionTables,
                                                                   recidivismIndicatorField, allRollupID)

dbDisconnect(adsConnection)

SummaryDataFrameList <- DashboardDataFrameLists$SummaryDataFrameList
CodeTableDataFrameList <- DashboardDataFrameLists$CodeTableDataFrameList
SMISummaryDataFrame <- DashboardDataFrameLists$SMISummaryDataFrame
DimensionalMetadata <- DashboardDataFrameLists$DimensionalMetadata

devtools::use_data(SummaryDataFrameList, CodeTableDataFrameList, SMISummaryDataFrame, DimensionalMetadata, overwrite = TRUE)

packageFile <- devtools::build()
install.packages(packageFile, repos=NULL)
# note: you either need to restart the R session, or unloadNamespace("JailBookingDashboardData") followed by loadNamespace("JailBookingDashboardData")

