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

#' Loads the booking/jail staging database with demo data.
#' @param databaseName the MySQL database in which to save the data (assumes write access to user=root and blank password)
#' @param countyFIPSCode the FIPS code for the county of interest
#' @param censusTractShapefileDSN Path to folder containing tract shapefiles.  Download shapefiles from https://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html
#' @param censusTractShapefileLayer Layer within the shapefile (should be same name as last part of DSN path)
#' @param censusTractPopulationFile file of population data at the census tract level.  Download this from the US
#' Census website, by visiting factfinder.census.gov, select Advanced Search, then Geographies.  Chose "Census Tract - 140" as
#' Geography Type, then the state and county of interest, and "Add to Selection".  Search for table name P1 (total population)
#' and download.
#' @import RMySQL
#' @import rgdal
#' @import sp
#' @import readr
#' @import tidyr
#' @import stringr
#' @examples
#' loadDemoStaging(censusTractShapefileDSN="/opt/data/Shapefiles/gz_2010_08_150_00_500k", censusTractShapefileLayer="gz_2010_08_150_00_500k", countyFIPSCode="001", censusTractPopulationFile="/opt/data/Census/DEC_10_SF1_P1_with_ann.csv")
#' @export
loadDemoStaging <- function(databaseName="ojbc_booking_staging_demo",
                                censusTractShapefileDSN, censusTractShapefileLayer, countyFIPSCode,
                            censusTractPopulationFile) {

  loadStartTime <- Sys.time()

  stagingConnection <- dbConnect(MySQL(), host="localhost", dbname=databaseName, username="root")

  dbSendQuery(stagingConnection, "set foreign_key_checks=0")

  dbSendQuery(stagingConnection, "truncate AgencyType")
  dbSendQuery(stagingConnection, "truncate BedType")
  dbSendQuery(stagingConnection, "truncate BehavioralHealthAssessment")
  dbSendQuery(stagingConnection, "truncate BehavioralHealthEvaluation")
  dbSendQuery(stagingConnection, "truncate BehavioralHealthDiagnosisType")
  dbSendQuery(stagingConnection, "truncate BondType")
  dbSendQuery(stagingConnection, "truncate Booking")
  dbSendQuery(stagingConnection, "truncate BookingArrest")
  dbSendQuery(stagingConnection, "truncate BookingCharge")
  dbSendQuery(stagingConnection, "truncate CaseStatusType")
  dbSendQuery(stagingConnection, "truncate ChargeType")
  dbSendQuery(stagingConnection, "truncate CustodyRelease")
  dbSendQuery(stagingConnection, "truncate CustodyStatusChange")
  dbSendQuery(stagingConnection, "truncate CustodyStatusChangeArrest")
  dbSendQuery(stagingConnection, "truncate CustodyStatusChangeCharge")
  dbSendQuery(stagingConnection, "truncate EducationLevelType")
  dbSendQuery(stagingConnection, "truncate Facility")
  dbSendQuery(stagingConnection, "truncate HousingStatusType")
  dbSendQuery(stagingConnection, "truncate IncomeLevelType")
  dbSendQuery(stagingConnection, "truncate JurisdictionType")
  dbSendQuery(stagingConnection, "truncate LanguageType")
  dbSendQuery(stagingConnection, "truncate Location")
  dbSendQuery(stagingConnection, "truncate MedicationType")
  dbSendQuery(stagingConnection, "truncate MilitaryServiceStatusType")
  dbSendQuery(stagingConnection, "truncate OccupationType")
  dbSendQuery(stagingConnection, "truncate Person")
  dbSendQuery(stagingConnection, "truncate PersonRaceType")
  dbSendQuery(stagingConnection, "truncate PersonSexType")
  dbSendQuery(stagingConnection, "truncate PrescribedMedication")
  dbSendQuery(stagingConnection, "truncate Treatment")

  loadCodeTables(stagingConnection, "StagingCodeTables.xlsx")

}

