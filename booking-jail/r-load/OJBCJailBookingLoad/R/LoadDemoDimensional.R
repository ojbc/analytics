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

#' Loads the booking/jail dimensional database with demo data.
#' @param bookingCount the approximate number of booking events to generate
#' @param lookback the number of days back in time for which to generate jail episodes
#' @param jailCapacity the approximate jail capacity to generate
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
#' loadDemoDimensional(500, 30, 20, censusTractShapefileDSN="/opt/data/Shapefiles/gz_2010_08_150_00_500k", censusTractShapefileLayer="gz_2010_08_150_00_500k", countyFIPSCode="001", censusTractPopulationFile="/opt/data/Census/DEC_10_SF1_P1_with_ann.csv")
#' @export
loadDemoDimensional <- function(bookingCount, lookback, jailCapacity, databaseName="ojbc_booking_analytics_demo",
                                censusTractShapefileDSN, censusTractShapefileLayer, countyFIPSCode,
                                censusTractPopulationFile) {

  loadStartTime <- Sys.time()

  adsConnection <- dbConnect(MySQL(), host="localhost", dbname=databaseName, username="root")

  dbSendQuery(adsConnection, "set foreign_key_checks=0")

  # clear out fact tables
  dbSendQuery(adsConnection, "truncate JailEpisodeCharge")
  dbSendQuery(adsConnection, "truncate JailEpisode")
  dbSendQuery(adsConnection, "truncate BehavioralHealthAssessment")
  dbSendQuery(adsConnection, "truncate Person")
  dbSendQuery(adsConnection, "truncate DailyPopulation")
  dbSendQuery(adsConnection, "truncate DailyPopulationCharges")
  dbSendQuery(adsConnection, "truncate DailyPopulationBehavioralHealth")

  # clear out dimension tables
  dbSendQuery(adsConnection, "truncate JurisdictionType")
  dbSendQuery(adsConnection, "truncate CaseStatusType")
  dbSendQuery(adsConnection, "truncate ChargeType")
  dbSendQuery(adsConnection, "truncate BondType")
  dbSendQuery(adsConnection, "truncate PersonAgeRange")
  dbSendQuery(adsConnection, "truncate PersonAge")
  dbSendQuery(adsConnection, "truncate PersonSex")
  dbSendQuery(adsConnection, "truncate PersonRace")
  dbSendQuery(adsConnection, "truncate PopulationType")
  dbSendQuery(adsConnection, "truncate BehavioralHealthType")
  dbSendQuery(adsConnection, "truncate LanguageType")
  dbSendQuery(adsConnection, "truncate EducationLevelType")
  dbSendQuery(adsConnection, "truncate OccupationType")
  dbSendQuery(adsConnection, "truncate IncomeLevelType")
  dbSendQuery(adsConnection, "truncate BedType")
  dbSendQuery(adsConnection, "truncate HousingStatusType")
  dbSendQuery(adsConnection, "truncate PretrialStatusType")
  dbSendQuery(adsConnection, "truncate AgencyType")

  loadCodeTables(adsConnection)

  # extremely simplifies the person test data
  personID <- 1:bookingCount
  Person <- data.table::data.table(PersonID=personID, StagingPersonUniqueIdentifier=as.character(personID))

  co_shp <- readOGR(censusTractShapefileDSN, censusTractShapefileLayer)
  county_shp <- subset(co_shp, COUNTY == countyFIPSCode)
  blockPop <- read.csv(file=censusTractPopulationFile, head=TRUE)
  geoBlockPop <- merge(county_shp@data, blockPop)
  blockProbs <- geoBlockPop$Population/sum(geoBlockPop$Population)

  getRandomCoordsInCounty <- function(geoId) {
    target_shp <- subset(county_shp, GEO_ID == geoId)
    df <- as.data.frame(coordinates(spsample(x=target_shp, n = 1, type = "regular", iter=80)))
    df$GEO_ID <- geoId
    return(dplyr::select(dplyr::mutate(dplyr::sample_n(df, 1), lat=x2, long=x1, GeoID=geoId), GeoID, lat, long))
  }

  getRandomCoordsInBlocks <- function(geoIds) {
    dplyr::bind_rows(Map(getRandomCoordsInCounty, geoIds))
  }

  #Create Booking test data
  bookingId <- 1:bookingCount
  n <- bookingCount

  buildBookingRow <- function(bookingId) {

    recidivismBookingID <- sample(bookingId, size = length(bookingId)*.49)

    jurisdictionTypeID <- sample(JurisdictionType$JurisdictionTypeID, size=n, replace=TRUE)
    agencyTypeID <- sample(AgencyType$AgencyTypeID, size=n, replace=TRUE)
    caseStatusTypeID <- sample(CaseStatusType$CaseStatusTypeID, size=n, replace=TRUE)

    recidivistIndicator <- sample(0:1, size=n, replace=TRUE, prob=c(0.51,0.49))

    # Making up bond amount and bond amount probs.
    bondAmountList <- c(0, 300, 500,2500, 10000, 20000, 40000, 50000, 100000, 500000, 503000,
                        550000, 660000,750000, 800000, 900000, 1000000)
    bondAmountProbs <- c(.30, .20, .10, .10, .08, .07, .03, .02, .02, .02, .01, .01, .01, .01, .01, .005, .005)
    bondAmount <- sample(bondAmountList, size=n, replace=TRUE, prob=bondAmountProbs)

    bondTypeID <- sample(BondType$BondTypeID, size=n, replace=TRUE, prob=c(.10, .66, .04, .20))

    pretrialStatusTypeID <- sample(PretrialStatusType$PretrialStatusTypeID, size=n, replace=TRUE, prob=c(.05, .2, .25, .25, .25))

    bedTypeID <- sample(BedType$BedTypeID, size=n, replace=TRUE)
    sexID <- sample(1:3, size=n, replace=TRUE, prob=c(.91, .089, .001))

    # http://www.prisonpolicy.org/graphs/2010rates/CO.html
    raceID <- sample(PersonRace$PersonRaceID, size=n, replace=TRUE, prob=c(.009, .43, .35, .06,.15, .001))

    populationTypeID <- sample(c(2,1), size=n, replace=TRUE, prob=c(.68, .32))

    # https://www.bop.gov/about/statistics/statistics_inmate_age.jsp
    personAgeProbs <- c(.0025,.0025, .0030,.0031,.014,.014,.014,.015,rep(.026,5),rep(.036,5),rep(.0366,5),
                        rep(.0304,5),rep(.0218, 5),.177, 0)
    personAgeID <- sample(PersonAge$PersonAgeID, size=n, replace=TRUE, prob=personAgeProbs)

    housingStatusTypeID <- sample(HousingStatusType$HousingStatusTypeID, size=n, replace=TRUE)
    occupationTypeID <- sample(OccupationType$OccupationTypeID, size=n, replace=TRUE)

    # need to work on the language sample to make more sense. correlation with race.
    languageTypeID <- sample(LanguageType$LanguageTypeID, size=n, replace=TRUE)

    incomeLevelTypeID <- sample(IncomeLevelType$IncomeLevelTypeID, size=n, replace=TRUE)
    educationLevelTypeID <- sample(EducationLevelType$EducationLevelTypeID, size=n, replace=TRUE)

    # load lat/long
    arresteeGeoID <- sample(geoBlockPop$GEO_ID, size=n, replace=TRUE, prob=blockProbs)
    coords <- getRandomCoordsInBlocks(arresteeGeoID)
    arrestLocationLatitude <- coords$lat
    arrestLocationLongitude <- coords$long

    # http://www.correction.org/Secondary%20Pages/AverageLengthOfStay.html
    lengthOfStay <- 1:549
    lengthOfStayProbs <- c(.645, .089, .027, .018, .013, rep(.007, 5), rep(.00265, 20), rep(.00143, 30), rep(.00083, 30), rep(.0006, 30),
                           rep(.0003, 30), rep(.00023, 30), rep(.00008, 185), rep(.000005,184))
    bookingLengthOfStay <- sample(lengthOfStay, size=n, replace=TRUE, prob=lengthOfStayProbs)

    df <- data.frame(BookingID=bookingId,
                     JurisdictionTypeID=jurisdictionTypeID,
                     AgencyTypeID=agencyTypeID,
                     BookingCaseNumber=as.character(bookingId),
                     StagingRecordID=bookingId,
                     CaseStatusTypeID=caseStatusTypeID,
                     RecidivistIndicator=recidivistIndicator,
                     BondAmount=bondAmount,
                     BondTypeID=bondTypeID,
                     PretrialStatusTypeID=pretrialStatusTypeID,
                     PersonID=personID,
                     BedTypeID=bedTypeID,
                     PopulationTypeID=populationTypeID,
                     PersonSexID=sexID,
                     PersonRaceID=raceID,
                     PersonAgeID=personAgeID,
                     HousingStatusTypeID=housingStatusTypeID,
                     OccupationTypeID=occupationTypeID,
                     IncomeLevelTypeID=incomeLevelTypeID,
                     EducationLevelTypeID=educationLevelTypeID,
                     LanguageTypeID=languageTypeID,
                     ArrestLocationLatitude=arrestLocationLatitude,
                     ArrestLocationLongitude=arrestLocationLongitude,
                     BookingLengthOfStay=bookingLengthOfStay
    )
  }

  createChargeTypeAssociationForBooking <- function(bookingId) {
    chargeTypeLength = length(ChargeType$ChargeTypeID)

    typeCount <- stats::rpois(n=1, lambda=1)
    # reasonable limit...no more than five different types with one incident
    if (typeCount > chargeTypeLength) {
      typeCount <- chargeTypeLength
    }
    if (typeCount == 0) {
      typeCount <- 1
    }
    types <- sample(1:chargeTypeLength, size=typeCount, prob=c(rep(.19, 4), .05))
    dplyr::bind_rows(Map(function(typeID) {
      data.frame(BookingID=c(bookingId), ChargeTypeID=c(typeID))
    }, types))
  }

  createJailEpisodesForBooking <- function(bookingId, bookingLengthOfStay) {
    lastDaysAgo <- sample((2-bookingLengthOfStay):lookback, size=1, replace=TRUE)

    if (lastDaysAgo < 1) {
      lengthOfStayAsOfYesterday <- bookingLengthOfStay + lastDaysAgo -1

      if (lengthOfStayAsOfYesterday <= lookback) {
        lengthOfStay <- (lengthOfStayAsOfYesterday):1

        daysAgo <- 1:(bookingLengthOfStay + lastDaysAgo -1)
      }
      else {
        lengthOfStay <- lengthOfStayAsOfYesterday:(lengthOfStayAsOfYesterday - lookback + 1)
        daysAgo <- 1:lookback
      }
    }
    else {
      if (lookback-lastDaysAgo + 1 >= bookingLengthOfStay) {
        lengthOfStay <- bookingLengthOfStay:1
        daysAgo <- lastDaysAgo:(lastDaysAgo + length(lengthOfStay) -1)
      }
      else {
        daysAgo <- lastDaysAgo:lookback
        lengthOfStay <- bookingLengthOfStay:(bookingLengthOfStay - length(daysAgo) + 1)
      }
    }
    dplyr::bind_rows(Map(function(daysAgo, lengthOfStay) {
      data.frame(BookingID=c(bookingId), DaysAgo=c(daysAgo), LengthOfStay=c(lengthOfStay))
    }, daysAgo, lengthOfStay))
  }

  createBehavioralHealthAssessment <- function(personId) {

    behavioralHealthTypeLength = length(BehavioralHealthType$BehavioralHealthTypeID)

    behavioralHealthTypeCount <- stats::rpois(n=1, lambda=1)
    # reasonable limit...no more than five different types with one incident
    if (behavioralHealthTypeCount > behavioralHealthTypeLength) {
      behavioralHealthTypeCount <- behavioralHealthTypeLength
    }
    if (behavioralHealthTypeCount == 0) {
      behavioralHealthTypeCount <- 1
    }
    types <- sample(1:behavioralHealthTypeLength, size=behavioralHealthTypeCount, prob=c(rep(1/behavioralHealthTypeLength, behavioralHealthTypeLength)))

    dplyr::bind_rows(Map(function(typeID) {
      data.frame(PersonID=c(personId), BehavioralHealthTypeID=c(typeID),
                 SevereMentalIllnessIndicator=rbinom(n=1, size=1, prob=.4))
    }, types))

  }

  getBookingWithMentalProblems <- function(booking, mentalHealthProblemRate) {
    bookingIdWithMentalProblems <- sample(booking$BookingID, size=length(booking$BookingID)*mentalHealthProblemRate)
    bookingWithMentalProblems <- booking[booking$BookingID %in% bookingIdWithMentalProblems,]
  }

  booking <- buildBookingRow(bookingId)

  Person <- plyr::join(Person, booking, by=c("PersonID"="PersonID"))
  personTableRows <- booking[,c("PersonID", "RecidivistIndicator", "PersonAgeID", "PersonSexID", "PopulationTypeID", "PersonRaceID", "IncomeLevelTypeID", "OccupationTypeID", "EducationLevelTypeID", "LanguageTypeID" )]
  StagingPersonUniqueIdentifier <- personTableRows$PersonID
  personTableRows <- cbind(personTableRows, StagingPersonUniqueIdentifier)

  # Generate JailEpisodeChargeType test data
  ChargeTypeAssociation <- dplyr::bind_rows(Map(createChargeTypeAssociationForBooking, booking$BookingID))
  jailEpisode <- dplyr::bind_rows(Map(createJailEpisodesForBooking, booking$BookingID, booking$BookingLengthOfStay))

  # Modify DaysAgo value to make dailyPopulation within the jail capacity.
  jailEpisodeCount <- plyr::count(jailEpisode$DaysAgo)
  colnames(jailEpisodeCount) <- c("DaysAgo", "EpisodeCount")
  minDailyEpisodeCount <- min(jailEpisodeCount$EpisodeCount)
  minDaysAgo <- jailEpisodeCount %>% dplyr::filter(EpisodeCount==minDailyEpisodeCount)
  beyondDaysAgo <- jailEpisodeCount %>% dplyr::filter(EpisodeCount > jailCapacity)

  for (r in 1:nrow(beyondDaysAgo)) {

    row <- beyondDaysAgo[r,]
    numberOfRowsToMove <- row$EpisodeCount - jailCapacity

    jailEpisodeOfBeyondDaysAgo <- dplyr::filter(jailEpisode, DaysAgo == row$DaysAgo, LengthOfStay == 1)
    jailEpisodeBookingIdsofBeyondDaysAgo <- dplyr::filter(jailEpisode, BookingID %in% jailEpisodeOfBeyondDaysAgo$BookingID)

    bookingIDCount <- plyr::count(jailEpisodeBookingIdsofBeyondDaysAgo, "BookingID")
    bookingIdsToMove <- dplyr::filter(bookingIDCount, freq==1)$BookingID[1:numberOfRowsToMove]

    jailEpisode$DaysAgo[jailEpisode$BookingID %in% bookingIdsToMove]  <- minDaysAgo$DaysAgo[1]

    jailEpisodeCount <- plyr::count(jailEpisode$DaysAgo)
    colnames(jailEpisodeCount) <- c("DaysAgo", "EpisodeCount")
    minDailyEpisodeCount <- min(jailEpisodeCount$EpisodeCount)
    minDaysAgo <- jailEpisodeCount %>% dplyr::filter(EpisodeCount==minDailyEpisodeCount)

  }

  # End of shifting DaysAgo
  JailEpisodeID <- 1:nrow(jailEpisode)
  jailEpisode <- cbind(jailEpisode, JailEpisodeID)
  jailEpisodeCharge <- plyr::join(ChargeTypeAssociation, jailEpisode, by=c("BookingID"="BookingID"), type="left", match="all")
  jailEpisodeCharge <- dplyr::select(jailEpisodeCharge, -BookingID, -DaysAgo, -LengthOfStay)

  # Generate JailEpisode Table test data
  jailEpisodeRows <- plyr::join(booking, jailEpisode, by=c("BookingID"="BookingID"), type="left", match="all")
  jailEpisodeTableRows <- jailEpisodeRows[,
                                          c("JailEpisodeID", "JurisdictionTypeID", "AgencyTypeID", "DaysAgo", "LengthOfStay",
                                            "CaseStatusTypeID", "BondAmount", "BondTypeID", "PretrialStatusTypeID", "PersonID",
                                            "BedTypeID", "HousingStatusTypeID", "ArrestLocationLatitude", "ArrestLocationLongitude" )]

  dbWriteTable(adsConnection, "Person", personTableRows, append=TRUE, row.names=FALSE)
  assign("Person", personTableRows, envir=.GlobalEnv)
  dbWriteTable(adsConnection, "JailEpisode", jailEpisodeTableRows, append=TRUE, row.names=FALSE)
  assign("JailEpisode", jailEpisodeTableRows, envir=.GlobalEnv)
  dbWriteTable(adsConnection, "JailEpisodeCharge", jailEpisodeCharge, append=TRUE, row.names=FALSE)
  assign("JailEpisodeCharge", jailEpisodeCharge, envir=.GlobalEnv)

  femaleBooking <- booking %>% dplyr::filter(PersonSexID==2)
  femaleBookingWithMentalProblems <- getBookingWithMentalProblems(femaleBooking, .74)
  femaleBehavioralAssessment <- dplyr::bind_rows(Map(createBehavioralHealthAssessment, femaleBookingWithMentalProblems$PersonID))


  maleBooking <- booking %>% dplyr::filter(PersonSexID==1)
  maleBookingWithMentalProblems <- getBookingWithMentalProblems(maleBooking, .59)
  maleBehavioralAssessment <- dplyr::bind_rows(Map(createBehavioralHealthAssessment, maleBookingWithMentalProblems$PersonID))

  behavioralHealthAssessment <- rbind(femaleBehavioralAssessment, maleBehavioralAssessment)
  dbWriteTable(adsConnection, "BehavioralHealthAssessment", data.table::data.table(behavioralHealthAssessment), append=TRUE, row.names=FALSE)
  assign("BehavioralHealthAssessment", behavioralHealthAssessment, envir=.GlobalEnv)

  # Generate DailyPopulation test data
  personJailEpisode <- dplyr::left_join(jailEpisodeTableRows, personTableRows, by=c("PersonID"="PersonID"))
  dailyPopulation <- dplyr::count(personJailEpisode, DaysAgo, JurisdictionTypeID, AgencyTypeID, PretrialStatusTypeID,
                                  CaseStatusTypeID, HousingStatusTypeID, BedTypeID, IncomeLevelTypeID,
                                  OccupationTypeID, EducationLevelTypeID, LanguageTypeID, PopulationTypeID, PersonRaceID,
                                  PersonSexID, PersonAgeID)
  dailyPopulation <- dplyr::rename(dailyPopulation, EpisodeCount = n)
  dbWriteTable(adsConnection, "DailyPopulation", data.table::data.table(dailyPopulation), append=TRUE, row.names=FALSE)
  assign("DailyPopulation", dailyPopulation, envir=.GlobalEnv)

  # Generate ChargedDailyPopulation data
  chargedJailEpisode <- dplyr::left_join(personJailEpisode, jailEpisodeCharge, by=c("JailEpisodeID"="JailEpisodeID"))
  chargedDailyPopulation <- dplyr::count(chargedJailEpisode, DaysAgo, JurisdictionTypeID, AgencyTypeID,
                                         PopulationTypeID, ChargeTypeID)
  chargedDailyPopulation <- dplyr::rename(chargedDailyPopulation, EpisodeCount = n)
  dbWriteTable(adsConnection, "DailyPopulationCharges", data.table::data.table(chargedDailyPopulation), append=TRUE, row.names=FALSE)
  assign("DailyPopulationCharges", chargedDailyPopulation, envir=.GlobalEnv)

  # Generate BehavioralHealthDailyPopulation data
  behavioralHealthJailEpisode <- dplyr::left_join(behavioralHealthAssessment, personJailEpisode, by=c("PersonID"="PersonID"))
  behavioralHealthDailyPopulation <- dplyr::count(behavioralHealthJailEpisode, DaysAgo, JurisdictionTypeID, AgencyTypeID,
                                                  PopulationTypeID, BehavioralHealthTypeID, SevereMentalIllnessIndicator)
  behavioralHealthDailyPopulation <- dplyr::rename(behavioralHealthDailyPopulation, EpisodeCount = n)
  dbWriteTable(adsConnection, "DailyPopulationBehavioralHealth", data.table::data.table(behavioralHealthDailyPopulation), append=TRUE, row.names=FALSE)
  assign("DailyPopulationBehavioralHealth", behavioralHealthDailyPopulation, envir=.GlobalEnv)

  dbDisconnect(adsConnection)

  invisible()

}
