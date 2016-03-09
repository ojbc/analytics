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
# Copyright 2012-2015 Open Justice Broker Consortium

# Loads the dimensional database with dummy/demo data

COUNTY <- "Adams"
demoBookingCount = 10000
library(RMySQL)
library(data.table)
library(dplyr)
library(plyr)
library(rgdal)
library(sp)
library(readr)
library(tidyr)
library(Hmisc)
library(stringr)
library(xlsx)

source("LoadCodeTables.R")

loadStartTime <- Sys.time()

#conn <- dbConnect(MySQL(), host="dw", dbname="ojbc_analytics_demo", username="root")
adsConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")

dbSendQuery(adsConnection, "set foreign_key_checks=0")

# clear out fact tables
dbSendQuery(adsConnection, "delete from JailEpisodeCharge")
dbSendQuery(adsConnection, "delete from JailEpisode")
dbSendQuery(adsConnection, "delete from BehavioralHealthAssessment")
dbSendQuery(adsConnection, "delete from Person")
dbSendQuery(adsConnection, "delete from DailyPopulation")
dbSendQuery(adsConnection, "delete from DailyPopulationCharges")
dbSendQuery(adsConnection, "delete from DailyPopulationBehavioralHealth")

# clear out dimension tables
dbSendQuery(adsConnection, "delete from JurisdictionType")
dbSendQuery(adsConnection, "delete from CaseStatusType")
dbSendQuery(adsConnection, "delete from ChargeType")
dbSendQuery(adsConnection, "delete from BondType")
dbSendQuery(adsConnection, "delete from PersonAgeRange")
dbSendQuery(adsConnection, "delete from PersonAge")
dbSendQuery(adsConnection, "delete from PersonSex")
dbSendQuery(adsConnection, "delete from PersonRace")
dbSendQuery(adsConnection, "delete from PopulationType")
dbSendQuery(adsConnection, "delete from BehavioralHealthType")
dbSendQuery(adsConnection, "delete from LanguageType")
dbSendQuery(adsConnection, "delete from EducationLevelType")
dbSendQuery(adsConnection, "delete from OccupationType")
dbSendQuery(adsConnection, "delete from IncomeLevelType")
dbSendQuery(adsConnection, "delete from BedType")
dbSendQuery(adsConnection, "delete from HousingStatusType")
dbSendQuery(adsConnection, "delete from PretrialStatusType")
dbSendQuery(adsConnection, "delete from AgencyType")

loadCodeTables("AnalyticsCodeTables.xlsx", adsConnection)

# extremely simplifies the person test data
personID <- 1:demoBookingCount
Person <- data.table(PersonID=personID, StagingPersonUniqueIdentifier=as.character(personID))

co_shp <- readOGR( "censusData/geoCensusBlocks", "gz_2010_08_150_00_500k")
adams_county_shp <- subset(co_shp, COUNTY== "001")
adamsBlockPopulation<-read.csv(file="censusData/DEC_10_SF1_P1_with_ann.csv", head=TRUE)
adamsGeoBlockPop<-merge(adams_county_shp@data, adamsBlockPopulation)
blockProbs<-adamsGeoBlockPop$Population/sum(adamsGeoBlockPop$Population)

getRandomCoordsInCounty <- function(geoId) {
    target_shp <- subset(adams_county_shp, GEO_ID == geoId)
    df <- as.data.frame(coordinates(spsample(x=target_shp, n = 1, type = "regular", iter=50)))
    df$GEO_ID <- geoId
    return(select(mutate(sample_n(df, 1), lat=x2, long=x1, GeoID=geoId), GeoID, lat, long))
}

getRandomCoordsInBlocks <- function(geoIds) {
  bind_rows(Map(getRandomCoordsInCounty, geoIds))
}

#Create Booking test data
bookingId<-1:demoBookingCount
n<-demoBookingCount

buildBookingRow<-function(bookingId){

  recidivismBookingID <- sample(bookingId, size = length(bookingId)*.49)

  jurisdictionTypeID <- sample(JurisdictionType$JurisdictionTypeID, size=n, replace=TRUE)
  agencyTypeID <-sample(AgencyType$AgencyTypeID, size=n, replace=TRUE)
  caseStatusTypeID<-sample(CaseStatusType$CaseStatusTypeID, size=n, replace=TRUE)

  recidivistIndicator<-sample(0:1, size=n, replace=TRUE, prob=c(0.51,0.49))

  # Making up bond amount and bond amount probs.
  bondAmountList<-c(0, 300, 500,2500, 10000, 20000, 40000, 50000, 100000, 500000, 503000,
                    550000, 660000,750000, 800000, 900000, 1000000)
  bondAmountProbs<-c(.30, .20, .10, .10, .08, .07, .03, .02, .02, .02, .01, .01, .01, .01, .01, .005, .005)
  bondAmount<-sample(bondAmountList, size=n, replace=TRUE, prob=bondAmountProbs)

  bondTypeID<-sample(BondType$BondTypeID, size=n, replace=TRUE, prob=c(.10, .66, .04, .20))

  pretrialStatusTypeID<-sample(PretrialStatusType$PretrialStatusTypeID, size=n, replace=TRUE)

  bedTypeID<-sample(BedType$BedTypeID, size=n, replace=TRUE)
  sexID <- sample(1:3, size=n, replace=TRUE, prob=c(.91, .089, .001))

  # http://www.prisonpolicy.org/graphs/2010rates/CO.html
  raceID<-sample(PersonRace$PersonRaceID, size=n, replace=TRUE, prob=c(.009, .43, .35, .06,.15, .001))
  populationTypeID<-sample(PopulationType$PopulationTypeID, size=n, replace=TRUE, prob=c(.68, .32))

  # https://www.bop.gov/about/statistics/statistics_inmate_age.jsp
  personAgeProbs<-c(.0025,.0025, .0030,.0031,.014,.014,.014,.015,rep(.026,5),rep(.036,5),rep(.0366,5),
                    rep(.0304,5),rep(.0218, 5),.177, 0)
  personAgeID<-sample(PersonAge$PersonAgeID, size=n, replace=TRUE, prob=personAgeProbs)

  housingStatusTypeID<-sample(HousingStatusType$HousingStatusTypeID, size=n, replace=TRUE)
  occupationTypeID<-sample(OccupationType$OccupationTypeID, size=n, replace=TRUE)

  # need to work on the language sample to make more sense. correlation with race.
  languageTypeID<-sample(LanguageType$LanguageTypeID, size=n, replace=TRUE)

  # http://www.bjs.gov/content/pub/pdf/pji02.pdf
  incomeLevelTypeID<-sample(IncomeLevelType$IncomeLevelTypeID, size=n, replace=TRUE, prob=c(.193,.106,.15,.142,.244,.164))
  educationLevelTypeID<-sample(EducationLevelType$EducationLevelTypeID, size=n, replace=TRUE, prob=c(.123,.316,.171,.259,.101,.029))

  # load lat/long
  arresteeGeoID <- sample(adamsGeoBlockPop$GEO_ID, size=n, replace=TRUE, prob=blockProbs)
  coords <- getRandomCoordsInBlocks(arresteeGeoID)
  arrestLocationLatitude <- coords$lat
  arrestLocationLongitude <- coords$long

  # http://www.correction.org/Secondary%20Pages/AverageLengthOfStay.html
  lengthOfStay<-1:549
  lengthOfStayProbs<-c(.645, .089, .027, .018, .013, rep(.007, 5), rep(.00265, 20), rep(.00143, 30), rep(.00083, 30), rep(.0006, 30),
                       rep(.0003, 30), rep(.00023, 30), rep(.00008, 185), rep(.000005,184))
  bookingLengthOfStay<-sample(lengthOfStay, size=n, replace=TRUE, prob=lengthOfStayProbs)

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

  typeCount <- rpois(n=1, lambda=1)
  # reasonable limit...no more than five different types with one incident
  if (typeCount > chargeTypeLength) {
    typeCount <- chargeTypeLength
  }
  if (typeCount == 0) {
    typeCount <- 1
  }
  types <- sample(1:chargeTypeLength, size=typeCount, prob=c(rep(.19, 5), .05))
  bind_rows(Map(function(typeID) {
    data.frame(BookingID=c(bookingId), ChargeTypeID=c(typeID))
  }, types))
}

createJailEpisodesForBooking <- function(bookingId, bookingLengthOfStay) {
  lastDaysAgo<-sample((2-bookingLengthOfStay):180, size=1, replace=TRUE)

  if (lastDaysAgo < 1){
    lengthOfStayAsOfYesterday <- bookingLengthOfStay + lastDaysAgo -1

    if (lengthOfStayAsOfYesterday <=180){
      lengthOfStay <- (lengthOfStayAsOfYesterday) : 1

      daysAgo <-1: (bookingLengthOfStay + lastDaysAgo -1)
    }
    else {
      lengthOfStay <- lengthOfStayAsOfYesterday : (lengthOfStayAsOfYesterday - 179)
      daysAgo <- 1:180
    }
  }
  else {
    if (180-lastDaysAgo + 1 >= bookingLengthOfStay){
      lengthOfStay<-bookingLengthOfStay:1
      daysAgo<-lastDaysAgo:(lastDaysAgo + length(lengthOfStay) -1)
    }
    else{
      daysAgo<-lastDaysAgo:180
      lengthOfStay<-bookingLengthOfStay:(bookingLengthOfStay - length(daysAgo) + 1)
    }
  }
  bind_rows(Map(function(daysAgo, lengthOfStay) {
    data.frame(BookingID=c(bookingId), DaysAgo=c(daysAgo), LengthOfStay=c(lengthOfStay))
  }, daysAgo, lengthOfStay))
}

createBehavioralHealthAssessment <- function(personId) {

  behavioralHealthTypeLength = length(BehavioralHealthType$BehavioralHealthTypeID)

  behavioralHealthTypeCount <- rpois(n=1, lambda=1)
  # reasonable limit...no more than five different types with one incident
  if (behavioralHealthTypeCount > behavioralHealthTypeLength) {
    behavioralHealthTypeCount <- behavioralHealthTypeLength
  }
  if (behavioralHealthTypeCount == 0) {
    behavioralHealthTypeCount <- 1
  }
  types <- sample(1:behavioralHealthTypeLength, size=behavioralHealthTypeCount, prob=c(rep(1/behavioralHealthTypeLength, behavioralHealthTypeLength)))

  bind_rows(Map(function(typeID) {
    data.frame(PersonID=c(personId), BehavioralHealthTypeID=c(typeID))
  }, types))

}

getBookingWithMentalProblems<-function(booking, mentalHealthProblemRate){
  bookingIdWithMentalProblems<-sample(booking$BookingID, size=length(booking$BookingID)*mentalHealthProblemRate)
  bookingWithMentalProblems<-booking[booking$BookingID %in% bookingIdWithMentalProblems,]
}

booking <- buildBookingRow(bookingId)
Person<-join(Person, booking)
personTableRows<-booking[,c("PersonID", "RecidivistIndicator", "PersonAgeID", "PersonSexID", "PopulationTypeID", "PersonRaceID", "IncomeLevelTypeID", "OccupationTypeID", "EducationLevelTypeID", "LanguageTypeID" )]
StagingPersonUniqueIdentifier<-personTableRows$PersonID
personTableRows<-cbind(personTableRows, StagingPersonUniqueIdentifier)

# Generate JailEpisodeChargeType test data
ChargeTypeAssociation <- bind_rows(Map(createChargeTypeAssociationForBooking, booking$BookingID))
jailEpisode<-bind_rows(Map(createJailEpisodesForBooking, booking$BookingID, booking$BookingLengthOfStay))
JailEpisodeID<-1:nrow(jailEpisode)
jailEpisode<-cbind(jailEpisode, JailEpisodeID)
jailEpisodeCharge<-join(ChargeTypeAssociation, jailEpisode, by=NULL, type="left", match="all")
jailEpisodeCharge<-select(jailEpisodeCharge, -BookingID, -DaysAgo, -LengthOfStay)

# Generate JailEpisode Table test data
jailEpisodeRows<-join(booking, jailEpisode, type="left", match="all")
jailEpisodeTableRows<-jailEpisodeRows[,
    c("JailEpisodeID", "JurisdictionTypeID", "AgencyTypeID", "DaysAgo", "LengthOfStay",
      "CaseStatusTypeID", "BondAmount", "BondTypeID", "PretrialStatusTypeID", "PersonID",
      "BedTypeID", "HousingStatusTypeID", "ArrestLocationLatitude", "ArrestLocationLongitude" )]

dbWriteTable(adsConnection, "Person", personTableRows, append=TRUE, row.names=FALSE)
dbWriteTable(adsConnection, "JailEpisode", jailEpisodeTableRows, append=TRUE, row.names=FALSE)
dbWriteTable(adsConnection, "JailEpisodeCharge", jailEpisodeCharge, append=TRUE, row.names=FALSE)

femaleBooking<-booking %>% filter(PersonSexID==2)
femaleBookingWithMentalProblems<-getBookingWithMentalProblems(femaleBooking, .74)
femaleBehavioralAssessment<-bind_rows(Map(createBehavioralHealthAssessment, femaleBookingWithMentalProblems$PersonID))


maleBooking<-booking %>% filter(PersonSexID==1)
maleBookingWithMentalProblems<-getBookingWithMentalProblems(maleBooking, .59)
maleBehavioralAssessment<-bind_rows(Map(createBehavioralHealthAssessment, maleBookingWithMentalProblems$PersonID))

behavioralHealthAssessment <- rbind(femaleBehavioralAssessment, maleBehavioralAssessment)
dbWriteTable(adsConnection, "BehavioralHealthAssessment", data.table(behavioralHealthAssessment), append=TRUE, row.names=FALSE)

# Generate DailyPopulation test data
personJailEpisode <- left_join(jailEpisodeTableRows, personTableRows)
dailyPopulation <- dplyr::count(personJailEpisode, DaysAgo, JurisdictionTypeID, AgencyTypeID, PretrialStatusTypeID,
                         CaseStatusTypeID, HousingStatusTypeID, BedTypeID, IncomeLevelTypeID,
                         OccupationTypeID, EducationLevelTypeID, LanguageTypeID, PopulationTypeID, PersonRaceID,
                         PersonSexID, PersonAgeID)
dailyPopulation <- dplyr::rename(dailyPopulation, EpisodeCount = n)
dbWriteTable(adsConnection, "DailyPopulation", data.table(dailyPopulation), append=TRUE, row.names=FALSE)

# Generate ChargedDailyPopulation data
chargedJailEpisode <-left_join(personJailEpisode, jailEpisodeCharge)
chargedDailyPopulation <- dplyr::count(chargedJailEpisode, DaysAgo, JurisdictionTypeID, AgencyTypeID,
                         PopulationTypeID, ChargeTypeID)
chargedDailyPopulation <- dplyr::rename(chargedDailyPopulation, EpisodeCount = n)
dbWriteTable(adsConnection, "DailyPopulationCharges", data.table(chargedDailyPopulation), append=TRUE, row.names=FALSE)

# Generate BehavioralHealthDailyPopulation data
behavioralHealthJailEpisode <-left_join(behavioralHealthAssessment, personJailEpisode)
behavioralHealthDailyPopulation <- dplyr::count(behavioralHealthJailEpisode, DaysAgo, JurisdictionTypeID, AgencyTypeID,
                         PopulationTypeID, BehavioralHealthTypeID)
behavioralHealthDailyPopulation <- dplyr::rename(behavioralHealthDailyPopulation, EpisodeCount = n)
dbWriteTable(adsConnection, "DailyPopulationBehavioralHealth", data.table(behavioralHealthDailyPopulation), append=TRUE, row.names=FALSE)

dbDisconnect(adsConnection)
