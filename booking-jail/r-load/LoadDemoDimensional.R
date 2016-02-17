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
demoBookingCount = 100
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
dbSendQuery(adsConnection, "delete from JailEpisodeChargeType")
dbSendQuery(adsConnection, "delete from JailEpisode")
dbSendQuery(adsConnection, "delete from BehaviorHealthAssessment")
dbSendQuery(adsConnection, "delete from Person")

# clear out dimension tables
#dbSendQuery(conn, "delete from YesNo")

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

buildBookingRow<-function(bookingId){

  recidivismBookingID <- sample(bookingId, size = length(bookingId)*.49)
  
  jurisdictionID <- sample(Jurisdiction$JurisdictionID, size=n, replace=TRUE)
  sendingAgencyID <-sample(Agency$AgencyID, size=n, replace=TRUE)
  caseStatusID<-sample(CaseStatus$StatusID, size=n, replace=TRUE)
  
  facilityCapacity<-as.integer(as.character(Facility$Capacity))
  facilityProbs<-facilityCapacity/sum(facilityCapacity)
  facilityID<-sample(Facility$FacilityID, size=n, replace=TRUE, prob = facilityProbs)
  
  recidivistIndicator<-sample(0:1, size=n, replace=TRUE, prob=c(0.51,0.49))

  # Making up bond amount and bond amount probs. 
  bondAmountList<-c(0, 300, 500,2500, 10000, 20000, 40000, 50000, 100000, 500000, 503000, 
                    550000, 660000,750000, 800000, 900000, 1000000)
  bondAmountProbs<-c(.30, .20, .10, .10, .08, .07, .03, .02, .02, .02, .01, .01, .01, .01, .01, .005, .005)
  bondAmount<-sample(bondAmountList, size=n, replace=TRUE, prob=bondAmountProbs)
  
  bondTypeID<-sample(BondType$BondTypeID, size=n, replace=TRUE, prob=c(.10, .66, .04, .20))
  
  pretrialStatusID<-sample(PretrialStatus$PretrialStatusID, size=n, replace=TRUE)
  
  # need to make release date. currently set it to be the same as supervisionReleaseDate. And 
  # need to populate more dateID to be able to have large lenght of stay. 
  supervisionReleaseDate<-runif(n=n, min=10, max=364) + bookingDate
  supervisionReleaseDateID <- format(supervisionReleaseDate, DATE_ID_FORMAT)
  
  bedTypeID<-sample(BedType$BedTypeID, size=n, replace=TRUE)
  sexID <- sample(1:3, size=n, replace=TRUE, prob=c(.91, .089, .001))
  
  # http://www.prisonpolicy.org/graphs/2010rates/CO.html
  raceID<-sample(PersonRace$PersonRaceID, size=n, replace=TRUE, prob=c(.009, .43, .35, .06,.15, .001))
  populationTypeID<-sample(PopulationType$PopulationTypeID, size=n, replace=TRUE, prob=c(.68, .32))
  
  # https://www.bop.gov/about/statistics/statistics_inmate_age.jsp
  personAgeProbs<-c(.0025,.0025, .0030,.0031,.014,.014,.014,.015,rep(.026,5),rep(.036,5),rep(.0366,5),
                    rep(.0304,5),rep(.0218, 5),.177, 0)
  personAgeID<-sample(PersonAge$PersonAgeID, size=n, replace=TRUE, prob=personAgeProbs)
  
  housingStatusID<-sample(HousingStatus$HousingStatusID, size=n, replace=TRUE)
  occupationID<-sample(Occupation$OccupationID, size=n, replace=TRUE)
  
  # need to work on the language sample to make more sense. correlation with race. 
  languageID<-sample(Language$LanguageID, size=n, replace=TRUE)
  
  # http://www.bjs.gov/content/pub/pdf/pji02.pdf
  incomeLevelID<-sample(IncomeLevel$IncomeLevelID, size=n, replace=TRUE, prob=c(.193,.106,.15,.142,.244,.164))
  educationID<-sample(Education$EducationID, size=n, replace=TRUE, prob=c(.123,.316,.171,.259,.101,.029))
  
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
  lastDaysAgo<-sample(c(1:180), size=n, replace=TRUE)
  
  df <- data.frame(BookingID=bookingId,
                   JurisdictionID=jurisdictionID,
                   SendingAgency=sendingAgencyID,
                   FacilityID=facilityID,
                   BookingCaseNumber=as.character(bookingId),
                   StagingRecordID=bookingId,
                   CaseStatusID=caseStatusID,
                   RecidivistIndicator=recidivistIndicator,
                   BondAmount=bondAmount,
                   BondTypeID=bondTypeID,
                   PretrialStatusID=pretrialStatusID,
                   PersonID=personID,
                   BedTypeID=bedTypeID,
                   PopulationTypeID=populationTypeID,
                   PersonSexID=sexID,
                   PersonRaceID=raceID,
                   PersonAgeID=personAgeID,
                   HousingStatusID=housingStatusID,
                   OccupationID=occupationID,
                   IncomeLevelID=incomeLevelID,
                   EducationID=educationID,
                   LanguageID=languageID, 
                   ArrestLocationLatitude=arrestLocationLatitude, 
                   ArrestLocationLongitude=arrestLocationLongitude,
                   BookingLengthOfStay=bookingLengthOfStay,
                   LastDaysAgo=lastDaysAgo
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

createJailEpisodesForBooking <- function(bookingId, bookingLengthOfStay, lastDaysAgo) {
  chargeTypeLength = length(ChargeType$ChargeTypeID)
  
  if (180-lastDaysAgo + 1 >= bookingLengthOfStay){
    lengthOfStay<-bookingLengthOfStay:1;  
    daysAgo<-lastDaysAgo:(lastDaysAgo + length(lengthOfStay) -1)
  }
  else{
    daysAgo<-lastDaysAgo:180
    lengthOfStay<-bookingLengthOfStay:(bookingLengthOfStay - length(daysAgo) + 1)
  }
  bind_rows(Map(function(daysAgo, lengthOfStay) {
    data.frame(BookingID=c(bookingId), DaysAgo=c(daysAgo), LengthOfStay=c(lengthOfStay))
  }, daysAgo, lengthOfStay))
}

createBehaviorHealthAssessment <- function(personId) {

  behaviorHealthTypeLength = length(BehaviorHealthType$BehaviorHealthTypeID)
  
  behaviorHealthTypeCount <- rpois(n=1, lambda=1)
  # reasonable limit...no more than five different types with one incident
  if (behaviorHealthTypeCount > behaviorHealthTypeLength) {
    behaviorHealthTypeCount <- behaviorHealthTypeLength
  }
  if (behaviorHealthTypeCount == 0) {
    behaviorHealthTypeCount <- 1
  }
  types <- sample(1:behaviorHealthTypeLength, size=behaviorHealthTypeCount, prob=c(rep(1/behaviorHealthTypeLength, behaviorHealthTypeLength)))

  bind_rows(Map(function(typeID) {
    data.frame(PersonID=c(personId), BehaviorHealthTypeID=c(typeID))
  }, types))

}

getBookingWithMentalProblems<-function(booking, mentalHealthProblemRate){
  bookingIdWithMentalProblems<-sample(booking$BookingID, size=length(booking$BookingID)*mentalHealthProblemRate)
  bookingWithMentalProblems<-booking[booking$BookingID %in% bookingIdWithMentalProblems,]
}

booking <- buildBookingRow(bookingId)
Person<-join(Person, booking)
personTableRows<-booking[,c("PersonID", "RecidivistIndicator", "PersonAgeID", "PersonSexID", "PopulationTypeID", "PersonRaceID", "IncomeLevelID", "OccupationID", "EducationID", "LanguageID" )]
StagingPersonUniqueIdentifier<-personTableRows$PersonID
personTableRows<-cbind(personTableRows, StagingPersonUniqueIdentifier)

ChargeTypeAssociation <- bind_rows(Map(createChargeTypeAssociationForBooking, booking$BookingID))

jailEpisode<-bind_rows(Map(createJailEpisodesForBooking, booking$BookingID, booking$BookingLengthOfStay, booking$LastDaysAgo))
JailEpisodeID<-1:nrow(jailEpisode)
jailEpisode<-cbind(jailEpisode, JailEpisodeID)
jailEpisodeChargeType<-join(ChargeTypeAssociation, jailEpisode, by=NULL, type="left", match="all")
jailEpisodeChargeType<-select(jailEpisodeChargeType, -BookingID, -DaysAgo, -LengthOfStay)

jailEpisodeRows<-join(booking, jailEpisode, type="left", match="all")
jailEpisodeTableRows<-jailEpisodeRows[,
    c("JailEpisodeID", "JurisdictionID", "SendingAgency", "DaysAgo", "LengthOfStay",
      "CaseStatusID", "BondAmount", "BondTypeID", "PretrialStatusID", "PersonID", 
      "FacilityID","BedTypeID", "HousingStatusID", "ArrestLocationLatitude", "ArrestLocationLongitude" )]

dbWriteTable(adsConnection, "Person", personTableRows, append=TRUE, row.names=FALSE)
dbWriteTable(adsConnection, "JailEpisode", jailEpisodeTableRows, append=TRUE, row.names=FALSE)
dbWriteTable(adsConnection, "JailEpisodeChargeType", jailEpisodeChargeType, append=TRUE, row.names=FALSE)

femaleBooking<-booking %>% filter(PersonSexID==2)
femaleBookingWithMentalProblems<-getBookingWithMentalProblems(femaleBooking, .74)
femaleBehaviorAssessment<-bind_rows(Map(createBehaviorHealthAssessment, femaleBookingWithMentalProblems$PersonID))


maleBooking<-booking %>% filter(PersonSexID==1)
maleBookingWithMentalProblems<-getBookingWithMentalProblems(maleBooking, .59)
maleBehaviorAssessment<-bind_rows(Map(createBehaviorHealthAssessment, maleBookingWithMentalProblems$PersonID))

dbWriteTable(adsConnection, "BehaviorHealthAssessment", data.table(femaleBehaviorAssessment), append=TRUE, row.names=FALSE)
dbWriteTable(adsConnection, "BehaviorHealthAssessment", data.table(maleBehaviorAssessment), append=TRUE, row.names=FALSE)


dbDisconnect(adsConnection)
