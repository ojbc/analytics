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

STATE <- "VT"
demoBookingCount = 100
library(RMySQL)
library(data.table)
library(dplyr)
library(rgdal)
library(sp)
library(readr)
library(tidyr)
library(Hmisc)
library(stringr)
library(xlsx)

source("LoadDateTimeDimensionTables.R")
source("LoadCodeTables.R")

loadStartTime <- Sys.time()

#conn <- dbConnect(MySQL(), host="dw", dbname="ojbc_analytics_demo", username="root")
adsConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")

dbSendQuery(adsConnection, "set foreign_key_checks=0")

# clear out fact tables
#dbSendQuery(conn, "delete from Booking")

# clear out dimension tables
#dbSendQuery(conn, "delete from YesNo")

loadCodeTables("AnalyticsCodeTables.xlsx", adsConnection)
loadDateDimensionTable(adsConnection)
loadTimeDimensionTable(adsConnection)

# extremely simplifies the person test data
personID <- 1:demoBookingCount
Person <- data.table(PersonID=personID, StagingPersonUniqueIdentifier=as.character(personID))
dbWriteTable(adsConnection, "Person", Person, append=TRUE, row.names=FALSE)

#Create Booking test data
bookingId<-1:demoBookingCount
recidivismBookingID <- sample(bookingId, size = length(bookingId)*.49)

n <- length(bookingId)
bookingDate <- runif(n=n, min=0, max=364) + as.Date("2013-01-01")
bookingDateID <- format(bookingDate, DATE_ID_FORMAT)

hours <- sample(0:23, size=n, replace=TRUE)
minutes <- sample(0:59, size=n, replace=TRUE)
seconds <- sample(0:59, size=n, replace=TRUE)
bookingTimeID <- makeTimeID(hours, minutes, seconds)

jurisdictionID <- sample(Jurisdiction$JurisdictionID, size=n, replace=TRUE)
sendingAgencyID <-sample(Agency$AgencyID, size=n, replace=TRUE)
caseStatusID<-sample(CaseStatus$StatusID, size=n, replace=TRUE)

recidivistIndicator<-sample(1:2, size=n, replace=TRUE, prob=c(0.49,0.51))

# Making up bond amount and bond amount probs. 
bondAmountList<-c(0, 300, 500,2500, 10000, 20000, 40000, 50000, 100000, 500000, 503000, 
                  550000, 660000,750000, 800000, 900000, 1000000)
bondAmountProbs<-c(.30, .20, .10, .10, .08, .07, .03, .02, .02, .02, .01, .01, .01, .01, .01, .005, .005)
bondAmount<-sample(bondAmountList, size=n, replace=TRUE, prob=bondAmountProbs)

bondTypeID<-sample(BondType$BondTypeID, size=n, replace=TRUE, prob=c(.10, .66, .04, .20))

pretrialStatusID<-sample(PretrialStatus$PretrialStatusID, size=n, replace=TRUE)

defendantID<-sample(Person$PersonID, size=n, replace=TRUE)

# need to make release date. currently set it to be the same as supervisionReleaseDate. And 
# need to populate more dateID to be able to have large lenght of stay. 
supervisionReleaseDate<-runif(n=n, min=10, max=364) + bookingDate
supervisionReleaseDateID <- format(supervisionReleaseDate, DATE_ID_FORMAT)

bedTypeID<-sample(BedType$BedTypeID, size=n, replace=TRUE)
sexID <- sample(1:3, size=n, replace=TRUE, prob=c(.91, .089, .001))
raceID<-sample(PersonRace$PersonRaceID, size=n, replace=TRUE, prob=c(.009, .43, .35, .06,.15, .001))
populationTypeID<-sample(PopulationType$PopulationTypeID, size=n, replace=TRUE, prob=c(.68, .32))
personAgeProbs<-c(.0025,.0025, .0030,.0031,.014,.014,.014,.015,.026,.026,.026,.026, .026,.036,.036,.036,.036, .036,.0366,.0366,.0366,
                  .0366,.0366,.0304,.0304, .0304,.0304,.0304,.0218, .0218, .0218,.0218,.0218,.177, 0)
personAgeID<-sample(PersonAge$PersonAgeID, size=n, replace=TRUE, prob=personAgeProbs)

df <- data.frame(BookingID=bookingId,
                 JurisdictionID=jurisdictionID,
                 SendingAgency=sendingAgencyID,
                 BookingDate=as.integer(bookingDateID),
                 TimeID=bookingTimeID,
                 BookingCaseNumber=as.character(bookingId),
                 StagingRecordID=bookingId,
                 CaseStatusID=caseStatusID,
                 RecidivistIndicator=recidivistIndicator,
                 BondAmount=bondAmount,
                 BondTypeID=bondTypeID,
                 PretrialStatusID=pretrialStatusID,
                 DefendantID=defendantID,
                 DententionStartDate=as.integer(bookingDateID),
                 ReleaseDate=as.integer(supervisionReleaseDateID),
                 SupervisionReleaseDate=as.integer(supervisionReleaseDateID), 
                 BedTypeID=bedTypeID,
                 PopulationTypeID=populationTypeID,
                 PersonSexID=sexID,
                 PersonRaceID=raceID,
                 PersonAgeID=personAgeID
                 )

dbWriteTable(adsConnection, "Booking", df, append=TRUE, row.names=FALSE)

dbDisconnect(adsConnection)
