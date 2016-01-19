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
source("LoadDemographicsDimensionTables.R")

loadStartTime <- Sys.time()

#conn <- dbConnect(MySQL(), host="dw", dbname="ojbc_analytics_demo", username="root")
conn <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")

dbSendQuery(conn, "set foreign_key_checks=0")

# clear out fact tables
#dbSendQuery(conn, "delete from Booking")

# clear out dimension tables
#dbSendQuery(conn, "delete from YesNo")

yesno <- c("Yes","No")
YesNo <- data.table(YesNoID=1:length(yesno), YesNoDescription=yesno)
dbWriteTable(conn, "YesNo", YesNo, append=TRUE, row.names=FALSE)

agencyID <- 1:11
agencyName <- paste("Agency", agencyID)
agencyName[11] <- "Unknown"
Agency <- data.table(AgencyID=agencyID, AgencyName=agencyName)
dbWriteTable(conn, "Agency", Agency, append=TRUE, row.names=FALSE)

bedTypes <- c("Normal","Special")
BedType <- data.table(BedTypeID=1:length(bedTypes), BedType=bedTypes)
dbSendQuery(conn, "delete from BedType")
dbWriteTable(conn, "BedType", BedType, append=TRUE, row.names=FALSE)

bondTypes <- c("Cash","Surety")
BondType <- data.table(BondTypeID=1:length(bondTypes), BondType=bondTypes)
dbSendQuery(conn, "delete from BondType")
dbWriteTable(conn, "BondType", BondType, append=TRUE, row.names=FALSE)

pretrialStatuses <- c("Jail","SRP", "Cash", "Surety", "PR")
PretrialStatus <- data.table(PretrialStatusID=1:length(pretrialStatuses), PretrialStatus=pretrialStatuses)
dbSendQuery(conn, "delete from PretrialStatus")
dbWriteTable(conn, "PretrialStatus", PretrialStatus, append=TRUE, row.names=FALSE)

behaviorHealthTypes <- c("Discorder 1","Discorder 2", "Discorder 3", "Illness 1", "Illness 2")
BehaviorHealthType <- data.table(BehaviorHealthTypeID=1:length(behaviorHealthTypes), BehaviorHealthDescription=behaviorHealthTypes)
dbSendQuery(conn, "delete from BehaviorHealthType")
dbWriteTable(conn, "BehaviorHealthType", BehaviorHealthType, append=TRUE, row.names=FALSE)

populationTypes <- c("Normal","Target")
PopulationType <- data.table(PopulationTypeID=1:length(populationTypes), PopulationType=populationTypes)
dbSendQuery(conn, "delete from PopulationType")
dbWriteTable(conn, "PopulationType", PopulationType, append=TRUE, row.names=FALSE)

chargeTypeID <- 1:5
chargeType <- paste("Charge", chargeTypeID)
ChargeType <- data.table(ChargeTypeID=chargeTypeID, ChargeType=chargeType)
dbWriteTable(conn, "ChargeType", ChargeType, append=TRUE, row.names=FALSE)

caseStatusID <- 1:5
caseStatus <- paste("Status", caseStatusID)
CaseStatus <- data.table(StatusID=caseStatusID, Status=caseStatus)
dbWriteTable(conn, "CaseStatus", CaseStatus, append=TRUE, row.names=FALSE)

jurisdictionID <- 1:5
jurisdiction <- paste("Jurisdiction", jurisdictionID)
Jurisdiction <- data.table(JurisdictionID=jurisdictionID, JurisdictionName=jurisdiction)
dbWriteTable(conn, "Jurisdiction", Jurisdiction, append=TRUE, row.names=FALSE)

PersonSex <- loadSexDimensionTable(conn)
PersonRace <- loadRaceDimensionTable(conn)

sexes <- PersonSex$PersonSexDescription
races <- PersonRace$PersonRaceDescription

loadDateDimensionTable(conn)
loadTimeDimensionTable(conn)
loadAgeDimensionTables(conn)

dbDisconnect(conn)
