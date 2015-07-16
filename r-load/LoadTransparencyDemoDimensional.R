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

# Load demo/dummy data for police transparency analytics

INCIDENT_COUNT = 12000
OFFICER_COUNT = 150
COMPLAINT_COUNT = 1000

library(RMySQL)
library(data.table)
library(dplyr)

conn <- dbConnect(MySQL(), host="localhost", dbname="ojbc_transp_analytics_demo", username="root")

source("LoadDateTimeDimensionTables.R")
source("LoadDemographicsDimensionTables.R")
source("LoadIncidentTypeDimensionTables.R")

loadDateDimensionTable(conn)
loadTimeDimensionTable(conn)
sexCount <- length(loadSexDimensionTable(conn))
raceCount <- length(loadRaceDimensionTable(conn))
loadAgeDimensionTables(conn)
incidentTypeCount <- length(loadIncidentTypeDimensionTable(conn))

useOfForceType <- c("Firearm", "Taser", "OC Spray", "Impact", "Personal", "K9", "Other", "None")
UseOfForceType <- data.table(UseOfForceTypeID=1:length(useOfForceType), UseOfForceTypeDescription=useOfForceType)
dbSendQuery(conn, "delete from UseOfForceType")
dbWriteTable(conn, "UseOfForceType", UseOfForceType, append=TRUE, row.names=FALSE)
useOfForceProbs <- c(.06,.08,.02,.06,.07,.03,.02)
useOfForceProbs <- c(useOfForceProbs, (1-sum(useOfForceProbs)))

complaintType <- c("Violation of Rules", "Integrity/Ethics", "Professionalism", "Conformance to Law", "Use of Force", "Other")
ComplaintType <- data.table(ComplaintTypeID=1:length(complaintType), ComplaintTypeDescription=complaintType)
dbSendQuery(conn, "delete from ComplaintType")
dbWriteTable(conn, "ComplaintType", ComplaintType, append=TRUE, row.names=FALSE)

complaintDispositionType <- c("Not Sustained (Inconclusive)", "Not Sustained (Lawful and Proper)", "Not Sustained (Training Referral)", "Sustained")
ComplaintDispositionType <- data.table(ComplaintDispositionTypeID=1:length(complaintDispositionType), ComplaintDispositionTypeDescription=complaintDispositionType)
dbSendQuery(conn, "delete from ComplaintDispositionType")
dbWriteTable(conn, "ComplaintDispositionType", ComplaintDispositionType, append=TRUE, row.names=FALSE)

agency <- c("Seattle PD")
Agency <- data.table(AgencyID=1:length(agency), AgencyName=agency)
dbSendQuery(conn, "delete from Agency")
dbWriteTable(conn, "Agency", Agency, append=TRUE, row.names=FALSE)

unit <- c("North Precinct", "South Precinct", "East Precinct", "West Precinct", "Criminal Investigations", "Special Operations")
unitID <- 1:length(unit)
Unit <- data.table(UnitID=unitID, UnitName=unit, AgencyID=rep(1, length(unit)))
dbSendQuery(conn, "delete from Unit")
dbWriteTable(conn, "Unit", Unit, append=TRUE, row.names=FALSE)

unitProbs <- c(.25, .15, .3, .1, .1, .1)

officerIDs <- 1:OFFICER_COUNT
Officer <- data.table(OfficerID=officerIDs, UnitID=sample(unitID, OFFICER_COUNT, replace = T, prob = unitProbs))
dbSendQuery(conn, "delete from Officer")
dbWriteTable(conn, "Officer", Officer, append=TRUE, row.names=FALSE)

generateRandomProbabilitiesWithUnknown <- function(n, unknownProb) {
  unknownProb <- runif(1, max=unknownProb*1.2, min=unknownProb*.8)
  remainingCumulativeProb <- 1 - unknownProb
  probs <- vector()
  for (i in 1:(n-2)) {
    probs <- append(probs, runif(1, max=remainingCumulativeProb))
    remainingCumulativeProb <- 1 - (sum(probs) + unknownProb)
  }
  probs <- append(probs, (1 - (sum(probs) + unknownProb)))
  append(probs, unknownProb)
}

Incident <- data.table(IncidentID=1:INCIDENT_COUNT,
                       DateID = format(runif(n=INCIDENT_COUNT, min=0, max=364) + as.Date("2013-01-01"), DATE_ID_FORMAT),
                       TimeID = makeTimeID(sample(0:23, size=INCIDENT_COUNT, replace=TRUE),
                                            sample(0:59, size=INCIDENT_COUNT, replace=TRUE),
                                            sample(0:59, size=INCIDENT_COUNT, replace=TRUE)),
                       IncidentTypeID = sample(incidentTypeCount, size=INCIDENT_COUNT, replace=TRUE,
                                               prob=generateRandomProbabilitiesWithUnknown(incidentTypeCount, .15))
)
dbSendQuery(conn, "delete from Incident")
dbWriteTable(conn, "Incident", Incident, append=TRUE, row.names=FALSE)

incidentSubjectInvolvementCount <- rpois(INCIDENT_COUNT, .8) + 1

involvedIncidentIDs <- vector()

for (incidentID in 1:INCIDENT_COUNT) {
  involveCount <- incidentSubjectInvolvementCount[incidentID]
  for (i in 1:involveCount) {
    involvedIncidentIDs <- append(involvedIncidentIDs, incidentID)
  }
}

involvedCount <- length(involvedIncidentIDs)
raceProbs = generateRandomProbabilitiesWithUnknown(raceCount, .2)
sexProbs = generateRandomProbabilitiesWithUnknown(sexCount, .1)
ageID <- rchisq(n=involvedCount, df=23)
for (i in 1:length(ageID)) {if (ageID[i] < 16) ageID[i] <- runif(min=16, max=23, n=1)}
Subject <- data.table(SubjectID=1:involvedCount,
                      PersonRaceID=sample(1:raceCount, size=involvedCount, prob=raceProbs, replace=T),
                      PersonAgeID=ageID,
                      PersonSexID=sample(1:sexCount, size=involvedCount, prob=sexProbs, replace=T))
dbSendQuery(conn, "delete from Subject")
dbWriteTable(conn, "Subject", Subject, append=TRUE, row.names=FALSE)

IncidentSubjectInvolvement <- data.table(IncidentSubjectInvolvementID=1:involvedCount,
                                         IncidentID=involvedIncidentIDs,
                                         SubjectID=1:involvedCount)
dbSendQuery(conn, "delete from IncidentSubjectInvolvement")
dbWriteTable(conn, "IncidentSubjectInvolvement", IncidentSubjectInvolvement, append=TRUE, row.names=FALSE)

incidentOfficerInvolvementCount <- rpois(INCIDENT_COUNT, .7) + 1

involvedIncidentIDs <- vector()

for (incidentID in 1:INCIDENT_COUNT) {
  involveCount <- incidentOfficerInvolvementCount[incidentID]
  for (i in 1:involveCount) {
    involvedIncidentIDs <- append(involvedIncidentIDs, incidentID)
  }
}

involvedOfficerIDs <- sample(officerIDs, size=length(involvedIncidentIDs), replace=T)

OfficerIncidentInvolvement <- data.table(OfficerIncidentInvolvementID=1:length(involvedIncidentIDs),
                                         IncidentID=involvedIncidentIDs,
                                         OfficerID=involvedOfficerIDs,
                                         UseOfForceTypeID=sample(length(useOfForceType), size=length(involvedIncidentIDs), replace=TRUE,
                                                                 useOfForceProbs))

dbSendQuery(conn, "delete from OfficerIncidentInvolvement")
dbWriteTable(conn, "OfficerIncidentInvolvement", OfficerIncidentInvolvement, append=TRUE, row.names=FALSE)

Complaint <- data.table(ComplaintID=1:COMPLAINT_COUNT,
                       DateID = format(runif(n=COMPLAINT_COUNT, min=0, max=364) + as.Date("2013-01-01"), DATE_ID_FORMAT),
                       OfficerID = sample(officerIDs, size=COMPLAINT_COUNT, replace=TRUE),
                       ComplaintTypeID = sample(length(complaintType), size=COMPLAINT_COUNT, replace=TRUE,
                                               prob=generateRandomProbabilitiesWithUnknown(length(complaintType), .15)),
                       ComplaintDispositionTypeID = sample(length(complaintDispositionType), size=COMPLAINT_COUNT, replace=TRUE)
)
dbSendQuery(conn, "delete from Complaint")
dbWriteTable(conn, "Complaint", Complaint, append=TRUE, row.names=FALSE)

dbDisconnect(conn)
