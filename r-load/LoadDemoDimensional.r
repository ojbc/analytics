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

demoArresteeCount = 12000
demoIncidentCount = 25000

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
source("LoadIncidentTypeDimensionTables.R")
source("LoadCountyPopulationData.R")

state_shp <- readOGR("/opt/data/Shapefiles/tl_2014_us_state", "tl_2014_us_state")
county_shp = readOGR("/opt/data/Shapefiles/tl_2014_us_county/", "tl_2014_us_county")

state_df <- state_shp@data
stateFips <- as.character(filter(state_df, STUSPS==STATE)$STATEFP)

county_df <- arrange(filter(county_shp@data, STATEFP==stateFips), GEOID)

#conn <- dbConnect(MySQL(), host="dw", dbname="ojbc_analytics_demo", username="root")
conn <- dbConnect(MySQL(), host="localhost", dbname="ojbc_analytics_demo", username="root")

# clear out fact tables
dbSendQuery(conn, "delete from IncidentTypeAssociation")
dbSendQuery(conn, "delete from Incident")
dbSendQuery(conn, "delete from Arrest")
dbSendQuery(conn, "delete from Charge")
dbSendQuery(conn, "delete from PretrialAssessedNeed")
dbSendQuery(conn, "delete from PretrialServiceParticipation")
dbSendQuery(conn, "delete from Population")

# clear out dimension tables
dbSendQuery(conn, "delete from Agency")
dbSendQuery(conn, "delete from County")
dbSendQuery(conn, "delete from Disposition")
dbSendQuery(conn, "delete from OffenseType")
dbSendQuery(conn, "delete from DispositionOffenseType")
dbSendQuery(conn, "delete from PretrialService")
dbSendQuery(conn, "delete from RiskScore")
dbSendQuery(conn, "delete from YesNo")
dbSendQuery(conn, "delete from AssessedNeed")
dbSendQuery(conn, "delete from ReadyCashOffenseCategory")
dbSendQuery(conn, "delete from Town")

yesno <- c("Yes","No")
YesNo <- data.table(YesNoID=1:length(yesno), YesNoDescription=yesno)
dbWriteTable(conn, "YesNo", YesNo, append=TRUE, row.names=FALSE)

countyCount <- length(county_df$GEOID)

regionID <- c(1:countyCount, (countyCount+1))
regionDescription <- c(paste0("Region ", 1:countyCount), "Unknown")
Region <- data.table(RegionID=regionID, RegionDescription=regionDescription)
dbWriteTable(conn, "Region", Region, append=TRUE, row.names=FALSE)

countyID <- c(as.character(county_df$GEOID), "99999")
countyName <- c(as.character(county_df$NAME), "Unknown")
regionID <- c(sample(1:countyCount, replace = TRUE), countyCount+1)
County <- data.table(CountyID=countyID, CountyName=countyName, RegionID=regionID)
dbWriteTable(conn, "County", County, append=TRUE, row.names=FALSE)

PersonSex <- loadSexDimensionTable(conn)
PersonRace <- loadRaceDimensionTable(conn)

sexes <- PersonSex$PersonSexDescription
races <- PersonRace$PersonRaceDescription

popData <- data.table(bind_rows(
  getCountyPopulationData("/opt/data/CC-EST2012-Alldata.csv", 2012, stateFips),
  getCountyPopulationData("/opt/data/CC-EST2013-Alldata.csv", 2013, stateFips),
  getCountyPopulationData("/opt/data/CC-EST2014-Alldata.csv", 2014, stateFips)))
dbWriteTable(conn, "Population", popData, append=TRUE, row.names=FALSE)

getCountyProbs <- function() {
  countyPopData <- summarise(group_by(popData, CountyID), totalPop=sum(PopulationCount))
  counties <- countyPopData$totalPop
  countyProb <- (.9*counties/sum(counties))  
  c(countyProb, .1)
}

raceProbs <- as.vector(wtd.table(popData$PersonRaceID, weights = popData$PopulationCount)$sum.of.weights)
raceProbs <- append(raceProbs, .1*sum(raceProbs), after=match("UNKNOWN", races)-1)
raceProbs <- raceProbs/sum(raceProbs)

readyCashCategory <- c("Larceny-Other", "Burglary", "Theft From Motor Vehicle", "Theft from Building", "Shoplifting", "None")
ReadyCashOffenseCategory <- data.table(ReadyCashOffenseCategoryID=1:length(readyCashCategory), ReadyCashOffenseCategoryDescription=readyCashCategory)
dbWriteTable(conn, "ReadyCashOffenseCategory", ReadyCashOffenseCategory, append=TRUE, row.names=FALSE)

OffenseType <- read.xlsx2("OffenseType.xlsx", sheetIndex=1)
OffenseType <- select(
  mutate(OffenseType, ReadyCashOffenseCategoryID=match(ReadyCashOffenseCategory, readyCashCategory, nomatch=match("None", readyCashCategory))),
  -ReadyCashOffenseCategory)

dbWriteTable(conn, "OffenseType", OffenseType, append=TRUE, row.names=FALSE)

DispositionOffenseType <- select(OffenseType, DispositionOffenseTypeID=OffenseTypeID, DispositionOffenseTypeDescription=OffenseDescription)
dbWriteTable(conn, "DispositionOffenseType", DispositionOffenseType, append=TRUE, row.names=FALSE)

serviceID <- 1:7
serviceDescription <- paste("service", serviceID)
serviceDescription[6] <- "unknown"
serviceDescription[7] <- "none"
isParticipant <- serviceDescription
isParticipant[isParticipant=="none"] <- "no"
isParticipant[1:5] <- "yes"
PretrialService <- data.table(PretrialServiceID=serviceID, PretrialServiceDescription=serviceDescription, IsParticipant=isParticipant)
dbWriteTable(conn, "PretrialService", PretrialService, append=TRUE, row.names=FALSE)

serviceProbs <- c(runif(n=length(serviceID)-2, min=0, max=.9), .05, .05)

riskScoreID <- 1:4
riskScoreDescription = c("High","Medium","Low","Unknown")
RiskScore <- data.table(RiskScoreID=riskScoreID, RiskScoreDescription=riskScoreDescription)
dbWriteTable(conn, "RiskScore", RiskScore, append=TRUE, row.names=FALSE)

townID <- 1:10
townDescription = paste0("Town ", townID)
Town <- data.table(TownID=townID, TownDescription=townDescription)
dbWriteTable(conn, "Town", Town, append=TRUE, row.names=FALSE)

dispoID <- 1:4
dispoDescription <- c("Conviction","Acquittal","Declined Prosecution","Unknown")
isConviction <- c("Y","N","N","N")
Disposition <- data.table(DispositionID=dispoID, DispositionDescription=dispoDescription, IsConviction=isConviction)
dbWriteTable(conn, "Disposition", Disposition, append=TRUE, row.names=FALSE)

agencyID <- 1:11
agencyName <- paste("Agency", agencyID)
agencyName[11] <- "Unknown"
Agency <- data.table(AgencyID=agencyID, AgencyName=agencyName)
dbWriteTable(conn, "Agency", Agency, append=TRUE, row.names=FALSE)

loadDateDimensionTable(conn)
loadTimeDimensionTable(conn)
loadAgeDimensionTables(conn)
loadIncidentCategoryDimensionTable(conn)
IncidentType <- loadIncidentTypeDimensionTable(conn)

getRandomCoordsInCounty <- function(countyId) {
  if (countyId != "99999") {
    l_county_shp <- subset(county_shp, GEOID == countyId)
    df <- as.data.frame(coordinates(spsample(x=l_county_shp, n = 1, type = "regular", iter=50)))
    df$countyId <- countyId
    return(select(mutate(df[1,], lat=x2, long=x1, CountyId=countyId), CountyId, lat, long))
  }
  data.frame(CountyId=countyId, lat=NA, long=NA)
}

getRandomCoordsInCounties <- function(countyIds) {
  bind_rows(Map(getRandomCoordsInCounty, countyIds))
}

#
# Arrest
# 

arrestID <- 1:demoArresteeCount

twoTimeArrestID <- sample(arrestID, size = length(arrestID)*.64)
threeTimeArrestID <- sample(twoTimeArrestID, size = length(twoTimeArrestID)*.56)
fourTimeArrestID <- sample(threeTimeArrestID, size = length(threeTimeArrestID)*.68)
fiveTimeArrestID <- sample(fourTimeArrestID, size = length(fourTimeArrestID)*.49)

buildArrestRow <- function(ids) {
  
  n <- length(ids)
  
  date <- runif(n=n, min=0, max=364) + as.Date("2013-01-01")
  dateID <- format(date, DATE_ID_FORMAT)
  
  hours <- sample(0:23, size=n, replace=TRUE)
  minutes <- sample(0:59, size=n, replace=TRUE)
  seconds <- sample(0:59, size=n, replace=TRUE)
  timeID <- makeTimeID(hours, minutes, seconds)
  
  raceTypeID <- sample(1:5, size=n, replace=TRUE, prob=raceProbs)
  
  ageID <- rchisq(n=n, df=23)
  for (i in 1:length(ageID)) {if (ageID[i] < 16) ageID[i] <- runif(min=16, max=23, n=1)}
  
  sexID <- sample(1:3, size=n, replace=TRUE, prob=c(.68, .29, .03))
  
  countyProb <- getCountyProbs()
  
  arresteeCountyID <- sample(countyID, size=n, replace=TRUE, prob=countyProb)
  arrestingAgencyID <- sample(agencyID, size=n, replace=TRUE)
  drugRelated <- sample(1:2, size=n, replace=TRUE, prob=c(.35, .65))
  pretrialEligible <- sample(1:2, size=n, replace=TRUE, prob=c(.6, .4))
  
  coords <- getRandomCoordsInCounties(arresteeCountyID)
  ArrestLocationLatitude <- coords$lat
  ArrestLocationLongitude <- coords$long

  # Note: in the demo database, we don't populate the incident ID or staging person ID, because it's not really needed (it's just there
  # for loading from staging)
  
  df <- data.frame(ArrestID=ids,
             DateID=as.integer(dateID),
             TimeID=timeID,
             ArresteeRaceID=raceTypeID,
             ArresteeAgeID=as.integer(ageID),
             ArresteeSexID=sexID,
             CountyID=arresteeCountyID,
             ArrestingAgencyID=arrestingAgencyID,
             ArrestDrugRelated=drugRelated,
             ArrestLocationLatitude,
             ArrestLocationLongitude,
             PretrialServiceEligible=pretrialEligible,
             temp_date=date)
  
}

arrest <- buildArrestRow(arrestID)

buildRearrestRow <- function(id, index, arrestDataFrame, idLookupField, depth) {
  
  lookupId <- id
  for(i in 1:depth) lookupId <- arrestDataFrame[arrestDataFrame$ArrestID==lookupId, idLookupField]
  newRowDf <- arrestDataFrame[arrestDataFrame$ArrestID==lookupId, ]
  
  newRowDf$ArrestID <- nrow(arrestDataFrame) + index
  newRowDf$PriorArrestID <- lookupId
  
  daysSincePriorArrest <- rgamma(n=1, shape=15, scale=20)
  
  newRowDf$temp_date <- newRowDf$temp_date + daysSincePriorArrest
  newRowDf$DateID <- as.integer(format(newRowDf$temp_date, DATE_ID_FORMAT))
  
  hours <- sample(0:23, size=1, replace=TRUE)
  minutes <- sample(0:59, size=1, replace=TRUE)
  seconds <- sample(0:59, size=1, replace=TRUE)
  timeID <- makeTimeID(hours, minutes, seconds)
  
  newRowDf$TimeID <- timeID
  newRowDf$ArresteeAgeID <- as.integer((runif(1, 0, 364) + daysSincePriorArrest)/365) + newRowDf$ArresteeAgeID
  
  if (rbinom(prob=.15, n=1, size=1)) {
    # 15% of the time someone is arrested by a different agency in a different county
    newRowDf$CountyID <- sample(arrestDataFrame$CountyID, size=1)
    newRowDf$ArrestingAgencyID <- sample(arrestDataFrame$ArrestingAgencyID, size=1)
  }
  
  coords <- getRandomCoordsInCounty(as.character(newRowDf$CountyID))
  newRowDf$ArrestLocationLatitude <- coords[1,"lat"]
  newRowDf$ArrestLocationLongitude <- coords[1,"long"]
  
  newRowDf$SubsequentArrestID <- as.integer(NA)
  
  newRowDf
  
}

twoTimeArrest <- bind_rows(Map(buildRearrestRow, twoTimeArrestID, 1:length(twoTimeArrestID), MoreArgs=list(arrest, "ArrestID", 1)))
newArrest <- left_join(arrest, select(twoTimeArrest, SubsequentArrestID=ArrestID, ArrestID=PriorArrestID), by = c("ArrestID"))
arrest <- rbind(newArrest, select(twoTimeArrest, -(PriorArrestID)))

threeTimeArrest <- bind_rows(Map(buildRearrestRow, threeTimeArrestID, 1:length(threeTimeArrestID), MoreArgs=list(arrest, "SubsequentArrestID", 1)))
newArrest <- left_join(arrest, select(threeTimeArrest, SubsequentArrestID=ArrestID, ArrestID=PriorArrestID), by = c("ArrestID"))
newArrest$SubsequentArrestID <- newArrest$SubsequentArrestID.x
newArrest$SubsequentArrestID[!is.na(newArrest$SubsequentArrestID.y)] <- newArrest$SubsequentArrestID.y[!is.na(newArrest$SubsequentArrestID.y)]
newArrest <- select(newArrest, -contains('.'))
arrest <- rbind(newArrest, select(threeTimeArrest, -(PriorArrestID)))

fourTimeArrest <- bind_rows(Map(buildRearrestRow, fourTimeArrestID, 1:length(fourTimeArrestID), MoreArgs=list(arrest, "SubsequentArrestID", 2)))
newArrest <- left_join(arrest, select(fourTimeArrest, SubsequentArrestID=ArrestID, ArrestID=PriorArrestID), by = c("ArrestID"))
newArrest$SubsequentArrestID <- newArrest$SubsequentArrestID.x
newArrest$SubsequentArrestID[!is.na(newArrest$SubsequentArrestID.y)] <- newArrest$SubsequentArrestID.y[!is.na(newArrest$SubsequentArrestID.y)]
newArrest <- select(newArrest, -contains('.'))
arrest <- rbind(newArrest, select(fourTimeArrest, -(PriorArrestID)))

fiveTimeArrest <- bind_rows(Map(buildRearrestRow, fiveTimeArrestID, 1:length(fiveTimeArrestID), MoreArgs=list(arrest, "SubsequentArrestID", 3)))
newArrest <- left_join(arrest, select(fiveTimeArrest, SubsequentArrestID=ArrestID, ArrestID=PriorArrestID), by = c("ArrestID"))
newArrest$SubsequentArrestID <- newArrest$SubsequentArrestID.x
newArrest$SubsequentArrestID[!is.na(newArrest$SubsequentArrestID.y)] <- newArrest$SubsequentArrestID.y[!is.na(newArrest$SubsequentArrestID.y)]
newArrest <- select(newArrest, -contains('.'))
arrest <- rbind(newArrest, select(fiveTimeArrest, -(PriorArrestID)))

arrest <- select(arrest, -(temp_date))

dbWriteTable(conn, "Arrest", arrest, append=TRUE, row.names=FALSE)

#
# Charge
#

offenseID <- OffenseType$OffenseTypeID
offenseProbs <- runif(n=length(offenseID))

makeCharges <- function(arrestID) {
  getDispoOffense <- function(offenseTypeID) {
    dispoOffenseTypeID <- offenseTypeID
    if (rbinom(n=1, prob=.4, size=1)) {
      dispoOffenseTypeID <- sample(offenseID, size=1)
    }
    dispoOffenseTypeID
  }
  makeCharge <- function(arrestID) {
    offenseTypeID <- sample(offenseID, size=1, prob = offenseProbs)
    dispoOffenseTypeID <- getDispoOffense(offenseTypeID)
    filedOffenseTypeID <- getDispoOffense(dispoOffenseTypeID)
    arrestDispo <- sample(dispoID, size=1, prob=c(.3, .4, .4, .1))
    dateID <- as.character(arrest[arrest$ArrestID==arrestID, "DateID"])
    arrestDate <- as.Date(dateID, DATE_ID_FORMAT)
    DispositionDate <- arrestDate + runif(n=1, min=60, max=400)
    DispositionDateID <- format(DispositionDate, DATE_ID_FORMAT)
    fine <- NA
    days <- NA
    if (arrestDispo == 1) {
      fine <- sample(size=1, c(0, 1, 50, 100, 500, 1000, 10000), replace=TRUE, prob=c(.25, .2, .15, .15, .1, .075, .075))
      days <- sample(size=1, c(0, 30, 90, 364, 5*365, 10*365, 25*365), replace=TRUE, prob=c(.4, .2, .2, .1, .05, .025, .025))
      if (fine == 0 && days == 0) {
        fine = 10
      }
    }
    chargeID <- NA
    data.frame(ChargeID=chargeID, ArrestOffenseTypeID=offenseTypeID, ArrestID=arrestID, SentenceTermDays=days,
               SentenceFineAmount=fine, DispositionID=arrestDispo, DispositionDateID, RecidivismEligibilityDate=DispositionDate,
               FiledOffenseTypeID=filedOffenseTypeID, DispositionOffenseTypeID=dispoOffenseTypeID)
  }
  df <- makeCharge(arrestID)
  if (rbinom(n = 1, prob = .4, size = 1)) {
    df <- rbind(df, makeCharge(arrestID))
    if (rbinom(n = 1, prob = .3, size = 1)) {
      df <- rbind(df, makeCharge(arrestID))
    }
  }
  df
}

charges <- bind_rows(Map(makeCharges, arrest$ArrestID))
charges$ChargeID <- 1:nrow(charges)

InvolvedDrugDescriptions <- c("Marijuana", "Heroin", "Cocaine", "Other Narcotics", "None")
InvolvedDrug <- data.table(InvolvedDrugID=1:length(InvolvedDrugDescriptions), InvolvedDrugDescription=InvolvedDrugDescriptions)
dbWriteTable(conn, "InvolvedDrug", InvolvedDrug, append=TRUE, row.names=FALSE)

Charge <- as.data.frame(charges)
Charge <- mutate(Charge, InvolvedDrugID=ifelse(
  ArrestOffenseTypeID %in% c(3560,3561,3562,3563,3564), match("Marijuana", InvolvedDrugDescriptions),
    ifelse(ArrestOffenseTypeID %in% c(3510,3511,3512,3513,3514), match("Heroin", InvolvedDrugDescriptions),
      ifelse(ArrestOffenseTypeID %in% c(3530,3531,3532,3533,3534), match("Cocaine", InvolvedDrugDescriptions),
        ifelse(ArrestOffenseTypeID >= 3500 & ArrestOffenseTypeID < 3600, match("Other Narcotics", InvolvedDrugDescriptions),
          match("None", InvolvedDrugDescriptions))))))
dbWriteTable(conn, "Charge", Charge, append=TRUE, row.names=FALSE)

#
# PretrialParticipation
#

riskScoreProbs <- runif(n=length(riskScoreID))

makePretrialParticipation <- function(arrestID) {
  PretrialServiceID <- sample(serviceID, size=1, prob=serviceProbs)
  countyProb <- getCountyProbs()
  CountyID <- sample(countyID, size=1, prob=countyProb)
  dateID <- as.character(arrest[arrest$ArrestID==arrestID, "DateID"])
  arrestDate <- as.Date(dateID, DATE_ID_FORMAT)
  IntakeDate <- arrestDate + runif(n=1, min=1, max=30)
  IntakeDateID <- format(IntakeDate, DATE_ID_FORMAT)
  RiskScoreID <- sample(riskScoreID, size=1, prob=riskScoreProbs)
  PretrialServiceParticipationID <- NA
  data.frame(PretrialServiceParticipationID, PretrialServiceID, CountyID, IntakeDateID, RiskScoreID, ArrestID=arrestID)
}
pretrialParticipants <- sample(arrest$ArrestID, nrow(arrest)*.75)
PretrialServiceParticipation <- bind_rows(Map(makePretrialParticipation, pretrialParticipants))
PretrialServiceParticipation$PretrialServiceParticipationID <- 1:nrow(PretrialServiceParticipation)

need <- c("Need 1","Need 2", "Need 3", "Need 4", "Need 5", "None", "Unknown")
AssessedNeed <- data.table(AssessedNeedID=1:length(need), AssessedNeedDescription=need)
dbWriteTable(conn, "AssessedNeed", AssessedNeed, append=TRUE, row.names=FALSE)

makePretrialAssessedNeed <- function(pretrialServiceParticipationID) {
  needCount <- rpois(n=1, lambda=1)
  # note tight coupling to needs...ok for demo data script
  if (needCount > 5) {
    needCount <- 5
  }
  needs <- c(6)
  if (needCount > 0) {
    needs <- sample(1:5, size=needCount, prob=c(.2,.45,.1,.1,.15))
  }
  bind_rows(Map(function(needID) {
    data.frame(PretrialServiceParticipationID=c(pretrialServiceParticipationID), AssessedNeedID=c(needID))
  }, needs))
}

PretrialAssessedNeed <- bind_rows(Map(makePretrialAssessedNeed, PretrialServiceParticipation$PretrialServiceParticipationID))
PretrialAssessedNeed$PretrialAssessedNeedID <- 1:nrow(PretrialAssessedNeed)

dbWriteTable(conn, "PretrialServiceParticipation", as.data.frame(PretrialServiceParticipation), append=TRUE, row.names=FALSE)
dbWriteTable(conn, "PretrialAssessedNeed", as.data.frame(PretrialAssessedNeed), append=TRUE, row.names=FALSE)

#
# Incident
#

IncidentID <- 1:demoIncidentCount
DateID <- format(runif(n=demoIncidentCount, min=0, max=364) + as.Date("2013-01-01"), DATE_ID_FORMAT)
hours <- sample(0:23, size=demoIncidentCount, replace=TRUE)
minutes <- sample(0:59, size=demoIncidentCount, replace=TRUE)
seconds <- sample(0:59, size=demoIncidentCount, replace=TRUE)
TimeID <- makeTimeID(hours, minutes, seconds)
ReportingAgencyID=sample(agencyID, size=demoIncidentCount, replace=TRUE)
countyProb <- getCountyProbs()
CountyID <- sample(countyID, size=demoIncidentCount, prob=countyProb, replace = T)
coords <- getRandomCoordsInCounties(CountyID)
IncidentLocationLatitude <- coords$lat
IncidentLocationLongitude <- coords$long
IncidentNumber <- paste0("INC", IncidentID)
Town <- sample(townID, size=demoIncidentCount, replace=TRUE)
Street <- paste(1:demoIncidentCount, c("Main", "Maple", "Dorset", "Williston", "Oak", "Springfield", "Elm"))

Incident <- data.table(IncidentID, DateID, TimeID, ReportingAgencyID,
                       CountyID, IncidentLocationLatitude, IncidentLocationLongitude, IncidentLocationStreetAddress=Street,
                       TownID=Town, IncidentCaseNumber=IncidentNumber)

IncidentTypeAssociation <- bind_rows(Map(function(incidentID) {
  typeCount <- rpois(n=1, lambda=1)
  # reasonable limit...no more than five different types with one incident
  if (typeCount > 5) {
    typeCount <- 5
  }
  types <- c(6)
  if (typeCount == 0) {
    typeCount <- 1
  }
  types <- sample(1:7, size=typeCount, prob=c(rep(.16, 6), .04))
  bind_rows(Map(function(typeID) {
    data.frame(IncidentID=c(incidentID), IncidentTypeID=c(typeID))
  }, types))
}, Incident$IncidentID))
IncidentTypeAssociation$IncidentTypeAssociationID <- 1:nrow(IncidentTypeAssociation)

dbWriteTable(conn, "Incident", Incident, append=TRUE, row.names=FALSE)
dbWriteTable(conn, "IncidentTypeAssociation", data.table(IncidentTypeAssociation), append=TRUE, row.names=FALSE)

dbDisconnect(conn)
