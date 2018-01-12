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

defaultCodeTableSpreadsheetFile <- system.file("raw", "DimensionalCodeTables.xlsx", package=getPackageName())

#' @importFrom RMariaDB MariaDB
defaultStagingConnectionBuilder <- function() {
  stagingConnection <- dbConnect(MariaDB(), host="db", dbname="ojbc_booking_staging_demo", username="root")
  stagingConnection
}

#' @importFrom RMariaDB MariaDB
defaultDimensionalConnectionBuilder <- function() {
  adsConnection <- dbConnect(MariaDB(), host="db", dbname="ojbc_booking_dimensional_demo", username="root")
  adsConnection
}

#' @importFrom openxlsx read.xlsx
defaultUnitTextValueConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['RespondingUnitType']]
  ret <- match(textValues, ct$RespondingUnitTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

#' @importFrom openxlsx read.xlsx
defaultPendingCriminalChargesTextValueConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['PendingCriminalChargesType']]
  ret <- match(textValues, ct$PendingCriminalChargesTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

#' @importFrom openxlsx read.xlsx
defaultDispositionLocationTextValueConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['DispositionLocationType']]
  ret <- match(textValues, ct$DispositionLocationTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

#' @importFrom openxlsx read.xlsx
defaultCallNatureTextValueConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['CallNatureType']]
  ret <- match(textValues, ct$CallNatureTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

#' @importFrom openxlsx read.xlsx
defaultEducationTextValueConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['EducationLevelType']]
  ret <- match(textValues, ct$EducationLevelTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

defaultOccupationTextValueConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['OccupationType']]
  ret <- match(textValues, ct$OccupationTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

defaultDiagnosisTextValueConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['BehavioralHealthEvaluationType']]
  ret <- match(textValues, ct$BehavioralHealthEvaluationTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

defaultMedicationTextValueConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['MedicationType']]
  ret <- match(textValues, ct$MedicationTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

defaultDispositionTextConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['ChargeDispositionType']]
  ret <- match(textValues, ct$ChargeDispositionTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

defaultChargeCodeTextConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['ChargeType']]
  ret <- match(textValues, ct$ChargeTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

defaultProviderTextValueConverter <- function(codeTableList, textValues, unknownCodeTableValue) {
  ct <- codeTableList[['TreatmentProviderType']]
  ret <- match(textValues, ct$TreatmentProviderTypeDescription)
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

defaultChargeDispositionAggregator <- function(codeTableList, BookingChargeDispositionDataFrame, unknownCodeTableValue) {
  df <- BookingChargeDispositionDataFrame %>%
    group_by(BookingID) %>%
    summarize(dispo=min(ChargeDisposition)) %>%
    mutate(dispo=defaultDispositionTextConverter(codeTableList, dispo, unknownCodeTableValue))
  df$dispo
}

defaultPopulationTypeConverter <- function(adsConnection, unknownCodeTableValue, writeToDatabase) {
  if (writeToDatabase) {
    executeQuery(adsConnection, "update Person set PopulationTypeID=2")
    executeQuery(adsConnection, "update Person set PopulationTypeID=1  where PersonID in (select distinct PersonID from BehavioralHealthAssessment)")
  }
}

#' @importFrom DBI dbGetQuery
#' @importFrom lubridate ymd_hms
getLastLoadingTime <- function(adsConnection) {
  loadHistory <- dbGetQuery(adsConnection, "select LoadHistoryTimestamp from LoadHistory order by LoadHistoryTimestamp desc")
  loadTimes <- loadHistory$LoadHistoryTimestamp
  lastLoadTime <- as.POSIXct("1971-01-01 00:00:01")
  if (length(loadTimes) > 0) {
    lastLoadTime <- ymd_hms(loadTimes[1], tz=Sys.timezone())
  }
  lastLoadTime
}

formatDateTimeForSQL <- function(dateTime) {
  format.Date(dateTime, "%Y-%m-%d %H:%M:%S")
}

#' @importFrom DBI dbClearResult dbSendQuery
executeQuery <- function(conn, query) {
  dbClearResult(dbSendStatement(conn, query))
}

#' @importFrom DBI dbGetQuery
getQuery <- function(conn, query, printSQL=FALSE) {
  if (printSQL) {
    writeLines(paste0("dbGetQuery: ", query))
  }
  # suppressWarnings is necessary to avoid annoying type conversion warnings from the RMariaDB driver
  suppressWarnings(dbGetQuery(conn, query))
}

#' @import dplyr
updateLoadHistory <- function(adsConnection, stagingConnection, currentLoadTime) {

  df <- bind_rows(getQuery(stagingConnection, "select max(BookingTimestamp) as ttt from Booking"),
                  getQuery(stagingConnection, "select max(PersonTimestamp) as ttt from Person"),
                  getQuery(stagingConnection, "select max(CustodyReleaseTimestamp) as ttt from CustodyRelease"),
                  getQuery(stagingConnection, "select max(CustodyStatusChangeTimestamp) as ttt from CustodyStatusChange"))

  maxStagingTimestamp <- as.POSIXlt(max(na.omit(df$ttt)))

  executeQuery(adsConnection, paste0("insert into LoadHistory (LoadHistoryTimestamp, MostRecentStagingTimestamp) ",
                                     "values ('", formatDateTimeForSQL(currentLoadTime), "', '", formatDateTimeForSQL(maxStagingTimestamp), "')"))
  loadHistory <- getQuery(adsConnection, paste0("select LoadHistoryID from LoadHistory where LoadHistoryTimestamp='", formatDateTimeForSQL(currentLoadTime), "'"))
  loadHistoryID <- loadHistory$LoadHistoryID
  loadHistoryID
}

defaultCodeTableValueTranslator <- function(stagingValue, codeTableName, unknownCodeTableValue, codeTableList) {
  # obviously a very simplistic translation that just replaces NA with the unknown value, and otherwise just returns the staging value as the ads value
  ret <- ifelse(is.na(stagingValue), unknownCodeTableValue, stagingValue)
  if ('PersonRaceType' == codeTableName) {
    ret <- ifelse(is.na(stagingValue) | stagingValue == 7, unknownCodeTableValue, stagingValue)
  }
  ct <- codeTableList[[codeTableName]]
  ids <- ct[[paste0(codeTableName, "ID")]]
  mismatches <- !(ret %in% ids)
  if (any(mismatches)) {
    writeLines(paste0("Found ", length(which(mismatches)), " mismatches against code table ", codeTableName))
    writeLines(paste0("Values were: ", paste0(sort(unique(ret[mismatches]), collapse=","))))
    ret[mismatches] <- unknownCodeTableValue
  }
  as.integer(ret)
}

translateCodeTableValue <- function(codeTableValueTranslator, stagingValue, codeTableName, unknownCodeTableValue, codeTableList) {
  args <- list()
  args$stagingValue <- stagingValue
  args$codeTableName <- codeTableName
  args$unknownCodeTableValue <- unknownCodeTableValue
  args$codeTableList <- codeTableList
  do.call(codeTableValueTranslator, args)
}

#' @importFrom lubridate ddays
buildJailEpisodeTables <- function(stagingConnection, adsConnection, lastLoadTime, currentLoadTime, currentStagingDate, loadHistoryID,
                                   unknownCodeTableValue, noneCodeTableValue, chargeDispositionAggregator, codeTableList, codeTableValueTranslator) {

  buildTable <- function(StagingBookingDf, StagingBookingChargeDispositionDf, chargeDispositionAggregator, tableName) {

    totalRows <- nrow(StagingBookingDf)
    StagingBookingDf <- filter(StagingBookingDf, BookingDate <= currentStagingDate)
    writeLines(paste0("Removing ", (totalRows - nrow(StagingBookingDf)), " ", tableName, " rows due to future Booking Date"))

    JailEpisode <- StagingBookingDf %>%
      transmute(
        JailEpisodeID=BookingID,
        PersonID=PersonID,
        BookingNumber=BookingNumber,
        IsActive='Y',
        CaseStatusTypeID=unknownCodeTableValue,
        EpisodeStartDate=as.Date(BookingDate),
        EpisodeStartDateID=format(EpisodeStartDate, "%Y%m%d"),
        EpisodeEndDate=NA,
        EpisodeEndDateID=noneCodeTableValue,
        FacilityID=translateCodeTableValue(codeTableValueTranslator, FacilityID, "Facility", unknownCodeTableValue, codeTableList),
        SupervisionUnitTypeID=translateCodeTableValue(codeTableValueTranslator, SupervisionUnitTypeID, "SupervisionUnitType", unknownCodeTableValue, codeTableList),
        DaysAgo=as.integer((currentStagingDate - EpisodeStartDate) / ddays(1)),
        LengthOfStay=DaysAgo,
        SixMonthRebooking='N',
        OneYearRebooking='N',
        TwoYearRebooking='N',
        DaysSinceLastEpisode=NA,
        DaysUntilNextEpisode=NA,
        LoadHistoryID=loadHistoryID,
        StagingPK=pk)

    if (nrow(StagingBookingChargeDispositionDf)) {
      writeLines(paste0("Aggregating ", nrow(StagingBookingChargeDispositionDf), " charge disposition records under ", tableName))
      args <- list()
      args$codeTableList <- codeTableList
      args$BookingChargeDispositionDataFrame <- StagingBookingChargeDispositionDf
      args$unknownCodeTableValue <- unknownCodeTableValue
      JailEpisode$CaseStatusTypeID <- as.integer(do.call(chargeDispositionAggregator, args))
    }

    JailEpisode

  }

  ret <- list()

  Booking <- getQuery(stagingConnection, paste0("select BookingNumber, BookingID, PersonID, BookingDate, BookingID as pk, ",
                                                "FacilityID, SupervisionUnitTypeID, InmateJailResidentIndicator from Booking ",
                                                " where BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BookingChargeDisposition <- getQuery(stagingConnection, paste0("select Booking.BookingID, ChargeDisposition from ",
                                                                 "Booking left join BookingArrest on Booking.BookingID=BookingArrest.BookingID ",
                                                                 "left join BookingCharge on BookingArrest.BookingArrestID=BookingCharge.BookingArrestID ",
                                                                 "where BookingDate <= '", currentStagingDate, "' and BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  ret$JailEpisode <- buildTable(Booking, BookingChargeDisposition, chargeDispositionAggregator, 'JailEpisode') %>%
    select(-StagingPK)

  Booking <- getQuery(stagingConnection, paste0("select Booking.BookingNumber, CustodyStatusChange.BookingID, CustodyStatusChange.PersonID, CustodyStatusChange.BookingDate, ",
                                                "CustodyStatusChange.FacilityID, CustodyStatusChange.SupervisionUnitTypeID, CustodyStatusChangeID as pk, ",
                                                "CustodyStatusChange.InmateJailResidentIndicator from Booking, CustodyStatusChange ",
                                                "where Booking.BookingID=CustodyStatusChange.BookingID and ",
                                                "CustodyStatusChangeTimestamp > '", formatDateTimeForSQL(lastLoadTime), "' order by CustodyStatusChangeTimestamp"))

  # note that it is possible to have multiple CSC records for a single booking.  if this is the case, we take the most recent one.
  total <- nrow(Booking)
  Booking <- Booking %>% group_by(BookingID) %>% filter(row_number()==n()) %>% ungroup()
  writeLines(paste0("Removed ", total-nrow(Booking), " custody status change records that are replaced by a more recent custody status change"))

  BookingChargeDisposition <- getQuery(stagingConnection, paste0("select CustodyStatusChange.BookingID, ChargeDisposition, CustodyStatusChange.CustodyStatusChangeID from ",
                                                                 "Booking inner join CustodyStatusChange on Booking.BookingID=CustodyStatusChange.BookingID ",
                                                                 "left join CustodyStatusChangeArrest on CustodyStatusChange.CustodyStatusChangeID=CustodyStatusChangeArrest.CustodyStatusChangeID ",
                                                                 "left join CustodyStatusChangeCharge on CustodyStatusChangeArrest.CustodyStatusChangeArrestID=CustodyStatusChangeCharge.CustodyStatusChangeArrestID ",
                                                                 "where CustodyStatusChange.BookingDate <= '", currentStagingDate, "' and CustodyStatusChangeTimestamp > '", formatDateTimeForSQL(lastLoadTime), "' order by CustodyStatusChangeTimestamp"))

  total <- nrow(BookingChargeDisposition)
  BookingChargeDisposition <- BookingChargeDisposition %>%
    inner_join(Booking %>% select(pk), by=c("CustodyStatusChangeID"="pk")) %>%
    select(-CustodyStatusChangeID)
  writeLines(paste0("Removed ", total-nrow(BookingChargeDisposition), " custody status change charge dispo records that are replaced by a more recent custody status change"))

  ret$JailEpisodeEdits <- buildTable(Booking, BookingChargeDisposition, chargeDispositionAggregator, 'CustodyStatusChange')

  ret

}

#' @importFrom lubridate dyears
buildPersonTable <- function(stagingConnection, lastLoadTime, currentStagingDate, unknownCodeTableValue, educationTextValueConverter,
                             occupationTextValueConverter, codeTableList, codeTableValueTranslator) {

  selectStatement <- paste0("select Person.PersonID as PersonID, PersonUniqueIdentifier, PersonUniqueIdentifier2, PersonAgeAtEvent, PersonBirthDate, ",
                            "EducationLevel, Occupation, LanguageTypeID, PersonSexTypeID, PersonRaceTypeID, ",
                            "PersonEthnicityTypeID, MilitaryServiceStatusTypeID, DomicileStatusTypeID, ",
                            "ProgramEligibilityTypeID, WorkReleaseStatusTypeID, SexOffenderStatusTypeID")

  Person <- getQuery(stagingConnection, paste0(selectStatement, ", BookingDate as EventDate from Person, Booking ",
                                               "where Person.PersonID=Booking.PersonID"))

  PersonE <- getQuery(stagingConnection, paste0(selectStatement, ", BookingDate as EventDate from Person, CustodyStatusChange ",
                                                "where Person.PersonID=CustodyStatusChange.PersonID"))

  PersonI <- getQuery(stagingConnection, paste0(selectStatement, ", IncidentReportedDate as EventDate from Person, Incident ",
                                                "where Person.PersonID=Incident.PersonID"))

  Person <- Person %>% bind_rows(PersonE, PersonI) %>%
    transmute(PersonID=PersonID,
              StagingPersonUniqueIdentifier=PersonUniqueIdentifier,
              StagingPersonUniqueIdentifier2=PersonUniqueIdentifier2,
              LanguageTypeID=translateCodeTableValue(codeTableValueTranslator, LanguageTypeID, "LanguageType", unknownCodeTableValue, codeTableList),
              PersonSexTypeID=translateCodeTableValue(codeTableValueTranslator, PersonSexTypeID, "PersonSexType", unknownCodeTableValue, codeTableList),
              PersonRaceTypeID=translateCodeTableValue(codeTableValueTranslator, PersonRaceTypeID, "PersonRaceType", unknownCodeTableValue, codeTableList),
              PersonEthnicityTypeID=translateCodeTableValue(codeTableValueTranslator, PersonEthnicityTypeID, "PersonEthnicityType", unknownCodeTableValue, codeTableList),
              MilitaryServiceStatusTypeID=translateCodeTableValue(codeTableValueTranslator, MilitaryServiceStatusTypeID, "MilitaryServiceStatusType", unknownCodeTableValue, codeTableList),
              DomicileStatusTypeID=translateCodeTableValue(codeTableValueTranslator, DomicileStatusTypeID, "DomicileStatusType", unknownCodeTableValue, codeTableList),
              ProgramEligibilityTypeID=translateCodeTableValue(codeTableValueTranslator, ProgramEligibilityTypeID, "ProgramEligibilityType", unknownCodeTableValue, codeTableList),
              WorkReleaseStatusTypeID=translateCodeTableValue(codeTableValueTranslator, WorkReleaseStatusTypeID, "WorkReleaseStatusType", unknownCodeTableValue, codeTableList),
              SexOffenderStatusTypeID=translateCodeTableValue(codeTableValueTranslator, SexOffenderStatusTypeID, "SexOffenderStatusType", unknownCodeTableValue, codeTableList),
              Occupation=Occupation,
              EducationLevel=EducationLevel,
              PopulationTypeID=unknownCodeTableValue,
              EducationLevelTypeID=unknownCodeTableValue,
              OccupationTypeID=unknownCodeTableValue,
              PersonAgeTypeID=ifelse(is.na(EventDate) | PersonBirthDate > currentStagingDate, ifelse(is.na(PersonAgeAtEvent), unknownCodeTableValue, PersonAgeAtEvent),
                                     (EventDate - PersonBirthDate) / dyears(1))
    ) %>%
    mutate(PersonAgeTypeID=as.integer(ifelse(is.na(PersonAgeTypeID), unknownCodeTableValue, PersonAgeTypeID)))

  dups <- unique(Person$PersonID[duplicated(Person$PersonID)])
  dups <- length(dups)
  if (dups) {
    stop(paste0("Dimensional load failed.  ", dups, " duplicate PersonIDs found in Booking / CustodyStatusChange."))
  }

  if (nrow(Person)) {
    args <- list()
    args$codeTableList <- codeTableList
    args$textValues <- Person$EducationLevel
    args$unknownCodeTableValue <- unknownCodeTableValue
    Person$EducationLevelTypeID <- as.integer(do.call(educationTextValueConverter, args))
    args$textValues <- Person$Occupation
    args$unknownCodeTableValue <- unknownCodeTableValue
    Person$OccupationTypeID <- as.integer(do.call(occupationTextValueConverter, args))
  }

  Person %>% select(-EducationLevel, -Occupation)

}

buildArrestTables <- function(stagingConnection, adsConnection, lastLoadTime, currentStagingDate, unknownCodeTableValue, codeTableList, codeTableValueTranslator, jailEpisodeEditPKs) {

  buildTable <- function(parentBookingTable, arrestTable, baseArrestID) {

    Arrest <- getQuery(stagingConnection, paste0("select BookingDate, ", parentBookingTable, ".BookingID, ", arrestTable, "ID as pk, LocationLatitude, LocationLongitude, ArrestAgencyID, ",
                                                 arrestTable, ".", parentBookingTable, "ID as parentFK ",
                                                 "from (", arrestTable, " inner join ", parentBookingTable,
                                                 " on ", arrestTable, ".", parentBookingTable, "ID=", parentBookingTable, ".", parentBookingTable, "ID) ",
                                                 "left join Location on ", arrestTable, ".LocationID=Location.LocationID where ", parentBookingTable, ".BookingID is not null and ",
                                                 parentBookingTable, "Timestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

    totalRows <- nrow(Arrest)
    Arrest <- filter(Arrest, BookingDate <= currentStagingDate) %>% select(-BookingDate)
    writeLines(paste0("Removing ", (totalRows - nrow(Arrest)), " ", arrestTable, " rows due to future Booking Date for the arrest record's booking"))

    Arrest <- Arrest %>%
      transmute(JailEpisodeArrestID=as.integer(row_number() + baseArrestID),
                JailEpisodeID=BookingID,
                ArrestLocationLatitude=LocationLatitude,
                ArrestLocationLongitude=LocationLongitude,
                AgencyID=translateCodeTableValue(codeTableValueTranslator, ArrestAgencyID, "Agency", unknownCodeTableValue, codeTableList),
                StagingPK=pk, StagingParentFK=parentFK)

    Arrest

  }

  baseArrestID <- getQuery(adsConnection, "select max(JailEpisodeArrestID) as BaseID from JailEpisodeArrest")
  baseArrestID <- baseArrestID$BaseID
  baseArrestID <- ifelse(is.na(baseArrestID), 0, baseArrestID) + 1

  ret <- list()
  ret$Arrest <- buildTable('Booking', 'BookingArrest', baseArrestID)

  baseArrestID <- baseArrestID + nrow(ret$Arrest) + 1

  ret$ArrestEdits <- buildTable('CustodyStatusChange', 'CustodyStatusChangeArrest', baseArrestID)

  total <- nrow(ret$ArrestEdits)

  ret$ArrestEdits <- ret$ArrestEdits %>%
    filter(StagingParentFK %in% jailEpisodeEditPKs) %>%
    select(-StagingParentFK)

  writeLines(paste0("Removed ", total-nrow(ret$ArrestEdits), " arrest edits that are replaced by a more recent custody status change"))

  ret

}

buildChargeTables <- function(stagingConnection, adsConnection, lastLoadTime, currentStagingDate, unknownCodeTableValue,
                              chargeCodeTypeTextConverter, dispositionTextConverter, ArrestDf, ArrestEditsDf, codeTableList,
                              codeTableValueTranslator) {

  buildTable <- function(grandparentBookingTable, parentArrestTable, chargeTable, baseChargeID) {

    Charge <- getQuery(stagingConnection, paste0("select BookingDate, ",
                                                 chargeTable, "ID as pk, ", chargeTable, ".", parentArrestTable, "ID as ParentArrestID, ",
                                                 parentArrestTable, ".", grandparentBookingTable, "ID as GrandparentBookingRecordPK, ",
                                                 grandparentBookingTable, ".BookingID as GrandparentBookingID, ",
                                                 "ChargeCode, ChargeDisposition, AgencyID, BondTypeID, BondAmount, ChargeJurisdictionTypeID, ",
                                                 "BondStatusTypeID, ChargeClassTypeID from ",
                                                 grandparentBookingTable, ", ", parentArrestTable, ", ", chargeTable, " where ", grandparentBookingTable, ".BookingID is not null and ",
                                                 chargeTable, ".", parentArrestTable, "ID=", parentArrestTable, ".", parentArrestTable, "ID and ",
                                                 parentArrestTable, ".", grandparentBookingTable, "ID=", grandparentBookingTable, ".", grandparentBookingTable, "ID and ",
                                                 grandparentBookingTable, "Timestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

    totalRows <- nrow(Charge)
    Charge <- filter(Charge, BookingDate <= currentStagingDate)
    writeLines(paste0("Removing ", (totalRows - nrow(Charge)), " ", chargeTable, " rows due to future Booking Date for the charge record's booking"))

    Charge <- Charge %>%
      transmute(JailEpisodeChargeID=as.integer(row_number() + baseChargeID),
                ParentArrestID=ParentArrestID,
                ChargeCode=ChargeCode,
                ChargeDisposition=ChargeDisposition,
                ChargeTypeID=unknownCodeTableValue,
                ChargeClassTypeID=translateCodeTableValue(codeTableValueTranslator, ChargeClassTypeID, "ChargeClassType", unknownCodeTableValue, codeTableList),
                ChargeDispositionTypeID=unknownCodeTableValue,
                AgencyID=translateCodeTableValue(codeTableValueTranslator, AgencyID, "Agency", unknownCodeTableValue, codeTableList),
                JurisdictionTypeID=translateCodeTableValue(codeTableValueTranslator, ChargeJurisdictionTypeID, "JurisdictionType", unknownCodeTableValue, codeTableList),
                BondStatusTypeID=translateCodeTableValue(codeTableValueTranslator, BondStatusTypeID, "BondStatusType", unknownCodeTableValue, codeTableList),
                BondTypeID=translateCodeTableValue(codeTableValueTranslator, BondTypeID, "BondType", unknownCodeTableValue, codeTableList),
                BondAmount=BondAmount, StagingPK=pk)

    if (nrow(Charge)) {

      args <- list()
      args$codeTableList <- codeTableList
      args$textValues <- Charge$ChargeCode
      args$unknownCodeTableValue <- unknownCodeTableValue
      Charge$ChargeTypeID <- as.integer(do.call(chargeCodeTypeTextConverter, args))

      args <- list()
      args$codeTableList <- codeTableList
      args$textValues <- Charge$ChargeDisposition
      args$unknownCodeTableValue <- unknownCodeTableValue
      Charge$ChargeDispositionTypeID <- as.integer(do.call(dispositionTextConverter, args))

    }

    Charge %>% select(-ChargeCode, -ChargeDisposition)

  }

  baseChargeID <- getQuery(adsConnection, "select max(JailEpisodeChargeID) as BaseID from JailEpisodeCharge")
  baseChargeID <- baseChargeID$BaseID
  baseChargeID <- ifelse(is.na(baseChargeID), 0, baseChargeID) + 1

  ret <- list()
  ret$Charge <- buildTable('Booking', 'BookingArrest', 'BookingCharge', baseChargeID) %>%
    inner_join(ArrestDf %>% select(StagingPK, JailEpisodeArrestID), by=c("ParentArrestID"="StagingPK")) %>%
    select(-ParentArrestID, -StagingPK)

  baseChargeID <- baseChargeID + nrow(ret$Charge) + 1

  ret$ChargeEdits <- buildTable('CustodyStatusChange', 'CustodyStatusChangeArrest', 'CustodyStatusChangeCharge', baseChargeID) %>%
    inner_join(ArrestEditsDf %>% select(StagingPK, JailEpisodeArrestID), by=c("ParentArrestID"="StagingPK")) %>%
    select(-ParentArrestID)

  ret

}

#' @importFrom lubridate ddays dyears
buildBHAssessmentTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, codeTableList, codeTableValueTranslator) {

  BHAssessmentBooking <- getQuery(stagingConnection, paste0("select BehavioralHealthAssessmentID, Person.PersonID, SeriousMentalIllnessIndicator, MedicaidStatusTypeID,",
                                                            " CareEpisodeStartDate, CareEpisodeEndDate, BookingDate from ",
                                                            "BehavioralHealthAssessment, Person, Booking where ",
                                                            "BehavioralHealthAssessment.PersonID=Person.PersonID and ",
                                                            "Booking.PersonID=Person.PersonID and ",
                                                            "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHAssessmentIncident <- getQuery(stagingConnection, paste0("select BehavioralHealthAssessmentID, Person.PersonID, SeriousMentalIllnessIndicator, MedicaidStatusTypeID,",
                                                             " CareEpisodeStartDate, CareEpisodeEndDate, IncidentReportedDate as EventDate from ",
                                                             "BehavioralHealthAssessment, Person, Incident where ",
                                                             "BehavioralHealthAssessment.PersonID=Person.PersonID and Incident.PersonID=Person.PersonID"))

  BHAssessmentCSC <- getQuery(stagingConnection, paste0("select BehavioralHealthAssessmentID, Person.PersonID, SeriousMentalIllnessIndicator, MedicaidStatusTypeID,",
                                                        " CareEpisodeStartDate, CareEpisodeEndDate, BookingDate from ",
                                                        "BehavioralHealthAssessment, Person, CustodyStatusChange where ",
                                                        "BehavioralHealthAssessment.PersonID=Person.PersonID and ",
                                                        "CustodyStatusChange.PersonID=Person.PersonID and ",
                                                        "CustodyStatusChangeTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHAssessment <- BHAssessmentBooking

  if (nrow(BHAssessmentCSC)) {
    BHAssessment <- bind_rows(BHAssessment, BHAssessmentCSC)
  }

  if (nrow(BHAssessmentIncident)) {
    BHAssessment <- bind_rows(BHAssessment, BHAssessmentIncident)
  }

  BHAssessment <- BHAssessment %>%
    transmute(BehavioralHealthAssessmentID=BehavioralHealthAssessmentID,
              PersonID=PersonID,
              SevereMentalIllnessIndicator=SeriousMentalIllnessIndicator,
              MedicaidStatusTypeID=translateCodeTableValue(codeTableValueTranslator, MedicaidStatusTypeID, "MedicaidStatusType", unknownCodeTableValue, codeTableList),
              InTreatmentAtEvent=case_when(
                is.na(CareEpisodeEndDate) ~ 'N',
                TRUE ~ 'Y'),
              EndedDaysBeforeEvent=(EventDate - CareEpisodeEndDate) / ddays(1)
    ) %>%
    mutate(EndedDaysBeforeEvent=as.integer(ifelse(is.na(EndedDaysBeforeEvent) | EndedDaysBeforeEvent < 0,
                                                  unknownCodeTableValue, EndedDaysBeforeEvent)))

  BHAssessment

}

buildBHAssessmentCategoryTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, codeTableList, codeTableValueTranslator) {

  BHAssessmentCategory <- getQuery(stagingConnection, paste0("select BehavioralHealthAssessmentCategoryID, bha.BehavioralHealthAssessmentID, AssessmentCategoryTypeID from ",
                                                             "BehavioralHealthAssessment bha, BehavioralHealthAssessmentCategory bhac, Person p, Booking b where ",
                                                             "bhac.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                             "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHAssessmentCategoryCSC <- getQuery(stagingConnection, paste0("select BehavioralHealthAssessmentCategoryID, bha.BehavioralHealthAssessmentID, AssessmentCategoryTypeID from ",
                                                                "BehavioralHealthAssessment bha, BehavioralHealthAssessmentCategory bhac, Person p, CustodyStatusChange b where ",
                                                                "bhac.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                                "CustodyStatusChangeTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  if (nrow(BHAssessmentCategoryCSC)) {
    BHAssessmentCategory <- bind_rows(BHAssessmentCategory, BHAssessmentCategoryCSC)
  }

  BHAssessmentCategory <- BHAssessmentCategory %>%
    transmute(BehavioralHealthAssessmentCategoryID=BehavioralHealthAssessmentCategoryID,
              BehavioralHealthAssessmentID=BehavioralHealthAssessmentID,
              AssessmentCategoryTypeID=translateCodeTableValue(codeTableValueTranslator, AssessmentCategoryTypeID, "AssessmentCategoryType", unknownCodeTableValue, codeTableList))

  BHAssessmentCategory


}

#' @importFrom lubridate ddays
buildBHTreatmentTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, providerTextValueConverter, codeTableList, codeTableValueTranslator) {

  BHTreatment <- getQuery(stagingConnection, paste0("select TreatmentID, bha.BehavioralHealthAssessmentID, TreatmentStatusTypeID, TreatmentAdmissionReasonTypeID, ",
                                                    "TreatmentProviderName, TreatmentStartDate, BookingDate from ",
                                                    "BehavioralHealthAssessment bha, Treatment bht, Person p, Booking b where ",
                                                    "bht.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                    "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHTreatmentCSC <- getQuery(stagingConnection, paste0("select TreatmentID, bha.BehavioralHealthAssessmentID, TreatmentStatusTypeID, TreatmentAdmissionReasonTypeID, ",
                                                       "TreatmentProviderName, TreatmentStartDate, BookingDate from ",
                                                       "BehavioralHealthAssessment bha, Treatment bht, Person p, CustodyStatusChange b where ",
                                                       "bht.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                       "CustodyStatusChangeTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  if (nrow(BHTreatmentCSC)) {
    BHTreatment <- bind_rows(BHTreatment, BHTreatmentCSC)
  }

  BHTreatment <- BHTreatment %>%
    transmute(BehavioralHealthTreatmentID=TreatmentID,
              BehavioralHealthAssessmentID=BehavioralHealthAssessmentID,
              TreatmentStatusTypeID=translateCodeTableValue(codeTableValueTranslator, TreatmentStatusTypeID, "TreatmentStatusType", unknownCodeTableValue, codeTableList),
              TreatmentAdmissionReasonTypeID=translateCodeTableValue(codeTableValueTranslator, TreatmentAdmissionReasonTypeID, "TreatmentAdmissionReasonType", unknownCodeTableValue, codeTableList),
              TreatmentProviderName=TreatmentProviderName,
              TreatmentProviderTypeID=unknownCodeTableValue,
              DaysBeforeBooking=(BookingDate - TreatmentStartDate) / ddays(1)
    ) %>%
    mutate(DaysBeforeBooking=as.integer(ifelse(is.na(DaysBeforeBooking) | DaysBeforeBooking < 0, unknownCodeTableValue, DaysBeforeBooking)))

  if (nrow(BHTreatment)) {
    args <- list()
    args$codeTableList <- codeTableList
    args$textValues <- BHTreatment$TreatmentProviderName
    args$unknownCodeTableValue <- unknownCodeTableValue
    BHTreatment$TreatmentProviderTypeID <- as.integer(do.call(providerTextValueConverter, args))
  }

  BHTreatment %>% select(-TreatmentProviderName)

}

buildBHEvaluationTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, diagnosisTextValueConverter, codeTableList, codeTableValueTranslator) {

  BHEvaluation <- getQuery(stagingConnection, paste0("select BehavioralHealthEvaluationID, bhe.BehavioralHealthAssessmentID, BehavioralHealthDiagnosisDescription from ",
                                                     "BehavioralHealthAssessment bha, BehavioralHealthEvaluation bhe, Person p, Booking b where ",
                                                     "bhe.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                     "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHEvaluationCSC <- getQuery(stagingConnection, paste0("select BehavioralHealthEvaluationID, bhe.BehavioralHealthAssessmentID, BehavioralHealthDiagnosisDescription from ",
                                                        "BehavioralHealthAssessment bha, BehavioralHealthEvaluation bhe, Person p, CustodyStatusChange b where ",
                                                        "bhe.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                        "CustodyStatusChangeTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  if (nrow(BHEvaluationCSC)) {
    BHEvaluation <- bind_rows(BHEvaluation, BHEvaluationCSC)
  }

  BHEvaluation <- BHEvaluation %>%
    mutate(BehavioralHealthEvaluationTypeID=unknownCodeTableValue) %>%
    select(BehavioralHealthEvaluationID, BehavioralHealthAssessmentID, BehavioralHealthDiagnosisDescription)

  if (nrow(BHEvaluation)) {
    args <- list()
    args$codeTableList <- codeTableList
    args$textValues <- BHEvaluation$BehavioralHealthDiagnosisDescription
    args$unknownCodeTableValue <- unknownCodeTableValue
    BHEvaluation$BehavioralHealthEvaluationTypeID <- as.integer(do.call(diagnosisTextValueConverter, args))
  }

  BHEvaluation %>% select(-BehavioralHealthDiagnosisDescription)

}

buildMedicationTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, medicationTextValueConverter, codeTableList, codeTableValueTranslator) {

  PrescribedMedication <- getQuery(stagingConnection, paste0("select PrescribedMedicationID, pm.BehavioralHealthAssessmentID, MedicationDescription from ",
                                                             "BehavioralHealthAssessment bha, PrescribedMedication pm, Person p, Booking b where ",
                                                             "pm.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                             "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  PrescribedMedicationCSC <- getQuery(stagingConnection, paste0("select PrescribedMedicationID, pm.BehavioralHealthAssessmentID, MedicationDescription from ",
                                                                "BehavioralHealthAssessment bha, PrescribedMedication pm, Person p, CustodyStatusChange b where ",
                                                                "pm.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                                "CustodyStatusChangeTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  if (nrow(PrescribedMedicationCSC)) {
    PrescribedMedication <- bind_rows(PrescribedMedication, PrescribedMedicationCSC)
  }

  PrescribedMedication <- PrescribedMedication %>%
    mutate(MedicationTypeID=unknownCodeTableValue) %>%
    select(PrescribedMedicationID, BehavioralHealthAssessmentID, MedicationDescription)

  if (nrow(PrescribedMedication)) {
    args <- list()
    args$codeTableList <- codeTableList
    args$textValues <- PrescribedMedication$MedicationDescription
    args$unknownCodeTableValue <- unknownCodeTableValue
    PrescribedMedication$MedicationTypeID <- as.integer(do.call(medicationTextValueConverter, args))
  }

  PrescribedMedication %>% select(-MedicationDescription)

}

buildReleaseTable <- function(stagingConnection, lastLoadTime, currentStagingDate, codeTableList, codeTableValueTranslator) {

  Release <- getQuery(stagingConnection, paste0("select CustodyRelease.BookingID, BookingDate, ReleaseDate, CustodyReleaseTimestamp ",
                                                "from CustodyRelease inner join Booking on CustodyRelease.BookingID=Booking.BookingID ",
                                                "where CustodyReleaseTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  Release <- Release %>% mutate(ReleaseDate=as_date(ReleaseDate), BookingDate=as_date(BookingDate))

  totalRows <- nrow(Release)
  Release <- filter(Release, ReleaseDate <= currentStagingDate)
  writeLines(paste0("Removing ", (totalRows - nrow(Release)), " Release rows due to future actual release dates"))

  weirdReleaseDateCount <- nrow(Release %>% filter(ReleaseDate < BookingDate))

  if (weirdReleaseDateCount) {
    writeLines(paste0(weirdReleaseDateCount, " release dates adjusted to staging record date, because release date was prior to booking date"))
  }

  Release <- mutate(Release, ReleaseDate=as_date(ifelse(ReleaseDate < BookingDate, as_date(CustodyReleaseTimestamp), ReleaseDate)))

  Release %>% select(BookingID, ReleaseDate)

}

buildAndLoadHistoricalPeriodTable <- function(adsConnection, lookbackPeriod, unknownCodeTableValue,
                                              noneCodeTableValue, completeLoad, writeToDatabase, localDatabase) {

  df <- tibble(HistoricalPeriodTypeID=0:lookbackPeriod,
                   DaysAgo=0:lookbackPeriod) %>%
    mutate(HistoricalPeriodTypeDescription1=paste0(90*(1 + DaysAgo %/% 90), " days"),
           HistoricalPeriodTypeDescription1=ifelse(DaysAgo >= 360, "360+ days", HistoricalPeriodTypeDescription1),
           HistoricalPeriodTypeDescription2=ifelse(DaysAgo >= 360, "36 months", HistoricalPeriodTypeDescription1),
           HistoricalPeriodTypeDescription2=ifelse(DaysAgo >= 360*3, "36 months+", HistoricalPeriodTypeDescription2))

  df <- df %>% bind_rows(tibble(HistoricalPeriodTypeID=c(unknownCodeTableValue, noneCodeTableValue),
                                    DaysAgo=as.integer(c(0,0)),
                                    HistoricalPeriodTypeDescription1=c("Unknown", "None"),
                                    HistoricalPeriodTypeDescription2=c("Unknown", "None")))

  if (completeLoad & writeToDatabase) {
    writeTableToDatabase(adsConnection, "HistoricalPeriodType", df, localDatabase)
  }

  ret <- list()
  ret$HistoricalPeriodType <- df
  ret

}

#' @importFrom lubridate now as_date %--%
#' @import dplyr
#' @import purrr
#' @export
loadDimensionalDatabase <- function(stagingConnectionBuilder=defaultStagingConnectionBuilder,
                                    dimensionalConnectionBuilder=defaultDimensionalConnectionBuilder,
                                    chargeDispositionAggregator=defaultChargeDispositionAggregator,
                                    callNatureTextValueConverter=defaultCallNatureTextValueConverter,
                                    dispositionLocationTextValueConverter=defaultDispositionLocationTextValueConverter,
                                    pendingCriminalChargesTextValueConverter=defaultPendingCriminalChargesTextValueConverter,
                                    unitTextValueConverter=defaultUnitTextValueConverter,
                                    educationTextValueConverter=defaultEducationTextValueConverter,
                                    occupationTextValueConverter=defaultOccupationTextValueConverter,
                                    diagnosisTextValueConverter=defaultDiagnosisTextValueConverter,
                                    medicationTextValueConverter=defaultMedicationTextValueConverter,
                                    chargeCodeTypeTextConverter=defaultChargeCodeTextConverter,
                                    dispositionTextConverter=defaultDispositionTextConverter,
                                    providerTextValueConverter=defaultProviderTextValueConverter,
                                    populationTypeConverter=defaultPopulationTypeConverter,
                                    codeTableSpreadsheetFile=defaultCodeTableSpreadsheetFile,
                                    codeTableValueTranslator=defaultCodeTableValueTranslator,
                                    unknownCodeTableValue=as.integer(99999),
                                    noneCodeTableValue=as.integer(99998),
                                    materializedViewsSqlFileName="MaterializedViews.sql",
                                    historicalPeriodLookback=365*10,
                                    completeLoad=TRUE,
                                    writeToDatabase=FALSE, localDatabase=TRUE) {

  writeLines(paste0("Running ADS load with completeLoad=", completeLoad, " and writeToDatabase=", writeToDatabase,
                    "with localDatabase=", localDatabase))

  ret <- list()

  stagingConnection = do.call(stagingConnectionBuilder, list())
  adsConnection = do.call(dimensionalConnectionBuilder, list())

  if (writeToDatabase & completeLoad) {
    writeLines("Truncating all current tables")
    truncateTables(adsConnection)
  }

  lastLoadTime <- getLastLoadingTime(adsConnection)

  currentLoadTime <- now()
  loadHistoryID <- updateLoadHistory(adsConnection, stagingConnection, currentLoadTime)

  writeLines("Loading code tables")
  codeTableList <- loadCodeTables(adsConnection, codeTableSpreadsheetFile, writeToDatabase & completeLoad)
  ret <- c(ret, codeTableList)

  historicalPeriodType <- buildAndLoadHistoricalPeriodTable(adsConnection, historicalPeriodLookback, unknownCodeTableValue, noneCodeTableValue, completeLoad, writeToDatabase, localDatabase)
  ret <- c(ret, historicalPeriodType)

  currentStagingDate <- getQuery(adsConnection, paste0("select MostRecentStagingTimestamp from LoadHistory where LoadHistoryID=", loadHistoryID))
  currentStagingDate <- as_date(currentStagingDate[1,1])
  writeLines(paste0("Most recent staging timestamp for this load is ", currentStagingDate))

  writeLines("Loading Crisis Incident tables")
  incidentTables <- buildIncidentTables(stagingConnection, adsConnection, currentStagingDate, unknownCodeTableValue,
                                        noneCodeTableValue, codeTableList, callNatureTextValueConverter,
                                        dispositionLocationTextValueConverter, pendingCriminalChargesTextValueConverter,
                                        unitTextValueConverter)
  ret <- c(ret, incidentTables)
  writeLines(paste0("Loaded Incident table with ", nrow(ret$Incident), " rows, and UnitResponse table with ", nrow(ret$UnitResponse), " rows."))

  writeLines("Loading JailEpisode tables")
  jailEpisodeTables <- buildJailEpisodeTables(stagingConnection, adsConnection, lastLoadTime, currentLoadTime, currentStagingDate, loadHistoryID,
                                              unknownCodeTableValue, noneCodeTableValue, chargeDispositionAggregator,
                                              codeTableList, codeTableValueTranslator)
  ret <- c(ret, jailEpisodeTables)
  writeLines(paste0("Loaded JailEpisode with ", nrow(ret$JailEpisode), " rows and JailEpisodeEdits with ", nrow(ret$JailEpisodeEdits), " rows"))

  writeLines("Loading Person table")
  ret$Person <- buildPersonTable(stagingConnection, lastLoadTime, currentStagingDate, unknownCodeTableValue, educationTextValueConverter,
                                 occupationTextValueConverter, codeTableList, codeTableValueTranslator)
  writeLines(paste0("Loaded Person table with ", nrow(ret$Person), " rows"))

  jailEpisodeEditPKs <- unique(ret$JailEpisodeEdits$StagingPK)

  writeLines("Loading Arrest tables")
  arrestTables <- buildArrestTables(stagingConnection, adsConnection, lastLoadTime, currentStagingDate, unknownCodeTableValue, codeTableList, codeTableValueTranslator, jailEpisodeEditPKs)
  ret <- c(ret, arrestTables)
  writeLines(paste0("Loaded Arrest with ", nrow(ret$Arrest), " rows and ArrestEdits with ", nrow(ret$ArrestEdits), " rows"))

  writeLines("Loading Charge tables")
  chargeTables <- buildChargeTables(stagingConnection, adsConnection, lastLoadTime, currentStagingDate, unknownCodeTableValue,
                                    chargeCodeTypeTextConverter, dispositionTextConverter,
                                    ret$Arrest, ret$ArrestEdits, codeTableList, codeTableValueTranslator)
  ret <- c(ret, chargeTables)
  writeLines(paste0("Loaded Charge with ", nrow(ret$Charge), " rows and ChargeEdits with ", nrow(ret$ChargeEdits), " rows"))

  ret$JailEpisodeEdits <- ret$JailEpisodeEdits %>% select(-StagingPK)
  ret$Arrest <- ret$Arrest %>% select(-StagingPK, -StagingParentFK)

  writeLines("Loading Behavioral Health tables")
  ret$BehavioralHealthAssessment <- buildBHAssessmentTable(stagingConnection, lastLoadTime, unknownCodeTableValue, codeTableList, codeTableValueTranslator)
  writeLines(paste0("Loaded BehavioralHealthAssessment with ", nrow(ret$BehavioralHealthAssessment), " rows"))
  ret$BehavioralHealthAssessmentCategory <- buildBHAssessmentCategoryTable(stagingConnection, lastLoadTime, unknownCodeTableValue, codeTableList, codeTableValueTranslator)
  writeLines(paste0("Loaded BehavioralHealthAssessmentCategory with ", nrow(ret$BehavioralHealthAssessmentCategory), " rows"))
  ret$BehavioralHealthTreatment <- buildBHTreatmentTable(stagingConnection, lastLoadTime, unknownCodeTableValue, providerTextValueConverter, codeTableList, codeTableValueTranslator)
  writeLines(paste0("Loaded BehavioralHealthTreatment with ", nrow(ret$BehavioralHealthTreatment), " rows"))
  ret$BehavioralHealthEvaluation <- buildBHEvaluationTable(stagingConnection, lastLoadTime, unknownCodeTableValue, diagnosisTextValueConverter, codeTableList, codeTableValueTranslator)
  writeLines(paste0("Loaded BehavioralHealthEvaluation with ", nrow(ret$BehavioralHealthEvaluation), " rows"))
  ret$PrescribedMedication <- buildMedicationTable(stagingConnection, lastLoadTime, unknownCodeTableValue, medicationTextValueConverter, codeTableList, codeTableValueTranslator)
  writeLines(paste0("Loaded PrescribedMedication with ", nrow(ret$PrescribedMedication), " rows"))

  writeLines("Loading Release table")
  ret$Release <- buildReleaseTable(stagingConnection, lastLoadTime, currentStagingDate, codeTableList, codeTableValueTranslator)
  writeLines(paste0("Loaded Release with ", nrow(ret$Release), " rows"))

  ret$Date <- loadDateDimension(adsConnection, ret, unknownCodeTableValue, noneCodeTableValue, writeToDatabase, localDatabase=localDatabase)

  persistTables(adsConnection, ret, unknownCodeTableValue, currentLoadTime, writeToDatabase=writeToDatabase, localDatabase=localDatabase)

  determineRecidivism(adsConnection, writeToDatabase=writeToDatabase, localDatabase=localDatabase)

  args <- list()
  args$adsConnection <- adsConnection
  args$unknownCodeTableValue <- unknownCodeTableValue
  args$writeToDatabase <- writeToDatabase
  do.call(populationTypeConverter, args)

  materializedViewsSqlFile=system.file("raw", materializedViewsSqlFileName, package=getPackageName())
  createMaterializedViews(adsConnection, materializedViewsSqlFile)

  dbDisconnect(stagingConnection)
  dbDisconnect(adsConnection)

  writeLines("ADS load complete")

  map(ret, as_tibble)

}

#' @importFrom lubridate as_date ymd_hms as_datetime dminutes
buildIncidentTables <- function(stagingConnection, adsConnection, currentStagingDate, unknownCodeTableValue,
                                noneCodeTableValue, codeTableList, callNatureTextValueConverter,
                                dispositionLocationTextValueConverter, pendingCriminalChargesTextValueConverter,
                                unitTextValueConverter) {

  stagingIncident <- getQuery(stagingConnection, 'select Incident.*, PersonUniqueIdentifier from Incident, Person where Incident.PersonID=Person.PersonID')
  stagingIncidentResponseUnit <- getQuery(stagingConnection, 'select * from IncidentResponseUnit')

  # todo: consider optimizing by calling this method after doing Jail Episodes, and derive this from the Jail Episode table
  stagingBooking <- getQuery(stagingConnection, 'select BookingDate, PersonUniqueIdentifier from Booking, Person where Booking.PersonID=Person.PersonID')

  stagingIncident <- suppressWarnings(
    stagingIncident <- stagingIncident %>%
      mutate(dd=case_when(is.na(IncidentReportedDate) | is.na(IncidentReportedTime) ~ as_datetime(NA),
                          TRUE ~ ymd_hms(paste0(format(IncidentReportedDate, '%Y-%m-%d'), ' ', IncidentReportedTime))))
  )

  stagingIncidentResponseUnit <- suppressWarnings(
    stagingIncidentResponseUnit <- stagingIncidentResponseUnit %>%
      mutate(dda=case_when(is.na(UnitArrivalDate) | is.na(UnitArrivalTime) ~ as_datetime(NA),
                           TRUE ~ ymd_hms(paste0(format(UnitArrivalDate, '%Y-%m-%d'), ' ', UnitArrivalTime))),
             ddc=case_when(is.na(UnitClearDate) | is.na(UnitClearTime) ~ as_datetime(NA),
                           TRUE ~ ymd_hms(paste0(format(UnitClearDate, '%Y-%m-%d'), ' ', UnitClearTime))))

  )

  Incident <- stagingIncident %>%
    mutate(
      IncidentReportedDate=as_date(IncidentReportedDate),
      IncidentReportedDateID=format(IncidentReportedDate, "%Y%m%d"),
      IncidentReportedHour=hour(dd)
    ) %>%
    select(-dd)

  IncidentResponseCostSum <- stagingIncidentResponseUnit %>%
    group_by(IncidentID, UnitIdentifier) %>%
    mutate(CostInUnitMinutes=(ddc - dda)/dminutes(1)) %>%
    group_by(IncidentID) %>%
    summarize(CostInUnitMinutes=sum(CostInUnitMinutes, na.rm=TRUE))

  IncidentResponseTimeSum <- stagingIncidentResponseUnit %>%
    group_by(IncidentID) %>%
    summarize(startTime=min(dda, na.rm=TRUE), endTime=max(ddc, na.rm=TRUE)) %>%
    mutate(DurationInMinutes=(endTime - startTime) / dminutes(1))

  Incident <- Incident %>%
    left_join(IncidentResponseCostSum, by='IncidentID') %>%
    left_join(IncidentResponseTimeSum, by='IncidentID') %>%
    mutate(
      CallNatureTypeID=callNatureTextValueConverter(codeTableList, CallNature, unknownCodeTableValue),
      DispositionLocationTypeID=dispositionLocationTextValueConverter(codeTableList, DispositionLocation, unknownCodeTableValue),
      PendingCriminalChargesTypeID=pendingCriminalChargesTextValueConverter(codeTableList, PendingCriminalCharges, unknownCodeTableValue))

  recid <- Incident %>%
    select(IncidentID, PersonUniqueIdentifier, IncidentReportedDate) %>%
    arrange(PersonUniqueIdentifier, IncidentReportedDate) %>%
    group_by(PersonUniqueIdentifier) %>%
    mutate(IncidentSeq=row_number(), NextIncidentSeq=IncidentSeq+1, PriorIncidentSeq=IncidentSeq-1) %>%
    ungroup()

  recid <- recid %>%
    left_join(recid %>% select(PersonUniqueIdentifier, PriorIncidentSeq=IncidentSeq, PriorIncidentReportedDate=IncidentReportedDate),
              by=c('PersonUniqueIdentifier', 'PriorIncidentSeq')) %>%
    select(-PriorIncidentSeq) %>%
    left_join(recid %>% select(PersonUniqueIdentifier, NextIncidentSeq=IncidentSeq, NextIncidentReportedDate=IncidentReportedDate),
              by=c('PersonUniqueIdentifier', 'NextIncidentSeq')) %>%
    select(IncidentID, PriorIncidentReportedDate, NextIncidentReportedDate)

  Incident <- left_join(Incident, recid, by='IncidentID') %>%
    mutate(DaysSinceLastIncident=(IncidentReportedDate - PriorIncidentReportedDate) / ddays(1),
           DaysUntilNextIncident=(NextIncidentReportedDate - IncidentReportedDate) / ddays(1))

  recid <- stagingBooking %>%
    semi_join(Incident %>% select(PersonUniqueIdentifier) %>% distinct(), by='PersonUniqueIdentifier') %>%
    full_join(Incident %>%
                semi_join(stagingBooking %>% select(PersonUniqueIdentifier) %>% distinct(), by='PersonUniqueIdentifier') %>%
                select(IncidentID, PersonUniqueIdentifier, IncidentReportedDate), by='PersonUniqueIdentifier') %>%
    mutate(DaysSinceLastBooking=(IncidentReportedDate - BookingDate) / ddays(1),
           DaysUntilNextBooking=(BookingDate - IncidentReportedDate) / ddays(1))

  recidPast <- recid %>% filter(DaysSinceLastBooking >= 0) %>% group_by(IncidentID) %>%
    summarize(DaysSinceLastBooking=min(DaysSinceLastBooking))
  recidFuture <- recid %>% filter(DaysUntilNextBooking >= 0) %>% group_by(IncidentID) %>%
    summarize(DaysUntilNextBooking=min(DaysUntilNextBooking))

  Incident <- left_join(Incident, recidPast, by='IncidentID')
  Incident <- left_join(Incident, recidFuture, by='IncidentID')

  Incident <- Incident %>%
    select(IncidentID, PersonID, IncidentReportedDate, IncidentReportedDateID, IncidentReportedHour, CallNatureTypeID,
           DispositionLocationTypeID, PendingCriminalChargesTypeID, IncidentNumber, OfficerCount, DurationInMinutes, CostInUnitMinutes,
           DaysSinceLastIncident, DaysUntilNextIncident, DaysSinceLastBooking, DaysUntilNextBooking)

  ret <- list()
  ret$Incident <- Incident

  UnitResponse <- stagingIncidentResponseUnit %>%
    mutate(
      RespondingUnitTypeID=unitTextValueConverter(codeTableList, UnitIdentifier, unknownCodeTableValue),
      ArrivalDate=as_date(dda),
      ArrivalDateID=format(ArrivalDate, "%Y%m%d"),
      ArrivalHour=hour(dda),
      ClearedDate=as_date(ddc),
      ClearedDateID=format(ClearedDate, "%Y%m%d"),
      ClearedHour=hour(ddc),
      ResponseDurationInMinutes=(ddc - dda) / dminutes(1)
    ) %>%
    select(UnitResponseID=IncidentResponseUnitID,
           IncidentID,
           RespondingUnitTypeID,
           ArrivalDateID,
           ArrivalDate,
           ArrivalHour,
           ClearedDateID,
           ClearedDate,
           ClearedHour,
           ResponseDurationInMinutes)

  ret$UnitResponse <- UnitResponse

  ret

}

#' @importFrom purrr map_df
loadDateDimension <- function(adsConnection, dfs, unknownCodeTableValue, noneCodeTableValue, writeToDatabase, localDatabase) {

  queries <- c(
    'select distinct EpisodeStartDate as d from JailEpisode',
    'select distinct EpisodeEndDate as d from JailEpisode',
    'select distinct IncidentReportedDate as d from Incident',
    'select distinct ArrivalDate as d from UnitResponse',
    'select distinct ClearedDate as d from UnitResponse'
  )

  newDates <- map_df(queries, function(query) {
    getQuery(adsConnection, query)
  }) %>% distinct() %>% filter(!is.na(d)) %>% .[['d']]

  newDates <- c(newDates, dfs$JailEpisode$EpisodeStartDate, dfs$JailEpisode$EpisodeEndDate,
                dfs$Incident$IncidentReportedDate,
                dfs$UnitResponse$ArrivalDate, dfs$UnitResponse$ClearedDate) %>% unique()

  newDates <- newDates[!is.na(newDates)]

  minDate <- as_date(min(newDates, na.rm=TRUE))
  maxDate <- as_date(max(newDates, na.rm=TRUE))

  currentDates <- getQuery(adsConnection, 'select distinct CalendarDate from Date')

  DateDf <- buildDateDimensionTable(minDate, maxDate, as_date(currentDates$CalendarDate), unknownCodeTableValue, noneCodeTableValue)

  if (writeToDatabase) {
    writeDataFrameToDatabase(adsConnection, DateDf, "Date", viaBulk = TRUE, localBulk=localDatabase)
  }

  DateDf

}

determineRecidivism <- function(adsConnection, writeToDatabase, localDatabase) {

  writeLines("Determining recidivism")

  # note: change StagingPersonUniqueIdentifier to "StagingPersonUniqueIdentifier2 as StagingPersonUniqueIdentifier" to use that as the basis for recidivism
  # todo: make this a method parameter
  df <- getQuery(adsConnection, paste0("select JailEpisodeID, StagingPersonUniqueIdentifier, EpisodeStartDate from JailEpisode, Person ",
                                       "where JailEpisode.PersonID=Person.PersonID order by StagingPersonUniqueIdentifier, EpisodeStartDate"))

  df <- df %>%
    mutate(EpisodeStartDate=as.Date(EpisodeStartDate)) %>%
    group_by(StagingPersonUniqueIdentifier) %>%
    mutate(first=row_number()==1, last=row_number()==n(), recidivist=!(first & last), DaysToNextEpisode=NA, DaysSinceLastEpisode=NA)

  recidivistIndices <- which(df$recidivist)

  writeLines(paste0("Found ", length(recidivistIndices), " recidivist booking records out of ", nrow(df), " total booking records"))

  if (writeToDatabase) {

    executeQuery(adsConnection, "update JailEpisode set DaysSinceLastEpisode=NULL, DaysUntilNextEpisode=NULL, SixMonthRebooking='N', OneYearRebooking='N', TwoYearRebooking='N'")

    for (i in recidivistIndices) {

      bookingDate <- df[[i, 'EpisodeStartDate']]
      first <- df[[i, 'first']]
      last <- df[[i, 'last']]
      priorBookingDate <- as.Date(NA)
      nextBookingDate <- as.Date(NA)

      if (!first) {
        priorBookingDate <- df[[i-1, "EpisodeStartDate"]]
      }

      if (!last) {
        nextBookingDate <- df[[i+1, "EpisodeStartDate"]]
      }

      # lubridate took considerably longer
      DaysUntilNextEpisode <- as.numeric(nextBookingDate - bookingDate) # (bookingDate %--% nextBookingDate) %/% days(1)
      DaysSinceLastEpisode <- as.numeric(bookingDate - priorBookingDate) # (priorBookingDate %--% bookingDate) %/% days(1)
      SixMonthRebooking <- ifelse(!is.na(DaysSinceLastEpisode) & DaysSinceLastEpisode <= 180, 'Y', 'N')
      OneYearRebooking <- ifelse(!is.na(DaysSinceLastEpisode) & DaysSinceLastEpisode <= 365, 'Y', 'N')
      TwoYearRebooking <- ifelse(!is.na(DaysSinceLastEpisode) & DaysSinceLastEpisode <= 730, 'Y', 'N')

      JailEpisodeID <- df[[i, 'JailEpisodeID']]

      sql <- paste0("update JailEpisode set ",
                    "DaysSinceLastEpisode=", ifelse(is.na(DaysSinceLastEpisode), 'NULL', as.character(DaysSinceLastEpisode)), ",",
                    "DaysUntilNextEpisode=", ifelse(is.na(DaysUntilNextEpisode), 'NULL', as.character(DaysUntilNextEpisode)), ",",
                    "SixMonthRebooking='", SixMonthRebooking, "', ",
                    "OneYearRebooking='", OneYearRebooking, "', ",
                    "TwoYearRebooking='", TwoYearRebooking, "' where JailEpisodeID=", JailEpisodeID)

      executeQuery(adsConnection, sql)

    }

  } else {
    writeLines("No actual recividivism updates persisted, because writeToDatabase=FALSE")
  }

  writeLines("Recidivism determination complete")

  invisible()

}

persistTables <- function(adsConnection, dfs, unknownCodeTableValue, currentLoadTime, writeToDatabase, localDatabase) {

  checkForAndRemoveDuplicateBookings(adsConnection, dfs, writeToDatabase=writeToDatabase, localDatabase)

  writeLines("Writing main transaction tables to database")

  writeDataFrameToDatabase(adsConnection, dfs$Person, "Person", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
  writeDataFrameToDatabase(adsConnection, dfs$JailEpisode, "JailEpisode", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
  writeDataFrameToDatabase(adsConnection, dfs$Arrest, "JailEpisodeArrest", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
  writeDataFrameToDatabase(adsConnection, dfs$Charge, "JailEpisodeCharge", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
  writeDataFrameToDatabase(adsConnection, dfs$BehavioralHealthAssessment, "BehavioralHealthAssessment", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
  writeDataFrameToDatabase(adsConnection, dfs$BehavioralHealthAssessmentCategory, "BehavioralHealthAssessmentCategory", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
  writeDataFrameToDatabase(adsConnection, dfs$BehavioralHealthTreatment, "BehavioralHealthTreatment", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
  writeDataFrameToDatabase(adsConnection, dfs$BehavioralHealthEvaluation, "BehavioralHealthEvaluation", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
  writeDataFrameToDatabase(adsConnection, dfs$PrescribedMedication, "PrescribedMedication", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)

  writeDataFrameToDatabase(adsConnection, dfs$Incident, "Incident", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
  writeDataFrameToDatabase(adsConnection, dfs$UnitResponse, "UnitResponse", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)

  writeLines("Done writing main transaction tables to database")

  writeLines("Applying JailEpisodeEdits")
  applyEdits(adsConnection, dfs, currentLoadTime, writeToDatabase=writeToDatabase, localDatabase)

  writeLines("Processing releases")
  persistReleases(adsConnection, dfs, unknownCodeTableValue, writeToDatabase=writeToDatabase, localDatabase)

}

#' @importFrom readr read_file
#' @importFrom stringr str_split str_length
createMaterializedViews <- function(adsConnection, sqlFile) {

  sql <- readr::read_file(sqlFile)
  sql <- stringr::str_split(sql, ";")[[1]]
  for (s in sql) {
    if (str_length(trimws(s))) {
      executeQuery(adsConnection, s)
    }
  }

}

checkForAndRemoveDuplicateBookings <- function(adsConnection, dfs, writeToDatabase, localDatabase) {

  writeLines("Checking for Jail Episode records with prior recorded booking number...")

  bookingNumberDf <- getQuery(adsConnection, "select distinct BookingNumber from JailEpisode")
  bookingNumbers <- bookingNumberDf$BookingNumber

  writeLines(paste0("...", length(bookingNumbers), " existing booking numbers in ADS"))

  newBookingNumbers <- unique(dfs$JailEpisode$BookingNumber)

  writeLines(paste0("...", length(newBookingNumbers), " new booking numbers in this load"))

  dups <- base::intersect(bookingNumbers, newBookingNumbers)

  writeLines(paste0("...found ", length(dups), " duplicates"))

  groupSize <- 50

  if (length(dups)) {

    groups <- split(dups, ceiling(seq_along(dups)/groupSize))

    dupBookingIDs <- integer()

    for (g in groups) {
      l <- paste0(paste0(paste0("'", g), "'"), collapse=",")
      bookingIDDf <- getQuery(adsConnection, paste0("select JailEpisodeID from JailEpisode where BookingNumber in (", l, ")"))
      dupBookingIDs <- c(dupBookingIDs, bookingIDDf$JailEpisodeID)
    }

    removeBookingsAndChildren(adsConnection, dupBookingIDs, writeToDatabase=writeToDatabase, localDatabase)

  } else {
    writeLines("...no duplicates found")
  }

}

applyEdits <- function(adsConnection, dfs, currentLoadTime, writeToDatabase, localDatabase) {

  if (nrow(dfs$JailEpisodeEdits)) {

    writeLines(paste0("Applying ", nrow(dfs$JailEpisodeEdits), " JailEpisodeEdits"))
    writeLines(paste0("Applying ", nrow(dfs$ArrestEdits), " ArrestEdits"))
    writeLines(paste0("Applying ", nrow(dfs$ChargeEdits), " ChargeEdits"))

    removeBookingsAndChildren(adsConnection, dfs$JailEpisodeEdits$JailEpisodeID, writeToDatabase=writeToDatabase)

    writeLines("Adding edited Booking, Arrest, and Charge records")
    writeDataFrameToDatabase(adsConnection, dfs$JailEpisodeEdits, "JailEpisode", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
    writeDataFrameToDatabase(adsConnection, dfs$ArrestEdits %>% select(-StagingPK), "JailEpisodeArrest", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)
    writeDataFrameToDatabase(adsConnection, dfs$ChargeEdits %>% select(-StagingPK), "JailEpisodeCharge", viaBulk = TRUE, writeToDatabase=writeToDatabase, localBulk=localDatabase)

  } else {
    writeLines("No Jail Episode edits found in this load")
  }

}

removeBookingsAndChildren <- function(adsConnection, BookingIDs, groupSize=50, writeToDatabase, localDatabase) {

  # split the booking edits into chunks and delete them in groups

  writeLines(paste0("Removing ", length(BookingIDs), " bookings and all their children"))

  if (!writeToDatabase) {
    writeLines("Skipping actual database updates, since writeToDatabase=FALSE")
  } else {

    episodeIDGroups <- split(BookingIDs, ceiling(seq_along(BookingIDs)/groupSize))

    orphanedPersonIDs <- integer()

    for (g in episodeIDGroups) {
      idList <- paste0(g, collapse=",")
      executeQuery(adsConnection, paste0("delete from JailEpisodeCharge where JailEpisodeArrestID in (select JailEpisodeArrestID from JailEpisodeArrest where JailEpisodeID in (", idList, "))"))
      executeQuery(adsConnection, paste0("delete from JailEpisodeArrest where JailEpisodeID in (", idList, ")"))
      personDf <- getQuery(adsConnection, paste0("select PersonID from JailEpisode where JailEpisodeID in (", idList, ")"))
      orphanedPersonIDs <- c(orphanedPersonIDs, personDf$PersonID)
      executeQuery(adsConnection, paste0("delete from JailEpisode where JailEpisodeID in (", idList, ")"))
    }

    orphanedBHAssessmentIDs <- integer()

    personIDGroups <- split(orphanedPersonIDs, ceiling(seq_along(orphanedPersonIDs)/groupSize))

    for (g in personIDGroups) {
      idList <- paste0(g, collapse=",")
      bhDf <- getQuery(adsConnection, paste0("select BehavioralHealthAssessmentID from BehavioralHealthAssessment where PersonID in (", idList, ")"))
      orphanedBHAssessmentIDs <- c(orphanedBHAssessmentIDs, bhDf$BehavioralHealthAssessmentID)
    }

    writeLines(paste0("Removing ", length(orphanedBHAssessmentIDs), " orphaned BH assessment records"))

    bhIDGroups <- split(orphanedBHAssessmentIDs, ceiling(seq_along(orphanedBHAssessmentIDs)/groupSize))

    for (g in bhIDGroups) {
      idList <- paste0(g, collapse=",")
      executeQuery(adsConnection, paste0("delete from BehavioralHealthAssessmentCategory where BehavioralHealthAssessmentID in (", idList, ")"))
      executeQuery(adsConnection, paste0("delete from PrescribedMedication where BehavioralHealthAssessmentID in (", idList, ")"))
      executeQuery(adsConnection, paste0("delete from BehavioralHealthEvaluation where BehavioralHealthAssessmentID in (", idList, ")"))
      executeQuery(adsConnection, paste0("delete from BehavioralHealthTreatment where BehavioralHealthAssessmentID in (", idList, ")"))
      executeQuery(adsConnection, paste0("delete from BehavioralHealthAssessment where BehavioralHealthAssessmentID in (", idList, ")"))
    }

    writeLines(paste0("Removing ", length(orphanedPersonIDs), " orphaned person records"))

    for (g in personIDGroups) {
      idList <- paste0(g, collapse=",")
      executeQuery(adsConnection, paste0("delete from Person where PersonID in (", idList, ")"))
    }

  }

  invisible()

}

#' @importFrom lubridate ddays
persistReleases <- function(adsConnection, dfs, unknownCodeTableValue, writeToDatabase, localDatabase) {

  bookingDf <- getQuery(adsConnection, "select JailEpisodeID, EpisodeStartDate from JailEpisode")

  if (nrow(bookingDf)) {

    bookingDf <- bookingDf %>%
      inner_join(dfs$Release, by=c("JailEpisodeID"="BookingID")) %>%
      mutate(LengthOfStay=(ReleaseDate - EpisodeStartDate) / ddays(1), EpisodeEndDate=ReleaseDate) %>% select(-ReleaseDate)

    if (nrow(bookingDf)) {

      for (r in seq(nrow(bookingDf))) {
        bookingID <- bookingDf[r, 'JailEpisodeID']
        lengthOfStay <- bookingDf[r, 'LengthOfStay']
        episodeEndDate <- bookingDf[r, 'EpisodeEndDate']
        episodeEndDateS <- ifelse(is.na(episodeEndDate), 'NULL', format(episodeEndDate, "%Y-%m-%d"))
        episodeEndDateID <- ifelse(is.na(episodeEndDate), unknownCodeTableValue, format(episodeEndDate, "%Y%m%d"))
        sql <- paste0("update JailEpisode set IsActive='N', LengthOfStay=", lengthOfStay, ", EpisodeEndDate='", episodeEndDateS, "'",
                      ", EpisodeEndDateID=", episodeEndDateID,
                      " where JailEpisodeID=", bookingID)
        if (writeToDatabase) {
          executeQuery(adsConnection, sql)
        }
      }

    }

    writeLines(paste0("Updated ", nrow(bookingDf), " JailEpisode records with release information"))
    if (!writeToDatabase) {
      writeLines("No JailEpisode records actually updated, because writeToDatabase=FALSE")
    }

  } else {
    writeLines("No releases found")
  }

  invisible()

}

#' @importFrom lubridate year month day wday quarter
#' @importFrom tibble tibble
buildDateDimensionTable <- function(minDate, maxDate, datesToExclude, unknownCodeTableValue, noneCodeTableValue) {
  writeLines(paste0("Building date dimension, earliest date=", minDate, ", latestDate=", maxDate))
  DateDf <- data.frame(CalendarDate=seq(minDate, maxDate, by="days")) %>%
    mutate(DateID=as.integer(format(CalendarDate, "%Y%m%d")),
           Year=year(CalendarDate),
           YearLabel=as.character(Year),
           CalendarQuarter=quarter(CalendarDate),
           Month=month(CalendarDate),
           MonthName=as.character(month(CalendarDate, label=TRUE, abbr=FALSE)),
           FullMonth=format(CalendarDate, paste0(Year, "-", Month)),
           Day=day(CalendarDate),
           DayOfWeek=as.character(wday(CalendarDate, label=TRUE, abbr=FALSE)),
           DayOfWeekSort=wday(CalendarDate),
           DateMMDDYYYY=format(CalendarDate, "%m%d%Y")
    ) %>%
    bind_rows(tibble(CalendarDate=as.Date("1899-01-01"),
                         DateID=unknownCodeTableValue,
                         Year=0,
                         YearLabel='Unk',
                         CalendarQuarter=0,
                         Month=0,
                         MonthName='Unknown',
                         FullMonth='Unknown',
                         Day=0,
                         DayOfWeek='Unknown',
                         DayOfWeekSort=0,
                         DateMMDDYYYY='Unknown')) %>%
    bind_rows(tibble(CalendarDate=as.Date("1899-01-02"),
                         DateID=noneCodeTableValue,
                         Year=0,
                         YearLabel='None',
                         CalendarQuarter=0,
                         Month=0,
                         MonthName='None',
                         FullMonth='None',
                         Day=0,
                         DayOfWeek='None',
                         DayOfWeekSort=0,
                         DateMMDDYYYY='None'))
  DateDf <- DateDf %>% filter(!(CalendarDate %in% datesToExclude))
  writeLines(paste0("Adding ", nrow(DateDf), " new dates to the Date dimension"))
  DateDf
}

truncateTables <- function(adsConnection) {

  dbExecute(adsConnection, 'set FOREIGN_KEY_CHECKS=0')

  executeQuery(adsConnection, "truncate JailEpisodeCharge")
  executeQuery(adsConnection, "truncate JailEpisodeArrest")
  executeQuery(adsConnection, "truncate JailEpisode")
  executeQuery(adsConnection, "truncate BehavioralHealthAssessmentCategory")
  executeQuery(adsConnection, "truncate BehavioralHealthTreatment")
  executeQuery(adsConnection, "truncate BehavioralHealthEvaluation")
  executeQuery(adsConnection, "truncate PrescribedMedication")
  executeQuery(adsConnection, "truncate BehavioralHealthAssessment")
  executeQuery(adsConnection, "truncate Person")
  executeQuery(adsConnection, "truncate LoadHistory")

  executeQuery(adsConnection, "truncate PersonSexType")
  executeQuery(adsConnection, "truncate PersonEthnicityType")
  executeQuery(adsConnection, "truncate PersonRaceType")
  executeQuery(adsConnection, "truncate PersonAgeType")
  executeQuery(adsConnection, "truncate PersonAgeRangeType")
  executeQuery(adsConnection, "truncate Facility")
  executeQuery(adsConnection, "truncate Agency")
  executeQuery(adsConnection, "truncate AssessmentCategoryType")
  executeQuery(adsConnection, "truncate BondStatusType")
  executeQuery(adsConnection, "truncate BondType")
  executeQuery(adsConnection, "truncate ChargeClassType")
  executeQuery(adsConnection, "truncate DomicileStatusType")
  executeQuery(adsConnection, "truncate JurisdictionType")
  executeQuery(adsConnection, "truncate LanguageType")
  executeQuery(adsConnection, "truncate MedicaidStatusType")
  executeQuery(adsConnection, "truncate MilitaryServiceStatusType")
  executeQuery(adsConnection, "truncate ProgramEligibilityType")
  executeQuery(adsConnection, "truncate SexOffenderStatusType")
  executeQuery(adsConnection, "truncate SupervisionUnitType")
  executeQuery(adsConnection, "truncate TreatmentAdmissionReasonType")
  executeQuery(adsConnection, "truncate TreatmentStatusType")
  executeQuery(adsConnection, "truncate WorkReleaseStatusType")
  executeQuery(adsConnection, "truncate CaseStatusType")
  executeQuery(adsConnection, "truncate ChargeDispositionType")
  executeQuery(adsConnection, "truncate EducationLevelType")
  executeQuery(adsConnection, "truncate OccupationType")
  executeQuery(adsConnection, "truncate PopulationType")
  executeQuery(adsConnection, "truncate Date")

}
