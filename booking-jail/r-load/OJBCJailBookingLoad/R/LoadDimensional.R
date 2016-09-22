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

#' @importFrom RMySQL MySQL
defaultStagingConnectionBuilder <- function() {
  stagingConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_staging_demo", username="root")
  stagingConnection
}

#' @importFrom RMySQL MySQL
defaultDimensionalConnectionBuilder <- function() {
  adsConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")
  adsConnection
}

defaultEducationTextValueConverter <- function(textValues, unknownCodeTableValue) {
  ret <- as.integer(gsub(x=textValues, pattern="Education Level ([0-9]+)", replacement="\\1"))
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

defaultOccupationTextValueConverter <- function(textValues, unknownCodeTableValue) {
  ret <- as.integer(gsub(x=textValues, pattern="Occupation ([0-9]+)", replacement="\\1"))
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
  ret
}

defaultDiagnosisTextValueConverter <- function(textValues, unknownCodeTableValue) {
  units <- as.integer(gsub(x=textValues, pattern="Diagnosis ([0-9]+)", replacement="\\1"))
  units <- (units %/% 10) + 1
  units <- ifelse(is.na(units) | units > 10, unknownCodeTableValue, units)
  units
}

defaultMedicationTextValueConverter <- function(textValues, unknownCodeTableValue) {
  units <- as.integer(gsub(x=textValues, pattern="Medication ([0-9]+)", replacement="\\1"))
  units <- (units %/% 10) + 1
  units <- ifelse(is.na(units) | units > 10, unknownCodeTableValue, units)
  units
}

defaultDispositionTextConverter <- function(textValues, unknownCodeTableValue) {
  ret <- as.integer(gsub(x=textValues, pattern="Charge Disposition ([0-9]+)", replacement="\\1"))
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

defaultChargeCodeTextConverter <- function(textValues, unknownCodeTableValue) {
  units <- as.integer(gsub(x=textValues, pattern="Charge Code ([0-9]+)", replacement="\\1"))
  units <- (units %% 10) + 1
  units <- ifelse(is.na(units) | units > 7, unknownCodeTableValue, units)
  units
}

defaultProviderTextValueConverter <- function(textValues, unknownCodeTableValue) {
  ret <- as.integer(gsub(x=textValues, pattern="Treatment Provider ([0-9]+)", replacement="\\1"))
  ret[is.na(ret)] <- unknownCodeTableValue
  ret
}

defaultChargeDispositionAggregator <- function(BookingChargeDispositionDataFrame, unknownCodeTableValue) {
  df <- BookingChargeDispositionDataFrame %>%
    group_by(BookingID) %>%
    summarize(dispo=min(ChargeDisposition)) %>%
    mutate(dispo=defaultDispositionTextConverter(dispo, unknownCodeTableValue))
  df$dispo
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
  # suppressWarnings is necessary to avoid annoying type conversion warnings from the RMySQL driver
  suppressWarnings(dbGetQuery(conn, query))
}

updateLoadHistory <- function(adsConnection, currentLoadTime) {
  executeQuery(adsConnection, paste0("insert into LoadHistory (LoadHistoryTimestamp) values ('", formatDateTimeForSQL(currentLoadTime), "')"))
  loadHistory <- getQuery(adsConnection, paste0("select LoadHistoryID from LoadHistory where LoadHistoryTimestamp='", formatDateTimeForSQL(currentLoadTime), "'"))
  loadHistoryID <- loadHistory$LoadHistoryID
  loadHistoryID
}

translateCodeTableValue <- function(stagingValue, codeTableName, unknownCodeTableValue, codeTableList) {
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

buildJailEpisodeTables <- function(stagingConnection, adsConnection, lastLoadTime, currentLoadTime, loadHistoryID, unknownCodeTableValue, chargeDispositionAggregator, codeTableList) {

  buildTable <- function(StagingBookingDf, StagingBookingChargeDispositionDf, chargeDispositionAggregator) {

    JailEpisode <- StagingBookingDf %>%
      transmute(
        JailEpisodeID=BookingID,
        PersonID=PersonID,
        BookingNumber=BookingNumber,
        IsActive='Y',
        CaseStatusTypeID=unknownCodeTableValue,
        EpisodeStartDate=as.Date(BookingDate),
        FacilityID=translateCodeTableValue(FacilityID, "Facility", unknownCodeTableValue, codeTableList),
        SupervisionUnitTypeID=translateCodeTableValue(SupervisionUnitTypeID, "SupervisionUnitType", unknownCodeTableValue, codeTableList),
        DaysAgo=(EpisodeStartDate %--% currentLoadTime) %/% days(1),
        LengthOfStay=DaysAgo,
        LoadHistoryID=loadHistoryID)

    if (nrow(StagingBookingChargeDispositionDf)) {
      args <- list()
      args$BookingChargeDispositionDataFrame <- StagingBookingChargeDispositionDf
      args$unknownCodeTableValue <- unknownCodeTableValue
      JailEpisode$CaseStatusTypeID <- as.integer(do.call(chargeDispositionAggregator, args))
    }

    JailEpisode$DaysAgo <- as.integer(JailEpisode$DaysAgo)
    JailEpisode$LengthOfStay <- as.integer(JailEpisode$LengthOfStay)
    JailEpisode[, "DaysSinceLastEpisode"] <- NA
    JailEpisode[, "DaysUntilNextEpisode"] <- NA
    JailEpisode[, "SixMonthRebooking"] <- 'N'
    JailEpisode[, "OneYearRebooking"] <- 'N'
    JailEpisode[, "TwoYearRebooking"] <- 'N'
    JailEpisode

  }

  ret <- list()

  Booking <- getQuery(stagingConnection, paste0("select BookingNumber, BookingID, PersonID, BookingDate, ",
                                                "FacilityID, SupervisionUnitTypeID, InmateJailResidentIndicator from Booking ",
                                                " where BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BookingChargeDisposition <- getQuery(stagingConnection, paste0("select Booking.BookingID, ChargeDisposition from ",
                                                                 "Booking, BookingArrest, BookingCharge ",
                                                                 "where Booking.BookingID=BookingArrest.BookingID and ",
                                                                 "BookingArrest.BookingArrestID=BookingCharge.BookingArrestID and ",
                                                                 "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  ret$JailEpisode <- buildTable(Booking, BookingChargeDisposition, chargeDispositionAggregator)

  Booking <- getQuery(stagingConnection, paste0("select BookingNumber, CustodyStatusChange.BookingID, CustodyStatusChange.PersonID, CustodyStatusChange.BookingDate, ",
                                                "CustodyStatusChange.FacilityID, CustodyStatusChange.SupervisionUnitTypeID, ",
                                                "CustodyStatusChange.InmateJailResidentIndicator from Booking, CustodyStatusChange ",
                                                "where Booking.BookingID=CustodyStatusChange.BookingID and ",
                                                "CustodyStatusChangeTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BookingChargeDisposition <- getQuery(stagingConnection, paste0("select CustodyStatusChange.BookingID, ChargeDisposition from ",
                                                                 "Booking, CustodyStatusChange, CustodyStatusChangeArrest, CustodyStatusChangeCharge ",
                                                                 "where CustodyStatusChange.CustodyStatusChangeID=CustodyStatusChangeArrest.CustodyStatusChangeID and ",
                                                                 "CustodyStatusChangeArrest.CustodyStatusChangeArrestID=CustodyStatusChangeCharge.CustodyStatusChangeArrestID and ",
                                                                 "Booking.BookingID=CustodyStatusChange.BookingID and ",
                                                                 "CustodyStatusChangeTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  ret$JailEpisodeEdits <- buildTable(Booking, BookingChargeDisposition, chargeDispositionAggregator)

  ret

}

buildPersonTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, educationTextValueConverter, occupationTextValueConverter, codeTableList) {

  selectStatement <- paste0("select Person.PersonID as PersonID, PersonUniqueIdentifier, PersonUniqueIdentifier2, PersonAgeAtBooking, PersonBirthDate, ",
                            "EducationLevel, Occupation, LanguageTypeID, PersonSexTypeID, PersonRaceTypeID, ",
                            "PersonEthnicityTypeID, MilitaryServiceStatusTypeID, DomicileStatusTypeID, ",
                            "ProgramEligibilityTypeID, WorkReleaseStatusTypeID, SexOffenderStatusTypeID, BookingDate")

  Person <- getQuery(stagingConnection, paste0(selectStatement, " from Person, Booking ",
                                               "where BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "' and Person.PersonID=Booking.PersonID"))

  PersonE <- getQuery(stagingConnection, paste0(selectStatement, " from Person, CustodyStatusChange ",
                                                "where CustodyStatusChangeTimestamp > '", formatDateTimeForSQL(lastLoadTime), "' and Person.PersonID=CustodyStatusChange.PersonID"))

  Person <- Person %>% bind_rows(PersonE) %>%
    transmute(PersonID=PersonID,
              StagingPersonUniqueIdentifier=PersonUniqueIdentifier,
              StagingPersonUniqueIdentifier2=PersonUniqueIdentifier2,
              LanguageTypeID=translateCodeTableValue(LanguageTypeID, "LanguageType", unknownCodeTableValue, codeTableList),
              PersonSexTypeID=translateCodeTableValue(PersonSexTypeID, "PersonSexType", unknownCodeTableValue, codeTableList),
              PersonRaceTypeID=translateCodeTableValue(PersonRaceTypeID, "PersonRaceType", unknownCodeTableValue, codeTableList),
              PersonEthnicityTypeID=translateCodeTableValue(PersonEthnicityTypeID, "PersonEthnicityType", unknownCodeTableValue, codeTableList),
              MilitaryServiceStatusTypeID=translateCodeTableValue(MilitaryServiceStatusTypeID, "MilitaryServiceStatusType", unknownCodeTableValue, codeTableList),
              DomicileStatusTypeID=translateCodeTableValue(DomicileStatusTypeID, "DomicileStatusType", unknownCodeTableValue, codeTableList),
              ProgramEligibilityTypeID=translateCodeTableValue(ProgramEligibilityTypeID, "ProgramEligibilityType", unknownCodeTableValue, codeTableList),
              WorkReleaseStatusTypeID=translateCodeTableValue(WorkReleaseStatusTypeID, "WorkReleaseStatusType", unknownCodeTableValue, codeTableList),
              SexOffenderStatusTypeID=translateCodeTableValue(SexOffenderStatusTypeID, "SexOffenderStatusType", unknownCodeTableValue, codeTableList),
              Occupation=Occupation,
              EducationLevel=EducationLevel,
              PopulationTypeID=unknownCodeTableValue,
              EducationLevelTypeID=unknownCodeTableValue,
              OccupationTypeID=unknownCodeTableValue,
              PersonAgeTypeID=ifelse(is.na(PersonBirthDate), PersonAgeAtBooking, (PersonBirthDate %--% BookingDate) %/% years(1))
    ) %>%
    mutate(PersonAgeTypeID=ifelse(is.na(PersonAgeTypeID), unknownCodeTableValue, as.integer(PersonAgeTypeID)))

  dups <- unique(Person$PersonID[duplicated(Person$PersonID)])
  dups <- length(dups)
  if (dups) {
    stop(paste0("Dimensional load failed.  ", dups, " duplicate PersonIDs found in Booking / CustodyStatusChange."))
  }

  if (nrow(Person)) {
    args <- list()
    args$textValues <- Person$EducationLevel
    args$unknownCodeTableValue <- unknownCodeTableValue
    Person$EducationLevelTypeID <- as.integer(do.call(educationTextValueConverter, args))
    args$textValues <- Person$Occupation
    args$unknownCodeTableValue <- unknownCodeTableValue
    Person$OccupationTypeID <- as.integer(do.call(occupationTextValueConverter, args))
  }

  Person %>% select(-EducationLevel, -Occupation)

}

buildArrestTables <- function(stagingConnection, adsConnection, lastLoadTime, unknownCodeTableValue, codeTableList) {

  buildTable <- function(parentBookingTable, arrestTable, baseArrestID) {

    Arrest <- getQuery(stagingConnection, paste0("select ", parentBookingTable, ".BookingID, ", arrestTable, "ID as pk, LocationLatitude, LocationLongitude, ArrestAgencyID ",
                                                 "from (", arrestTable, " inner join ", parentBookingTable,
                                                 " on ", arrestTable, ".", parentBookingTable, "ID=", parentBookingTable, ".", parentBookingTable, "ID) ",
                                                 "left join Location on ", arrestTable, ".LocationID=Location.LocationID where ",
                                                 parentBookingTable, "Timestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

    Arrest <- Arrest %>%
      transmute(JailEpisodeArrestID=as.integer(row_number() + baseArrestID),
                JailEpisodeID=BookingID,
                ArrestLocationLatitude=LocationLatitude,
                ArrestLocationLongitude=LocationLongitude,
                AgencyID=translateCodeTableValue(ArrestAgencyID, "Agency", unknownCodeTableValue, codeTableList),
                StagingPK=pk)

    Arrest

  }

  baseArrestID <- getQuery(adsConnection, "select max(JailEpisodeArrestID) as BaseID from JailEpisodeArrest")
  baseArrestID <- baseArrestID$BaseID
  baseArrestID <- ifelse(is.na(baseArrestID), 0, baseArrestID) + 1

  ret <- list()
  ret$Arrest <- buildTable('Booking', 'BookingArrest', baseArrestID)

  baseArrestID <- baseArrestID + nrow(ret$Arrest) + 1

  ret$ArrestEdits <- buildTable('CustodyStatusChange', 'CustodyStatusChangeArrest', baseArrestID)

  ret

}

buildChargeTables <- function(stagingConnection, adsConnection, lastLoadTime, unknownCodeTableValue,
                              chargeCodeTypeTextConverter, chargeCodeClassTextConverter, dispositionTextConverter, ArrestDf, ArrestEditsDf, codeTableList) {

  buildTable <- function(grandparentBookingTable, parentArrestTable, chargeTable, baseChargeID) {

    Charge <- getQuery(stagingConnection, paste0("select ",
                                                 chargeTable, "ID as pk, ", chargeTable, ".", parentArrestTable, "ID as ParentArrestID, ",
                                                 parentArrestTable, ".", grandparentBookingTable, "ID as GrandparentBookingRecordPK, ",
                                                 grandparentBookingTable, ".BookingID as GrandparentBookingID, ",
                                                 "ChargeCode, ChargeDisposition, AgencyID, BondTypeID, BondAmount, ChargeJurisdictionTypeID, ",
                                                 "BondStatusTypeID from ",
                                                 grandparentBookingTable, ", ", parentArrestTable, ", ", chargeTable, " where ",
                                                 chargeTable, ".", parentArrestTable, "ID=", parentArrestTable, ".", parentArrestTable, "ID and ",
                                                 parentArrestTable, ".", grandparentBookingTable, "ID=", grandparentBookingTable, ".", grandparentBookingTable, "ID and ",
                                                 grandparentBookingTable, "Timestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

    Charge <- Charge %>%
      transmute(JailEpisodeChargeID=as.integer(row_number() + baseChargeID),
                ParentArrestID=ParentArrestID,
                ChargeCode=ChargeCode,
                ChargeDisposition=ChargeDisposition,
                ChargeTypeID=as.integer(unknownCodeTableValue),
                ChargeClassTypeID=as.integer(unknownCodeTableValue),
                ChargeDispositionTypeID=unknownCodeTableValue,
                AgencyID=translateCodeTableValue(AgencyID, "Agency", unknownCodeTableValue, codeTableList),
                JurisdictionTypeID=translateCodeTableValue(ChargeJurisdictionTypeID, "JurisdictionType", unknownCodeTableValue, codeTableList),
                BondStatusTypeID=translateCodeTableValue(BondStatusTypeID, "BondStatusType", unknownCodeTableValue, codeTableList),
                BondTypeID=translateCodeTableValue(BondTypeID, "BondType", unknownCodeTableValue, codeTableList),
                BondAmount=BondAmount, StagingPK=pk)

    if (nrow(Charge)) {

      args <- list()
      args$textValues <- Charge$ChargeCode
      args$unknownCodeTableValue <- unknownCodeTableValue
      Charge$ChargeTypeID <- as.integer(do.call(chargeCodeTypeTextConverter, args))
      Charge$ChargeClassTypeID <- as.integer(do.call(chargeCodeClassTextConverter, args))

      args <- list()
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

#' @importFrom lubridate %--%
buildBHAssessmentTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, codeTableList) {

  BHAssessment <- getQuery(stagingConnection, paste0("select BehavioralHealthAssessmentID, Person.PersonID, SeriousMentalIllnessIndicator, MedicaidStatusTypeID,",
                                                     " CareEpisodeStartDate, CareEpisodeEndDate, BookingDate from ",
                                                     "BehavioralHealthAssessment, Person, Booking where ",
                                                     "BehavioralHealthAssessment.PersonID=Person.PersonID and ",
                                                     "Booking.PersonID=Person.PersonID and ",
                                                     "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHAssessment <- BHAssessment %>%
    transmute(BehavioralHealthAssessmentID=BehavioralHealthAssessmentID,
              PersonID=PersonID,
              SevereMentalIllnessIndicator=SeriousMentalIllnessIndicator,
              MedicaidStatusTypeID=translateCodeTableValue(MedicaidStatusTypeID, "MedicaidStatusType", unknownCodeTableValue, codeTableList),
              InTreatmentAtBooking=is.na(CareEpisodeEndDate),
              EndedDaysBeforeBooking=(CareEpisodeEndDate %--% BookingDate) %/% days(1)
    ) %>%
    mutate(EndedDaysBeforeBooking=as.integer(ifelse(is.na(EndedDaysBeforeBooking), unknownCodeTableValue, EndedDaysBeforeBooking)))

  BHAssessment

}

buildBHAssessmentCategoryTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, codeTableList) {

  BHAssessmentCategory <- getQuery(stagingConnection, paste0("select BehavioralHealthAssessmentCategoryID, bha.BehavioralHealthAssessmentID, AssessmentCategoryTypeID from ",
                                                             "BehavioralHealthAssessment bha, BehavioralHealthAssessmentCategory bhac, Person p, Booking b where ",
                                                             "bhac.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                             "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHAssessmentCategory <- BHAssessmentCategory %>%
    transmute(BehavioralHealthAssessmentCategoryID=BehavioralHealthAssessmentCategoryID,
              BehavioralHealthAssessmentID=BehavioralHealthAssessmentID,
              AssessmentCategoryTypeID=translateCodeTableValue(AssessmentCategoryTypeID, "AssessmentCategoryType", unknownCodeTableValue, codeTableList))

  BHAssessmentCategory


}

buildBHTreatmentTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, providerTextValueConverter, codeTableList) {

  BHTreatment <- getQuery(stagingConnection, paste0("select TreatmentID, bha.BehavioralHealthAssessmentID, TreatmentStatusTypeID, TreatmentAdmissionReasonTypeID, ",
                                                    "TreatmentProviderName, TreatmentStartDate, BookingDate from ",
                                                    "BehavioralHealthAssessment bha, Treatment bht, Person p, Booking b where ",
                                                    "bht.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                    "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHTreatment <- BHTreatment %>%
    transmute(BehavioralHealthTreatmentID=TreatmentID,
              BehavioralHealthAssessmentID=BehavioralHealthAssessmentID,
              TreatmentStatusTypeID=translateCodeTableValue(TreatmentStatusTypeID, "TreatmentStatusType", unknownCodeTableValue, codeTableList),
              TreatmentAdmissionReasonTypeID=translateCodeTableValue(TreatmentAdmissionReasonTypeID, "TreatmentAdmissionReasonType", unknownCodeTableValue, codeTableList),
              TreatmentProviderName=TreatmentProviderName,
              TreatmentProviderTypeID=unknownCodeTableValue,
              DaysBeforeBooking=(TreatmentStartDate %--% BookingDate) %/% days(1)
    ) %>%
    mutate(DaysBeforeBooking=as.integer(ifelse(is.na(DaysBeforeBooking), unknownCodeTableValue, DaysBeforeBooking)))

  if (nrow(BHTreatment)) {
    args <- list()
    args$textValues <- BHTreatment$TreatmentProviderName
    args$unknownCodeTableValue <- unknownCodeTableValue
    BHTreatment$TreatmentProviderTypeID <- as.integer(do.call(providerTextValueConverter, args))
  }

  BHTreatment %>% select(-TreatmentProviderName)

}

buildBHEvaluationTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, diagnosisTextValueConverter, codeTableList) {

  BHEvaluation <- getQuery(stagingConnection, paste0("select BehavioralHealthEvaluationID, bhe.BehavioralHealthAssessmentID, BehavioralHealthDiagnosisDescription from ",
                                                     "BehavioralHealthAssessment bha, BehavioralHealthEvaluation bhe, Person p, Booking b where ",
                                                     "bhe.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                     "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHEvaluation <- BHEvaluation %>%
    mutate(BehavioralHealthEvaluationTypeID=unknownCodeTableValue) %>%
    select(BehavioralHealthEvaluationID, BehavioralHealthAssessmentID, BehavioralHealthDiagnosisDescription)

  if (nrow(BHEvaluation)) {
    args <- list()
    args$textValues <- BHEvaluation$BehavioralHealthDiagnosisDescription
    args$unknownCodeTableValue <- unknownCodeTableValue
    BHEvaluation$BehavioralHealthEvaluationTypeID <- as.integer(do.call(diagnosisTextValueConverter, args))
  }

  BHEvaluation %>% select(-BehavioralHealthDiagnosisDescription)

}

buildMedicationTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, medicationTextValueConverter, codeTableList) {

  PrescribedMedication <- getQuery(stagingConnection, paste0("select PrescribedMedicationID, pm.BehavioralHealthAssessmentID, MedicationDescription from ",
                                                             "BehavioralHealthAssessment bha, PrescribedMedication pm, Person p, Booking b where ",
                                                             "pm.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                             "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  PrescribedMedication <- PrescribedMedication %>%
    mutate(MedicationTypeID=unknownCodeTableValue) %>%
    select(PrescribedMedicationID, BehavioralHealthAssessmentID, MedicationDescription)

  if (nrow(PrescribedMedication)) {
    args <- list()
    args$textValues <- PrescribedMedication$MedicationDescription
    args$unknownCodeTableValue <- unknownCodeTableValue
    PrescribedMedication$MedicationTypeID <- as.integer(do.call(medicationTextValueConverter, args))
  }

  PrescribedMedication %>% select(-MedicationDescription)

}

buildReleaseTable <- function(stagingConnection, lastLoadTime, codeTableList) {

  Release <- getQuery(stagingConnection, paste0("select ReleaseDate, BookingID from CustodyRelease ",
                                                "where CustodyReleaseTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  Release

}

buildAndLoadHistoricalPeriodTable <- function(adsConnection, unknownCodeTableValue, noneCodeTableValue) {

  lookbackPeriod <- 365*10 # ten years

  df <- data.frame(HistoricalPeriodTypeID=as.integer(0:lookbackPeriod),
                   DaysAgo=0:lookbackPeriod) %>%
    mutate(HistoricalPeriodTypeDescription1=paste0(90*(1 + DaysAgo %/% 90), " days"),
           HistoricalPeriodTypeDescription1=ifelse(DaysAgo >= 360, "360+ days", HistoricalPeriodTypeDescription1),
           HistoricalPeriodTypeDescription2=ifelse(DaysAgo >= 360, "36 months", HistoricalPeriodTypeDescription1),
           HistoricalPeriodTypeDescription2=ifelse(DaysAgo >= 360*3, "36 months+", HistoricalPeriodTypeDescription2))

  df <- df %>% bind_rows(data.frame(HistoricalPeriodTypeID=c(unknownCodeTableValue, noneCodeTableValue),
                                    DaysAgo=as.integer(c(0,0)),
                                    HistoricalPeriodTypeDescription1=c("Unknown", "None"),
                                    HistoricalPeriodTypeDescription2=c("Unknown", "None")))

  writeTableToDatabase(adsConnection, "HistoricalPeriodType", df)

  ret <- list()
  ret$HistoricalPeriodType <- df
  ret

}

#' @importFrom lubridate now as_date %--%
#' @import dplyr
#' @export
loadDimensionalDatabase <- function(stagingConnectionBuilder=defaultStagingConnectionBuilder,
                                    dimensionalConnectionBuilder=defaultDimensionalConnectionBuilder,
                                    chargeDispositionAggregator=defaultChargeDispositionAggregator,
                                    educationTextValueConverter=defaultEducationTextValueConverter,
                                    occupationTextValueConverter=defaultOccupationTextValueConverter,
                                    diagnosisTextValueConverter=defaultDiagnosisTextValueConverter,
                                    medicationTextValueConverter=defaultMedicationTextValueConverter,
                                    chargeCodeTypeTextConverter=defaultChargeCodeTextConverter,
                                    chargeCodeClassTextConverter=defaultChargeCodeTextConverter,
                                    dispositionTextConverter=defaultDispositionTextConverter,
                                    providerTextValueConverter=defaultProviderTextValueConverter,
                                    unknownCodeTableValue=as.integer(99999),
                                    noneCodeTableValue=as.integer(99998),
                                    completeLoad=TRUE,
                                    writeToDatabase=FALSE) {

  writeLines(paste0("Running ADS load with completeLoad=", completeLoad, " and writeToDatabase=", writeToDatabase))

  ret <- list()

  stagingConnection = do.call(stagingConnectionBuilder, list())
  adsConnection = do.call(dimensionalConnectionBuilder, list())

  if (writeToDatabase & completeLoad) {
    writeLines("Truncating all current tables")
    truncateTables(adsConnection)
  }

  lastLoadTime <- getLastLoadingTime(adsConnection)
  writeLines(paste0("lastLoadTime=", lastLoadTime))

  currentLoadTime <- now()
  loadHistoryID <- updateLoadHistory(adsConnection, currentLoadTime)

  writeLines("Loading code tables")
  codeTableList <- loadCodeTables(adsConnection, "DimensionalCodeTables.xlsx", writeToDatabase & completeLoad)
  ret <- c(ret, codeTableList)

  historicalPeriodType <- buildAndLoadHistoricalPeriodTable(adsConnection, unknownCodeTableValue, noneCodeTableValue)
  ret <- c(ret, historicalPeriodType)

  writeLines("Loading JailEpisode tables")
  jailEpisodeTables <- buildJailEpisodeTables(stagingConnection, adsConnection, lastLoadTime, currentLoadTime, loadHistoryID,
                                              unknownCodeTableValue, chargeDispositionAggregator, codeTableList)
  ret <- c(ret, jailEpisodeTables)
  writeLines(paste0("Loaded JailEpisode with ", nrow(ret$JailEpisode), " rows and JailEpisodeEdits with ", nrow(ret$JailEpisodeEdits), " rows"))

  writeLines("Loading Person table")
  ret$Person <- buildPersonTable(stagingConnection, lastLoadTime, unknownCodeTableValue, educationTextValueConverter,
                                 occupationTextValueConverter, codeTableList)
  writeLines(paste0("Loaded Person table with ", nrow(ret$Person), " rows"))

  writeLines("Loading Arrest tables")
  arrestTables <- buildArrestTables(stagingConnection, adsConnection, lastLoadTime, unknownCodeTableValue, codeTableList)
  ret <- c(ret, arrestTables)
  writeLines(paste0("Loaded Arrest with ", nrow(ret$Arrest), " rows and ArrestEdits with ", nrow(ret$ArrestEdits), " rows"))

  writeLines("Loading Charge tables")
  chargeTables <- buildChargeTables(stagingConnection, adsConnection, lastLoadTime, unknownCodeTableValue,
                                    chargeCodeTypeTextConverter, chargeCodeClassTextConverter, dispositionTextConverter,
                                    ret$Arrest, ret$ArrestEdits, codeTableList)
  ret <- c(ret, chargeTables)
  writeLines(paste0("Loaded Charge with ", nrow(ret$Charge), " rows and ChargeEdits with ", nrow(ret$ChargeEdits), " rows"))

  ret$Arrest <- ret$Arrest %>% select(-StagingPK)

  writeLines("Loading Behavioral Health tables")
  ret$BehavioralHealthAssessment <- buildBHAssessmentTable(stagingConnection, lastLoadTime, unknownCodeTableValue, codeTableList)
  writeLines(paste0("Loaded BehavioralHealthAssessment with ", nrow(ret$BehavioralHealthAssessment), " rows"))
  ret$BehavioralHealthAssessmentCategory <- buildBHAssessmentCategoryTable(stagingConnection, lastLoadTime, unknownCodeTableValue, codeTableList)
  writeLines(paste0("Loaded BehavioralHealthAssessmentCategory with ", nrow(ret$BehavioralHealthAssessmentCategory), " rows"))
  ret$BehavioralHealthTreatment <- buildBHTreatmentTable(stagingConnection, lastLoadTime, unknownCodeTableValue, providerTextValueConverter, codeTableList)
  writeLines(paste0("Loaded BehavioralHealthTreatment with ", nrow(ret$BehavioralHealthTreatment), " rows"))
  ret$BehavioralHealthEvaluation <- buildBHEvaluationTable(stagingConnection, lastLoadTime, unknownCodeTableValue, diagnosisTextValueConverter, codeTableList)
  writeLines(paste0("Loaded BehavioralHealthEvaluation with ", nrow(ret$BehavioralHealthEvaluation), " rows"))
  ret$PrescribedMedication <- buildMedicationTable(stagingConnection, lastLoadTime, unknownCodeTableValue, medicationTextValueConverter, codeTableList)
  writeLines(paste0("Loaded PrescribedMedication with ", nrow(ret$PrescribedMedication), " rows"))

  writeLines("Loading Release table")
  ret$Release <- buildReleaseTable(stagingConnection, lastLoadTime, codeTableList)
  writeLines(paste0("Loaded Release with ", nrow(ret$Release), " rows"))

  persistTables(adsConnection, ret)

  determineRecidivism(adsConnection)

  dbDisconnect(stagingConnection)
  dbDisconnect(adsConnection)

  writeLines("ADS load complete")

  ret

}

determineRecidivism <- function(adsConnection) {

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

  writeLines("Recidivism determination complete")

  invisible()

}

persistTables <- function(adsConnection, dfs) {

  checkForAndRemoveDuplicateBookings(adsConnection, dfs)

  writeLines("Writing main transaction tables to database")

  writeDataFrameToDatabase(adsConnection, dfs$Person, "Person", viaBulk = TRUE)
  writeDataFrameToDatabase(adsConnection, dfs$JailEpisode, "JailEpisode", viaBulk = TRUE)
  writeDataFrameToDatabase(adsConnection, dfs$Arrest, "JailEpisodeArrest", viaBulk = TRUE)
  writeDataFrameToDatabase(adsConnection, dfs$Charge, "JailEpisodeCharge", viaBulk = TRUE)
  writeDataFrameToDatabase(adsConnection, dfs$BehavioralHealthAssessment, "BehavioralHealthAssessment", viaBulk = TRUE)
  writeDataFrameToDatabase(adsConnection, dfs$BehavioralHealthAssessmentCategory, "BehavioralHealthAssessmentCategory", viaBulk = TRUE)
  writeDataFrameToDatabase(adsConnection, dfs$BehavioralHealthTreatment, "BehavioralHealthTreatment", viaBulk = TRUE)
  writeDataFrameToDatabase(adsConnection, dfs$BehavioralHealthEvaluation, "BehavioralHealthEvaluation", viaBulk = TRUE)
  writeDataFrameToDatabase(adsConnection, dfs$PrescribedMedication, "PrescribedMedication", viaBulk = TRUE)

  writeLines("Done writing main transaction tables to database")

  writeLines("Applying JailEpisodeEdits")
  applyEdits(adsConnection, dfs)

  writeLines("Processing releases")
  persistReleases(adsConnection, dfs)

}

checkForAndRemoveDuplicateBookings <- function(adsConnection, dfs) {

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

    removeBookingsAndChildren(adsConnection, dupBookingIDs)

  } else {
    writeLines("...no duplicates found")
  }

}

applyEdits <- function(adsConnection, dfs) {

  if (nrow(dfs$JailEpisodeEdits)) {

    writeLines(paste0("Applying ", nrow(dfs$JailEpisodeEdits), " JailEpisodeEdits"))

    removeBookingsAndChildren(adsConnection, dfs$JailEpisodeEdits$JailEpisodeID)

    writeLines("Adding edited Booking, Arrest, and Charge records")
    writeDataFrameToDatabase(adsConnection, dfs$JailEpisodeEdits, "JailEpisode", viaBulk = TRUE)
    writeDataFrameToDatabase(adsConnection, dfs$ArrestEdits %>% select(-StagingPK), "JailEpisodeArrest", viaBulk = TRUE)
    writeDataFrameToDatabase(adsConnection, dfs$ChargeEdits %>% select(-StagingPK), "JailEpisodeCharge", viaBulk = TRUE)

  } else {
    writeLines("No Jail Episode edits found in this load")
  }

}

removeBookingsAndChildren <- function(adsConnection, BookingIDs, groupSize=50) {

  # split the booking edits into chunks and delete them in groups

  writeLines(paste0("Removing ", length(BookingIDs), " bookings and all their children"))

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

  invisible()

}

#' @importFrom lubridate %--%
persistReleases <- function(adsConnection, dfs) {

  bookingDf <- getQuery(adsConnection, "select JailEpisodeID, EpisodeStartDate from JailEpisode")

  if (nrow(bookingDf)) {

    bookingDf <- bookingDf %>%
      inner_join(dfs$Release, by=c("JailEpisodeID"="BookingID")) %>%
      mutate(LengthOfStay=(EpisodeStartDate %--% ReleaseDate) %/% days(1))

    if (nrow(bookingDf)) {

      for (r in seq(nrow(bookingDf))) {
        bookingID <- bookingDf[r, 'JailEpisodeID']
        lengthOfStay <- bookingDf[r, 'LengthOfStay']
        sql <- paste0("update JailEpisode set IsActive='N', LengthOfStay=", lengthOfStay, " where JailEpisodeID=", bookingID)
        executeQuery(adsConnection, sql)
      }

    }

    writeLines(paste0("Updated ", nrow(bookingDf), " JailEpisode records with release information"))

  } else {
    writeLines("No releases found")
  }

  invisible()

}

truncateTables <- function(adsConnection) {

  executeQuery(adsConnection, "delete from JailEpisodeCharge")
  executeQuery(adsConnection, "delete from JailEpisodeArrest")
  executeQuery(adsConnection, "delete from JailEpisode")
  executeQuery(adsConnection, "delete from BehavioralHealthAssessmentCategory")
  executeQuery(adsConnection, "delete from BehavioralHealthTreatment")
  executeQuery(adsConnection, "delete from BehavioralHealthEvaluation")
  executeQuery(adsConnection, "delete from PrescribedMedication")
  executeQuery(adsConnection, "delete from BehavioralHealthAssessment")
  executeQuery(adsConnection, "delete from Person")
  executeQuery(adsConnection, "delete from LoadHistory")

  executeQuery(adsConnection, "delete from PersonSexType")
  executeQuery(adsConnection, "delete from PersonEthnicityType")
  executeQuery(adsConnection, "delete from PersonRaceType")
  executeQuery(adsConnection, "delete from PersonAgeType")
  executeQuery(adsConnection, "delete from PersonAgeRangeType")
  executeQuery(adsConnection, "delete from Facility")
  executeQuery(adsConnection, "delete from Agency")
  executeQuery(adsConnection, "delete from AssessmentCategoryType")
  executeQuery(adsConnection, "delete from BondStatusType")
  executeQuery(adsConnection, "delete from BondType")
  executeQuery(adsConnection, "delete from ChargeClassType")
  executeQuery(adsConnection, "delete from DomicileStatusType")
  executeQuery(adsConnection, "delete from JurisdictionType")
  executeQuery(adsConnection, "delete from LanguageType")
  executeQuery(adsConnection, "delete from MedicaidStatusType")
  executeQuery(adsConnection, "delete from MilitaryServiceStatusType")
  executeQuery(adsConnection, "delete from ProgramEligibilityType")
  executeQuery(adsConnection, "delete from SexOffenderStatusType")
  executeQuery(adsConnection, "delete from SupervisionUnitType")
  executeQuery(adsConnection, "delete from TreatmentAdmissionReasonType")
  executeQuery(adsConnection, "delete from TreatmentStatusType")
  executeQuery(adsConnection, "delete from WorkReleaseStatusType")
  executeQuery(adsConnection, "delete from CaseStatusType")
  executeQuery(adsConnection, "delete from ChargeDispositionType")
  executeQuery(adsConnection, "delete from EducationLevelType")
  executeQuery(adsConnection, "delete from OccupationType")
  executeQuery(adsConnection, "delete from PopulationType")

}
