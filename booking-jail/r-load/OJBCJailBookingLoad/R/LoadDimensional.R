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

defaultCodeTableDataFrameBuilder <- NA
defaultCodeValueTranslationListBuilder <- NA

#' @importFrom RMySQL MySQL
defaultDimensionalConnectionBuilder <- function() {
  adsConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")
  adsConnection
}

defaultEducationTextValueConverter <- function(textValues) {
  as.integer(gsub(x=textValues, pattern="Education Level ([0-9]+)", replacement="\\1"))
}

defaultOccupationTextValueConverter <- function(textValues) {
  as.integer(gsub(x=textValues, pattern="Occupation ([0-9]+)", replacement="\\1"))
}

defaultDiagnosisTextValueConverter <- function(textValues) {
  as.integer(gsub(x=textValues, pattern="Diagnosis ([0-9]+)", replacement="\\1"))
}

defaultMedicationTextValueConverter <- function(textValues) {
  as.integer(gsub(x=textValues, pattern="Medication ([0-9]+)", replacement="\\1"))
}

defaultDispositionTextConverter <- function(textValues) {
  as.integer(gsub(x=textValues, pattern="Charge Disposition ([0-9]+)", replacement="\\1"))
}

defaultChargeCodeTypeTextConverter <- function(textValues) {
  as.integer(gsub(x=textValues, pattern="Charge Code ([0-9]+)", replacement="\\1"))
}

defaultChargeCodeClassTextConverter <- function(textValues) {
  as.integer(gsub(x=textValues, pattern="Charge Code ([0-9]+)", replacement="\\1"))
}

defaultProviderTextValueConverter <- function(textValues) {
  as.integer(gsub(x=textValues, pattern="Treatment Provider ([0-9]+)", replacement="\\1"))
}

defaultChargeDispositionAggregator <- function(BookingChargeDispositionDataFrame) {
  df <- BookingChargeDispositionDataFrame %>%
    group_by(BookingID) %>%
    summarize(dispo=min(ChargeDisposition)) %>%
    mutate(dispo=defaultDispositionTextConverter(dispo))
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
  dbClearResult(dbSendQuery(conn, query))
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

translateCodeTableValue <- function(stagingValue, codeTableName, unknownCodeTableValue, translationVectorList=list()) {
  # obviously a very simplistic translation that just replaces NA with the unknown value, and otherwise just returns the staging value as the ads value
  ifelse(is.na(stagingValue), unknownCodeTableValue, stagingValue)
}

buildJailEpisodeTables <- function(stagingConnection, adsConnection, lastLoadTime, currentLoadTime, loadHistoryID, unknownCodeTableValue, chargeDispositionAggregator) {

  buildTable <- function(stagingBookingTableName, extraFields="", stagingArrestTableName, stagingChargeTableName, chargeDispositionAggregator) {

    Booking <- getQuery(stagingConnection, paste0("select ", extraFields, " BookingID, PersonID, BookingDate, ",
                                                  "FacilityID, SupervisionUnitTypeID, InmateJailResidentIndicator from ", stagingBookingTableName,
                                                  " where ", stagingBookingTableName, "Timestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

    JailEpisode <- Booking %>%
      transmute(
        JailEpisodeID=BookingID,
        PersonID=PersonID,
        BookingNumber=BookingNumber,
        IsActive='Y',
        EpisodeStartDate=as.Date(BookingDate),
        FacilityID=translateCodeTableValue(FacilityID, "Facility", unknownCodeTableValue),
        SupervisionUnitTypeID=translateCodeTableValue(SupervisionUnitTypeID, "SupervisionUnitType", unknownCodeTableValue),
        DaysAgo=(EpisodeStartDate %--% currentLoadTime) %/% days(1),
        LengthOfStay=DaysAgo,
        LoadHistoryID=loadHistoryID)

    BookingChargeDisposition <- getQuery(stagingConnection, paste0("select ", stagingBookingTableName, ".BookingID, ChargeDisposition from ",
                                                                   stagingBookingTableName, ", ", stagingArrestTableName, ", ", stagingChargeTableName,
                                                                   " where ", stagingBookingTableName, ".", stagingBookingTableName, "ID=",
                                                                   stagingArrestTableName, ".", stagingBookingTableName, "ID and ",
                                                                   stagingArrestTableName, ".", stagingArrestTableName, "ID=",
                                                                   stagingChargeTableName, ".", stagingArrestTableName, "ID and ",
                                                                   stagingBookingTableName, "Timestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

    args <- list()
    args$BookingChargeDispositionDataFrame <- BookingChargeDisposition
    JailEpisode$CaseStatusTypeID <- do.call(chargeDispositionAggregator, args)

    JailEpisode

  }

  ret <- list()
  ret$JailEpisode <- buildTable('Booking', "BookingNumber,", "BookingArrest", "BookingCharge", chargeDispositionAggregator)

  existingBookingNumberDf <- getQuery(adsConnection, "select distinct BookingNumber from JailEpisode")
  allBookingNumbers <- c(ret$JailEpisode$BookingNumber, existingBookingNumberDf$BookingNumber)
  dups <- unique(allBookingNumbers[duplicated(allBookingNumbers)])
  if (length(dups)) {
    print(paste0(head(dups), sep=","))
    stop(paste0("Dimensional load failed, ", length(dups), " duplicate booking numbers encountered."))
  }

  # todo: determine if "null as BookingNumber" works on SQL Server
  ret$JailEpisodeEdits <- buildTable('CustodyStatusChange', "null as BookingNumber,", "CustodyStatusChangeArrest",
                                     "CustodyStatusChangeCharge", chargeDispositionAggregator)

  ret

}

buildPersonTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, educationTextValueConverter, occupationTextValueConverter) {

  selectStatement <- paste0("select Person.PersonID as PersonID, PersonUniqueIdentifier, PersonAgeAtBooking, PersonBirthDate, ",
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
              LanguageTypeID=translateCodeTableValue(LanguageTypeID, "LanguageType", unknownCodeTableValue),
              PersonSexTypeID=translateCodeTableValue(PersonSexTypeID, "PersonSexType", unknownCodeTableValue),
              PersonRaceTypeID=translateCodeTableValue(PersonRaceTypeID, "PersonRaceType", unknownCodeTableValue),
              PersonEthnicityTypeID=translateCodeTableValue(PersonEthnicityTypeID, "PersonEthnicityType", unknownCodeTableValue),
              MilitaryServiceStatusTypeID=translateCodeTableValue(MilitaryServiceStatusTypeID, "MilitaryServiceStatusType", unknownCodeTableValue),
              DomicileStatusTypeID=translateCodeTableValue(DomicileStatusTypeID, "DomicileStatusType", unknownCodeTableValue),
              ProgramEligibilityTypeID=translateCodeTableValue(ProgramEligibilityTypeID, "ProgramEligibilityType", unknownCodeTableValue),
              WorkReleaseStatusTypeID=translateCodeTableValue(WorkReleaseStatusTypeID, "WorkReleaseStatusType", unknownCodeTableValue),
              SexOffenderStatusTypeID=translateCodeTableValue(SexOffenderStatusTypeID, "SexOffenderStatusType", unknownCodeTableValue),
              Occupation=Occupation,
              EducationLevel=EducationLevel,
              PersonAgeTypeID=ifelse(is.na(PersonBirthDate), PersonAgeAtBooking, (PersonBirthDate %--% BookingDate) %/% years(1))
              )

  dups <- unique(Person$PersonID[duplicated(Person$PersonID)])
  dups <- length(dups)
  if (dups) {
    stop(paste0("Dimensional load failed.  ", dups, " duplicate PersonIDs found in Booking / CustodyStatusChange."))
  }

  args <- list()
  args$textValues <- Person$EducationLevel
  Person$EducationLevelTypeID <- do.call(educationTextValueConverter, args)
  args$textValues <- Person$Occupation
  Person$OccupationTypeID <- do.call(occupationTextValueConverter, args)

  Person %>% select(-EducationLevel, -Occupation)

}

buildArrestTables <- function(stagingConnection, adsConnection, lastLoadTime, unknownCodeTableValue) {

  buildTable <- function(parentBookingTable, arrestTable, baseArrestID) {

    Arrest <- getQuery(stagingConnection, paste0("select ", parentBookingTable, ".BookingID, ", arrestTable, "ID as pk, LocationLatitude, LocationLongitude, ArrestAgencyID ",
                                                 "from (", arrestTable, " inner join ", parentBookingTable,
                                                 " on ", arrestTable, ".", parentBookingTable, "ID=", parentBookingTable, ".", parentBookingTable, "ID) ",
                                                 "left join Location on ", arrestTable, ".LocationID=Location.LocationID where ",
                                                 parentBookingTable, "Timestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

    Arrest <- Arrest %>%
      transmute(JailEpisodeArrestID=row_number() + baseArrestID,
                JailEpisodeID=BookingID,
                ArrestLocationLatitude=LocationLatitude,
                ArrestLocationLongitude=LocationLongitude,
                AgencyID=translateCodeTableValue(ArrestAgencyID, "AgencyType", unknownCodeTableValue),
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
                              chargeCodeTypeTextConverter, chargeCodeClassTextConverter, dispositionTextConverter, ArrestDf, ArrestEditsDf) {

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
      transmute(JailEpisodeChargeID=row_number() + baseChargeID,
                ParentArrestID=ParentArrestID,
                ChargeCode=ChargeCode,
                ChargeDisposition=ChargeDisposition,
                AgencyID=translateCodeTableValue(AgencyID, "AgencyType", unknownCodeTableValue),
                JurisdictionTypeID=translateCodeTableValue(ChargeJurisdictionTypeID, "JurisdictionType", unknownCodeTableValue),
                BondStatusTypeID=translateCodeTableValue(BondStatusTypeID, "BondStatusType", unknownCodeTableValue),
                BondTypeID=translateCodeTableValue(BondTypeID, "BondType", unknownCodeTableValue),
                BondAmount=BondAmount, StagingPK=pk)

    args <- list()
    args$textValues <- Charge$ChargeCode
    Charge$ChargeTypeID <- do.call(chargeCodeTypeTextConverter, args)
    Charge$ChargeClassTypeID <- do.call(chargeCodeClassTextConverter, args)
    args$textValues <- Charge$ChargeDisposition
    Charge$ChargeDispositionTypeID <- do.call(dispositionTextConverter, args)

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
buildBHAssessmentTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue) {

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
              MedicaidStatusTypeID=translateCodeTableValue(MedicaidStatusTypeID, "MedicaidStatusType", unknownCodeTableValue),
              InTreatmentAtBooking=is.na(CareEpisodeEndDate),
              EndedDaysBeforeBooking=(CareEpisodeEndDate %--% BookingDate) %/% days(1)
              ) %>%
    mutate(EndedDaysBeforeBooking=ifelse(is.na(EndedDaysBeforeBooking), unknownCodeTableValue, EndedDaysBeforeBooking))

  BHAssessment

}

buildBHAssessmentCategoryTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue) {

  BHAssessmentCategory <- getQuery(stagingConnection, paste0("select BehavioralHealthAssessmentCategoryID, bha.BehavioralHealthAssessmentID, AssessmentCategoryTypeID from ",
                                                             "BehavioralHealthAssessment bha, BehavioralHealthAssessmentCategory bhac, Person p, Booking b where ",
                                                             "bhac.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                             "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHAssessmentCategory <- BHAssessmentCategory %>%
    transmute(BehavioralHealthAssessmentCategoryID=BehavioralHealthAssessmentCategoryID,
              BehavioralHealthAssessmentID=BehavioralHealthAssessmentID,
              AssessmentCategoryTypeID=translateCodeTableValue(AssessmentCategoryTypeID, "AssessmentCategoryType", unknownCodeTableValue))

  BHAssessmentCategory


}

buildBHTreatmentTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, providerTextValueConverter) {

  BHTreatment <- getQuery(stagingConnection, paste0("select TreatmentID, bha.BehavioralHealthAssessmentID, TreatmentStatusTypeID, TreatmentAdmissionReasonTypeID, ",
                                                    "TreatmentProviderName, TreatmentStartDate, BookingDate from ",
                                                    "BehavioralHealthAssessment bha, Treatment bht, Person p, Booking b where ",
                                                    "bht.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                    "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHTreatment <- BHTreatment %>%
    transmute(BehavioralHealthTreatmentID=TreatmentID,
              BehavioralHealthAssessmentID=BehavioralHealthAssessmentID,
              TreatmentStatusTypeID=translateCodeTableValue(TreatmentStatusTypeID, "TreatmentStatusType", unknownCodeTableValue),
              TreatmentAdmissionReasonTypeID=translateCodeTableValue(TreatmentAdmissionReasonTypeID, "TreatmentAdmissionReasonType", unknownCodeTableValue),
              TreatmentProviderName=TreatmentProviderName,
              DaysBeforeBooking=(TreatmentStartDate %--% BookingDate) %/% days(1)
    ) %>%
    mutate(DaysBeforeBooking=ifelse(is.na(DaysBeforeBooking), unknownCodeTableValue, DaysBeforeBooking))

  args <- list()
  args$textValues <- BHTreatment$TreatmentProviderName
  BHTreatment$TreatmentProviderTypeID <- do.call(providerTextValueConverter, args)

  BHTreatment %>% select(-TreatmentProviderName)

}

buildBHEvaluationTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, diagnosisTextValueConverter) {

  BHEvaluation <- getQuery(stagingConnection, paste0("select BehavioralHealthEvaluationID, bhe.BehavioralHealthAssessmentID, BehavioralHealthDiagnosisDescription from ",
                                                     "BehavioralHealthAssessment bha, BehavioralHealthEvaluation bhe, Person p, Booking b where ",
                                                     "bhe.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                     "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHEvaluation <- BHEvaluation %>%
    select(BehavioralHealthEvaluationID, BehavioralHealthAssessmentID, BehavioralHealthDiagnosisDescription)

  args <- list()
  args$textValues <- BHEvaluation$BehavioralHealthDiagnosisDescription
  BHEvaluation$BehavioralHealthEvaluationTypeID <- do.call(diagnosisTextValueConverter, args)

  BHEvaluation %>% select(-BehavioralHealthDiagnosisDescription)

}

buildMedicationTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, medicationTextValueConverter) {

  PrescribedMedication <- getQuery(stagingConnection, paste0("select PrescribedMedicationID, pm.BehavioralHealthAssessmentID, MedicationDescription from ",
                                                             "BehavioralHealthAssessment bha, PrescribedMedication pm, Person p, Booking b where ",
                                                             "pm.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                             "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  PrescribedMedication <- PrescribedMedication %>%
    select(PrescribedMedicationID, BehavioralHealthAssessmentID, MedicationDescription)

  args <- list()
  args$textValues <- PrescribedMedication$MedicationDescription
  PrescribedMedication$MedicationTypeID <- do.call(medicationTextValueConverter, args)

  PrescribedMedication %>% select(-MedicationDescription)

}

buildReleaseTable <- function(stagingConnection, lastLoadTime) {

  Release <- getQuery(stagingConnection, paste0("select ReleaseDate, BookingID from CustodyRelease ",
                                                "where CustodyReleaseTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  Release

}

buildAndLoadHistoricalPeriodTable <- function(adsConnection, unknownCodeTableValue, noneCodeTableValue) {

  lookbackPeriod <- 365*10 # ten years

  df <- data.frame(HistoricalPeriodTypeID=1:lookbackPeriod,
                   DaysAgo=1:lookbackPeriod) %>%
    mutate(HistoricalPeriodTypeDescription1=paste0(90*(1 + DaysAgo %/% 90), " days"),
           HistoricalPeriodTypeDescription1=ifelse(DaysAgo >= 360, "360+ days", HistoricalPeriodTypeDescription1),
           HistoricalPeriodTypeDescription2=ifelse(DaysAgo >= 360, "36 months", HistoricalPeriodTypeDescription1),
           HistoricalPeriodTypeDescription2=ifelse(DaysAgo >= 360*3, "36 months+", HistoricalPeriodTypeDescription2))

  df <- df %>% bind_rows(data.frame(HistoricalPeriodTypeID=c(unknownCodeTableValue, noneCodeTableValue),
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
                                    codeTableListBuilder=defaultCodeTableDataFrameBuilder,
                                    codeValueTranslationListBuilder=defaultCodeValueTranslationListBuilder,
                                    chargeDispositionAggregator=defaultChargeDispositionAggregator,
                                    educationTextValueConverter=defaultEducationTextValueConverter,
                                    occupationTextValueConverter=defaultOccupationTextValueConverter,
                                    diagnosisTextValueConverter=defaultDiagnosisTextValueConverter,
                                    medicationTextValueConverter=defaultMedicationTextValueConverter,
                                    chargeCodeTypeTextConverter=defaultChargeCodeTypeTextConverter,
                                    chargeCodeClassTextConverter=defaultChargeCodeClassTextConverter,
                                    dispositionTextConverter=defaultDispositionTextConverter,
                                    providerTextValueConverter=defaultProviderTextValueConverter,
                                    unknownCodeTableValue=99999,
                                    noneCodeTableValue=99998,
                                    completeLoad=TRUE,
                                    writeToDatabase=FALSE) {

  writeLines(paste0("Running ADS load with completeLoad=", completeLoad, " and writeToDatabase=", writeToDatabase))

  ret <- list()

  stagingConnection = do.call(stagingConnectionBuilder, list())
  adsConnection = do.call(dimensionalConnectionBuilder, list())

  lastLoadTime <- getLastLoadingTime(adsConnection)
  writeLines(paste0("lastLoadTime=", lastLoadTime))

  currentLoadTime <- now()
  loadHistoryID <- updateLoadHistory(adsConnection, currentLoadTime)

  executeQuery(adsConnection, "set foreign_key_checks=0")

  if (writeToDatabase & completeLoad) {
    writeLines("Truncating all current tables")
    truncateTables(adsConnection)
  }

  writeLines("Loading code tables")
  codeTableList <- loadCodeTables(adsConnection, "DimensionalCodeTables.xlsx", writeToDatabase & completeLoad)
  ret <- c(ret, codeTableList)

  historicalPeriodType <- buildAndLoadHistoricalPeriodTable(adsConnection, unknownCodeTableValue, noneCodeTableValue)
  ret <- c(ret, historicalPeriodType)

  writeLines("Loading JailEpisode tables")
  jailEpisodeTables <- buildJailEpisodeTables(stagingConnection, adsConnection, lastLoadTime, currentLoadTime, loadHistoryID, unknownCodeTableValue, chargeDispositionAggregator)
  ret <- c(ret, jailEpisodeTables)
  writeLines(paste0("Loaded JailEpisode with ", nrow(ret$JailEpisode), " rows and JailEpisodeEdits with ", nrow(ret$JailEpisodeEdits), " rows"))

  writeLines("Loading Person table")
  ret$Person <- buildPersonTable(stagingConnection, lastLoadTime, unknownCodeTableValue, educationTextValueConverter, occupationTextValueConverter)
  writeLines(paste0("Loaded Person table with ", nrow(ret$Person), " rows"))

  writeLines("Loading Arrest tables")
  arrestTables <- buildArrestTables(stagingConnection, adsConnection, lastLoadTime, unknownCodeTableValue)
  ret <- c(ret, arrestTables)
  writeLines(paste0("Loaded Arrest with ", nrow(ret$Arrest), " rows and ArrestEdits with ", nrow(ret$ArrestEdits), " rows"))

  writeLines("Loading Charge tables")
  chargeTables <- buildChargeTables(stagingConnection, adsConnection, lastLoadTime, unknownCodeTableValue,
                                    chargeCodeTypeTextConverter, chargeCodeClassTextConverter, dispositionTextConverter,
                                    ret$Arrest, ret$ArrestEdits)
  ret <- c(ret, chargeTables)
  writeLines(paste0("Loaded Charge with ", nrow(ret$Charge), " rows and ChargeEdits with ", nrow(ret$ChargeEdits), " rows"))

  ret$Arrest <- ret$Arrest %>% select(-StagingPK)

  writeLines("Loading Behavioral Health tables")
  ret$BehavioralHealthAssessment <- buildBHAssessmentTable(stagingConnection, lastLoadTime, unknownCodeTableValue)
  writeLines(paste0("Loaded BehavioralHealthAssessment with ", nrow(ret$BehavioralHealthAssessment), " rows"))
  ret$BehavioralHealthAssessmentCategory <- buildBHAssessmentCategoryTable(stagingConnection, lastLoadTime, unknownCodeTableValue)
  writeLines(paste0("Loaded BehavioralHealthAssessmentCategory with ", nrow(ret$BehavioralHealthAssessmentCategory), " rows"))
  ret$BehavioralHealthTreatment <- buildBHTreatmentTable(stagingConnection, lastLoadTime, unknownCodeTableValue, providerTextValueConverter)
  writeLines(paste0("Loaded BehavioralHealthTreatment with ", nrow(ret$BehavioralHealthTreatment), " rows"))
  ret$BehavioralHealthEvaluation <- buildBHEvaluationTable(stagingConnection, lastLoadTime, unknownCodeTableValue, diagnosisTextValueConverter)
  writeLines(paste0("Loaded BehavioralHealthEvaluation with ", nrow(ret$BehavioralHealthEvaluation), " rows"))
  ret$PrescribedMedication <- buildMedicationTable(stagingConnection, lastLoadTime, unknownCodeTableValue, medicationTextValueConverter)
  writeLines(paste0("Loaded PrescribedMedication with ", nrow(ret$PrescribedMedication), " rows"))

  writeLines("Loading Release table")
  ret$Release <- buildReleaseTable(stagingConnection, lastLoadTime)
  writeLines(paste0("Loaded Release with ", nrow(ret$Release), " rows"))

  persistTables(adsConnection, ret)

  # todo: now that you have episodes and people, you can do recidivism.  but note that you need to read the whole booking/person wad to do that right,
  #  so you have to wait until you write the final booking/person wad to the db

  dbDisconnect(stagingConnection)
  dbDisconnect(adsConnection)

  writeLines("ADS load complete")

  ret

}

persistTables <- function(adsConnection, dfs) {

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

applyEdits <- function(adsConnection, dfs) {

  writeLines(paste0("Applying ", nrow(dfs$JailEpisodeEdits), " JailEpisodeEdits"))

  # split the booking edits into chunks and delete them in groups
  groupSize <- 50
  episodeIDGroups <- split(dfs$JailEpisodeEdits$JailEpisodeID, ceiling(seq_along(dfs$JailEpisodeEdits$JailEpisodeID)/groupSize))

  for (g in episodeIDGroups) {
    idList <- paste0(g, collapse=",")
    executeQuery(adsConnection, paste0("delete from JailEpisodeCharge where JailEpisodeArrestID in (select JailEpisodeArrestID from JailEpisodeArrest where JailEpisodeID in (", idList, "))"))
    executeQuery(adsConnection, paste0("delete from JailEpisodeArrest where JailEpisodeID in (", idList, ")"))
    executeQuery(adsConnection, paste0("delete from JailEpisode where JailEpisodeID in (", idList, ")"))
  }

  writeLines("Adding edited Booking, Arrest, and Charge records")
  writeDataFrameToDatabase(adsConnection, dfs$JailEpisodeEdits, "JailEpisode", viaBulk = TRUE)
  writeDataFrameToDatabase(adsConnection, dfs$ArrestEdits %>% select(-StagingPK), "JailEpisodeArrest", viaBulk = TRUE)
  writeDataFrameToDatabase(adsConnection, dfs$ChargeEdits %>% select(-StagingPK), "JailEpisodeCharge", viaBulk = TRUE)

}

#' @importFrom lubridate %--%
persistReleases <- function(adsConnection, dfs) {

  bookingDf <- getQuery(adsConnection, "select JailEpisodeID, EpisodeStartDate from JailEpisode")

  if (nrow(bookingDf)) {

    bookingDf <- bookingDf %>%
      inner_join(dfs$Release, by=c("JailEpisodeID"="BookingID")) %>%
      mutate(LengthOfStay=(EpisodeStartDate %--% ReleaseDate) %/% days(1))

    for (r in seq(nrow(bookingDf))) {
      bookingID <- bookingDf[r, 'JailEpisodeID']
      lengthOfStay <- bookingDf[r, 'LengthOfStay']
      sql <- paste0("update JailEpisode set IsActive='N', LengthOfStay=", lengthOfStay, " where JailEpisodeID=", bookingID)
      executeQuery(adsConnection, sql)
    }

    writeLines(paste0("Updated ", nrow(bookingDf), " JailEpisode records with release information"))

  } else {
    writeLines("No releases found")
  }

  invisible()

}

truncateTables <- function(adsConnection) {
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

  executeQuery(adsConnection, "truncate JailEpisode")
  executeQuery(adsConnection, "truncate Person")
  executeQuery(adsConnection, "truncate JailEpisodeArrest")
  executeQuery(adsConnection, "truncate JailEpisodeCharge")
  executeQuery(adsConnection, "truncate BehavioralHealthAssessment")
  executeQuery(adsConnection, "truncate BehavioralHealthAssessmentCategory")
  executeQuery(adsConnection, "truncate BehavioralHealthTreatment")
  executeQuery(adsConnection, "truncate BehavioralHealthEvaluation")
  executeQuery(adsConnection, "truncate PrescribedMedication")

}
