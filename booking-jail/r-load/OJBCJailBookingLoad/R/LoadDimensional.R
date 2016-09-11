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
  dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_staging_demo", username="root")
}

defaultCodeTableDataFrameBuilder <- NA
defaultCodeValueTranslationListBuilder <- NA

#' @importFrom RMySQL MySQL
defaultDimensionalConnectionBuilder <- function() {
  dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")
}

defaultEducationTextValueConverter <- function(textValues) {
  as.integer(gsub(x=textValues, pattern="Education Level ([0-9]+)", replacement="\\1"))
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

defaultChargeCodeTextConverter <- function(textValues) {
  as.integer(gsub(x=textValues, pattern="Charge Code ([0-9]+)", replacement="\\1"))
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

buildJailEpisodeTables <- function(stagingConnection, lastLoadTime, currentLoadTime, loadHistoryID, unknownCodeTableValue) {

  buildTable <- function(stagingTableName, extraFields="") {

    Booking <- getQuery(stagingConnection, paste0("select ", extraFields, " BookingID, PersonID, BookingDate, ",
                                                  "FacilityID, SupervisionUnitTypeID, InmateJailResidentIndicator from ", stagingTableName,
                                                  " where ", stagingTableName, "Timestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

    JailEpisode <- Booking %>%
      transmute(
        JailEpisodeID=BookingID,
        PersonID=PersonID,
        BookingNumber=BookingNumber,
        IsActive='Y',
        EpisodeStartDate=BookingDate,
        PretrialStatusTypeID=unknownCodeTableValue,
        FacilityID=unknownCodeTableValue,
        BedTypeID=unknownCodeTableValue,
        CaseStatusTypeID=unknownCodeTableValue,
        DaysAgo=(EpisodeStartDate %--% currentLoadTime) %/% days(1),
        LengthOfStay=DaysAgo,
        LoadHistoryID=loadHistoryID)

    JailEpisode

  }

  ret <- list()
  ret$JailEpisode <- buildTable('Booking', "BookingNumber,")
  # todo: determine if "null as BookingNumber" works on SQL Server
  ret$JailEpisodeEdits <- buildTable('CustodyStatusChange', "null as BookingNumber,")
  ret

}

buildPersonTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, educationTextValueConverter) {

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
              LanguageTypeID=unknownCodeTableValue,
              PersonSexTypeID=unknownCodeTableValue,
              PersonRaceTypeID=unknownCodeTableValue,
              PersonEthnicityTypeID=unknownCodeTableValue,
              MilitaryServiceStatusTypeID=unknownCodeTableValue,
              DomicileStatusTypeID=unknownCodeTableValue,
              ProgramEligibilityTypeID=unknownCodeTableValue,
              WorkReleaseStatusTypeID=unknownCodeTableValue,
              SexOffenderStatusTypeID=unknownCodeTableValue,
              OcupationTypeID=unknownCodeTableValue,
              EducationLevel=EducationLevel,
              PersonAgeID=ifelse(is.na(PersonBirthDate), PersonAgeAtBooking, (PersonBirthDate %--% BookingDate) %/% years(1))
              )

  args <- list()
  args$textValues <- Person$EducationLevel
  Person$EducationLevelTypeID <- do.call(educationTextValueConverter, args)

  Person %>% select(-EducationLevel)

}

buildArrestTables <- function(stagingConnection, lastLoadTime, unknownCodeTableValue) {

  buildTable <- function(parentBookingTable, arrestTable) {

    Arrest <- getQuery(stagingConnection, paste0("select ", parentBookingTable, ".BookingID, ", arrestTable, "ID as pk, LocationLatitude, LocationLongitude, ArrestAgencyID ",
                                                 "from (", arrestTable, " inner join ", parentBookingTable,
                                                 " on ", arrestTable, ".", parentBookingTable, "ID=", parentBookingTable, ".", parentBookingTable, "ID) ",
                                                 "left join Location on ", arrestTable, ".LocationID=Location.LocationID where ",
                                                 parentBookingTable, "Timestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

    Arrest <- Arrest %>%
      transmute(ArrestTablePK=pk,
                ParentBookingID=BookingID,
                ArrestLocationLatitude=LocationLatitude,
                ArrestLocationLongitude=LocationLongitude,
                AgencyTypeID=unknownCodeTableValue)

    Arrest

  }

  ret <- list()
  ret$Arrest <- buildTable('Booking', 'BookingArrest')
  ret$ArrestEdits <- buildTable('CustodyStatusChange', 'CustodyStatusChangeArrest')
  ret

}

buildChargeTables <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, chargeCodeTextConverter, dispositionTextConverter) {

  buildTable <- function(grandparentBookingTable, parentArrestTable, chargeTable) {

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
      transmute(ChargeTablePK=pk,
                ParentArrestID=ParentArrestID,
                GrandparentBookingRecordPK=GrandparentBookingRecordPK,
                GrandparentBookingID=GrandparentBookingID,
                ChargeCode=ChargeCode,
                ChargeDisposition=ChargeDisposition,
                ChargeClassTypeID=unknownCodeTableValue,
                AgencyTypeID=unknownCodeTableValue,
                JurisdictionTypeID=unknownCodeTableValue,
                BondStatusTypeID=unknownCodeTableValue,
                BondTypeID=unknownCodeTableValue,
                BondAmount=BondAmount)

    args <- list()
    args$textValues <- Charge$ChargeCode
    Charge$ChargeTypeID <- do.call(chargeCodeTextConverter, args)
    args$textValues <- Charge$ChargeDisposition
    Charge$ChargeDispositionTypeID <- do.call(dispositionTextConverter, args)

    Charge %>% select(-ChargeCode, -ChargeDisposition)

  }

  ret <- list()
  ret$Charge <- buildTable('Booking', 'BookingArrest', 'BookingCharge')
  ret

}

buildBHAssessmentTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue) {

  BHAssessment <- getQuery(stagingConnection, paste0("select BehavioralHealthAssessmentID, Person.PersonID, SeriousMentalIllnessIndicator, MedicaidStatusTypeID from ",
                                                     "BehavioralHealthAssessment, Person, Booking where ",
                                                     "BehavioralHealthAssessment.PersonID=Person.PersonID and ",
                                                     "Booking.PersonID=Person.PersonID and ",
                                                     "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHAssessment <- BHAssessment %>%
    transmute(BehavioralHealthAssessmentID=BehavioralHealthAssessmentID,
              PersonID=PersonID,
              SevereMentalIllnessIndicator=SeriousMentalIllnessIndicator,
              MedicaidStatusTypeID=unknownCodeTableValue)

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
              AssessmentCategoryTypeID=unknownCodeTableValue)

  BHAssessmentCategory


}

buildBHTreatmentTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue) {

  BHTreatment <- getQuery(stagingConnection, paste0("select TreatmentID, bha.BehavioralHealthAssessmentID, TreatmentStatusTypeID, TreatmentAdmissionReasonTypeID from ",
                                                    "BehavioralHealthAssessment bha, Treatment bht, Person p, Booking b where ",
                                                    "bht.BehavioralHealthAssessmentID=bha.BehavioralHealthAssessmentID and bha.PersonID=p.PersonID and b.PersonID=p.PersonID and ",
                                                    "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  BHTreatment <- BHTreatment %>%
    transmute(BehavioralHealthTreatmentID=TreatmentID,
              BehavioralHealthAssessmentID=BehavioralHealthAssessmentID,
              TreatmentStatusTypeID=unknownCodeTableValue,
              TreatmentAdmissionReasonTypeID=unknownCodeTableValue
    )

  BHTreatment

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

  # We will later just iterate through these records and run update queries
  Release

}

#' @importFrom lubridate now as_date %--%
#' @import dplyr
#' @export
loadDimensionalDatabase <- function(stagingConnectionBuilder=defaultStagingConnectionBuilder,
                                    dimensionalConnectionBuilder=defaultDimensionalConnectionBuilder,
                                    codeTableListBuilder=defaultCodeTableDataFrameBuilder,
                                    codeValueTranslationListBuilder=defaultCodeValueTranslationListBuilder,
                                    educationTextValueConverter=defaultEducationTextValueConverter,
                                    diagnosisTextValueConverter=defaultDiagnosisTextValueConverter,
                                    medicationTextValueConverter=defaultMedicationTextValueConverter,
                                    chargeCodeTextConverter=defaultChargeCodeTextConverter,
                                    dispositionTextConverter=defaultDispositionTextConverter,
                                    unknownCodeTableValue=99999,
                                    noneCodeTableValue=99998) {

  ret <- list()

  stagingConnection = do.call(stagingConnectionBuilder, list())
  adsConnection = do.call(dimensionalConnectionBuilder, list())

  lastLoadTime <- getLastLoadingTime(adsConnection)
  currentLoadTime <- now()
  loadHistoryID <- updateLoadHistory(adsConnection, currentLoadTime)

  jailEpisodeTables <- buildJailEpisodeTables(stagingConnection, lastLoadTime, currentLoadTime, loadHistoryID, unknownCodeTableValue)
  ret <- c(ret, jailEpisodeTables)

  ret$Person <- buildPersonTable(stagingConnection, lastLoadTime, unknownCodeTableValue, educationTextValueConverter)

  arrestTables <- buildArrestTables(stagingConnection, lastLoadTime, unknownCodeTableValue)
  ret <- c(ret, arrestTables)

  chargeTables <- buildChargeTables(stagingConnection, lastLoadTime, unknownCodeTableValue, chargeCodeTextConverter, dispositionTextConverter)
  ret <- c(ret, chargeTables)

  ret$BehavioralHealthAssessment <- buildBHAssessmentTable(stagingConnection, lastLoadTime, unknownCodeTableValue)
  ret$BehavioralHealthAssessmentCategory <- buildBHAssessmentCategoryTable(stagingConnection, lastLoadTime, unknownCodeTableValue)
  ret$BehavioralHealthTreatment <- buildBHTreatmentTable(stagingConnection, lastLoadTime, unknownCodeTableValue)
  ret$BehavioralHealthEvaluation <- buildBHEvaluationTable(stagingConnection, lastLoadTime, unknownCodeTableValue, diagnosisTextValueConverter)
  ret$PrescribedMedication <- buildMedicationTable(stagingConnection, lastLoadTime, unknownCodeTableValue, medicationTextValueConverter)
  ret$Release <- buildReleaseTable(stagingConnection, lastLoadTime)
  # todo: now that you have episodes and people, you can do recidivism.  but note that you need to read the whole booking/person wad to do that right,
  #  so you have to wait until you write the final booking/person wad to the db

  # When inserting arrest and charge records, just let autoincrement determine the PK value.  we don't rely on any implicit link for these
  # fields between staging and dimensional

  # When writing release records, be sure to set JailEpisode:IsActive to 'N'

  dbDisconnect(stagingConnection)
  dbDisconnect(adsConnection)

  ret

}
