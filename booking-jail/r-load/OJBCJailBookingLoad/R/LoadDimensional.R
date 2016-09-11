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

buildInitialJailEpisodeTable <- function(stagingConnection, lastLoadTime, currentLoadTime, loadHistoryID, unknownCodeTableValue) {

  Booking <- getQuery(stagingConnection, paste0("select BookingID, BookingNumber, PersonID, BookingDate, ",
                                                "FacilityID, SupervisionUnitTypeID, InmateJailResidentIndicator from Booking ",
                                                "where BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

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

buildPersonTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue, educationTextValueConverter) {

  Person <- getQuery(stagingConnection, paste0("select Person.PersonID as PersonID, PersonUniqueIdentifier, PersonAgeAtBooking, PersonBirthDate, ",
                                               "EducationLevel, Occupation, LanguageTypeID, PersonSexTypeID, PersonRaceTypeID, ",
                                               "PersonEthnicityTypeID, MilitaryServiceStatusTypeID, DomicileStatusTypeID, ",
                                               "ProgramEligibilityTypeID, WorkReleaseStatusTypeID, SexOffenderStatusTypeID, BookingDate from Person, Booking ",
                                               "where BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "' and Person.PersonID=Booking.PersonID"))

  Person <- Person %>%
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

buildArrestTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue) {

  Arrest <- getQuery(stagingConnection, paste0("select BookingArrest.BookingID, BookingArrestID, LocationLatitude, LocationLongitude, ArrestAgencyID ",
                                               "from (BookingArrest inner join Booking on BookingArrest.BookingID=Booking.BookingID) ",
                                               "left join Location on BookingArrest.LocationID=Location.LocationID where ",
                                               "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  Arrest <- Arrest %>%
    transmute(JailEpisodeArrestID=BookingArrestID,
              JailEpisodeID=BookingID,
              ArrestLocationLatitude=LocationLatitude,
              ArrestLocationLongitude=LocationLongitude,
              AgencyTypeID=unknownCodeTableValue)

  Arrest

}

buildChargeTable <- function(stagingConnection, lastLoadTime, unknownCodeTableValue) {

  Charge <- getQuery(stagingConnection, paste0("select BookingCharge.* from BookingCharge, BookingArrest, Booking where ",
                                               "BookingCharge.BookingArrestID=BookingArrest.BookingArrestID and ",
                                               "BookingArrest.BookingID=Booking.BookingID and ",
                                               "BookingTimestamp > '", formatDateTimeForSQL(lastLoadTime), "'"))

  Charge <- Charge %>%
    transmute(JailEpisodeCharge=BookingChargeID,
              JailEpisodeArrestID=BookingArrestID,
              ChargeTypeID=unknownCodeTableValue,
              ChargeClassTypeID=unknownCodeTableValue,
              ChargeDispositionTypeID=unknownCodeTableValue,
              AgencyTypeID=unknownCodeTableValue,
              JurisdictionTypeID=unknownCodeTableValue,
              BondStatusTypeID=unknownCodeTableValue,
              BondTypeID=unknownCodeTableValue,
              BondAmount=BondAmount)

  Charge

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
                                    unknownCodeTableValue=99999,
                                    noneCodeTableValue=99998) {

  ret <- list()

  stagingConnection = do.call(stagingConnectionBuilder, list())
  adsConnection = do.call(dimensionalConnectionBuilder, list())

  lastLoadTime <- getLastLoadingTime(adsConnection)
  currentLoadTime <- now()
  loadHistoryID <- updateLoadHistory(adsConnection, currentLoadTime)

  ret$JailEpisode <- buildInitialJailEpisodeTable(stagingConnection, lastLoadTime, currentLoadTime, loadHistoryID, unknownCodeTableValue)
  ret$Person <- buildPersonTable(stagingConnection, lastLoadTime, unknownCodeTableValue, educationTextValueConverter)
  ret$JailEpisodeArrest <- buildArrestTable(stagingConnection, lastLoadTime, unknownCodeTableValue)
  ret$JailEpisodeCharge <- buildChargeTable(stagingConnection, lastLoadTime, unknownCodeTableValue)
  ret$BehavioralHealthAssessment <- buildBHAssessmentTable(stagingConnection, lastLoadTime, unknownCodeTableValue)
  ret$BehavioralHealthAssessmentCategory <- buildBHAssessmentCategoryTable(stagingConnection, lastLoadTime, unknownCodeTableValue)
  ret$BehavioralHealthTreatment <- buildBHTreatmentTable(stagingConnection, lastLoadTime, unknownCodeTableValue)
  ret$BehavioralHealthEvaluation <- buildBHEvaluationTable(stagingConnection, lastLoadTime, unknownCodeTableValue, diagnosisTextValueConverter)
  ret$PrescribedMedication <- buildMedicationTable(stagingConnection, lastLoadTime, unknownCodeTableValue, medicationTextValueConverter)
  ret$Release <- buildReleaseTable(stagingConnection, lastLoadTime)
  # todo: now that you have episodes and people, you can do recidivism.  but note that you need to read the whole booking/person wad to do that right,
  #  so you have to wait until you write the final booking/person wad to the db

  dbDisconnect(stagingConnection)
  dbDisconnect(adsConnection)

  ret

}
