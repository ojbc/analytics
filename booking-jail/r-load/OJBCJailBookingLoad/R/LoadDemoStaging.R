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

#' Loads the booking/jail staging database with demo data.
#' @param databaseName the MySQL database in which to save the data (assumes write access to user=root and blank password)
#' @param databaseHost the hostname of the machine on which MySQL is running, with a database named "databaseName" created and populated via DDL
#' @param countyFIPSCode the FIPS code for the county of interest
#' @param censusTractShapefileDSN Path to folder containing tract shapefiles.  Download shapefiles from https://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html
#' @param censusTractShapefileLayer Layer within the shapefile (should be same name as last part of DSN path)
#' @param censusTractPopulationFile file of population data at the census tract level.  Download this from the US
#' Census website, by visiting factfinder.census.gov, select Advanced Search, then Geographies.  Chose "Census Tract - 140" as
#' Geography Type, then the state and county of interest, and "Add to Selection".  Search for table name P1 (total population)
#' and download.
#' @param lookbackDayCount Number of days into the past to generate bookings
#' @param averageDailyBookingVolume average number of bookings to generate per day
#' @param percentPretrial Percentage of bookings into jail for pretrial detainees
#' @param percentSentenced Percentage of bookings into jail for sentenced people
#' @param averagePretrialStay Average number of days in jail for pretrial detainees
#' @param averageSentenceStay Average number of days in jail for sentenced people
#' @param recidivismRate Percentage of people who recidivate in each recidivism episode
#' @param recidivistEpisodes Number of times to cycle through the booked population generating recidivism
#' @param percentAssessments Percentage of booked individuals that have Behavioral Health Assessments
#' @param baseDate the "current date" for the database...the date from which the lookback period begins
#' @param writeToDatabase whether to write to the database, or just create the data frames in the local environment (the return value)
#' @import RMySQL
#' @import rgdal
#' @import sp
#' @import readr
#' @import tidyr
#' @import stringr
#' @examples
#' loadDemoStaging(censusTractShapefileDSN="/opt/data/Shapefiles/gz_2010_08_150_00_500k", censusTractShapefileLayer="gz_2010_08_150_00_500k", countyFIPSCode="001", censusTractPopulationFile="/opt/data/Census/DEC_10_SF1_P1_with_ann.csv")
#' @export
loadDemoStaging <- function(databaseName="ojbc_booking_staging_demo", databaseHost="localhost",
                                censusTractShapefileDSN, censusTractShapefileLayer, countyFIPSCode,
                            censusTractPopulationFile, lookbackDayCount=365, averageDailyBookingVolume=2000,
                            percentPretrial=.39, percentSentenced=.01, averagePretrialStay=1.5,
                            averageSentenceStay=60, recidivismRate=.5, recidivistEpisodes=5, percentAssessments=.5,
                            baseDate=Sys.Date(), writeToDatabase=TRUE) {

  loadStartTime <- Sys.time()

  stagingConnection <- NULL

  if (writeToDatabase) {
    stagingConnection <- dbConnect(MySQL(), host=databaseHost, dbname=databaseName, username="root")
    wipeCurrentDatabase(stagingConnection)
  } else {
    writeLines("writeToDatabase set to FALSE, therefore current db was not wiped")
  }

  codeTableList <- loadCodeTables(stagingConnection, "StagingCodeTables.xlsx", writeToDatabase)

  txTableList <- createTransactionTables(codeTableList, lookbackDayCount, averageDailyBookingVolume,
                                         percentPretrial, percentSentenced, averagePretrialStay,
                                         averageSentenceStay, recidivismRate, recidivistEpisodes, percentAssessments,
                                         baseDate)

  if (writeToDatabase) {
    writeTablesToDatabase(stagingConnection, txTableList)
  } else {
    writeLines("writeToDatabase set to FALSE, therefore new tables not written to database")
  }

  c(codeTableList, txTableList)

}

wipeCurrentDatabase <- function(stagingConnection) {

  dbSendQuery(stagingConnection, "set foreign_key_checks=0")

  dbSendQuery(stagingConnection, "truncate BehavioralHealthAssessmentCategory")
  dbSendQuery(stagingConnection, "truncate BehavioralHealthEvaluation")
  dbSendQuery(stagingConnection, "truncate BehavioralHealthAssessment")
  dbSendQuery(stagingConnection, "truncate BookingCharge")
  dbSendQuery(stagingConnection, "truncate BookingArrest")
  dbSendQuery(stagingConnection, "truncate CustodyRelease")
  dbSendQuery(stagingConnection, "truncate CustodyStatusChangeCharge")
  dbSendQuery(stagingConnection, "truncate CustodyStatusChangeArrest")
  dbSendQuery(stagingConnection, "truncate CustodyStatusChange")
  dbSendQuery(stagingConnection, "truncate Booking")
  dbSendQuery(stagingConnection, "truncate Treatment")
  dbSendQuery(stagingConnection, "truncate PrescribedMedication")
  dbSendQuery(stagingConnection, "truncate Person")
  dbSendQuery(stagingConnection, "truncate Location")

  dbSendQuery(stagingConnection, "truncate Agency")
  dbSendQuery(stagingConnection, "truncate AssessmentCategoryType")
  dbSendQuery(stagingConnection, "truncate SupervisionUnitType")
  dbSendQuery(stagingConnection, "truncate BondStatusType")
  dbSendQuery(stagingConnection, "truncate BondType")
  dbSendQuery(stagingConnection, "truncate CaseStatusType")
  dbSendQuery(stagingConnection, "truncate ChargeClassType")
  dbSendQuery(stagingConnection, "truncate DomicileStatusType")
  dbSendQuery(stagingConnection, "truncate EducationLevelType")
  dbSendQuery(stagingConnection, "truncate Facility")
  dbSendQuery(stagingConnection, "truncate JurisdictionType")
  dbSendQuery(stagingConnection, "truncate LanguageType")
  dbSendQuery(stagingConnection, "truncate MedicaidStatusType")
  dbSendQuery(stagingConnection, "truncate MilitaryServiceStatusType")
  dbSendQuery(stagingConnection, "truncate OccupationType")
  dbSendQuery(stagingConnection, "truncate PersonEthnicityType")
  dbSendQuery(stagingConnection, "truncate PersonRaceType")
  dbSendQuery(stagingConnection, "truncate PersonSexType")
  dbSendQuery(stagingConnection, "truncate ProgramEligibilityType")
  dbSendQuery(stagingConnection, "truncate SexOffenderStatusType")
  dbSendQuery(stagingConnection, "truncate TreatmentStatusType")
  dbSendQuery(stagingConnection, "truncate WorkReleaseStatusType")
  dbSendQuery(stagingConnection, "truncate TreatmentAdmissionReasonType")

}

#' @import RMySQL
writeTablesToDatabase <- function(conn, txTables) {

  dbWriteTable(conn, "Person", txTables$Person, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "BehavioralHealthAssessment", txTables$BehavioralHealthAssessment, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "BehavioralHealthEvaluation", txTables$BehavioralHealthEvaluation, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "BehavioralHealthAssessmentCategory", txTables$BehavioralHealthAssessmentCategory, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "PrescribedMedication", txTables$PrescribedMedication, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "Booking", txTables$Booking, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "BookingArrest", txTables$BookingArrest, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "BookingCharge", txTables$BookingCharge, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "CustodyRelease", txTables$CustodyRelease, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "CustodyStatusChange", txTables$CustodyStatusChange, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "CustodyStatusChangeArrest", txTables$CustodyStatusChangeArrest, append=TRUE, row.names=FALSE)
  dbWriteTable(conn, "CustodyStatusChangeCharge", txTables$CustodyStatusChangeCharge, append=TRUE, row.names=FALSE)

}

#' @importFrom lubridate hour<- minute<- second<- ddays
#' @import dplyr
createTransactionTables <- function(codeTableList, lookbackDayCount, averageDailyBookingVolume,
                                    percentPretrial, percentSentenced, averagePretrialStay,
                                    averageSentenceStay, recidivismRate, recidivistEpisodes, percentAssessments,
                                    baseDate) {

  writeLines("Creating Booking Table...")

  ret <- list()

  dailyBookings <- sample((.9*averageDailyBookingVolume):(1.1*averageDailyBookingVolume), size=lookbackDayCount, replace=TRUE)

  day <- integer()
  bookingNumber <- integer()

  for (i in seq(lookbackDayCount)) {
    dbi <- dailyBookings[i]
    day <- c(day, rep(i, dbi))
    bookingNumber <- c(bookingNumber, 1:dbi)
  }

  df <- day %>%
    projectJailStay(seq(length(day)), bookingNumber, percentPretrial, percentSentenced,
                    averagePretrialStay, averageSentenceStay) %>%
    generateRecidivism(recidivismRate, recidivistEpisodes, priorBookingNumbers=integer(),
                       percentPretrial, percentSentenced, averagePretrialStay, averageSentenceStay)

  df$FacilityID <- generateRandomIDsFromCodeTable(codeTableList, "Facility", nrow(df))
  df$SupervisionUnitTypeID <- generateRandomIDsFromCodeTable(codeTableList, "SupervisionUnitType", nrow(df))
  df$CaseStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "CaseStatusType", nrow(df))
  df$InmateJailResidentIndicator <- as.logical(rbinom(n=nrow(df), size=1, prob=.8))

  BookingDateTime <- baseDate - ddays(df$DaysAgo)
  hour(BookingDateTime) <- sample(0:23, size=length(BookingDateTime), replace = TRUE)
  minute(BookingDateTime) <- sample(0:59, size=length(BookingDateTime), replace = TRUE)
  second(BookingDateTime) <- sample(0:59, size=length(BookingDateTime), replace = TRUE)

  df$BookingDateTime <- as.POSIXct(BookingDateTime)

  # can't use mutate and lubridate together on this because the lubridate overloaded minus ("-") doesn't handle NAs well...
  df[!is.na(df$ReleaseDay), 'ScheduledReleaseDate'] <- as.Date(baseDate - ddays(df[!is.na(df$ReleaseDay), 'ReleaseDay']), origin="1970-01-01")
  class(df$ScheduledReleaseDate) <- "Date"

  df <- df %>%
    mutate(BookingNumber=paste0("B", formatC(BookingNumber, width=12, flag="0")))

  ReleaseDateTime <- df$ScheduledReleaseDate
  hour(ReleaseDateTime) <- sample(0:23, size=length(BookingDateTime), replace = TRUE)
  minute(ReleaseDateTime) <- sample(0:59, size=length(BookingDateTime), replace = TRUE)
  second(ReleaseDateTime) <- sample(0:59, size=length(BookingDateTime), replace = TRUE)
  df$ReleaseDateTime <- ReleaseDateTime

  ret$Booking <- df %>% select(-DaysAgo, -LengthOfStay, -ReleaseDay, -ReleaseDateTime, -Disposition)

  writeLines(paste0("...Booking table created with ", nrow(df), " rows."))

  Person <- data.frame(PersonID=df$PersonID) %>%
    mutate(PersonUniqueIdentifier=PersonID)

  Person$PersonID <- 1:nrow(Person)

  actualPersonDf <- buildActualPersonTable(codeTableList, unique(Person$PersonUniqueIdentifier), baseDate)

  Person <- Person %>% inner_join(actualPersonDf, by=c("PersonUniqueIdentifier"="PersonUniqueIdentifier")) %>%
    mutate(PersonUniqueIdentifier=paste0("P", formatC(PersonUniqueIdentifier, width=16, flag="0")))

  ret$Person <- Person
  ret$Booking$PersonID <- Person$PersonID
  bookingID <- seq(nrow(ret$Booking))
  ret$Booking$BookingID <- bookingID

  writeLines("Created Person table")

  bhTableList <- buildBehavioralHealthTables(Person$PersonID, df$BookingDateTime, percentAssessments, codeTableList)
  ret <- c(ret, bhTableList)

  bookingChildTableList <- buildBookingChildTables(bookingID, ReleaseDateTime, codeTableList)
  ret <- c(ret, bookingChildTableList)

  changeTableList <- buildChangeTables(ret, codeTableList)
  ret <- c(ret, changeTableList)

  ret

}

#' @import dplyr
buildChangeTables <- function(txTableList, codeTableList) {

  ret <- list()

  # 20% of bookings have edits
  changedBookings <- txTableList$Booking %>% sample_frac(.2) %>% select(-BookingNumber) %>%
    mutate(CustodyStatusChangeID=seq(n()))
  changedArrests <- txTableList$BookingArrest %>% filter(BookingID %in% changedBookings$BookingID) %>% sample_frac(.5) %>%
    inner_join(changedBookings %>% select(BookingID, CustodyStatusChangeID), by=c("BookingID"="BookingID")) %>% select(-BookingID) %>%
    mutate(CustodyStatusChangeArrestID=seq(n()))
  changedCharges <- txTableList$BookingCharge %>% filter(BookingArrestID %in% changedArrests$BookingArrestID) %>% sample_frac(.5) %>%
    inner_join(changedArrests %>% select(BookingArrestID, CustodyStatusChangeArrestID), by=c("BookingArrestID"="BookingArrestID")) %>%
    select(-BookingArrestID) %>%
    mutate(CustodyStatusChangeChargeID=seq(n()))

  changedArrests <- changedArrests %>% select(-BookingArrestID)
  changedCharges <- changedCharges %>% select(-BookingChargeID)

  # note:  we can change more attributes as we learn what is important to test in the ADS loading process
  changedBookings$SupervisionUnitTypeID <- generateRandomIDsFromCodeTable(codeTableList, "SupervisionUnitType", nrow(changedBookings))
  changedArrests$ArrestAgencyID <- generateRandomIDsFromCodeTable(codeTableList, "Agency", nrow(changedArrests))
  changedCharges$ChargeClassTypeID <- generateRandomIDsFromCodeTable(codeTableList, "ChargeClassType", nrow(changedCharges))

  ret$CustodyStatusChange <- changedBookings
  writeLines(paste0("Created CustodyStatusChange table with ", nrow(changedBookings), " rows."))
  ret$CustodyStatusChangeArrest <- changedArrests
  writeLines(paste0("Created CustodyStatusChangeArrest table with ", nrow(changedArrests), " rows."))
  ret$CustodyStatusChangeCharge <- changedCharges
  writeLines(paste0("Created CustodyStatusChangeCharge table with ", nrow(changedCharges), " rows."))

  ret

}

#' @import dplyr
buildBookingChildTables <- function(bookingID, releaseDateTime, codeTableList) {

  ret <- list()

  CustodyRelease <- data.frame(BookingID=bookingID, ReleaseDateTime=releaseDateTime) %>%
    filter(!is.na(ReleaseDateTime)) %>%
    mutate(CustodyReleaseID=seq(n()))

  writeLines(paste0("Created CustodyRelease table with ", nrow(CustodyRelease), " rows"))

  ret$CustodyRelease <- CustodyRelease

  n_Arrest <- sample(1:6, size=length(bookingID), replace=TRUE, prob=c(.7, .2, .05, .01, .01, .01))
  BookingArrest <- data.frame(BookingID=rep(bookingID, times=n_Arrest)) %>%
    mutate(BookingArrestID=seq(n()))
  recs <- nrow(BookingArrest)
  BookingArrest$ArrestAgencyID <- generateRandomIDsFromCodeTable(codeTableList, "Agency", recs)

  ret$BookingArrest <- BookingArrest

  writeLines(paste0("Created BookingArrest table with ", nrow(BookingArrest), " rows"))

  bookingArrestID <- BookingArrest$BookingArrestID

  n_Charge <- sample(1:5, size=length(bookingArrestID), replace=TRUE, prob=c(.5, .2, .15, .1, .05))
  BookingCharge <- data.frame(BookingArrestID=rep(bookingArrestID, times=n_Charge)) %>%
    mutate(BookingChargeID=seq(n()))
  recs <- nrow(BookingCharge)
  BookingCharge$ChargeCode <- paste0("Charge Code ", sample(1:200, size=recs, replace=TRUE))
  BookingCharge$BondAmount <- sample(c(10000, 5000, 2500, 1000, 500, 50), size=recs, replace=TRUE, prob=c(10,40,30,10,5,5))
  BookingCharge$AgencyID <- generateRandomIDsFromCodeTable(codeTableList, "Agency", recs)
  BookingCharge$BondTypeID <- generateRandomIDsFromCodeTable(codeTableList, "BondType", recs)
  BookingCharge$ChargeJurisdictionTypeID <- generateRandomIDsFromCodeTable(codeTableList, "JurisdictionType", recs)
  BookingCharge$ChargeClassTypeID <- generateRandomIDsFromCodeTable(codeTableList, "ChargeClassType", recs)
  BookingCharge$BondStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "BondStatusType", recs)

  ret$BookingCharge <- BookingCharge

  writeLines(paste0("Created BookingCharge table with ", nrow(BookingCharge), " rows"))

  ret

}

#' @importFrom dplyr sample_frac mutate select left_join
#' @importFrom lubridate ddays %--% days
buildBehavioralHealthTables <- function(PersonID, BookingDateTime, percentAssessments, codeTableList) {

  tenProbs <- sample(10)

  ret <- list()

  # 50% of inmates have BH issues
  bha <- data.frame(PersonID, BookingDateTime) %>%
    sample_frac(percentAssessments)

  recs <- nrow(bha)

  bha$CareEpisodeStartDate <- as.Date(bha$BookingDateTime - ddays(rchisq(n=recs, df=90)))
  bha$CareEpisodeEndDate <- as.Date(bha$CareEpisodeStartDate + ddays(rchisq(n=recs, df=45)))

  bha <- bha %>%
    mutate(CareEpisodeEndDate=replace(CareEpisodeEndDate, CareEpisodeEndDate > as.Date(BookingDateTime), NA)) %>%
    select(-BookingDateTime)

  bha[sample(recs, recs*.1), 'CareEpisodeEndDate'] <- NA

  bha$MedicaidStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "MedicaidStatusType", recs)
  bha$SeriousMentalIllnessIndicator <- as.logical(rbinom(n=recs, size=2, prob=.3))
  bha$EnrolledProviderName <- paste0("Enrolled Provider ", sample(1:10, recs, prob=tenProbs, replace=TRUE))
  bha$BehavioralHealthAssessmentID <- seq(recs)

  bha$n_Treatment <- sample(1:4, size=recs, replace=TRUE)

  writeLines(paste0("Created BH Assessment table with ", recs, " rows."))

  # 80% of assessments result in treatment
  tmt <- bha %>% sample_frac(.8) %>%
    select(BehavioralHealthAssessmentID, CareEpisodeStartDate, CareEpisodeEndDate, n_Treatment)

  recs <- nrow(tmt)

  bhaIDs <- rep(tmt$BehavioralHealthAssessmentID, times=tmt$n_Treatment)

  tmt <- data.frame(BehavioralHealthAssessmentID=bhaIDs) %>%
    left_join(tmt, by=c("BehavioralHealthAssessmentID"="BehavioralHealthAssessmentID")) %>%
    mutate(EpisodeLength=(CareEpisodeStartDate %--% CareEpisodeEndDate) %/% days(1)) %>%
    mutate(EpisodeLength=ifelse(is.na(EpisodeLength), -1, EpisodeLength))

  recs <- nrow(tmt)

  tmt$DaysBeforeTreatmentStarts <- sapply(tmt$EpisodeLength, sample, size=1)

  tmt <- tmt %>%
    mutate(DaysBeforeTreatmentStarts=ifelse(DaysBeforeTreatmentStarts == -1, sample(1:5, 1), DaysBeforeTreatmentStarts)) %>%
    mutate(TreatmentStartDate=CareEpisodeStartDate + ddays(DaysBeforeTreatmentStarts)) %>%
    select(BehavioralHealthAssessmentID, TreatmentStartDate)

  tenProbs <- sample(10)

  tmt$TreatmentAdmissionReasonTypeID <- generateRandomIDsFromCodeTable(codeTableList, "TreatmentAdmissionReasonType", recs)
  tmt$TreatmentStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "TreatmentStatusType", recs)
  tmt$TreatmentProviderName <- paste0("Treatment Provider ", sample(1:10, recs, prob=tenProbs, replace=TRUE))

  tmt$TreatmentID <- seq(nrow(tmt))

  writeLines(paste0("Created Treatment table with ", nrow(tmt), " rows."))

  # 95% of assessments result in diagnosis
  recs <- nrow(bha)
  bha$n_Evaluation <- sample(1:3, size=recs, replace=TRUE)
  ev <- bha %>% sample_frac(.95) %>%
    select(BehavioralHealthAssessmentID, n_Evaluation)

  recs <- nrow(ev)
  bhaIDs <- rep(ev$BehavioralHealthAssessmentID, times=ev$n_Evaluation)

  ev <- data.frame(BehavioralHealthAssessmentID=bhaIDs)
  recs <- nrow(ev)
  ev$BehavioralHealthDiagnosisDescription <- paste0("Diagnosis ", sample(1:100, recs, prob=sample(100), replace=TRUE))
  ev$BehavioralHealthEvaluationID <- seq(recs)

  writeLines(paste0("Created BH Evaluation table with ", recs, " rows."))

  # 60% of assessments have medication

  recs <- nrow(bha)
  bha$n_Medication <- sample(1:3, size=recs, replace=TRUE, prob=c(4, 4, 2))
  med <- bha %>% sample_frac(.6) %>%
    select(BehavioralHealthAssessmentID, n_Medication)

  recs <- nrow(med)
  bhaIDs <- rep(med$BehavioralHealthAssessmentID, times=med$n_Medication)

  med <- data.frame(BehavioralHealthAssessmentID=bhaIDs)
  recs <- nrow(med)

  # leaving dispensing date null for now...don't think we will use it
  med$MedicationDescription <- paste0("Medication ", sample(1:100, recs, prob=sample(100), replace=TRUE))
  med$MedicationDoseMeasure <- paste0(sample(c("5", "10", "50", "500", "1000"), size=recs, replace=TRUE),
                                      sample(c(" mg", " oz", " Units", " ml"), size=recs, replace=TRUE))
  med$PrescribedMedicationID <- seq(recs)

  writeLines(paste0("Created PrescribedMedication table with ", recs, " rows."))

  # 30% of assessments have assigned categories
  recs <- nrow(bha)
  bha$n_Category <- sample(1:2, size=recs, prob=c(.6, .4), replace=TRUE)
  bhc <- bha %>% sample_frac(.3) %>%
    select(BehavioralHealthAssessmentID, n_Category)

  recs <- nrow(bhc)
  bhaIDs <- rep(bhc$BehavioralHealthAssessmentID, times=bhc$n_Category)

  bhc <- data.frame(BehavioralHealthAssessmentID=bhaIDs)
  recs <- nrow(bhc)
  bhc$AssessmentCategoryTypeID <- generateRandomIDsFromCodeTable(codeTableList, "AssessmentCategoryType", recs)
  bhc <- bhc %>% distinct() # avoid duplicate categories per assessment
  recs <- nrow(bhc)
  bhc$BehavioralHealthAssessmentCategoryID <- seq(recs)

  writeLines(paste0("Created BH Assessment Category table with ", recs, " rows."))

  ret$BehavioralHealthAssessment <- bha %>% select(-starts_with("n_"))
  ret$Treatment <- tmt
  ret$BehavioralHealthEvaluation <- ev
  ret$BehavioralHealthAssessmentCategory <- bhc
  ret$PrescribedMedication <- med

  ret

}

#' @importFrom lubridate dyears %--% years
buildActualPersonTable <- function(codeTableList, PersonUniqueIdentifier, baseDate) {

  ret <- data.frame(PersonUniqueIdentifier)

  nPeople <- length(PersonUniqueIdentifier)

  birthdates <- baseDate - dyears(generateRandomArresteeAges(23, nPeople))

  ret$PersonBirthDate <- as.Date(birthdates)
  ret$PersonAgeAtBooking <- ((birthdates %--% baseDate) %/% years(1)) - 1
  # recode 1 in 50 birthdates as NA, to simulate missing birthdates
  ret[sample(nPeople, as.integer(nPeople/50)), 'PersonBirthDate'] <- NA

  ret$LanguageTypeID <- generateRandomIDsFromCodeTable(codeTableList, "LanguageType", nPeople)
  ret$PersonSexTypeID <- generateRandomIDsFromCodeTable(codeTableList, "PersonSexType", nPeople)
  ret$PersonRaceTypeID <- generateRandomIDsFromCodeTable(codeTableList, "PersonRaceType", nPeople)
  ret$PersonEthnicityTypeID <- generateRandomIDsFromCodeTable(codeTableList, "PersonEthnicityType", nPeople)
  ret$MilitaryServiceStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "MilitaryServiceStatusType", nPeople)
  ret$OccupationTypeID <- generateRandomIDsFromCodeTable(codeTableList, "OccupationType", nPeople)
  ret$EducationLevelTypeID <- generateRandomIDsFromCodeTable(codeTableList, "EducationLevelType", nPeople)
  ret$DomicileStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "DomicileStatusType", nPeople)
  ret$ProgramEligibilityTypeID <- generateRandomIDsFromCodeTable(codeTableList, "ProgramEligibilityType", nPeople)
  ret$WorkReleaseStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "WorkReleaseStatusType", nPeople)
  ret$SexOffenderStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "SexOffenderStatusType", nPeople)

  ret

}

#' @importFrom truncdist rtrunc
generateRandomArresteeAges <- function(approximateMeanAge, size, minimumAge=12, maximumAge=85) {

  # need this hack so that rtrunc can find these functions
  # very tiny risk, in that this will overwrite variables with these names in the current environment, thus
  # we err if that occurs
  if (exists("pigamma")) {
    stop("Variable pigamma exists in current global environment, thus cannot dynamically invoke pscl::pigamma via truncdist::rtrunc")
  }
  assign("pigamma", pscl::pigamma, envir=.GlobalEnv)
  if (exists("qigamma")) {
    stop("Variable qigamma exists in current global environment, thus cannot dynamically invoke pscl::qigamma via truncdist::rtrunc")
  }
  assign("qigamma", pscl::qigamma, envir=.GlobalEnv)

  # we use a shape (alpha) parameter of 12, which seems to produce about the right height
  alphaParameter <- 12

  # have to muck with options because of a minor bug in rtrunc that produces a false-positive warning
  # this just turns warnings off, then turns them back on.  if an error occurs, that will still get raised.
  warnVal <- getOption("warn")
  options(warn=-1)
  # assume ages are distributed according to the (truncated) inverse gamma distribution with shape as
  # specified above, and scale (beta) = 1.  this seems to produce a very nice age distribution.
  ret <- rtrunc(n=size,
                spec="igamma",
                a=minimumAge/(approximateMeanAge*(alphaParameter-1)),
                b=maximumAge/(approximateMeanAge*(alphaParameter-1)),
                alpha=alphaParameter, beta=1)*(approximateMeanAge*(alphaParameter-1))
  options(warn=warnVal)

  # clean up from hack
  rm(pigamma)
  rm(qigamma)

  ret

}

generateRandomIDsFromCodeTable <- function(codeTableList, codeTableName, size) {
  codeTableDf <- codeTableList[[codeTableName]]
  idVector <- codeTableDf[[paste0(codeTableName, "ID")]]
  probs <- sample(nrow(codeTableDf))
  sample(idVector, replace=TRUE, size=size, prob=probs)
}

projectJailStay <- function(day, personID, bookingNumber, percentPretrial, percentSentenced, averagePretrialStay, averageSentenceStay) {

  n <- length(day)
  bookingDisposition <- sample(c("R", "D", "S"), n, TRUE, prob=c(1 - (percentPretrial + percentSentenced), percentPretrial, percentSentenced))
  shortStayLength <- 1 + as.integer(rchisq(n, averagePretrialStay-1))
  longStayLength <- as.integer(rnorm(n=n, mean=averageSentenceStay, sd=averageSentenceStay/4))

  df <- data.frame(DaysAgo=as.integer(day), BookingNumber=bookingNumber, Disposition=bookingDisposition,
                   ShortStayLength=shortStayLength, LongStayLength=longStayLength, PersonID=personID, stringsAsFactors=FALSE) %>%
    mutate(LengthOfStay=as.integer(ifelse(Disposition=='R', 0, ifelse(Disposition=='D', ShortStayLength, LongStayLength)))) %>%
    select(-ShortStayLength, -LongStayLength) %>%
    mutate(ReleaseDay=ifelse(LengthOfStay==0, DaysAgo, DaysAgo-LengthOfStay)) %>%
    mutate(ReleaseDay=as.integer(ifelse(ReleaseDay < 1, NA, ReleaseDay)))

  df

}

summarizeDailyPopulation <- function(df) {

  # utility function to help with testing...produces a summary of daily population over time

  dailyPop <- rep(0, max(df$DaysAgo))

  for (r in seq(nrow(df))) {
    los <- df[r, 'LengthOfStay']
    daysAgo <- df[r, 'DaysAgo']
    if (los > 0) {
      for (d in daysAgo:(max(1, daysAgo-(los-1)))) {
        dailyPop[d] <- dailyPop[d] + 1
      }
    }
  }

  data.frame(DaysAgo=seq(length(dailyPop)), DailyPopulation=dailyPop)

}

generateRecidivism <- function(daySummaryDf, recidivismRate, iterations=5, priorBookingNumbers=integer(),
                               percentPretrial, percentSentenced, averagePretrialStay, averageSentenceStay) {

  #print(paste0("Iteration ", iterations))

  sampleDf <- daySummaryDf

  if (length(priorBookingNumbers) > 0) {
    sampleDf <- sampleDf %>% filter(BookingNumber %in% priorBookingNumbers)
  }

  sampleDf <- sampleDf %>%
    sample_n(recidivismRate*nrow(sampleDf))

  DaysUntilNext <- as.integer(rweibull(n=nrow(sampleDf), shape=2, scale=180))
  sampleDf$DaysUntilNext <- DaysUntilNext

  sampleDf <- sampleDf %>%
    select(BookingNumber, PersonID, ReleaseDay, DaysUntilNext) %>%
    mutate(NextBookingDay = ReleaseDay - DaysUntilNext) %>%
    mutate(NextBookingDay = ifelse(NextBookingDay < 1, NA, NextBookingDay)) %>%
    select(-DaysUntilNext, -ReleaseDay) %>%
    filter(!is.na(NextBookingDay)) %>%
    rename(DaysAgo=NextBookingDay)

  n <- nrow(sampleDf)

  newBookingNumbers <- integer()

  if (n) {

    sumSampleDf <- sampleDf %>% group_by(DaysAgo) %>% summarize(n=n()) %>% as.data.frame()

    rowsToDelete <- integer()

    for (r in seq(nrow(sumSampleDf))) {
      da <- as.integer(sumSampleDf[r, 'DaysAgo'])
      nn <- as.integer(sumSampleDf[r, 'n'])
      rowsToDelete <- c(rowsToDelete, sample(which(daySummaryDf$DaysAgo==da), nn))
    }

    daySummaryDf <- daySummaryDf[-rowsToDelete, ]

    day <- sampleDf$DaysAgo
    personID <- sampleDf$PersonID
    newBookingNumbers <- seq(max(daySummaryDf$BookingNumber) + 1, max(daySummaryDf$BookingNumber) + n)

    df <- projectJailStay(day, personID, newBookingNumbers, percentPretrial,
                          percentSentenced, averagePretrialStay, averageSentenceStay)
    daySummaryDf <- bind_rows(daySummaryDf, df)

  }

  if (iterations == 1) {
    return(daySummaryDf)
  } else {
    return(generateRecidivism(daySummaryDf, recidivismRate, iterations-1, newBookingNumbers,
                              percentPretrial, percentSentenced, averagePretrialStay, averageSentenceStay))
  }

}

