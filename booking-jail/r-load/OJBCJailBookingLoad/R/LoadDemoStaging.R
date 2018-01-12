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
#' @param localDatabase whether the staging database is local to the machine where R is running, or FALSE if on a remote server
#' @param averageDailyCrisisIncidents average number of crisis Incident records added per day
#' @param seed seed for random number generation (to guarantee same data are generated in subsequent runs)
#' @import DBI
#' @import readr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import purrr
#' @examples
#' loadDemoStaging(connection=DBI::dbConnect(RMariaDB::MariaDB(), host="localhost", dbname="ojbc_booking_staging_demo", username="root"))
#' @export
loadDemoStaging <- function(connection=NULL,
                            censusTractShapefileDSN=NA, censusTractShapefileLayer=NA, countyFIPSCode=NA,
                            censusTractPopulationFile=NA, lookbackDayCount=365, averageDailyBookingVolume=100,
                            percentPretrial=.39, percentSentenced=.01, averagePretrialStay=1.5,
                            averageSentenceStay=60, recidivismRate=.5, recidivistEpisodes=5, percentAssessments=.5,
                            baseDate=Sys.Date(), averageDailyCrisisIncidents=10, crisisRecidivismRate=.1,
                            writeToDatabase=TRUE, localDatabase=TRUE, seed=12345) {

  set.seed(seed)

  loadStartTime <- Sys.time()

  stagingConnection <- connection

  if (writeToDatabase) {
    wipeCurrentDatabase(stagingConnection)
  } else {
    writeLines("writeToDatabase set to FALSE, therefore current db was not wiped")
  }

  spreadsheetFile <- system.file("raw", "StagingCodeTables.xlsx", package=getPackageName())
  codeTableList <- loadCodeTables(stagingConnection, spreadsheetFile, writeToDatabase)

  txTableList <- createTransactionTables(codeTableList, lookbackDayCount, averageDailyBookingVolume,
                                         percentPretrial, percentSentenced, averagePretrialStay,
                                         averageSentenceStay, recidivismRate, recidivistEpisodes, percentAssessments,
                                         averageDailyCrisisIncidents, crisisRecidivismRate, baseDate)

  if (writeToDatabase) {
    writeTablesToDatabase(stagingConnection, txTableList, localDatabase)
  } else {
    writeLines("writeToDatabase set to FALSE, therefore new tables not written to database")
  }

  ret <- c(codeTableList, txTableList)

  map(ret, as_tibble)

}

wipeCurrentDatabase <- function(stagingConnection) {

  writeLines('Truncating current database...')

  dbExecute(stagingConnection, 'set FOREIGN_KEY_CHECKS=0')

  dbExecute(stagingConnection, "truncate Incident")
  dbExecute(stagingConnection, "truncate IncidentResponseUnit")
  dbExecute(stagingConnection, "truncate Treatment")
  dbExecute(stagingConnection, "truncate PrescribedMedication")
  dbExecute(stagingConnection, "truncate BehavioralHealthAssessmentCategory")
  dbExecute(stagingConnection, "truncate BehavioralHealthEvaluation")
  dbExecute(stagingConnection, "truncate BehavioralHealthAssessment")
  dbExecute(stagingConnection, "truncate BookingCharge")
  dbExecute(stagingConnection, "truncate BookingArrest")
  dbExecute(stagingConnection, "truncate CustodyRelease")
  dbExecute(stagingConnection, "truncate CustodyStatusChangeCharge")
  dbExecute(stagingConnection, "truncate CustodyStatusChangeArrest")
  dbExecute(stagingConnection, "truncate CustodyStatusChange")
  dbExecute(stagingConnection, "truncate Booking")
  dbExecute(stagingConnection, "truncate Person")
  dbExecute(stagingConnection, "truncate Location")

  dbExecute(stagingConnection, "truncate Agency")
  dbExecute(stagingConnection, "truncate AssessmentCategoryType")
  dbExecute(stagingConnection, "truncate SupervisionUnitType")
  dbExecute(stagingConnection, "truncate BondStatusType")
  dbExecute(stagingConnection, "truncate BondType")
  dbExecute(stagingConnection, "truncate ChargeClassType")
  dbExecute(stagingConnection, "truncate DomicileStatusType")
  dbExecute(stagingConnection, "truncate Facility")
  dbExecute(stagingConnection, "truncate JurisdictionType")
  dbExecute(stagingConnection, "truncate LanguageType")
  dbExecute(stagingConnection, "truncate MedicaidStatusType")
  dbExecute(stagingConnection, "truncate MilitaryServiceStatusType")
  dbExecute(stagingConnection, "truncate PersonEthnicityType")
  dbExecute(stagingConnection, "truncate PersonRaceType")
  dbExecute(stagingConnection, "truncate PersonSexType")
  dbExecute(stagingConnection, "truncate ProgramEligibilityType")
  dbExecute(stagingConnection, "truncate SexOffenderStatusType")
  dbExecute(stagingConnection, "truncate TreatmentStatusType")
  dbExecute(stagingConnection, "truncate WorkReleaseStatusType")
  dbExecute(stagingConnection, "truncate TreatmentAdmissionReasonType")

  writeLines('Truncation complete')

}

#' @import RMariaDB
writeTableToDatabase <- function(conn, tableName, df, localDatabase) {
  writeLines(paste0("Writing table ", tableName, " to database..."))
  writeDataFrameToDatabase(conn, df, tableName, viaBulk=TRUE, localBulk=localDatabase, append=FALSE)
  writeLines("...done")
}

writeTablesToDatabase <- function(conn, txTables, localDatabase) {
  writeTableToDatabase(conn, "Person", txTables$Person, localDatabase)
  writeTableToDatabase(conn, "BehavioralHealthAssessment", txTables$BehavioralHealthAssessment, localDatabase)
  writeTableToDatabase(conn, "BehavioralHealthEvaluation", txTables$BehavioralHealthEvaluation, localDatabase)
  writeTableToDatabase(conn, "BehavioralHealthAssessmentCategory", txTables$BehavioralHealthAssessmentCategory, localDatabase)
  writeTableToDatabase(conn, "PrescribedMedication", txTables$PrescribedMedication, localDatabase)
  writeTableToDatabase(conn, "Treatment", txTables$Treatment, localDatabase)
  writeTableToDatabase(conn, "Booking", txTables$Booking, localDatabase)
  writeTableToDatabase(conn, "BookingArrest", txTables$BookingArrest, localDatabase)
  writeTableToDatabase(conn, "BookingCharge", txTables$BookingCharge, localDatabase)
  writeTableToDatabase(conn, "CustodyRelease", txTables$CustodyRelease, localDatabase)
  writeTableToDatabase(conn, "CustodyStatusChange", txTables$CustodyStatusChange, localDatabase)
  writeTableToDatabase(conn, "CustodyStatusChangeArrest", txTables$CustodyStatusChangeArrest, localDatabase)
  writeTableToDatabase(conn, "CustodyStatusChangeCharge", txTables$CustodyStatusChangeCharge, localDatabase)
  writeTableToDatabase(conn, "Incident", txTables$Incident, localDatabase)
  writeTableToDatabase(conn, "IncidentResponseUnit", txTables$IncidentResponseUnit, localDatabase)
}

#' @importFrom lubridate hour<- minute<- second<- ddays
#' @import dplyr
createTransactionTables <- function(codeTableList, lookbackDayCount, averageDailyBookingVolume,
                                    percentPretrial, percentSentenced, averagePretrialStay,
                                    averageSentenceStay, recidivismRate, recidivistEpisodes, percentAssessments,
                                    averageDailyCrisisIncidents, crisisRecidivismRate, baseDate) {

  ret <- list()

  writeLines("Creating Booking Table...")

  dailyBookings <- sample((.9*averageDailyBookingVolume):(1.1*averageDailyBookingVolume), size=lookbackDayCount, replace=TRUE)

  day <- integer()
  bookingNumber <- integer()

  lastBookingNumber <- 0
  for (i in seq(lookbackDayCount)) {
    dbi <- dailyBookings[i]
    day <- c(day, rep(i, dbi))
    bookingNumber <- c(bookingNumber, lastBookingNumber + 1:dbi)
    lastBookingNumber <- lastBookingNumber + dbi
  }

  df <- day %>%
    projectJailStay(seq(length(day)), bookingNumber, percentPretrial, percentSentenced,
                    averagePretrialStay, averageSentenceStay) %>%
    generateRecidivism(recidivismRate, recidivistEpisodes, priorBookingNumbers=integer(),
                       percentPretrial, percentSentenced, averagePretrialStay, averageSentenceStay)

  df$FacilityID <- generateRandomIDsFromCodeTable(codeTableList, "Facility", nrow(df))
  df$SupervisionUnitTypeID <- generateRandomIDsFromCodeTable(codeTableList, "SupervisionUnitType", nrow(df))
  df$InmateJailResidentIndicator <- rbinom(n=nrow(df), size=1, prob=.8)

  df$BookingDate <- baseDate - ddays(df$DaysAgo)

  nn <- nrow(df)

  df$BookingTime <- paste0(
    sample(0:23, size=nn, replace = TRUE), ':',
    sample(0:59, size=nn, replace = TRUE), ':',
    sample(0:59, size=nn, replace = TRUE))

  # can't use mutate and lubridate together on this because the lubridate overloaded minus ("-") doesn't handle NAs well...
  df[!is.na(df$ReleaseDay), 'ScheduledReleaseDate'] <- as.Date(baseDate - ddays(df[!is.na(df$ReleaseDay), 'ReleaseDay']), origin="1970-01-01")
  class(df$ScheduledReleaseDate) <- "Date"

  df <- df %>%
    mutate(BookingNumber=paste0("B", formatC(BookingNumber, width=12, flag="0", format="d")))

  ReleaseDateTime <- df$ScheduledReleaseDate
  hour(ReleaseDateTime) <- sample(0:23, size=nn, replace = TRUE)
  minute(ReleaseDateTime) <- sample(0:59, size=nn, replace = TRUE)
  second(ReleaseDateTime) <- sample(0:59, size=nn, replace = TRUE)

  df$BookingTimestamp <- NA

  ret$Booking <- df %>% select(-DaysAgo, -LengthOfStay, -ReleaseDay, -Disposition)

  writeLines(paste0("...Booking table created with ", nrow(df), " rows."))

  Person <- data.frame(PersonID=df$PersonID) %>%
    mutate(PersonUniqueIdentifier=PersonID)

  Person$PersonID <-as.integer(1:nrow(Person))

  actualPersonDf <- buildActualPersonTable(codeTableList, unique(Person$PersonUniqueIdentifier), baseDate)

  Person <- Person %>% inner_join(actualPersonDf, by=c("PersonUniqueIdentifier"="PersonUniqueIdentifier")) %>%
    mutate(PersonUniqueIdentifier=paste0("P", formatC(PersonUniqueIdentifier, width=16, flag="0"))) %>%
    mutate(PersonUniqueIdentifier2=PersonUniqueIdentifier)
  Person$PersonTimestamp <- NA

  ret$Person <- Person
  ret$Booking$PersonID <- Person$PersonID
  bookingID <- seq(nrow(ret$Booking))
  ret$Booking$BookingID <- bookingID

  writeLines(paste0("Created Person table for Bookings with ", nrow(Person), " records"))

  writeLines("Creating Incident Table...")

  dailyIncidents <- sample((.5*averageDailyCrisisIncidents):(1.3*averageDailyCrisisIncidents), size=lookbackDayCount, replace=TRUE)
  day <- rep(seq_along(dailyIncidents), dailyIncidents)
  lastPersonID <- max(Person$PersonID)

  Incident <- tibble(IncidentReportedDate=baseDate - ddays(day)) %>%
    mutate(
      IncidentID=row_number(),
      IncidentNumber=paste0("I", formatC(IncidentID, width=12, flag="0", format="d")),
      IncidentReportedTime=paste0(
        str_pad(sample(0:23, size=n(), replace = TRUE), 2, 'left', '0'), ':',
        str_pad(sample(0:59, size=n(), replace = TRUE), 2, 'left', '0'), ':',
        str_pad(sample(0:59, size=n(), replace = TRUE), 2, 'left', '0')),
      OfficerCount=sample(1:5, size=n(), replace=TRUE, prob=c(.3, .3, .2, .15, .05)),
      DispositionLocation=sample(
        c('Eastside Hospital', 'Downtown Hospital ER', 'Northend Crisis Center', '#$FHSDUKJddf1'),
        size=n(), replace=TRUE, prob=c(.3, .3, .3, .1)
      ),
      CallNature=sample(
        c('Wellness Check', 'Domestic Violence', 'Threats', 'Medical', 'sdfd939sfg##$'),
        size=n(), replace=TRUE, prob=c(.2, .3, .2, .15, .05)
      ),
      PendingCriminalCharges=sample(
        c('Crisis Hold', 'Juvenile', 'Other'),
        size=n(), replace=TRUE, prob=c(.7, .2, .1)
      ),
      LocationID=NA_integer_,
      PersonID=lastPersonID + IncidentID,
      PersonUniqueIdentifier=PersonID
    )

  IncidentPerson <- Incident %>%
    select(IncidentID, IncidentReportedDate) %>%
    sample_frac(crisisRecidivismRate) %>%
    mutate(IncidentReportedDate=IncidentReportedDate - ddays(sample(7:lookbackDayCount, size=n()))) %>%
    inner_join(
      Incident %>% select(IncidentReportedDate, PriorPersonUniqueIdentifier=PersonID),
      by='IncidentReportedDate'
    ) %>%
    group_by(IncidentID) %>%
    sample_n(1) %>%
    select(IncidentID, PriorPersonUniqueIdentifier)

  if (nrow(IncidentPerson) > 0) {
    Incident <- Incident %>% left_join(IncidentPerson, by='IncidentID') %>%
      mutate(PersonUniqueIdentifier=case_when(!is.na(PriorPersonUniqueIdentifier) ~ PriorPersonUniqueIdentifier,
                                              TRUE ~ PersonUniqueIdentifier)) %>%
      select(-PriorPersonUniqueIdentifier)
    writeLines(paste0('Added ', nrow(IncidentPerson), ' crisis recidivist records'))
  } else {
    writeLines('Note: no crisis recidivists generated...sample set too small')
  }

  IncidentPerson <- Incident %>%
    select(PersonID, PersonUniqueIdentifier)

  actualPersonDf <- buildActualPersonTable(codeTableList, unique(IncidentPerson$PersonUniqueIdentifier), baseDate)

  IncidentPerson <- IncidentPerson %>% inner_join(actualPersonDf, by=c("PersonUniqueIdentifier"="PersonUniqueIdentifier")) %>%
    mutate(PersonUniqueIdentifier=paste0("P", formatC(PersonUniqueIdentifier, width=16, flag="0"))) %>%
    mutate(PersonUniqueIdentifier2=PersonUniqueIdentifier)
  IncidentPerson$PersonTimestamp <- NA

  Person <- Person %>% bind_rows(IncidentPerson)

  IncidentTemp <- Incident %>%
    select(-PersonUniqueIdentifier) %>%
    inner_join(Person, by='PersonID') %>%
    group_by(PersonUniqueIdentifier) %>%
    filter(n()==1) %>%
    ungroup() %>%
    sample_frac(.05) %>%
    select(PersonID)

  PersonTemp <- Person %>%
    semi_join(ret$Booking %>% sample_n(nrow(IncidentTemp)), by='PersonID') %>%
    mutate(PersonID=IncidentTemp$PersonID) %>%
    inner_join(Incident %>% select(PersonID, IncidentReportedDate), by='PersonID') %>%
    mutate(PersonAgeAtEvent=(IncidentReportedDate - PersonBirthDate) / dyears(1)) %>%
    select(-IncidentReportedDate)

  Person <- Person %>% anti_join(PersonTemp, by='PersonID') %>%
    bind_rows(PersonTemp)

  writeLines(paste0('Updated Incident Persons with ', nrow(IncidentTemp), ' unique person IDs associated with bookings'))
  writeLines(paste0('Updated Person table for ', nrow(IncidentPerson), ' Crisis Incident subjects'))
  writeLines(paste0('Created Incident table with ', nrow(Incident), ' rows'))

  Incident <- select(Incident, -PersonUniqueIdentifier)
  ret$Incident <- Incident

  ret$Person <- Person

  writeLines('Creating IncidentResponseUnit table')

  IncidentResponseUnit <- tibble(IncidentID=rep(Incident$IncidentID, sample(1:4, size=nrow(Incident), replace=TRUE))) %>%
    inner_join(Incident %>% select(IncidentID, dd=IncidentReportedDate, tt=IncidentReportedTime), by='IncidentID')

  IncidentResponseUnit <- suppressWarnings(
    IncidentResponseUnit <- IncidentResponseUnit %>%
      mutate(dd=case_when(is.na(dd) | is.na(tt) ~ as_datetime(NA), TRUE ~ ymd_hms(paste0(format(dd, '%Y-%m-%d'), ' ', tt)))) %>%
      select(-tt)
  )

  IncidentResponseUnit <- IncidentResponseUnit %>%
    mutate(dda=dd - dminutes(sample(seq(0, 60), size=n(), replace=TRUE)), ddc=dda + dminutes(sample(seq(5, 60*2), size=n(), replace=TRUE))) %>%
    mutate(UnitArrivalDate=as_date(dda), UnitArrivalTime=format(dda, '%H:%M:%S'),
           UnitClearDate=as_date(ddc), UnitClearTime=format(ddc, '%H:%M:%S')) %>%
    select(-dda, -ddc, -dd) %>%
    mutate(UnitIdentifier=paste0('Unit ', sample(1:8, size=n(), replace=TRUE))) %>%
    mutate(IncidentResponseUnitID=row_number())

  writeLines(paste0('Created IncidentResponseUnit table with ', nrow(IncidentResponseUnit), ' rows'))

  ret$IncidentResponseUnit <- IncidentResponseUnit

  tdf <- bind_rows(
    df %>% select(PersonID, EventDate=BookingDate),
    Incident %>% select(PersonID, EventDate=IncidentReportedDate)
  )

  bhTableList <- buildBehavioralHealthTables(tdf$PersonID, tdf$EventDate, percentAssessments, codeTableList)
  ret <- c(ret, bhTableList)

  bookingChildTableList <- buildBookingChildTables(bookingID, ret$Booking$BookingNumber, ReleaseDateTime, codeTableList)
  ret <- c(ret, bookingChildTableList)

  chargeDf <- bookingChildTableList$BookingCharge
  maxArrestID <- max(chargeDf$BookingArrestID)
  arrestDf <- bookingChildTableList$BookingArrest
  arrestDf <- filter(arrestDf, BookingArrestID == maxArrestID)
  bookingIDToAlter <- arrestDf[1, 'BookingID']

  ret$Booking[ret$Booking$BookingID==bookingIDToAlter, 'BookingDate'] <- baseDate + 30

  # create one weird release record where the release date is before the corresponding booking date

  releaseRecordBookingID <- sample(ret$CustodyRelease$BookingID, 1)
  bd <- ret$Booking[ret$Booking$BookingID==releaseRecordBookingID, 'BookingDate']
  ret$CustodyRelease[ret$CustodyRelease$BookingID==releaseRecordBookingID, 'ReleaseDate'] <- bd - 10

  changeTableList <- buildChangeTables(ret, codeTableList)

  ret$Person <- NULL
  ret$BehavioralHealthAssessment <- NULL
  ret$Treatment <- NULL
  ret$BehavioralHealthEvaluation <- NULL
  ret$BehavioralHealthAssessmentCategory <- NULL
  ret$PrescribedMedication <- NULL

  ret <- c(ret, changeTableList)

  chargeDf <- changeTableList$CustodyStatusChangeCharge
  maxArrestID <- max(chargeDf$CustodyStatusChangeArrestID)
  arrestDf <- changeTableList$CustodyStatusChangeArrest
  arrestDf <- filter(arrestDf, CustodyStatusChangeArrestID == maxArrestID)
  cscIDToAlter <- arrestDf[1, 'CustodyStatusChangeID']

  ret$CustodyStatusChange[ret$CustodyStatusChange$CustodyStatusChangeID==cscIDToAlter, 'BookingDate'] <- baseDate + 30

  ret

}

#' @import dplyr
buildChangeTables <- function(txTableList, codeTableList) {

  ret <- list()

  # 20% of bookings have edits
  changedBookings <- txTableList$Booking %>% sample_frac(.2) %>% select(-BookingTimestamp)

  multipleCsc <- changedBookings %>% sample_n(size=10)
  futureBookingEdits <- changedBookings %>% sample_n(size=10) %>% mutate(BookingID=NA, BookingNumber=paste0(BookingNumber, 'F'))
  changedBookings <- bind_rows(changedBookings, multipleCsc, futureBookingEdits) %>%
    mutate(CustodyStatusChangeID=seq(n()))

  futureBookingEdits <- changedBookings %>% filter(is.na(BookingID))

  changedArrests <- txTableList$BookingArrest %>% filter(BookingID %in% changedBookings$BookingID) %>%
    inner_join(changedBookings %>% select(BookingID, CustodyStatusChangeID), by=c("BookingID"="BookingID"))

  futureBookingArrests <- txTableList$BookingArrest %>% sample_n(size=10)
  futureBookingArrests$CustodyStatusChangeID <- futureBookingEdits$CustodyStatusChangeID

  changedArrests <- bind_rows(changedArrests, futureBookingArrests) %>%
    mutate(CustodyStatusChangeArrestID=seq(n())) %>% select(-BookingID, -BookingArrestTimestamp)

  futureBookingArrests <- changedArrests %>% filter(CustodyStatusChangeID %in% futureBookingArrests$CustodyStatusChangeID)

  changedCharges <- txTableList$BookingCharge %>% filter(BookingArrestID %in% changedArrests$BookingArrestID) %>%
    inner_join(changedArrests %>% select(BookingArrestID, CustodyStatusChangeArrestID), by=c("BookingArrestID"="BookingArrestID"))

  futureBookingCharges <- txTableList$BookingCharge %>% sample_n(size=10)
  futureBookingCharges$CustodyStatusChangeArrestID <- futureBookingArrests$CustodyStatusChangeArrestID

  changedCharges <- bind_rows(changedCharges, futureBookingCharges) %>%
    mutate(CustodyStatusChangeChargeID=seq(n())) %>%
    select(-BookingArrestID, -BookingChargeTimestamp)

  changedArrests <- changedArrests %>%
    select(-BookingArrestID) %>%
    mutate(LocationID=NA, CustodyStatusChangeArrestTimestamp=NA)

  changedCharges <- changedCharges %>%
    select(-BookingChargeID) %>%
    mutate(BondRemainingAmount=NA, CustodyStatusChangeChargeTimestamp=NA)

  # note:  we can change more attributes as we learn what is important to test in the ADS loading process
  changedBookings$SupervisionUnitTypeID <- generateRandomIDsFromCodeTable(codeTableList, "SupervisionUnitType", nrow(changedBookings))
  changedArrests$ArrestAgencyID <- generateRandomIDsFromCodeTable(codeTableList, "Agency", nrow(changedArrests))
  changedCharges$ChargeClassTypeID <- generateRandomIDsFromCodeTable(codeTableList, "ChargeClassType", nrow(changedCharges))

  nextPersonID <- as.integer(max(txTableList$Person$PersonID) + 1)

  ChangedPerson <- txTableList$Person %>%
    right_join(changedBookings %>% select(PersonID), by=c("PersonID"="PersonID")) %>%
    mutate(PersonID=as.integer(nextPersonID + row_number()))

  writeLines(paste0("Adding ", nrow(ChangedPerson), " rows to Person table for CustodyStatusChange records."))
  ret$Person <- bind_rows(txTableList$Person, ChangedPerson)

  changedBookings$PersonID <- ChangedPerson$PersonID
  changedBookings$CustodyStatusChangeTimestamp <- NA

  ret$CustodyStatusChange <- changedBookings
  writeLines(paste0("Created CustodyStatusChange table with ", nrow(changedBookings), " rows."))
  ret$CustodyStatusChangeArrest <- changedArrests
  writeLines(paste0("Created CustodyStatusChangeArrest table with ", nrow(changedArrests), " rows."))
  ret$CustodyStatusChangeCharge <- changedCharges
  writeLines(paste0("Created CustodyStatusChangeCharge table with ", nrow(changedCharges), " rows."))

  changedBHPerson <- ChangedPerson %>%
    inner_join(changedBookings, by='PersonID') %>%
    sample_n(5) %>%
    select(PersonID, BookingDate)

  startingBehavioralHealthAssessmentID <- nrow(txTableList$BehavioralHealthAssessment) + 1
  startingTreatmentID <- nrow(txTableList$Treatment) + 1
  startingBehavioralHealthEvaluationID <- nrow(txTableList$BehavioralHealthEvaluation) + 1
  startingBehavioralHealthAssessmentCategoryID <- nrow(txTableList$BehavioralHealthAssessmentCategory) + 1
  startingPrescribedMedicationID <- nrow(txTableList$PrescribedMedication) + 1

  bhTables <- buildBehavioralHealthTables(changedBHPerson$PersonID, changedBHPerson$BookingDate, 1, codeTableList,
                                          startingBehavioralHealthAssessmentID,
                                          startingTreatmentID,
                                          startingBehavioralHealthEvaluationID,
                                          startingBehavioralHealthAssessmentCategoryID,
                                          startingPrescribedMedicationID
  )

  ret$BehavioralHealthAssessment <- txTableList$BehavioralHealthAssessment %>% bind_rows(bhTables$BehavioralHealthAssessment)
  ret$Treatment <- txTableList$Treatment %>% bind_rows(bhTables$Treatment)
  ret$BehavioralHealthEvaluation <- txTableList$BehavioralHealthEvaluation %>% bind_rows(bhTables$BehavioralHealthEvaluation)
  ret$BehavioralHealthAssessmentCategory <- txTableList$BehavioralHealthAssessmentCategory %>% bind_rows(bhTables$BehavioralHealthAssessmentCategory)
  ret$PrescribedMedication <- txTableList$PrescribedMedication %>% bind_rows(bhTables$PrescribedMedication)

  ret

}

#' @import dplyr
#' @importFrom lubridate as_date hour minute second
buildBookingChildTables <- function(bookingID, bookingNumber, releaseDateTime, codeTableList) {

  ret <- list()

  ReleaseTime <- paste0(hour(releaseDateTime), ':', minute(releaseDateTime), ':', as.integer(second(releaseDateTime)))
  ReleaseDate <- as_date(releaseDateTime)

  CustodyRelease <- data.frame(BookingID=bookingID, BookingNumber=bookingNumber, ReleaseDate=ReleaseDate, ReleaseTime=ReleaseTime) %>%
    filter(!is.na(ReleaseDate)) %>%
    mutate(CustodyReleaseID=seq(n()), ReleaseCondition=NA, CustodyReleaseTimestamp=NA)

  writeLines(paste0("Created CustodyRelease table with ", nrow(CustodyRelease), " rows"))

  ret$CustodyRelease <- CustodyRelease

  n_Arrest <- sample(0:6, size=length(bookingID), replace=TRUE, prob=c(.05, .65, .2, .05, .01, .01, .01))
  nonZeros <- n_Arrest > 0
  n_Arrest <- n_Arrest[nonZeros]
  bookingID <- bookingID[nonZeros]
  BookingArrest <- data.frame(BookingID=rep(bookingID, times=n_Arrest)) %>%
    mutate(BookingArrestID=seq(n()))
  recs <- nrow(BookingArrest)
  BookingArrest$ArrestAgencyID <- generateRandomIDsFromCodeTable(codeTableList, "Agency", recs)
  BookingArrest$LocationID <- NA
  BookingArrest$BookingArrestTimestamp <- NA

  ret$BookingArrest <- BookingArrest

  writeLines(paste0("Created BookingArrest table with ", nrow(BookingArrest), " rows"))

  bookingArrestID <- BookingArrest$BookingArrestID

  n_Charge <- sample(0:5, size=length(bookingArrestID), replace=TRUE, prob=c(.05, .45, .2, .15, .1, .05))
  nonZeros <- n_Charge > 0
  n_Charge <- n_Charge[nonZeros]
  bookingArrestID <- bookingArrestID[nonZeros]
  BookingCharge <- data.frame(BookingArrestID=rep(bookingArrestID, times=n_Charge)) %>%
    mutate(BookingChargeID=seq(n()))
  recs <- nrow(BookingCharge)
  BookingCharge$ChargeCode <- generateRandomLabelFromDimensionalCodeTable('ChargeType', recs)
  BookingCharge$ChargeDisposition <- generateRandomLabelFromDimensionalCodeTable('ChargeDispositionType', recs)
  BookingCharge$BondAmount <- sample(c(10000, 5000, 2500, 1000, 500, 50, NA), size=recs, replace=TRUE, prob=c(10,40,30,10,5,4,1))
  BookingCharge$AgencyID <- generateRandomIDsFromCodeTable(codeTableList, "Agency", recs)
  BookingCharge$BondTypeID <- generateRandomIDsFromCodeTable(codeTableList, "BondType", recs)
  BookingCharge$ChargeJurisdictionTypeID <- generateRandomIDsFromCodeTable(codeTableList, "JurisdictionType", recs)
  BookingCharge$ChargeClassTypeID <- generateRandomIDsFromCodeTable(codeTableList, "ChargeClassType", recs)
  BookingCharge$BondStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "BondStatusType", recs)
  BookingCharge$BookingChargeTimestamp <-NA

  ret$BookingCharge <- BookingCharge

  writeLines(paste0("Created BookingCharge table with ", nrow(BookingCharge), " rows"))

  ret

}

#' @importFrom dplyr sample_frac mutate select left_join
#' @importFrom lubridate ddays
buildBehavioralHealthTables <- function(PersonID, EventDate, percentAssessments, codeTableList,
                                        startingBehavioralHealthAssessmentID=1,
                                        startingTreatmentID=1,
                                        startingBehavioralHealthEvaluationID=1,
                                        startingBehavioralHealthAssessmentCategoryID=1,
                                        startingPrescribedMedicationID=1) {

  tenProbs <- sample(10)

  ret <- list()

  # 50% of inmates have BH issues
  bha <- data.frame(PersonID, EventDate) %>%
    sample_frac(percentAssessments)

  recs <- nrow(bha)

  bha$CareEpisodeStartDate <- as.Date(bha$EventDate - ddays(rchisq(n=recs, df=90)))
  bha$CareEpisodeEndDate <- as.Date(bha$CareEpisodeStartDate + ddays(rchisq(n=recs, df=45)))

  bha <- bha %>%
    mutate(CareEpisodeEndDate=replace(CareEpisodeEndDate, CareEpisodeEndDate > EventDate, NA)) %>%
    select(-EventDate)

  bha[sample(recs, recs*.1), 'CareEpisodeEndDate'] <- NA

  bha$MedicaidStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "MedicaidStatusType", recs)
  bha$SeriousMentalIllnessIndicator <- as.integer(rbinom(n=recs, size=1, prob=.3))
  bha$EnrolledProviderName <- generateRandomLabelFromDimensionalCodeTable('TreatmentProviderType', recs)
  bha$BehavioralHealthAssessmentID <- seq(recs) + startingBehavioralHealthAssessmentID - 1
  bha$BehavioralHealthAssessmentTimestamp <- NA

  bha$n_Treatment <- sample(1:4, size=recs, replace=TRUE)

  writeLines(paste0("Created BH Assessment table with ", recs, " rows."))

  # 80% of assessments result in treatment
  tmt <- bha %>% sample_frac(.8) %>%
    select(BehavioralHealthAssessmentID, CareEpisodeStartDate, CareEpisodeEndDate, n_Treatment)

  recs <- nrow(tmt)

  bhaIDs <- rep(tmt$BehavioralHealthAssessmentID, times=tmt$n_Treatment)

  tmt <- data.frame(BehavioralHealthAssessmentID=bhaIDs) %>%
    left_join(tmt, by=c("BehavioralHealthAssessmentID"="BehavioralHealthAssessmentID")) %>%
    mutate(EpisodeLength=(CareEpisodeEndDate - CareEpisodeStartDate) / ddays(1)) %>%
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
  tmt$TreatmentProviderName <- generateRandomLabelFromDimensionalCodeTable('TreatmentProviderType', recs)
  tmt$TreatmentTimestamp <- NA

  tmt$TreatmentID <- seq(nrow(tmt)) + startingTreatmentID - 1

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
  ev$BehavioralHealthDiagnosisDescription <- generateRandomLabelFromDimensionalCodeTable('BehavioralHealthEvaluationType', recs)
  ev$BehavioralHealthEvaluationID <- seq(recs) + startingBehavioralHealthEvaluationID - 1
  ev$BehavioralHealthEvaluationTimestamp <- NA

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

  med$MedicationDescription <- generateRandomLabelFromDimensionalCodeTable('MedicationType', recs)
  med$MedicationDoseMeasure <- paste0(sample(c("5", "10", "50", "500", "1000"), size=recs, replace=TRUE),
                                      sample(c(" mg", " oz", " Units", " ml"), size=recs, replace=TRUE))
  med$PrescribedMedicationID <- seq(recs) + startingPrescribedMedicationID - 1
  med$MedicationDispensingDate <- NA
  med$PrescribedMedicationTimestamp <- NA

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
  bhc$BehavioralHealthAssessmentCategoryTimestamp <- NA
  recs <- nrow(bhc)
  bhc$BehavioralHealthAssessmentCategoryID <- seq(recs) + startingBehavioralHealthAssessmentCategoryID - 1

  writeLines(paste0("Created BH Assessment Category table with ", recs, " rows."))

  ret$BehavioralHealthAssessment <- bha %>% select(-starts_with("n_"))
  ret$Treatment <- tmt
  ret$BehavioralHealthEvaluation <- ev
  ret$BehavioralHealthAssessmentCategory <- bhc
  ret$PrescribedMedication <- med

  ret

}

#' @importFrom openxlsx read.xlsx
#' @import dplyr
#' @importFrom lazyeval interp
generateRandomLabelFromDimensionalCodeTable <- function(codeTableName, n) {
  ct <- read.xlsx(defaultCodeTableSpreadsheetFile, sheet=codeTableName) %>%
    filter_(interp(~ !(v %in% s), v=as.name(paste0(codeTableName, "ID")), s=c(99998,99999)))
  labelColumn <- paste0(codeTableName, 'Description')
  labels <- ct[[labelColumn]]
  ret <- sample(labels, n, TRUE, prob=sample(seq(length(labels)), length(labels), TRUE))
  ret[as.logical(rbinom(n=length(ret), size=1, prob=.02))] <- NA
  ret
}

#' @importFrom lubridate dyears as_date
buildActualPersonTable <- function(codeTableList, PersonUniqueIdentifier, baseDate) {

  ret <- data.frame(PersonUniqueIdentifier)

  nPeople <- length(PersonUniqueIdentifier)

  birthdates <- baseDate - dyears(generateRandomArresteeAges(23, nPeople))

  birthdates <- as_date(birthdates)
  ret$PersonBirthDate <- birthdates

  ret$PersonAgeAtEvent <- as.integer((baseDate - birthdates) / dyears(1) - 1)
  # recode 1 in 50 birthdates as NA, to simulate missing birthdates
  ret[sample(nPeople, as.integer(nPeople/50)), 'PersonBirthDate'] <- NA

  tenProbs <- sample(10)

  ret$LanguageTypeID <- generateRandomIDsFromCodeTable(codeTableList, "LanguageType", nPeople)
  ret$PersonSexTypeID <- generateRandomIDsFromCodeTable(codeTableList, "PersonSexType", nPeople)
  ret$PersonRaceTypeID <- generateRandomIDsFromCodeTable(codeTableList, "PersonRaceType", nPeople)
  ret$PersonEthnicityTypeID <- generateRandomIDsFromCodeTable(codeTableList, "PersonEthnicityType", nPeople)
  ret$MilitaryServiceStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "MilitaryServiceStatusType", nPeople)
  ret$Occupation <- generateRandomLabelFromDimensionalCodeTable('OccupationType', nPeople)
  ret$EducationLevel <- generateRandomLabelFromDimensionalCodeTable('EducationLevelType', nPeople)
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
  # assume ages are distributed according to the (delete fromd) inverse gamma distribution with shape as
  # specified above, and scale (beta) = 1.  this seems to produce a very nice age distribution.
  ret <- rtrunc(n=size,
                spec="igamma",
                a=minimumAge/(approximateMeanAge*(alphaParameter-1)),
                b=maximumAge/(approximateMeanAge*(alphaParameter-1)),
                alpha=alphaParameter, beta=1)*(approximateMeanAge*(alphaParameter-1))
  options(warn=warnVal)

  # clean up from hack
  rm(pigamma, envir=.GlobalEnv)
  rm(qigamma, envir=.GlobalEnv)

  ret

}

generateRandomIDsFromCodeTable <- function(codeTableList, codeTableName, size) {
  codeTableDf <- codeTableList[[codeTableName]]
  idVector <- codeTableDf[[paste0(codeTableName, "ID")]]
  probs <- sample(nrow(codeTableDf))
  as.integer(sample(idVector, replace=TRUE, size=size, prob=probs))
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

