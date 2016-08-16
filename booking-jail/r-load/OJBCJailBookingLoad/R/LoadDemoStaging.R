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
#' @param countyFIPSCode the FIPS code for the county of interest
#' @param censusTractShapefileDSN Path to folder containing tract shapefiles.  Download shapefiles from https://www.census.gov/geo/maps-data/data/cbf/cbf_tracts.html
#' @param censusTractShapefileLayer Layer within the shapefile (should be same name as last part of DSN path)
#' @param censusTractPopulationFile file of population data at the census tract level.  Download this from the US
#' Census website, by visiting factfinder.census.gov, select Advanced Search, then Geographies.  Chose "Census Tract - 140" as
#' Geography Type, then the state and county of interest, and "Add to Selection".  Search for table name P1 (total population)
#' and download.
#' @import RMySQL
#' @import rgdal
#' @import sp
#' @import readr
#' @import tidyr
#' @import stringr
#' @examples
#' loadDemoStaging(censusTractShapefileDSN="/opt/data/Shapefiles/gz_2010_08_150_00_500k", censusTractShapefileLayer="gz_2010_08_150_00_500k", countyFIPSCode="001", censusTractPopulationFile="/opt/data/Census/DEC_10_SF1_P1_with_ann.csv")
#' @export
loadDemoStaging <- function(databaseName="ojbc_booking_staging_demo",
                                censusTractShapefileDSN, censusTractShapefileLayer, countyFIPSCode,
                            censusTractPopulationFile) {

  loadStartTime <- Sys.time()

  stagingConnection <- dbConnect(MySQL(), host="localhost", dbname=databaseName, username="root")

  dbSendQuery(stagingConnection, "set foreign_key_checks=0")

  dbSendQuery(stagingConnection, "truncate AgencyType")
  dbSendQuery(stagingConnection, "truncate AssessmentCategoryType")
  dbSendQuery(stagingConnection, "truncate BedType")
  dbSendQuery(stagingConnection, "truncate BehavioralHealthAssessment")
  dbSendQuery(stagingConnection, "truncate BehavioralHealthAssessmentCategory")
  dbSendQuery(stagingConnection, "truncate BehavioralHealthDiagnosisType")
  dbSendQuery(stagingConnection, "truncate BehavioralHealthEvaluation")
  dbSendQuery(stagingConnection, "truncate BondStatusType")
  dbSendQuery(stagingConnection, "truncate BondType")
  dbSendQuery(stagingConnection, "truncate Booking")
  dbSendQuery(stagingConnection, "truncate BookingArrest")
  dbSendQuery(stagingConnection, "truncate BookingCharge")
  dbSendQuery(stagingConnection, "truncate CaseStatusType")
  dbSendQuery(stagingConnection, "truncate ChargeClassType")
  dbSendQuery(stagingConnection, "truncate ChargeType")
  dbSendQuery(stagingConnection, "truncate CustodyRelease")
  dbSendQuery(stagingConnection, "truncate CustodyStatusChange")
  dbSendQuery(stagingConnection, "truncate CustodyStatusChangeArrest")
  dbSendQuery(stagingConnection, "truncate CustodyStatusChangeCharge")
  dbSendQuery(stagingConnection, "truncate DomicileStatusType")
  dbSendQuery(stagingConnection, "truncate EducationLevelType")
  dbSendQuery(stagingConnection, "truncate EnrolledHealthcareProvider")
  dbSendQuery(stagingConnection, "truncate Facility")
  dbSendQuery(stagingConnection, "truncate JurisdictionType")
  dbSendQuery(stagingConnection, "truncate LanguageType")
  dbSendQuery(stagingConnection, "truncate Location")
  dbSendQuery(stagingConnection, "truncate MedicaidStatusType")
  dbSendQuery(stagingConnection, "truncate MedicationType")
  dbSendQuery(stagingConnection, "truncate MilitaryServiceStatusType")
  dbSendQuery(stagingConnection, "truncate OccupationType")
  dbSendQuery(stagingConnection, "truncate Person")
  dbSendQuery(stagingConnection, "truncate PersonEthnicityType")
  dbSendQuery(stagingConnection, "truncate PersonRaceType")
  dbSendQuery(stagingConnection, "truncate PersonSexType")
  dbSendQuery(stagingConnection, "truncate PrescribedMedication")
  dbSendQuery(stagingConnection, "truncate ProgramEligibilityType")
  dbSendQuery(stagingConnection, "truncate SexOffenderStatusType")
  dbSendQuery(stagingConnection, "truncate Treatment")
  dbSendQuery(stagingConnection, "truncate TreatmentInitiationType")
  dbSendQuery(stagingConnection, "truncate TreatmentProvider")
  dbSendQuery(stagingConnection, "truncate TreatmentStatusType")
  dbSendQuery(stagingConnection, "truncate WorkReleaseStatusType")

  codeTableList <- loadCodeTables(stagingConnection, "StagingCodeTables.xlsx")

  txTableList <- createTransactionTables(codeTableList, 365, 2000, .25, .01, 1.4, 65)

  c(codeTableList, txTableList)

}

#' @importFrom lubridate hour minute second ddays
#' @import dplyr
#' @export
createTransactionTables <- function(codeTableList, lookbackDayCount=365, averageDailyBookingVolume=2000,
                                    percentPretrial=.39, percentSentenced=.01, averagePretrialStay=1.5,
                                    averageSentenceStay=60, recidivismRate=.5, recidivistEpisodes=5,
                                    baseDate=Sys.Date()) {

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
  df$BedTypeID <- generateRandomIDsFromCodeTable(codeTableList, "BedType", nrow(df))

  BookingDateTime <- baseDate - ddays(df$DaysAgo)
  hour(BookingDateTime) <- sample(0:23, size=length(BookingDateTime), replace = TRUE)
  minute(BookingDateTime) <- sample(0:59, size=length(BookingDateTime), replace = TRUE)
  second(BookingDateTime) <- sample(0:59, size=length(BookingDateTime), replace = TRUE)

  df$BookingDateTime <- BookingDateTime

  df <- df %>%
    mutate(ScheduledReleaseDate=baseDate - ddays(ReleaseDay),
           BookingNumber=paste0("B", formatC(BookingNumber, width=12, flag="0")))

  ret$Booking <- df

  Person <- data.frame(PersonID=df$PersonID) %>%
    mutate(PersonUniqueIdentifier=PersonID)

  Person$PersonID <- 1:nrow(Person)

  actualPersonDf <- buildActualPersonTable(codeTableList, unique(Person$PersonUniqueIdentifier), baseDate)

  Person <- Person %>% inner_join(actualPersonDf, by=c("PersonUniqueIdentifier"="PersonUniqueIdentifier")) %>%
    mutate(PersonUniqueIdentifier=paste0("P", formatC(PersonUniqueIdentifier, width=16, flag="0")))

  ret$Person <- Person
  ret$Booking$PersonID <- Person$PersonID

  bhTableList <- buildBehavioralHealthTables(Person$PersonID, codeTableList)

  ret <- c(ret, bhTableList)

  ret

}

#' @export
buildBehavioralHealthTables <- function(PersonID, codeTableList) {

  ret <- list()

  # 50% of inmates have BH issues
  BehavioralHealthAssessment <- data.frame(PersonID=sample(PersonID, size=length(PersonID)*.5))
  BehavioralHealthAssessment$MedicaidStatusTypeID <- generateRandomIDsFromCodeTable(codeTableList, "MedicaidStatusType", nrow(BehavioralHealthAssessment))
  BehavioralHealthAssessment$EnrolledHealthcareProviderID <- generateRandomIDsFromCodeTable(codeTableList, "EnrolledHealthcareProvider", nrow(BehavioralHealthAssessment))
  BehavioralHealthAssessment$SeriousMentalIllnessIndicator <- as.logical(rbinom(n=nrow(BehavioralHealthAssessment), size=2, prob=.3))
  BehavioralHealthAssessment$BehavioralHealthAssessmentID <- 1:nrow(BehavioralHealthAssessment)

  ret$BehavioralHealthAssessment <- BehavioralHealthAssessment

  ret

}

#' @importFrom lubridate dyears %--%
#' @export
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
#' @importFrom pscl rigamma
generateRandomArresteeAges <- function(approximateMeanAge, size, minimumAge=12, maximumAge=85) {

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

