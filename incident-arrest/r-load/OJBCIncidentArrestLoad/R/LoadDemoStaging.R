#' @import DBI
#' @import purrr
#' @import dplyr
#' @importFrom lubridate dyears as_date
#' @importFrom openxlsx read.xlsx getSheetNames
#' @importFrom stringi stri_rand_strings
#' @export
loadDemoStaging <- function(connection, incidentCount=1000, maxIncidentDate=Sys.Date(), minIncidentDate=function(d) {d-dyears(3)}) {

  spreadsheetFile <- system.file("raw", "StagingCodeTables.xlsx", package=getPackageName())
  codeTableNames <- getSheetNames(spreadsheetFile)

  dfs <- map(codeTableNames, function(codeTableName) {
    read.xlsx(spreadsheetFile, sheet=codeTableName) %>% as_tibble()
  })

  names(dfs) <- codeTableNames

  towns <- getTowns(dfs$Agency)

  if ("function" == class(minIncidentDate)) {
    minIncidentDate <- minIncidentDate(maxIncidentDate)
  }

  dates <- seq(from=minIncidentDate, to=maxIncidentDate, by=1)

  Incident <- tibble(
    IncidentID=seq(incidentCount),
    ReportingAgencyID=sample(dfs$Agency$AgencyID, incidentCount, TRUE, 1/(2^seq(nrow(dfs$Agency)))),
    IncidentCaseNumber=paste0(stri_rand_strings(incidentCount, 2, '[A-Z]'), stri_rand_strings(incidentCount, 6, '[A-Z0-9]')),
    IncidentLocationLatitude=as.numeric(NA),
    IncidentLocationLongitude=as.numeric(NA),
    IncidentLocationStreetAddress=generateRandomStreetAddress(incidentCount),
    IncidentLocationTown=sample(towns, incidentCount, TRUE, 1/(2^seq(towns))),
    IncidentDate=sample(dates, incidentCount, TRUE),
    IncidentTime=getRandomTimes(incidentCount),
    ReportingSystem=sample(c('System A', 'System B'), incidentCount, TRUE, c(.6, .4))
  )

  incidentTypeIncidentIDs <- rep(Incident$IncidentID, sample(1:3, incidentCount, TRUE, c(.6, .3, .1)))

  IncidentType <- tibble(
    IncidentID=incidentTypeIncidentIDs,
    IncidentDescriptionText=sample(
      c('Domestic Disturbance', 'Traffic Stop', 'Assault', 'Burglary', 'Shoplifting', 'Drug Possession'),
      length(incidentTypeIncidentIDs),
      TRUE,
      c(.2, .4, .1, .15, .2, .2)
    )
  ) %>%
    distinct() %>%
    mutate(IncidentTypeID=row_number())

  Arrest <- Incident %>%
    sample_frac(.2) %>%
    select(IncidentID, ArrestDate=IncidentDate, ArrestTime=IncidentTime, ReportingSystem, ArrestingAgencyID=ReportingAgencyID)

  arrestIncidentIDs <- rep(Arrest$IncidentID, sample(1:5, nrow(Arrest), TRUE, c(.5, .3, .15, .04, .01)))

  Arrest <- inner_join(Arrest, tibble(IncidentID=arrestIncidentIDs), by='IncidentID') %>%
    mutate(DifferentArrestingAgency=rbinom(n(), 1, .05),
           ArrestingAgencyID=ifelse(DifferentArrestingAgency, sample(dfs$Agency$AgencyID, n(), TRUE), ArrestingAgencyID)) %>%
    inner_join(dfs$Agency, by=c('ArrestingAgencyID'='AgencyID')) %>%
    rename(ArrestingAgencyName=AgencyName) %>%
    mutate(ArrestID=row_number(), PersonID=row_number()) %>%
    select(ArrestID, PersonID, IncidentID, ArrestDate, ArrestTime, ArrestingAgencyName, ReportingSystem)

  Person <- Arrest %>%
    select(ArrestDate) %>%
    mutate(
      PersonSexID=sample(dfs$PersonSex$PersonSexID, n(), TRUE, c(.5, .475, .025)),
      PersonRaceID=sample(dfs$PersonRace$PersonRaceID, n(), TRUE, 1/(2^seq(nrow(dfs$PersonRace)))),
      PersonAge=generateRandomArresteeAges(size=n()),
      PersonUniqueIdentifier=paste0('P', str_pad(row_number(), 7, 'left', '0'))
    ) %>%
    sample_frac(.60)

  recidivistCount <- nrow(Arrest) - nrow(Person)
  Recidivists <- Person %>% sample_n(recidivistCount) %>% sample_frac(.60)
  Person <- bind_rows(Person, Recidivists)
  recidivistCount <- nrow(Arrest) - nrow(Person)
  Recidivists <- Recidivists %>% sample_n(recidivistCount) %>% sample_frac(.60)
  Person <- bind_rows(Person, Recidivists)
  recidivistCount <- nrow(Arrest) - nrow(Person)
  Recidivists <- Recidivists %>% sample_n(recidivistCount) %>% sample_frac(.60)
  Person <- bind_rows(Person, Recidivists)
  recidivistCount <- nrow(Arrest) - nrow(Person)
  Recidivists <- Recidivists %>% sample_n(recidivistCount)

  Person <- bind_rows(Person, Recidivists) %>%
    mutate(PersonID=row_number(),
           PersonBirthDate=ArrestDate - (PersonAge*365 - as.integer(round(runif(n=n(), min=1, max=365))))) %>%
    select(-PersonAge, -ArrestDate)

  PersonAges <- Person %>%
    group_by(PersonUniqueIdentifier) %>%
    summarize(PersonBirthDate=min(PersonBirthDate)) %>%
    mutate(NullBirthDate=rbinom(n(), 1, .015),
           PersonBirthDate=ifelse(NullBirthDate, as_date(NA), PersonBirthDate)) %>%
    select(-NullBirthDate) %>%
    mutate(PersonBirthDate=as_date(PersonBirthDate))

  Person <- Person %>%
    select(-PersonBirthDate) %>%
    inner_join(PersonAges, by='PersonUniqueIdentifier') %>%
    select(PersonID, PersonSexID, PersonRaceID, PersonBirthDate, PersonUniqueIdentifier)

  chargeArrestIDs <- rep(Arrest$ArrestID, sample(1:8, nrow(Arrest), TRUE, 1/(2^seq(8))))

  Charge <- tibble(
    ChargeID=seq(length(chargeArrestIDs)),
    ArrestID=chargeArrestIDs,
    OffenseDescriptionText=sample(getOffenseDescriptions(), length(chargeArrestIDs), TRUE)
  ) %>%
    mutate(OffenseDescriptionText1=OffenseDescriptionText)

  TrafficStop <- IncidentType %>%
    filter(IncidentDescriptionText=='Traffic Stop') %>%
    select(IncidentID) %>%
    mutate(
      TrafficStopReasonDescription=sample(
        getTrafficStopReasons(),
        n(), TRUE, 1/(2^seq(length(getTrafficStopReasons())))
      ),
      TrafficStopSearchTypeDescription=sample(
        getTrafficStopSearchTypes(),
        n(), TRUE
      ),
      TrafficStopContrabandStatus=sample(
        getTrafficStopContrabandStatuses(),
          n(), TRUE
      )
    )

  TrafficStop <- bind_rows(
    TrafficStop %>% inner_join(Arrest %>% select(IncidentID), by='IncidentID') %>%
      mutate(TrafficStopOutcomeDescription=TRAFFIC_STOP_ARREST_OUTCOME_CODE),
    TrafficStop %>% anti_join(Arrest %>% select(IncidentID), by='IncidentID') %>%
      mutate(TrafficStopOutcomeDescription=sample(setdiff(getTrafficStopOutcomes(), TRAFFIC_STOP_ARREST_OUTCOME_CODE), n(), TRUE))
  ) %>% arrange(IncidentID) %>%
    mutate(TrafficStopID=row_number()) %>%
    mutate(DriverAge=as.integer(generateRandomArresteeAges(approximateMeanAge=32, size=n(), minimumAge=16, maximumAge=90)),
           DriverSex=sample(c('M','F','U'), n(), TRUE, c(.5, .5, .01)),
           DriverRace=sample(c('W','B','A','H','U'), n(), TRUE, c(.7,.2,.1,.2,.01)),
           DriverResidenceTown=sample(towns, n(), TRUE),
           DriverResidenceState='DS',
           VehicleMake='Vehicle Make',
           VehicleModel='Vehicle Model',
           VehicleYear=2010,
           VehicleRegistrationState='DS')

  Disposition <- Arrest %>%
    inner_join(Person, by='PersonID') %>%
    inner_join(Incident, by='IncidentID') %>%
    inner_join(dfs$Agency, by=c('ArrestingAgencyName'='AgencyName')) %>%
    inner_join(Charge, by='ArrestID') %>%
    select(
      PersonUniqueIdentifier,
      IncidentCaseNumber,
      ArrestDate,
      ArrestingAgencyORI=AgencyORI,
      InitialChargeCode=OffenseDescriptionText) %>%
    mutate(SampleWeight=as.numeric(((maxIncidentDate-ArrestDate)/ddays(1))/((maxIncidentDate-minIncidentDate)/ddays(1)))) %>%
    sample_frac(.8, weight=SampleWeight) %>%
    select(-SampleWeight) %>%
    mutate(DispositionID=row_number())

  DispositionPerson <- inner_join(Person %>% select(-PersonID) %>% group_by(PersonUniqueIdentifier) %>% filter(row_number()==1) %>% ungroup(),
                                  Disposition %>% select(PersonUniqueIdentifier, DispositionID),
                                  by='PersonUniqueIdentifier') %>%
    ungroup() %>%
    mutate(PersonID=row_number() + max(Person$PersonID))

  dispoTypeProbs <- c(.2, .3, .3, .3, .01, .005)

  if (length(dfs$DispositionType$DispositionTypeID) != length(dispoTypeProbs)) {
    stop('Looks like the number of disposition types has changed; need to update the code to specify the probability of each in the generated data.')
  }

  convictedDispositionTypeID <- dfs$DispositionType$DispositionTypeID[dfs$DispositionType$DispositionDescription=='Convicted']

  if (length(convictedDispositionTypeID) != 1) {
    stop('Looks like there is not exactly one disposition type with a description of "Convicted".  Fix this and rerun.')
  }

  Disposition <- Disposition %>%
    select(-PersonUniqueIdentifier) %>%
    inner_join(DispositionPerson %>% select(DispositionID, PersonID), by='DispositionID') %>%
    mutate(
      DispositionTypeID=sample(dfs$DispositionType$DispositionTypeID, n(), TRUE, dispoTypeProbs),
      DispositionDate=ArrestDate + map_int(ArrestDate, function(d) {
        as.integer(sample(seq(1, as.integer((maxIncidentDate-d)/ddays(1)), by=1), 1, TRUE))
      }),
      SentenceTermDays=ifelse(DispositionTypeID==convictedDispositionTypeID & rbinom(n(), 1, .2),
                              sample(1:365, n(), TRUE),
                              as.integer(NA)),
      SentenceFineAmount=ifelse(DispositionTypeID==convictedDispositionTypeID & is.na(SentenceTermDays),
                                sample(c(100,500,1000,2000,5000,10000,25000), n(), TRUE),
                                as.integer(NA)),
      InitialChargeCode1=InitialChargeCode,
      FinalChargeCode=ifelse(rbinom(n(), 1, .15), sample(getOffenseDescriptions(), n(), TRUE), InitialChargeCode),
      FinalChargeCode1=FinalChargeCode,
      RecordType='N',
      IsProbationViolation=ifelse(FinalChargeCode=='Violation of Probation Conditions', 'Y', 'N'),
      IsProbationViolationOnOldCharge=ifelse(IsProbationViolation=='Y' & rbinom(n(), 1, .1), 'Y', 'N'),
      RecidivismEligibilityDate=DispositionDate + ddays(ifelse(is.na(SentenceTermDays), 0, SentenceTermDays)),
      DocketChargeNumber=FinalChargeCode
    ) %>%
    select(-ArrestDate)

  Person <- bind_rows(Person, DispositionPerson %>% select(-DispositionID))

  dfs <- c(dfs, list(
    'Incident'=Incident,
    'IncidentType'=IncidentType,
    'Person'=Person,
    'Arrest'=Arrest,
    'TrafficStop'=TrafficStop,
    'Charge'=Charge,
    'Disposition'=Disposition
    )
  )

  persistData(dfs, conn)

  dfs

}

persistData <- function(dfs, conn) {
  truncateDatabase(names(dfs), conn)
  writeData(dfs, conn)
}

#' @importFrom DBI dbWriteTable
writeData <- function(tableList, conn) {
  for (table in names(tableList)) {
    tt <- tableList[[table]]
    writeLines(paste0('Writing ', nrow(tt), ' rows to table ', table))
    dbWriteTable(conn, table, tt, row.names=FALSE, append=TRUE)
  }
}

#' @importFrom DBI dbClearResult dbSendQuery
truncateDatabase <- function(tableNames, conn) {

  dbClearResult(dbSendQuery(conn, 'set foreign_key_checks=0'))

  for (table in tableNames) {
    writeLines(paste0('Truncating table ', table))
    dbClearResult(dbSendQuery(conn, paste0('truncate ', table)))
  }

}

getTrafficStopOutcomes <- function() {
  c(
    'A',  # Arrest
    'AW', # Arrest for Warrant
    'N',  # No Action
    'T',  # Ticket/VCVC
    'V',  # Verbal Warning
    'W'   # Written Warning
  )
}
TRAFFIC_STOP_ARREST_OUTCOME_CODE <- getTrafficStopOutcomes()[1]

getTrafficStopContrabandStatuses <- function() {
  c(
    'C',  # Contraband
    'NC', # No Contraband
    'X'   # No Search/Not Applicable
  )
}

getTrafficStopSearchTypes <- function() {
  c(
    'NS',  # No Search
    'SPC', # Probable Cause
    'SRS', # Reasonable Suspicion
    'SW'   # Warrant
  )
}

getTrafficStopReasons <- function() {
  # from Vermont...
  c(
    'M', # Moving Violation
    'D', # Suspicion of DWI
    'I', # Investigatory
    'V', # Vehicle Equipment
    'E'  # Externally Generated
  )
}

getOffenseDescriptions <- function() {
  c(
    'Simple Assault',
    'Aggravated Assault',
    'Burglary',
    'Motor Vehicle Theft',
    'Larceny',
    'Possession of Marijuana',
    'Possession of Heroin',
    'Possession of Methamphetamine',
    'Possession of Drug Paraphernalia',
    'Robbery',
    'Sexual Assault',
    'Forgery',
    'Arson',
    'Stalking',
    'Disorderly Conduct',
    'Resisting Arrest',
    'Violation of Probation Conditions',
    'Vehicular Assault',
    'Reckless Endangerment (MV)',
    'Driving Under Influence'
  )
}

#' @importFrom stringr str_pad
getRandomTimes <- function(count) {
  paste0(
    str_pad(sample(0:23, count, TRUE, c(.2, .2, rep(.1, 6), .2, .2, .3, .4, .5, .5, .5, .5, .6, .6, .4, .5, .7, .6, .4, .3)), 2, 'left', '0'),
    ':',
    str_pad(sample(0:59, count, TRUE), 2, 'left', '0'),
    ':',
    str_pad(sample(0:59, count, TRUE), 2, 'left', '0')
  )
}

getTowns <- function(agencyCodeTable) {
  agencyCodeTable$AgencyName[which(grepl(x=agencyCodeTable$AgencyName, pattern=' PD'))] %>%
    gsub(x=., pattern='(.+) PD', replacement='\\1')
}

generateRandomStreetAddress <- function(count) {
  paste0(
    sample(1:9999, count, TRUE),
    ' ',
    sample(
      c('First', 'Second', 'Main', 'Elm', 'Park', 'Pine', 'Hill'),
      count, TRUE
    ),
    ' ',
    sample(c('St.', 'Ave.', 'Road'), count, TRUE, c(.6, .2, .2))
  )
}

#' @importFrom truncdist rtrunc
generateRandomArresteeAges <- function(approximateMeanAge=27, size, minimumAge=12, maximumAge=85) {

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
