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

library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)
library(RMySQL)

# Assumes Arrest dataframe has already been created
source('LoadData.R')

# edit mysql connection info here
connection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_analytics_cch_maine", username="root")

UNKNOWN_VALUE <- as.integer(999999)
NONE_VALUE <- as.integer(999998)
EXTRACT_DATE <- as_date('2017-06-20') # set this date to be when Maine provided extract

buildDateDimensionTable <- function(minDate, maxDate, datesToExclude=as_date(x = integer(0)),
                                    unknownCodeTableValue=UNKNOWN_VALUE, noneCodeTableValue=NONE_VALUE) {
  minDate <- as_date(minDate)
  maxDate <- as_date(maxDate)
  writeLines(paste0("Building date dimension, earliest date=", minDate, ", latestDate=", maxDate))
  DateDf <- tibble(CalendarDate=seq(minDate, maxDate, by="days")) %>%
    mutate(DateTypeID=as.integer(format(CalendarDate, "%Y%m%d")),
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
    bind_rows(tibble(CalendarDate=as_date("1899-01-01"),
                     DateTypeID=unknownCodeTableValue,
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
    bind_rows(tibble(CalendarDate=as_date("1899-01-02"),
                     DateTypeID=noneCodeTableValue,
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

# clean up agencies

Towns <- read_html('https://en.wikipedia.org/wiki/List_of_towns_in_Maine') %>% html_node('table') %>% html_table() %>%
  select(Place=Town, County) %>%
  mutate(Place=case_when(
    .$Place=='Richmont' ~ 'Richmond',
    .$Place=='Dover-Foxcroft' ~ 'Dover Foxcroft',
    .$Place=='Lisbon' ~ 'Lisbon Falls',
    .$Place=='Waldoboro, Maine' ~ 'Waldoboro',
    .$Place=='Carrabessett Valley' ~ 'Carrabasset Valley',
    .$Place=='Swans Island' ~ "Swan's Island",
    TRUE ~ .$Place
  )) %>%
  bind_rows(tibble(Place=c('Indian Township','Maliseet','Penobscot Indian Nation','Pleasant Point'),
                   County=c('Washington', 'Aroostook','Penobscot','Washington')))

Cities <- read_html('https://en.wikipedia.org/wiki/List_of_cities_in_Maine') %>% html_node('table') %>% html_table() %>%
  select(Place=City, County) %>%
  mutate(Place=trimws(gsub(x=Place, pattern='†', replacement='')))

Places <- bind_rows(Towns, Cities)

AgencyType <- Arrest %>% select(ArrestingAgencyName) %>% distinct() %>% arrange(ArrestingAgencyName) %>%
  mutate(ArrestingAgencyNameMod=gsub(x=ArrestingAgencyName, pattern='(.+) POLICE DEPARTMENT', replacement='\\1 PD')) %>%
  mutate(ArrestingAgencyNameMod=gsub(x=ArrestingAgencyNameMod, pattern='BARRACK ', replacement='BARRACKS ')) %>%
  mutate(ArrestingAgencyNameMod=gsub(x=ArrestingAgencyNameMod, pattern='^SP |^STATE POLICE |^ME STATE POLICE |^MAINE ST POLICE |^MAINE STATE POLICE ', replacement='MSP ')) %>%
  mutate(AgencyTypeDescription=case_when(
    grepl(x=.$ArrestingAgencyNameMod, pattern='.+ CO SO.*') & !grepl(x=.$ArrestingAgencyNameMod, pattern='^SOR.+') ~
      paste0(str_to_title(gsub(x=.$ArrestingAgencyNameMod, pattern='(.+) CO SO.*', replacement='\\1')), ' County SO'),
    TRUE ~ as.character(NA)
  )) %>%
  mutate(AgencyTypeDescription=case_when(
    grepl(x=.$ArrestingAgencyNameMod, pattern='.+ PD.*') & !grepl(x=.$ArrestingAgencyNameMod, pattern='^SOR .+|^UNIV.+') ~
      paste0(str_to_title(gsub(x=.$ArrestingAgencyNameMod, pattern='(.+) PD.*', replacement='\\1')), ' PD'),
    TRUE ~ .$AgencyTypeDescription
  )) %>%
  mutate(AgencyTypeDescription=case_when(
    grepl(x=.$AgencyTypeDescription, pattern='^Togus Va PD$') ~ 'Togus VA PD',
    TRUE ~ .$AgencyTypeDescription
  )) %>%
  mutate(AgencyTypeDescription=case_when(
    grepl(x=.$ArrestingAgencyNameMod, pattern='UNIV.+MAINE PD .+') ~
      paste0(str_to_title(gsub(x=.$ArrestingAgencyNameMod, pattern='UNIV.+MAINE PD (.+)', replacement='UNIV OF MAINE - \\1')), ' PD'),
    TRUE ~ .$AgencyTypeDescription
  )) %>%
  mutate(ArrestingAgencyNameMod=case_when(
    grepl(x=.$ArrestingAgencyNameMod, pattern='DEPT MARINE RESOURCES.+\\)(.+)') ~
      paste0('DEPT OF MARINE RESOURCES BUR OF MARINE PATROL ', trimws(gsub(x=.$ArrestingAgencyNameMod, pattern='DEPT MARINE RESOURCES.+\\)(.+)', replacement='\\1'))),
    TRUE ~ .$ArrestingAgencyNameMod
  )) %>%
  mutate(ArrestingAgencyNameMod=case_when(
    grepl(x=.$ArrestingAgencyNameMod, pattern='SOR -.+') ~
      paste0(trimws(gsub(x=.$ArrestingAgencyNameMod, pattern='SOR -(.+)(PD|SO).*', replacement='\\1 \\2 (SOR)'))),
    TRUE ~ .$ArrestingAgencyNameMod
  )) %>%
  mutate(ArrestingAgencyNameMod=gsub(x=ArrestingAgencyNameMod, pattern='DISTRICT ATTORNEYS', replacement="DISTRICT ATTORNEY'S")) %>%
  mutate(ArrestingAgencyNameMod=gsub(x=ArrestingAgencyNameMod, pattern='MAINE DRUG ENFORCEMENT LEWISTON', replacement="MAINE DRUG ENFORCEMENT AGENCY LEWISTON")) %>%
  mutate(ArrestingAgencyNameMod=gsub(x=ArrestingAgencyNameMod, pattern='DO NOT USE -', replacement="")) %>%
  mutate(AgencyTypeDescription=case_when(
    is.na(.$AgencyTypeDescription) ~ str_to_title(.$ArrestingAgencyNameMod),
    TRUE ~ .$AgencyTypeDescription
  )) %>%
  mutate(AgencyTypeDescription=gsub(x=AgencyTypeDescription, pattern='Msp', replacement='MSP')) %>%
  mutate(AgencyTypeDescription=gsub(x=AgencyTypeDescription, pattern='Doc Bur', replacement='DOC Bureau')) %>%
  mutate(AgencyTypeDescription=gsub(x=AgencyTypeDescription, pattern='Dept Of Corrections State House Sta 111', replacement='Dept of Corrections')) %>%
  mutate(AgencyTypeDescription=gsub(x=AgencyTypeDescription, pattern='So Portland', replacement='South Portland')) %>%
  mutate(AgencyTypeDescription=gsub(x=AgencyTypeDescription, pattern='Cumberland Co ', replacement='Cumberland County ')) %>%
  mutate(AgencyTypeDescription=gsub(x=AgencyTypeDescription, pattern='Sor', replacement='SOR')) %>%
  mutate(AgencyTypeDescription=gsub(x=AgencyTypeDescription, pattern=' So ', replacement=' SO ')) %>%
  mutate(AgencyTypeDescription=gsub(x=AgencyTypeDescription, pattern=' Pd ', replacement=' PD ')) %>%
  select(-ArrestingAgencyNameMod) %>%
  mutate(TownPoss=case_when(
    grepl(x=.$AgencyTypeDescription, pattern='^Univ') ~
      trimws(gsub(x=.$AgencyTypeDescription, pattern='Univ Of Maine - (.+) PD.*', replacement='\\1')),
    grepl(x=.$AgencyTypeDescription, pattern='PD') ~
      trimws(gsub(x=.$AgencyTypeDescription, pattern='(.+) PD.*', replacement='\\1'))
  )) %>%
  left_join(Places, by=c('TownPoss'='Place')) %>%
  mutate(County=case_when(
    grepl(x=.$AgencyTypeDescription, pattern='County') ~
      gsub(x=.$AgencyTypeDescription, pattern='(.+) County.*', replacement='\\1'),
    TRUE ~ .$County
  )) %>%
  select(-TownPoss) %>%
  mutate(AgencyTypeCategory=case_when(
    .$AgencyTypeDescription %in% c('Boston And Maine Railroad PD', 'Togus VA PD') ~ 'Other',
    is.na(.$County) & !(grepl(x=.$AgencyTypeDescription, pattern='District Attorney')) ~ 'State Agency',
    TRUE ~ 'Local Agency'
  )) %>%
  mutate(ParentAgencyDescription=case_when(
    grepl(x=.$AgencyTypeDescription, pattern='^MSP') ~ 'Maine State Police',
    grepl(x=.$AgencyTypeDescription, pattern='^Dept Of Marine Resources') ~ 'Dept Of Marine Resources',
    grepl(x=.$AgencyTypeDescription, pattern='^DOC Bureau Of Forestry') ~ 'DOC Bureau Of Forestry',
    grepl(x=.$AgencyTypeDescription, pattern='^Maine Drug Enforcement Agency') ~ 'Maine Drug Enforcement Agency',
    grepl(x=.$AgencyTypeDescription, pattern='^Maine Warden Service') ~ 'Maine Warden Service',
    grepl(x=.$AgencyTypeDescription, pattern='^Motor Vehicle Investigations') ~ 'Motor Vehicle Investigations',
    TRUE ~ .$AgencyTypeDescription
  )) %>%
  mutate(AgencyTypeDetailCategory=case_when(
    .$AgencyTypeCategory=='Other' ~ 'Other',
    grepl(x=.$AgencyTypeDescription, pattern=' PD |( PD$)') ~ 'Municipal Police',
    grepl(x=.$AgencyTypeDescription, pattern=' SO |( SO$)') ~ 'County Sheriff',
    grepl(x=.$AgencyTypeDescription, pattern='District Attorney') ~ 'District Attorney',
    grepl(x=.$AgencyTypeDescription, pattern='Jail') ~ 'County Jail',
    .$AgencyTypeCategory=='State Agency' ~ 'State Agency'
  )) %>%
  arrange(AgencyTypeDescription) %>%
  mutate(AgencyTypeID=row_number())

# clean up courts

Counties <- read_html('https://en.wikipedia.org/wiki/List_of_counties_in_Maine') %>% html_node('.wikitable') %>% html_table() %>%
  select(County, Seat=`Seat[6]`) %>%
  mutate(County=trimws(gsub(x=County, pattern='(.+) County', replacement='\\1')),
         Seat=trimws(case_when(.$Seat=='Dover-Foxcroft' ~ 'Dover Foxcroft', TRUE ~ .$Seat)))

CourtType <- Arrest %>% select(CourtName) %>% distinct() %>% arrange(CourtName) %>% filter(!is.na(CourtName)) %>%
  mutate(CourtTypeCategory=case_when(
    grepl(x=.$CourtName, pattern='DISTRICT COURT') ~ 'District Court',
    grepl(x=.$CourtName, pattern='SUPERIOR COURT') ~ 'Superior Court',
    grepl(x=.$CourtName, pattern='CRIMINAL') ~ 'Criminal Court'
  )) %>%
  mutate(Place=trimws(case_when(
    grepl(x=.$CourtName, pattern='DISTRICT COURT') ~
      str_to_title(gsub(x=.$CourtName, pattern='.+DISTRICT COURT (.+)', replacement='\\1')),
    grepl(x=.$CourtName, pattern='CRIMINAL (COURT)|(DOCKET)') ~
      str_to_title(gsub(x=.$CourtName, pattern='(.+) CRIMINAL.+', replacement='\\1')),
    grepl(x=.$CourtName, pattern='SUPERIOR COURT') ~
      str_to_title(gsub(x=.$CourtName, pattern='(.*)SUPERIOR COURT(.*)', replacement='\\1\\2'))
  ))) %>%
  mutate(CountyPoss=case_when(
    grepl(x=.$CourtName, pattern='CRIMINAL (COURT)|(DOCKET)') ~ .$Place
  )) %>%
  mutate(TownPoss=case_when(
    grepl(x=.$CourtName, pattern='DISTRICT COURT') ~ .$Place
  )) %>%
  left_join(Counties, by=c('Place'='Seat')) %>% rename(CountyPoss1=County) %>%
  left_join(Counties, by=c('Place'='County')) %>% rename(TownPoss1=Seat) %>%
  left_join(Places, by='Place') %>% rename(CountyPoss2=County) %>%
  mutate(Town=case_when(
    !is.na(.$TownPoss) ~ .$TownPoss,
    !is.na(.$TownPoss1) ~ .$TownPoss1,
    TRUE ~ .$Place
  ), County=case_when(
    !is.na(.$CountyPoss) ~ .$CountyPoss,
    !is.na(.$CountyPoss1) ~ .$CountyPoss1,
    !is.na(.$CountyPoss2) ~ .$CountyPoss2,
    TRUE ~ .$Place
  )) %>%
  select(-contains('Poss'), -Place) %>%
  mutate(County=case_when(.$Town=='Springvale' ~ 'York', .$Town=='South Paris' ~ 'Oxford', TRUE ~ .$County)) %>%
  mutate(CourtTypeDescription=case_when(
    .$CourtTypeCategory=='Superior Court' ~ paste0(.$County, ' County Superior Court - ', .$Town),
    .$CourtTypeCategory=='Criminal Court' ~ paste0(.$County, ' County Criminal Court'),
    TRUE ~ paste0(.$Town, ' District Court')
  )) %>%
  mutate(CourtJurisdictionName=case_when(
    grepl(x=.$CourtTypeDescription, pattern='Alfred|Biddeford|Springvale|York') ~ 'Region 1',
    grepl(x=.$CourtTypeDescription, pattern='Cumberland|Portland|Bridgton') ~ 'Region 2',
    grepl(x=.$CourtTypeDescription, pattern='Androscoggin|Lewiston|Franklin|Farmington|Oxford|South Paris|Rumford') ~ 'Region 3',
    grepl(x=.$CourtTypeDescription, pattern='Kennebec|Augusta|Waterville|Somerset|Skowhegan') ~ 'Region 4',
    grepl(x=.$CourtTypeDescription, pattern='Penobscot|Bangor|Lincoln District|Newport|Piscataquis|Dover') ~ 'Region 5',
    grepl(x=.$CourtTypeDescription, pattern='Knox|Rockland|Lincoln|Wiscasset|Sagadahoc|Bath|Waldo|Belfast') ~ 'Region 6',
    grepl(x=.$CourtTypeDescription, pattern='Hancock|Ellsworth|Washington|Machias|Calais') ~ 'Region 7',
    grepl(x=.$CourtTypeDescription, pattern='Aroostook|Caribou|Fort Kent|Houlton|Presque Isle') ~ 'Region 8'
  )) %>%
  select(-Town, -County) %>%
  arrange(CourtTypeDescription) %>%
  mutate(CourtTypeID=row_number())

SexType <- Arrest %>% select(ArresteeSex) %>% distinct() %>%
  mutate(SexTypeDescription=str_to_title(ArresteeSex)) %>%
  arrange(SexTypeDescription) %>%
  mutate(SexTypeID=row_number())

RaceType <- Arrest %>% select(ArresteeRace) %>% distinct() %>%
  mutate(RaceTypeDescription=str_to_title(ArresteeRace)) %>%
  arrange(RaceTypeDescription) %>%
  mutate(RaceTypeID=row_number()) %>%
  mutate(RaceTypeID=case_when(.$RaceTypeDescription=='Unknown' ~ UNKNOWN_VALUE, TRUE ~ .$RaceTypeID))

ChargeSeverityType <- Arrest %>% select(ChargeSeverity) %>% distinct() %>%
  arrange(ChargeSeverity) %>%
  rename(ChargeSeverityTypeDescription=ChargeSeverity) %>%
  mutate(ChargeSeverityTypeID=row_number())

ChargeOriginType <- Arrest %>% select(ChargeOrigin) %>% distinct() %>%
  arrange(ChargeOrigin) %>%
  mutate(ChargeOriginTypeDescription=str_to_title(ChargeOrigin)) %>%
  mutate(ChargeOriginTypeDescription=case_when(.$ChargeOriginTypeDescription=='Lea' ~ 'Law Enforcement Agency', TRUE ~ .$ChargeOriginTypeDescription)) %>%
  mutate(ChargeOriginTypeID=row_number())

MAX_DAYS_DURATION <- 365*3 %>% as.integer()

createYesNoColumnSort <- function(v) {
  case_when(v=='Yes' ~ 1,
            v=='No' ~ 2,
            v=='Unknown' ~ 3,
            TRUE ~ 4) %>% as.integer()
}

DaysDurationType <- tibble(DaysCount=0:(MAX_DAYS_DURATION + 1)) %>%
  mutate(WithinSixMonths=case_when(.$DaysCount <= 180 ~ 'Yes', TRUE ~ 'No'),
         WithinOneYear=case_when(.$DaysCount <= 365 ~ 'Yes', TRUE ~ 'No'),
         WithinTwoYears=case_when(.$DaysCount <= 730 ~ 'Yes', TRUE ~ 'No'),
         DaysRangeLabel=case_when(
           .$DaysCount == 0 ~ '0 Days',
           .$DaysCount <= 30 ~ '1-30 Days',
           .$DaysCount <= 60 ~ '31-60 Days',
           .$DaysCount <= 90 ~ '61-90 Days',
           .$DaysCount <= 180 ~ '91-180 Days',
           .$DaysCount <= 365 ~ '181-365 Days',
           .$DaysCount <= 730 ~ '13-24 Months',
           .$DaysCount <= 1095 ~ '25-36 Months',
           TRUE ~ '>3 years'
         ),
         DaysRangeLabelSort=as.integer(case_when(
           .$DaysCount == 0 ~ 1,
           .$DaysCount <= 30 ~ 2,
           .$DaysCount <= 60 ~ 3,
           .$DaysCount <= 90 ~ 4,
           .$DaysCount <= 180 ~ 5,
           .$DaysCount <= 365 ~ 6,
           .$DaysCount <= 730 ~ 7,
           .$DaysCount <= 1095 ~ 8,
           TRUE ~ 9
         ))) %>%
  mutate(DaysDurationTypeID=row_number()-1) %>%
  bind_rows(tibble(WithinSixMonths='Unknown',
                   WithinOneYear='Unknown',
                   WithinTwoYears='Unknown',
                   DaysRangeLabel='Unknown',
                   DaysDurationTypeID=UNKNOWN_VALUE)) %>%
  bind_rows(tibble(WithinSixMonths='None',
                   WithinOneYear='None',
                   WithinTwoYears='None',
                   DaysRangeLabel='None',
                   DaysDurationTypeID=NONE_VALUE)) %>%
  mutate(WithinSixMonthsSort=createYesNoColumnSort(WithinSixMonths),
         WithinOneYearSort=createYesNoColumnSort(WithinOneYear),
         WithinTwoYearsSort=createYesNoColumnSort(WithinTwoYears))

convertDaysToDurationType <- function(days) {
  case_when(
    is.na(days) ~ NONE_VALUE,
    days > MAX_DAYS_DURATION ~ as.integer(MAX_DAYS_DURATION + 1),
    TRUE ~ as.integer(days)
  )
}

AgeYearsType <- tibble(AgeYearsTypeID=c(0:100, UNKNOWN_VALUE, NONE_VALUE),
                       AgeYearsTypeDescription=c(as.character(0:100), 'Unknown', 'None'),
                       AgeGroupTypeID=c(rep(1:20, each=5), 21, UNKNOWN_VALUE, NONE_VALUE))

AgeGroupType <- tibble(AgeGroupTypeID=c(1:21, UNKNOWN_VALUE, NONE_VALUE),
                       AgeGroupType1Description=c(
                         paste0(seq(0, 95, 5), '-', seq(4, 99, 5), ' years'),
                         '100+ years', 'Unknown', 'None'))

CountyType <- AgencyType %>% select(County) %>% filter(!is.na(County)) %>% distinct() %>% arrange(County) %>%
  rename(CountyName=County) %>%
  mutate(CountyTypeID=row_number())

# Get populations from this url
# https://factfinder.census.gov/bkmk/table/1.0/en/PEP/2016/PEPANNRES/0400000US23.05000

CountyType <- inner_join(CountyType,
                         c('Androscoggin', 107319,
                           'Aroostook', 67959,
                           'Cumberland', 292041,
                           'Franklin', 30001,
                           'Hancock', 54419,
                           'Kennebec', 120569,
                           'Knox', 39744,
                           'Lincoln', 34216,
                           'Oxford', 57217,
                           'Penobscot', 151806,
                           'Piscataquis', 16843,
                           'Sagadahoc', 35273,
                           'Somerset', 50915,
                           'Waldo', 39364,
                           'Washington', 31450,
                           'York', 202343) %>% matrix(ncol=2, byrow=TRUE) %>% as_tibble() %>%
                           select(CountyName=V1, CountyPopulation2016=V2) %>%
                           mutate(CountyPopulation2016=as.integer(CountyPopulation2016)), by='CountyName') %>%
  bind_rows(tibble(CountyTypeID=UNKNOWN_VALUE, CountyName='Unknown', CountyPopulation2016=as.integer(NA))) %>%
  bind_rows(tibble(CountyTypeID=NONE_VALUE, CountyName='None', CountyPopulation2016=as.integer(NA)))

AgencyType <- AgencyType %>%
  left_join(CountyType %>% select(CountyTypeID, CountyName), by=c('County'='CountyName')) %>%
  mutate(CountyTypeID=case_when(is.na(.$CountyTypeID) ~ NONE_VALUE, TRUE ~ .$CountyTypeID))

Arrest2 <- Arrest %>%
  arrange(ArresteeID, ArrestDate) %>%
  group_by(ArresteeID) %>%
  mutate(ArrestIndex=row_number()) %>%
  mutate(PriorArrestIndex=ArrestIndex-1) %>%
  ungroup()

ArrestOut <- Arrest2 %>%
  left_join(Arrest2 %>% select(PriorArrestIndex=ArrestIndex, PriorArrestDate=ArrestDate, ArresteeID), by=c('ArresteeID', 'PriorArrestIndex')) %>%
  inner_join(ChargeSeverityType, by=c('ChargeSeverity'='ChargeSeverityTypeDescription')) %>%
  inner_join(AgencyType, by='ArrestingAgencyName') %>%
  inner_join(SexType, by='ArresteeSex') %>%
  inner_join(RaceType, by='ArresteeRace') %>%
  inner_join(ChargeOriginType, by='ChargeOrigin') %>%
  left_join(CourtType, by='CourtName') %>%
  mutate(AgeOfRecord=(EXTRACT_DATE - ArrestDate) / ddays(1)) %>%
  mutate(ArrestDateTypeID=as.integer(format(ArrestDate, '%Y%m%d')),
         DispositionDateTypeID=case_when(
           !is.na(.$DispositionDate) ~ as.integer(format(.$DispositionDate, '%Y%m%d')),
           TRUE ~ NONE_VALUE
         ),
         AgeYearsTypeID=case_when(is.na(.$ArresteeAgeAtArrest) ~ UNKNOWN_VALUE, TRUE ~ .$ArresteeAgeAtArrest),
         DaysSincePriorArrest=(ArrestDate - PriorArrestDate) / ddays(1),
         HasDisposition=case_when(
           is.na(.$DispositionDate) ~ case_when(
             .$ChargeSeverity %in% c('Felony', 'Mixed') & .$AgeOfRecord < 365*2 ~ 'Unknown',
             .$ChargeSeverity %in% c('Misdemeanor') & .$AgeOfRecord < 365*1 ~ 'Unknown',
             TRUE ~ 'No'
           ),
           TRUE ~ 'Yes'),
         DaysToDispose=(DispositionDate - ArrestDate) / ddays(1),
         CourtTypeID=case_when(is.na(.$CourtTypeID) ~ NONE_VALUE, TRUE ~ .$CourtTypeID),
         DaysSincePriorArrestDurationTypeID=convertDaysToDurationType(DaysSincePriorArrest),
         DaysToDisposeDurationTypeID=convertDaysToDurationType(DaysToDispose),
         ArrestID=row_number()) %>%
  mutate(HasDispositionSort=createYesNoColumnSort(HasDisposition)) %>%
  select(ArrestID, ChargeSeverityTypeID, ArrestDateTypeID,
         ArrestingAgencyTypeID=AgencyTypeID,
         ArresteeAgeYearsTypeID=AgeYearsTypeID,
         ArresteeSexTypeID=SexTypeID,
         ArresteeRaceTypeID=RaceTypeID,
         ChargeOriginTypeID,
         DaysSincePriorArrest,
         DaysSincePriorArrestDurationTypeID,
         HasDisposition,
         HasDispositionSort,
         DaysToDispose,
         DaysToDisposeDurationTypeID,
         CourtTypeID,
         DispositionDateTypeID)

DateType <- buildDateDimensionTable(min(Arrest$ArrestDate), max(Arrest$ArrestDate))

writeTable <- function(name, df, colsToExclude=character(0)) {
  dbClearResult(dbSendStatement(connection, paste0('truncate ', name)))
  dbWriteTable(connection, name, df %>% select(-one_of(colsToExclude)), append=TRUE, row.names=FALSE)
}

dbClearResult(dbSendStatement(connection, paste0('set foreign_key_checks=0')))
writeTable('SexType', SexType, 'ArresteeSex')
writeTable('RaceType', RaceType, 'ArresteeRace')
writeTable('ChargeSeverityType', ChargeSeverityType)
writeTable('ChargeOriginType', ChargeOriginType, 'ChargeOrigin')
writeTable('AgeGroupType', AgeGroupType)
writeTable('AgeYearsType', AgeYearsType)
writeTable('CourtType', CourtType, 'CourtName')
writeTable('CountyType', CountyType)
writeTable('AgencyType', AgencyType, c('ArrestingAgencyName', 'County'))
writeTable('DateType', DateType)
writeTable('DaysDurationType', DaysDurationType)
writeTable('Arrest', ArrestOut)
dbClearResult(dbSendStatement(connection, paste0('set foreign_key_checks=1')))
