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

library(readr)
library(tidyverse)
library(lubridate)

# adjust these paths as needed based on where you put the extracts from Maine
ARREST_FILE_PATH <- '/opt/data/maine/chri/CHRI_extract_for_SEARCH_demo_arrests.csv'
CHARGE_FILE_PATH <- '/opt/data/maine/chri/CHRI_extract_for_SEARCH_demo_charges.csv'
DISPOSITION_FILE_PATH <- '/opt/data/maine/chri/CHRI_extract_for_SEARCH_demo_dispositions.csv'
STATUTE_FILE_PATH <- '/opt/data/maine/chri/CHRI_extract_for_SEARCH_demo_statutes.csv'

Arrest <- read_csv(ARREST_FILE_PATH,
                   col_names=c('ArrestID', 'ArrestDate', 'ArrestRecordedDate', 'ArrestingAgencyORI',
                               'ArrestingAgencyName', 'ArresteeID', 'ArresteeAgeAtArrest', 'ArresteeSex', 'ArresteeRace'),
                   skip=1, col_types='ccccccccc') %>%
  mutate(ArrestDate=mdy(ArrestDate),
         ArrestRecordedDateTime=mdy_hms(ArrestRecordedDate),
         ArresteeAgeAtArrest=as.integer(ArresteeAgeAtArrest)) %>%
  select(-ArrestRecordedDate)

# have to use read.csv from utils, because readr isn't able to handle non-UTF8 characters (there are some in STATUTE_CITATION)
Charge <- read.csv(CHARGE_FILE_PATH, stringsAsFactors=FALSE) %>%
  as_tibble() %>%
  mutate(STATUTE_CITATION=gsub(x=STATUTE_CITATION, pattern='\xa7', replacement='')) %>%
  rename(ChargeID=CHARGE_ID, ArrestID=ARREST_ID, ChargeOrigin=CHARGE_ORIGIN, StatuteCode=STATUTE_CODE, StatuteCitation=STATUTE_CITATION,
         StatuteDescription=STATUTE_DESCRIPTION, NCICCode=NCIC_CD)

Disposition <- read_csv(DISPOSITION_FILE_PATH,
                        col_names=c('DispositionID', 'ChargeID', 'DispositionDate', 'DispositionRecordedDate', 'CourtORI',
                                    'CourtName', 'DispositionType'),
                        skip=1,
                        col_types='iiccccc') %>%
  mutate(DispositionDate=mdy(DispositionDate),
         DispositionRecordedDateTime=mdy_hms(DispositionRecordedDate)) %>%
  select(-DispositionRecordedDate)

unknownStatuteChargeID <- Charge %>% filter(StatuteCode==999993) %>% .$ChargeID
unknownStatuteArrestID <- Charge %>% filter(StatuteCode==999993) %>% .$ArrestID %>% unique()

Arrest <- Arrest %>% filter(!(ArrestID %in% unknownStatuteArrestID))
Charge <- Charge %>% filter(!(ChargeID %in% unknownStatuteChargeID))

Statutes <- read_csv(STATUTE_FILE_PATH,
                     col_names=c('StatuteCode', 'ChargeClassCode', 'ChargeClassDescription', 'ChargeSeverity'),
                     skip=1, col_types='iccc') %>%
  inner_join(
    Charge %>% select(starts_with('Statute')) %>% distinct(), by='StatuteCode'
  )

Disposition <- Disposition %>%
  select(ChargeID, DispositionDate, CourtName, DispositionType) %>%
  inner_join(Charge %>% select(ChargeID, ArrestID), by='ChargeID') %>%
  arrange(ArrestID, DispositionDate) %>%
  group_by(ArrestID) %>%
  filter(row_number()==1)

summarizeSeverity <- function(severities) {
  ret <- 'Mixed'
  s <- unique(severities)
  if (length(s)==1) {
    ret <- s
  }
  ret
}

FullCharge <- Charge %>%
  inner_join(Statutes %>% select(StatuteCode, ChargeSeverity), by='StatuteCode')

Charge <- FullCharge %>%
  select(ArrestID, ChargeOrigin, ChargeSeverity) %>%
  group_by(ArrestID, ChargeOrigin) %>%
  summarize(ChargeSeverity=summarizeSeverity(ChargeSeverity)) %>%
  ungroup() %>%
  mutate(ChargeOriginSort=case_when(
    .$ChargeOrigin=='COURT' ~ 1,
    .$ChargeOrigin=='PROSECUTOR' ~ 2,
    TRUE ~ 3
  )) %>%
  arrange(ArrestID, ChargeOriginSort) %>%
  group_by(ArrestID) %>%
  filter(row_number()==1) %>%
  select(ArrestID, ChargeOrigin, ChargeSeverity)

Arrest <- Arrest %>%
  inner_join(Charge, by='ArrestID') %>%
  left_join(Disposition, by='ArrestID') %>%
  mutate(DaysToDisposition=(ArrestDate %--% DispositionDate) / ddays(1))



