library(tidyverse)
library(lubridate)

arrests <- read_csv('/opt/data/maine/chri/jan-2018-extracts/analytics_arrests-20180103100915.csv')
charges <- read.csv('/opt/data/maine/chri/jan-2018-extracts/analytics_charges-20180103100915.csv', stringsAsFactors=FALSE) %>% as_tibble()
dispositions <- read_csv('/opt/data/maine/chri/jan-2018-extracts/analytics_dispositions-20180103100915.csv')

arrests <- arrests %>%
  mutate(ARREST_DATE=dmy(ARREST_DATE)) %>%
  mutate(ARREST_DATE=case_when(
    year(ARREST_DATE) > 2017 ~ ARREST_DATE - dyears(100),
    TRUE ~ ARREST_DATE
  ))

arrests %>% mutate(yy=year(ARREST_DATE),
                   ArrestYearRange=paste0(yy - (yy %% 5), '-', 4 + yy - (yy %% 5))) %>%
  group_by(ArrestYearRange) %>% summarize(Arrests=n(), Years=n_distinct(yy), Arrestees=n_distinct(ARRESTEE_ID)) %>%
  mutate(ArrestsPerYear=Arrests/Years) %>% select(ArrestYearRange, YearsOfData=Years, Arrests, ArrestsPerYear, Arrestees) %>% print(n=20)

nrow(arrests)

arrests %>% select(ARRESTEE_ID) %>% distinct()

arrests %>% group_by(ARRESTEE_ID) %>% filter(n() > 100) %>% select(ARRESTEE_ID) %>% distinct() %>% nrow()

chargesWithDispo <- inner_join(charges, dispositions, by='CHARGE_ID') %>%
  select(ARREST_ID) %>% distinct()

arrests %>% anti_join(chargesWithDispo, by='ARREST_ID') %>% nrow()
