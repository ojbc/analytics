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
# Copyright 2012-2015 Open Justice Broker Consortium

library(RMySQL)
library(data.table)
library(dplyr)

getCountyPopulationData <- function(dataFile, year, state) {
  countyData <- read_csv(dataFile)
  countyData <- mutate(filter(countyData, SUMLEV==50), fips=paste0(formatC(STATE, width=2, digits=2, flag="0"), formatC(COUNTY, width=3, digits=3, flag="0")))
  countyData <- arrange(filter(countyData, STATE==state), fips)
  finalYear <- max(countyData$YEAR)
  popData <- gather(select(filter(countyData, AGEGRP!=0, YEAR==finalYear), fips, AGEGRP, YEAR, matches(".AC_.*")), group, PopulationCount, matches(".AC_.*"))
  popData <- filter(popData, !grepl("H.+", group), !grepl("N.+", group))
  popData <- mutate(popData, PersonSexID=ifelse(grepl("_MALE", group), match("Male", sexes), match("Female", sexes)))
  popData <- mutate(popData, PersonRaceID=
                      ifelse(grepl("WAC", group), match("WHITE", races),
                             ifelse(grepl("BAC", group), match("BLACK", races),
                                    ifelse(grepl("IAC", group), match("AMERICAN INDIAN", races),
                                           ifelse(grepl("AAC", group), match("ASIAN", races), match("UNKNOWN", races))))))
  popData <- mutate(popData, PersonAgeRangeID=AGEGRP)
  popData <- select(mutate(rename(popData, CountyID=fips), PopulationID=rownames(popData), Year=year), -YEAR, -AGEGRP, -group)
  data.table(popData)
}

