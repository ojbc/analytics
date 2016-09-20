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

#' @import dplyr
#' @importFrom DBI dbDisconnect
#' @export
loadSummaryTables <- function(dimensionalConnectionBuilder=defaultDimensionalConnectionBuilder, noneColumnValue=99998, writeToDatabase=TRUE) {

  ret <- list()

  adsConnection = do.call(dimensionalConnectionBuilder, list())

#   ret$DailyPopulation <- loadSummaryTable(adsConnection,
#                                           "DailyPopulation",
#                                           "from JailEpisode, Person where JailEpisode.PersonID=Person.PersonID",
#                                           c('DaysAgo', 'CaseStatusTypeID', 'DomicileStatusTypeID', 'SupervisionUnitTypeID',
#                                             'OccupationTypeID', 'EducationLevelTypeID', 'LanguageTypeID', 'PersonRaceTypeID',
#                                             'PersonSexTypeID', 'PersonAgeTypeID', 'PopulationTypeID'),
#                                           noneColumnValue, writeToDatabase)

#   ret$DailyPopulationCharges <- loadSummaryTable(adsConnection,
#                                                  "DailyPopulationCharges",
#                                                  paste0("from ((JailEpisode inner join Person on JailEpisode.PersonID=Person.PersonID) ",
#                                                         "left join JailEpisodeArrest on JailEpisodeArrest.JailEpisodeID=JailEpisode.JailEpisodeID) ",
#                                                         "left join JailEpisodeCharge on JailEpisodeCharge.JailEpisodeArrestID=JailEpisodeArrest.JailEpisodeArrestID"
#                                                         ),
#                                                  c('DaysAgo', 'ChargeTypeID', 'JurisdictionTypeID', 'JailEpisodeCharge.AgencyID', 'PopulationTypeID'),
#                                                  noneColumnValue, writeToDatabase)

    ret$DailyPopulation <- loadSummaryTable(adsConnection,
                                            "DailyPopulationBehavioralHealth",
                                            "from (JailEpisode inner join Person on JailEpisode.PersonID=Person.PersonID) ",
                                            "left join BehavioralHealthAssessment on BehavioralHealthAssessment.PersonID=Person.PersonID ",
                                            "left join BehavioralHealthEvaluation on BehavioralHealthAssessment.BehavioralHealthAssessmentID=BehavioralHealthEvaluation.BehavioralHealthAssessmentID ",
                                            c('DaysAgo', 'CaseStatusTypeID', 'DomicileStatusTypeID', 'SupervisionUnitTypeID',
                                              'OccupationTypeID', 'EducationLevelTypeID', 'LanguageTypeID', 'PersonRaceTypeID',
                                              'PersonSexTypeID', 'PersonAgeTypeID', 'PopulationTypeID'),
                                            writeToDatabase)


  dbDisconnect(adsConnection)

  ret

}

loadSummaryTable <- function(adsConnection, tableName, fromClause, groupByFields, noneColumnValue, writeToDatabase) {

  writeLines("Loading summary tables")

  # we could consider doing the summarization in the database, as an alternative for performance

  writeLines(paste0("Loading ", tableName))

  querySql <- paste0("select ", paste0(groupByFields, collapse=","), ", SixMonthRebooking, OneYearRebooking, TwoYearRebooking, LengthOfStay ", fromClause)

  jep <- getQuery(adsConnection, querySql)

  writeLines(paste0("...summarizing ", nrow(jep), " records"))

  groupByFields <- gsub(x=groupByFields, pattern=".+\\.(.+)", replacement="\\1")

  sdf <- jep %>%
    mutate_at(.cols=groupByFields, .funs="as.numeric") %>%
    mutate_at(.cols=groupByFields, .funs="recode", .missing=noneColumnValue) %>%
    mutate_at(.cols=groupByFields, .funs="as.integer") %>%
    group_by_(.dots=groupByFields) %>%
    summarize(EpisodeCount=n(), SixMonthRebookingCount=sum(SixMonthRebooking=='Y'), OneYearRebookingCount=sum(OneYearRebooking=='Y'),
              TwoYearRebookingCount=sum(TwoYearRebooking=='Y'), TotalLengthOfStay=sum(LengthOfStay)) %>% arrange(DaysAgo)

  writeLines(paste0("...writing ", nrow(sdf), " ", tableName, " records to database"))

  if (writeToDatabase) {
    executeQuery(adsConnection, paste0("truncate ", tableName))
    writeDataFrameToDatabase(adsConnection, sdf, tableName, viaBulk = TRUE)
  }

  writeLines(paste0("Done loading ", tableName))

  sdf

}
