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

#' Build summary jail-booking dashboard data
#' Notes regarding rollup and dimension tables:
#' The vectors in these next two lists represent all of the dimensions on which we either roll-up or summarize
#' The name of each element in the vector is the name of the dimension table.  The first element in the vector is the "label" column.
#' For the "rollup tables", the second vector element represents the join column (FK) in the Full Booking Data Frame "fact table"
#' For the dimension tables, the second column is the "distinct count" column...the unit of observation.  the third column, which
#' is optional, is the fact table join column.  (If this isn't included, the fact table FK is assumed to be the dimension table name + "ID")
#' @export
#' @import dplyr
#' @param adsConnection the connection to the dimensional database
#' @param rollupTables a list of table specifications for the "rollups"
#' @param dimensionTables a list of dimensional values to summarize
#' @param recidivismIndicatorField the fact table field to roll up for recidivism
#' @param allRollupID the value (default is -1) to use as the ID for the "all" levels of each rollup
#' @return A list, with two lists, both names.  One contains the code tables, the other contains the summary data frames for each
#' dimension table.
buildDashboardData <- function(adsConnection, rollupTables, dimensionTables, recidivismIndicatorField, allRollupID=-1) {

  lastLoadTime <- getQuery(adsConnection, "select max(LoadHistoryTimestamp) as ttt from LoadHistory")
  lastLoadTime <- as.POSIXct(lastLoadTime[1,1])

  DimensionalMetadata <- list()
  DimensionalMetadata$LastLoadTime <- lastLoadTime

  df <- getFullBookingDataFrame(adsConnection)

  SummaryDataFrameList <- list()
  CodeTableDataFrameList <- list()

  for (name in names(rollupTables)) {
    CodeTableDataFrameList[[name]] <- buildCodeTable(adsConnection, rollupTables, name)
  }

  for (name in names(dimensionTables)) {
    CodeTableDataFrameList[[name]] <- buildCodeTable(adsConnection, dimensionTables, name)
    SummaryDataFrameList[[name]] <- buildSummaryTable(df, dimensionTables, name, rollupTables, CodeTableDataFrameList,
                                                      recidivismIndicatorField, allRollupID)
  }

  writeLines("Making SMI Summary Data Frame...")

  rollupColumns <- character()
  for (rollupTable in rollupTables) {
    rollupColumns <- c(rollupColumns, rollupTable[2])
  }

  smiSummaryDataFrame <- makeSummaryDf(df, 'JailEpisodeID', 'SevereMentalIllnessIndicator', NULL, rollupTables,
                                       CodeTableDataFrameList, recidivismIndicatorField, allRollupID) %>%
    mutate(SMIPopulation=SevereMentalIllnessIndicator*Population,
           SMILengthOfStay=SevereMentalIllnessIndicator*TotalLengthOfStay,
           SMIRebookingCount=SevereMentalIllnessIndicator*RebookingCount) %>%
    group_by_(.dots=c('Date', rollupColumns)) %>%
    summarize(SMIPopulation=sum(SMIPopulation, na.rm=TRUE),
              SMILengthOfStay=sum(SMILengthOfStay, na.rm=TRUE),
              SMIRebookingCount=sum(SMIRebookingCount, na.rm=TRUE),
              Population=sum(Population, na.rm=TRUE)) %>% ungroup()

  for (ctName in names(CodeTableDataFrameList)) {
    ct <- CodeTableDataFrameList[[ctName]]
    ct[[paste0(ctName, 'ID')]] <- ct$DimensionID
    ct$DimensionID <- NULL
    CodeTableDataFrameList[[ctName]] <- ct %>% distinct()
  }

  ret <- list()
  ret$SummaryDataFrameList <- SummaryDataFrameList
  ret$CodeTableDataFrameList <- CodeTableDataFrameList
  ret$SMISummaryDataFrame <- smiSummaryDataFrame
  ret$DimensionalMetadata <- DimensionalMetadata

  ret

}

#' @import dplyr
getFullBookingDataFrame <- function(adsConnection) {
  df <- getQuery(adsConnection, "select * from FullBookingView")
  ageDf <- getQuery(adsConnection, "select PersonAgeTypeID, PersonAgeRangeTypeID from PersonAgeType")
  df <- df %>% left_join(ageDf, by=setNames(nm='PersonAgeTypeID'))
  df
}

#' @import dplyr
makeSummaryDf <- function(df, distinctIDColumn, dimensionColumn, dimensionTableName, rollupTables, ctList, recidivismIndicatorField, allRollupID) {

  rollupColumns <- character()
  for (rollupTable in rollupTables) {
    rollupColumns <- c(rollupColumns, rollupTable[2])
  }

  # in these next two blocks, we "recode" the values of the original dimension FK columns (including the filter tables)
  # to their "rollup" values (e.g., if we are using a "Category" label rather than the raw, fine-grained ID)

  if (!is.null(dimensionTableName)) {
    ct <- ctList[[dimensionTableName]] %>%
      select_(.dots=c('DimensionID', paste0(dimensionTableName, 'ID')))
    df <- df %>% inner_join(ct, by=setNames(paste0(dimensionTableName, 'ID'), nm=dimensionColumn))
    df[[dimensionColumn]] <- df$DimensionID
    df[['DimensionID']] <- NULL
  }

  for (filterTableName in base::setdiff(names(rollupTables), dimensionTableName)) {
    filterTableInfo <- rollupTables[[filterTableName]]
    joinColumn <- filterTableInfo[2]
    ct <- ctList[[filterTableName]]
    ct <- ct %>%
      select_(.dots=c('DimensionID', paste0(filterTableName, 'ID')))
    df <- df %>% inner_join(ct, by=setNames(paste0(filterTableName, 'ID'), nm=joinColumn))
    df[[joinColumn]] <- df$DimensionID
    df[['DimensionID']] <- NULL
  }

  # end recode

  distinctSubsetDf <- df %>% select_(.dots=c(distinctIDColumn, 'EpisodeStartDate', 'LengthOfStay', recidivismIndicatorField)) %>% distinct() %>%
    mutate(EpisodeStartDate=lubridate::as_date(EpisodeStartDate))

  dis <- rep(distinctSubsetDf[[distinctIDColumn]], distinctSubsetDf$LengthOfStay + 1)
  los <- sequence(distinctSubsetDf$LengthOfStay + 1)
  d <- rep(distinctSubsetDf$EpisodeStartDate, distinctSubsetDf$LengthOfStay + 1) + los - 1
  recid <- rep(distinctSubsetDf[[recidivismIndicatorField]], distinctSubsetDf$LengthOfStay + 1)

  distinctSubsetDf <- data.frame(dis=dis, LengthOfStay=los, Date=d, recid=recid, stringsAsFactors=FALSE) %>%
    mutate(recid=recid=='Y')

  ret <- data.frame()

  for (i in 0:length(rollupColumns)) {
    comboList <- combn(rollupColumns, i, simplify=FALSE)
    for (combo in comboList) {
      sdf <- df %>%
        select_(.dots=c(distinctIDColumn, combo, dimensionColumn)) %>%
        distinct() %>%
        inner_join(distinctSubsetDf, by=setNames("dis", nm=distinctIDColumn)) %>%
        group_by_(.dots=c("Date", combo, dimensionColumn)) %>%
        summarize(Population=n(), RebookingCount=sum(recid), TotalLengthOfStay=sum(LengthOfStay))
      for (rollup in base::setdiff(rollupColumns, combo)) {
        sdf <- sdf %>% mutate_(.dots=setNames(list(as.integer(allRollupID)), rollup))
      }
      ret <- bind_rows(ret, sdf)
    }
  }

  ret

}

#' @import dplyr
buildCodeTable <- function(adsConnection, tableList, name) {
  infoVector <- tableList[[name]]
  physicalName <- name
  if (length(infoVector) == 4) {
    physicalName <- infoVector[4]
  }
  writeLines(paste0("Creating code table data frame for dimension ", name))
  labels <- getQuery(adsConnection, paste0("select distinct ", infoVector[1], " as ", name, "Label from ", physicalName))
  labels$DimensionID <- seq(nrow(labels))
  ct <- getQuery(adsConnection, paste0("select ", physicalName, "ID as ", name, "ID, ", infoVector[1], " as ", name, "Label from ", physicalName))
  ct %>% inner_join(labels, by=setNames(nm=paste0(name, "Label")))
}

buildSummaryTable <- function(df, tableList, name, rollupTables, ctList, recidivismIndicatorField, allRollupID) {
  infoVector <- tableList[[name]]
  writeLines(paste0("Creating summary data frame for dimension ", name))
  dimensionColumn <- paste0(name, "ID")
  if (length(infoVector)==3) {
    dimensionColumn <- infoVector[3]
  }
  makeSummaryDf(df, infoVector[2], dimensionColumn, name, rollupTables, ctList, recidivismIndicatorField, allRollupID)
}

