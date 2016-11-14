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

#' @export
#' @import dplyr
getBarChartDataList <- function(measureLabel, dimensionTableName, factTableJoinColumn, filterDimensionList, filterValues,
                                summaryDataFrameList, codeTableDataFrameList, allRollupID, excludedCodeValues=c('None')) {

  df <- summaryDataFrameList[[dimensionTableName]]
  ct <- codeTableDataFrameList[[dimensionTableName]]

  dates <- max(df$Date)

  label <- paste0(dimensionTableName, 'Label')

  filteredDf <- filterDataFrame(df, ct, dimensionTableName, factTableJoinColumn, filterDimensionList, filterValues,
                                dates, excludedCodeValues, codeTableDataFrameList, allRollupID)

  ret <- list()
  ret$type <- 'snapshot'
  ret$stat <- 'count'
  ret$title <- dimensionTableName

  if (nrow(filteredDf)) {

    measure <- barMeasureList[[measureLabel]][1]
    stat <- barMeasureList[[measureLabel]][2]
    percentage <- barMeasureList[[measureLabel]][3]
    label <- paste0(dimensionTableName, 'Label')

    sdf <- filteredDf %>%
      mutate_(.dots=c('m'=measure, 'lab'=label)) %>%
      select(lab, m) %>%
      rename(name=lab, value=m)

    if (percentage) {
      sdf <- sdf %>% mutate(value=value*100)
      ret$stat <- 'percent'
    }

    ret$values <- sdf

  }

  ret

}
