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

# list contains data for each measure:  measure formula, geom_bar 'stat' value, and whether to format as a percentage
barMeasureList <- list("Population Count"=c('Population/sum(Population)', 'identity', TRUE),
                       "Length of Stay"=c('TotalLengthOfStay/Population', 'identity', FALSE),
                       'Rebooking Rate'=c('RebookingCount/Population', 'identity', TRUE))

timelineMeasureList <- list("Population Count"=c('Population', FALSE),
                            "Length of Stay"=c('TotalLengthOfStay/Population', FALSE),
                            'Rebooking Rate'=c('RebookingCount/Population', TRUE))

emptyGraphic <- ggplot2::ggplot(data=data.frame(x=1:3, y=1:3), mapping=ggplot2::aes(x=x, y=y)) +
  ggplot2::theme_void() + ggplot2::annotate("text", x=3, y=3, label="No Data Available", size=8, fontface="italic")

#' @import ggplot2
#' @import scales
#' @import dplyr
#' @export
plotBar <- function(measureLabel, dimensionTableName, factTableJoinColumn, filterDimensionList, filterValues,
                    summaryDataFrameList, codeTableDataFrameList, theme, allRollupID,
                    horizontal=TRUE, width=5, height=3, svgMode=TRUE, excludedCodeValues=c('None'), showBasedOnNLabel = FALSE) {

  df <- summaryDataFrameList[[dimensionTableName]]
  ct <- codeTableDataFrameList[[dimensionTableName]]

  dates <- max(df$Date)

  label <- paste0(dimensionTableName, 'Label')

  filteredDf <- filterDataFrame(df, ct, dimensionTableName, factTableJoinColumn, filterDimensionList, filterValues,
                                dates, excludedCodeValues, codeTableDataFrameList, allRollupID)

  ret <- emptyGraphic

  if (nrow(filteredDf)) {

    measure <- barMeasureList[[measureLabel]][1]
    stat <- barMeasureList[[measureLabel]][2]
    percentage <- barMeasureList[[measureLabel]][3]

    plot <- ggplot(data=filteredDf, mapping=aes_string(x=label)) + geom_bar(mapping=aes_string(y=measure), stat = stat, fill="#08305c")

    if (percentage) {
      plot <- plot + scale_y_continuous(labels=percent)
    }

    if (horizontal) {
      plot <- plot + coord_flip()
    }

    plot <- plot +
      theme + theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
                         plot.title=element_text(size=rel(.7), face="italic", hjust=0))

    if (!horizontal) {
      plot <- plot + theme(axis.text.x = element_text(angle = 60, hjust = 1))
    }

    if (showBasedOnNLabel) {
      totalN <- sum(filteredDf$Population)
      plot <- plot + labs(title=paste0("Based on N=", as.character(totalN)))
    }

    ret <- plot

  }

  if (svgMode) {
    callingFunctionStack <- as.list(sys.call(-1))[[1]]
    callingFunctionName <- ifelse(class(callingFunctionStack) == "name", callingFunctionStack,
                                  callingFunctionStack[length(callingFunctionStack)])
    svgFileName <- paste0(callingFunctionName, ".svg")
    svgPrint(ret, svgFileName, width, height)
    ret <- as.character(callingFunctionName)
  }

  ret

}

#' @import ggplot2
#' @import scales
#' @import dplyr
#' @importFrom lubridate days
#' @export
plotTimeline <- function(measureLabel, dimensionTableName, factTableJoinColumn, filterDimensionList,
                         summaryDataFrameList, codeTableDataFrameList, theme, allRollupID, filterValues, periodFilterDays,
                         horizontal=TRUE, width=10.5, height=4.5, svgMode=TRUE, excludedCodeValues=c('None')) {

  df <- summaryDataFrameList[[dimensionTableName]]
  ct <- codeTableDataFrameList[[dimensionTableName]]

  maxDate <- max(df$Date)
  dates <- maxDate - lubridate::days(seq(periodFilterDays))

  if (periodFilterDays > 60) {
    dates <- dates[seq(periodFilterDays) %% 7 == 0]
  }

  label <- paste0(dimensionTableName, 'Label')

  ret <- emptyGraphic

  filteredDf <- filterDataFrame(df, ct, dimensionTableName, factTableJoinColumn, filterDimensionList,
                                filterValues, dates, excludedCodeValues, codeTableDataFrameList, allRollupID)

  if (nrow(filteredDf)) {

    measure <- timelineMeasureList[[measureLabel]][1]
    percentage <- timelineMeasureList[[measureLabel]][2]

    plot <- ggplot(data=filteredDf, mapping=aes_string(x='Date', y=measure, color=label))  + geom_line(size=1.2) +
      scale_x_date() +
      theme +
      theme(axis.title.y = element_blank(), axis.title.x = element_blank(), legend.title = element_blank(), legend.position = "bottom")

    if (percentage) {
      plot <- plot + scale_y_continuous(labels=percent)
    }

    ret <- plot

  }

  if (svgMode) {
    callingFunctionStack <- as.list(sys.call(-1))[[1]]
    callingFunctionName <- ifelse(class(callingFunctionStack) == "name", callingFunctionStack,
                                  callingFunctionStack[length(callingFunctionStack)])
    svgFileName <- paste0(callingFunctionName, ".svg")
    svgPrint(ret, svgFileName, width, height)
    ret <- as.character(callingFunctionName)
  }

  ret

}

#' @import dplyr
#' @importFrom lazyeval interp
#' @export
filterDataFrameForRollups <- function(df, filterDimensionList, filterValues, dimensionTableName, codeTableDataFrameList, allRollupID) {

  jurisdictionTypeCodeTable <- codeTableDataFrameList[['JurisdictionType']]
  agencyCodeTable <- codeTableDataFrameList[['Agency']]
  populationTypeCodeTable <- codeTableDataFrameList[['PopulationType']]

  filteredDf <- df

  for (filterDimensionName in names(filterDimensionList)) {

    filterDimension <- filterDimensionList[[filterDimensionName]]
    ct <- codeTableDataFrameList[[filterDimensionName]]

    queryLabel <- filterValues[filterDimensionName]
    allLabel <- filterDimension$AllLabel
    dimensionTableID <- filterDimension$FactTableFK

    if (queryLabel != allLabel) {
      filterCriteria <- interp(~ v1 == v2, v1=as.name(paste0(filterDimensionName, 'Label')), v2=queryLabel)
      id <- (ct %>% filter_(filterCriteria))[1, 1]
      filterCriteria <- interp(~ v1 == v2, v1=as.name(dimensionTableID), v2=id)
      filteredDf <- filteredDf %>% filter_(filterCriteria)
    } else {
      if (filterDimensionName == dimensionTableName) {
        filterCriteria <- interp(~ v1 != v2, v1=as.name(dimensionTableID), v2=allRollupID)
        filteredDf <- filter_(filteredDf, filterCriteria)
      } else {
        filterCriteria <- interp(~ v1 == v2, v1=as.name(dimensionTableID), v2=allRollupID)
        filteredDf <- filter_(filteredDf, filterCriteria)
      }
    }

  }

  filteredDf

}

#' @importFrom lazyeval interp
#' @import dplyr
#' @export
filterDataFrame <- function(df, ct, dimensionTableName, factTableJoinColumn, filterDimensionList, filterValues,
                            dates, excludedCodeValues, codeTableDataFrameList, allRollupID) {

  label <- paste0(dimensionTableName, 'Label')
  id <- paste0(dimensionTableName, 'ID')

  filteredDf <- df %>%
    filter(Date %in% dates) %>%
    filterDataFrameForRollups(filterDimensionList, filterValues, dimensionTableName, codeTableDataFrameList, allRollupID)

  filteredDf <- filteredDf %>%
    inner_join(ct, by=setNames(id, nm=factTableJoinColumn))

  if (length(excludedCodeValues)) {
    filteredDf <- filteredDf %>%
      filter_(.dots=lazyeval::interp(~ !(col %in% excludedCodeValues), col=as.name(label)))
  }

  filteredDf

}

#' Print a plot to an SVG file
#'
#' @import svglite
#' @export
svgPrint <- function(plot, filename, width, height) {
  w <- width
  h <- height
  svglite(filename, width=w, height=h)
  print(plot)
  dev.off()
  invisible()
}

