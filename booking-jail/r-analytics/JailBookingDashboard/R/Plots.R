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

allJurisdictionsLabel <- 'All Jurisdictions'
allAgenciesLabel <- 'All Agencies'
targetPopulationLabel <- 'Target Population'

# list contains data for each measure:  measure formula, geom_bar 'stat' value, and whether to format as a percentage
barMeasureList <- list("Population Count"=c('Population/sum(Population)', 'identity', TRUE),
                       "Length of Stay"=c('TotalLengthOfStay/Population', 'identity', FALSE),
                       'Rebooking Rate'=c('RebookingCount/Population', 'identity', TRUE))

timelineMeasureList <- list("Population Count"=c('Population', FALSE),
                            "Length of Stay"=c('TotalLengthOfStay/Population', FALSE),
                            'Rebooking Rate'=c('RebookingCount/Population', TRUE))

#' @import ggplot2
#' @import scales
#' @import dplyr
#' @export
plotBar <- function(measureLabel, dimensionTableName, factTableJoinColumn, jurisdictionLabel, agencyLabel, targetPopulationOnly,
                    horizontal=TRUE, width=5, height=3, svgMode=TRUE) {

  df <- JailBookingDashboardData::SummaryDataFrameList[[dimensionTableName]]
  ct <- JailBookingDashboardData::CodeTableDataFrameList[[dimensionTableName]]

  dates <- max(df$Date)

  label <- paste0(dimensionTableName, 'Label')

  filteredDf <- filterDataFrame(df, ct, dimensionTableName, factTableJoinColumn, jurisdictionLabel, agencyLabel, targetPopulationOnly, dates)

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
    getTheme() + theme(axis.title.y = element_blank(), axis.title.x = element_blank())

  if (!horizontal) {
    plot <- plot + theme(axis.text.x = element_text(angle = 60, hjust = 1))
  }

  ret <- plot

  if (svgMode) {
    callingFunctionStack <- as.list(sys.call(-1))[[1]]
    callingFunctionName <- ifelse(class(callingFunctionStack) == "name", callingFunctionStack,
                                  callingFunctionStack[length(callingFunctionStack)])
    svgFileName <- paste0(callingFunctionName, ".svg")
    svgPrint(plot, svgFileName, width, height)
    ret <- as.character(callingFunctionName)
  }

  ret

}

#' @import ggplot2
#' @import scales
#' @import dplyr
#' @importFrom lubridate days
#' @export
plotTimeline <- function(measureLabel, dimensionTableName, factTableJoinColumn, jurisdictionLabel, agencyLabel, targetPopulationOnly, periodFilterDays,
                         horizontal=TRUE, width=10.5, height=4.5, svgMode=TRUE) {

  df <- JailBookingDashboardData::SummaryDataFrameList[[dimensionTableName]]
  ct <- JailBookingDashboardData::CodeTableDataFrameList[[dimensionTableName]]

  maxDate <- max(df$Date)
  dates <- maxDate - lubridate::days(seq(periodFilterDays))

  if (periodFilterDays > 60) {
    dates <- dates[seq(periodFilterDays) %% 7 == 0]
  }

  label <- paste0(dimensionTableName, 'Label')

  filteredDf <- filterDataFrame(df, ct, dimensionTableName, factTableJoinColumn, jurisdictionLabel, agencyLabel, targetPopulationOnly, dates)

  measure <- timelineMeasureList[[measureLabel]][1]
  percentage <- timelineMeasureList[[measureLabel]][2]

  plot <- ggplot(data=filteredDf, mapping=aes_string(x='Date', y=measure, color=label))  + geom_line(size=1.2) +
    scale_x_date() +
    getTheme() +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank(), legend.title = element_blank(), legend.position = "bottom")

  if (percentage) {
    plot <- plot + scale_y_continuous(labels=percent)
  }

  ret <- plot

  if (svgMode) {
    callingFunctionStack <- as.list(sys.call(-1))[[1]]
    callingFunctionName <- ifelse(class(callingFunctionStack) == "name", callingFunctionStack,
                                  callingFunctionStack[length(callingFunctionStack)])
    svgFileName <- paste0(callingFunctionName, ".svg")
    svgPrint(plot, svgFileName, width, height)
    ret <- as.character(callingFunctionName)
  }

  ret

}

#' @import dplyr
filterDataFrameForRollups <- function(df, jurisdictionLabel, agencyLabel, targetPopulationOnly, dimensionTableName) {

  jurisdictionTypeCodeTable <- JailBookingDashboardData::CodeTableDataFrameList[['JurisdictionType']]
  agencyCodeTable <- JailBookingDashboardData::CodeTableDataFrameList[['Agency']]
  populationTypeCodeTable <- JailBookingDashboardData::CodeTableDataFrameList[['PopulationType']]

  filteredDf <- df

  if (jurisdictionLabel != allJurisdictionsLabel) {
    jurisdictionTypeID <- (jurisdictionTypeCodeTable %>% filter(JurisdictionTypeLabel == jurisdictionLabel))[1, 1]
    filteredDf <- filter(filteredDf, JurisdictionTypeID == jurisdictionTypeID)
  } else {
    if ('JurisdictionType' == dimensionTableName) {
      filteredDf <- filter(filteredDf, JurisdictionTypeID != allRollupID)
    } else {
      filteredDf <- filter(filteredDf, JurisdictionTypeID == allRollupID)
    }
  }

  if (agencyLabel != allAgenciesLabel) {
    agencyID <- (agencyCodeTable %>% filter(AgencyLabel == agencyLabel))[1, 1]
    filteredDf <- filter(filteredDf, ChargeAgencyID == agencyID)
  } else {
    if ('Agency' == dimensionTableName) {
      filteredDf <- filter(filteredDf, ChargeAgencyID != allRollupID)
    } else {
      filteredDf <- filter(filteredDf, ChargeAgencyID == allRollupID)
    }
  }

  if (targetPopulationOnly) {
    populationTypeID <- (populationTypeCodeTable %>% filter(PopulationTypeLabel == targetPopulationLabel))[1, 1]
    filteredDf <- filter(filteredDf, PopulationTypeID==populationTypeID)
  } else {
    filteredDf <- filter(filteredDf, PopulationTypeID==allRollupID)
  }

  filteredDf

}

#' @importFrom lazyeval interp
#' @import dplyr
filterDataFrame <- function(df, ct, dimensionTableName, factTableJoinColumn, jurisdictionLabel, agencyLabel, targetPopulationOnly, dates) {

  label <- paste0(dimensionTableName, 'Label')
  id <- paste0(dimensionTableName, 'ID')

  filteredDf <- df %>%
    filter(Date %in% dates) %>%
    filterDataFrameForRollups(jurisdictionLabel, agencyLabel, targetPopulationOnly, dimensionTableName)

  filteredDf <- filteredDf %>%
    inner_join(ct, by=setNames(id, nm=factTableJoinColumn)) %>%
    filter_(.dots=lazyeval::interp(~ col != 'None', col=as.name(label)))

  filteredDf

}

