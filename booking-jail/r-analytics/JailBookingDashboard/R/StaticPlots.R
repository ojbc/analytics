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
plotStaticCaseStatus <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "CaseStatusType", "CaseStatusTypeID", jurisdiction, originatingAgency, targetPopulationOnly, height=2.9)
}

#' @export
plotStaticOriginatingAgency <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  # , height=1.8, width=4, horizontal = FALSE
  plotBar(measure, "Agency", "ChargeAgencyID", jurisdiction, originatingAgency, targetPopulationOnly, height=2.5)
}

#' @export
plotStaticJurisdiction <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "JurisdictionType", "JurisdictionTypeID", jurisdiction, originatingAgency, targetPopulationOnly, height=2.5)
}

#' @export
plotStaticBondType <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "BondType", "BondTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @export
plotStaticGender <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "PersonSexType", "PersonSexTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @export
plotStaticRace <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "PersonRaceType", "PersonRaceTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @export
plotStaticAge <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "PersonAgeRangeType", "PersonAgeRangeTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @export
plotStaticWorkReleaseStatus <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "WorkReleaseStatusType", "WorkReleaseStatusTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @export
plotStaticLanguage <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "LanguageType", "LanguageTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @export
plotStaticEducation <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "EducationLevelType", "EducationLevelTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @export
plotStaticMilitaryServiceStatus <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "MilitaryServiceStatusType", "MilitaryServiceStatusTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @export
plotStaticChargeType <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "ChargeClassType", "ChargeClassTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @export
plotStaticChargeDisposition <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "ChargeDispositionType", "ChargeDispositionTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @export
plotStaticIllnessDisorder <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  plotBar(measure, "BehavioralHealthEvaluationType", "BehavioralHealthEvaluationTypeID", jurisdiction, originatingAgency, targetPopulationOnly)
}

#' @import dplyr
#' @export
plotStaticJailUtilization <- function(jurisdiction, originatingAgency, targetPopulationOnly, svgMode=TRUE) {

  # for now, we are just doing population, regardless of the selected measure

  df <- JailBookingDashboardData::SMISummaryDataFrame

  maxDate <- max(df$Date)

  df <- df %>%
    filterDataFrameForRollups(jurisdiction, originatingAgency, targetPopulationOnly, "") %>%
    filter(Date == maxDate) %>%
    select(Population) %>% mutate(Capacity=JailCapacity) %>%
    gather() %>% arrange(value)

  ret <- ggplot() + theme_void()

  if (nrow(df)) {

    plot <- ggplot(data=df, aes(x=1, y=value, fill=key)) +
      geom_bar(stat="identity") + coord_flip() + theme_void() + theme(legend.position="none") +
      scale_fill_manual(values=c('#9ecae1', '#deebf7')) + geom_text(aes(label=value, hjust=1.5), position="stack")

    ret <- plot

  }

  if (svgMode) {
    svgPrint(ret, "plotStaticJailUtilization.svg", width=10, height=.35)
    ret <- "plotStaticJailUtilization"
  }

  ret

}

#' @import dplyr
#' @import tidyr
#' @export
plotStaticSMI <- function(jurisdiction, originatingAgency, targetPopulationOnly, svgMode=TRUE) {

  # for now, we are just doing the SMI count.  Later on, we can see about doing a bar with the different measures...

  df <- JailBookingDashboardData::SMISummaryDataFrame

  maxDate <- max(df$Date)

  df <- df %>%
    filterDataFrameForRollups(jurisdiction, originatingAgency, targetPopulationOnly, "") %>%
    filter(Date == maxDate) %>%
    select(Population, SMIPopulation) %>%
    gather() %>% arrange(value)

  ret <- ggplot() + theme_void()

  if (nrow(df)) {

    plot <- ggplot(data=df, aes(x=1, y=value, fill=key)) +
      geom_bar(stat="identity") + coord_flip() + theme_void() + theme(legend.position="none") +
      scale_fill_manual(values=c('#9ecae1', '#deebf7')) + geom_text(aes(label=value, hjust=1.5), position="stack")

    ret <- plot

  }

  if (svgMode) {
    svgPrint(ret, "plotStaticSMI.svg", width=5, height=.35)
    ret <- "plotStaticSMI"
  }

  ret

}

#' @export
allStaticOperationalDashboardPlots <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  c(
    plotStaticOriginatingAgency(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticJurisdiction(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticCaseStatus(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticWorkReleaseStatus(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticChargeType(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticChargeDisposition(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticIllnessDisorder(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticBondType(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticSMI(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticJailUtilization(jurisdiction, originatingAgency, targetPopulationOnly)
  )
}

#' @export
allStaticDemographicDashboardPlots <- function(jurisdiction, originatingAgency, targetPopulationOnly, measure) {
  c(
    plotStaticGender(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticRace(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticAge(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticLanguage(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticMilitaryServiceStatus(jurisdiction, originatingAgency, targetPopulationOnly, measure),
    plotStaticEducation(jurisdiction, originatingAgency, targetPopulationOnly, measure)
  )
}
