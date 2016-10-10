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
plotTimelineCaseStatus <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "CaseStatusType", "CaseStatusTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineOriginatingAgency <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "Agency", "ArrestAgencyID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineJurisdiction <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "JurisdictionType", "JurisdictionTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineIllnessDisorder <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "BehavioralHealthEvaluationType", "BehavioralHealthEvaluationTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineBondType <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "BondType", "BondTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineGender <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "PersonSexType", "PersonSexTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineRace <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "PersonRaceType", "PersonRaceTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineAge <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "PersonAgeRangeType", "PersonAgeRangeTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineWorkReleaseStatus <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "WorkReleaseStatusType", "WorkReleaseStatusTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineLanguage <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "LanguageType", "LanguageTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineEducation <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "EducationLevelType", "EducationLevelTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineMilitaryServiceStatus <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "MilitaryServiceStatusType", "MilitaryServiceStatusTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineChargeType <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "ChargeClassType", "ChargeClassTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineChargeDisposition <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "ChargeDispositionType", "ChargeDispositionTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' Generate all operational timeline plots
#'
#' @export
allTimelineOperationalDashboardPlots <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  c(
    plotTimelineOriginatingAgency(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineJurisdiction(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineCaseStatus(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineBondType(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineChargeType(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineChargeDisposition(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineIllnessDisorder(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineWorkReleaseStatus(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure)
  )
}

#' Generate all demographic timeline plots
#'
#' @export
allTimelineDemographicDashboardPlots <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  c(
    plotTimelineGender(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineRace(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineAge(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineLanguage(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineMilitaryServiceStatus(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineEducation(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure)
  )
}
