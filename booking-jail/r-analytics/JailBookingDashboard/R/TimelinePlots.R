#' @export
plotTimelineCaseStatus <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "CaseStatusType", "CaseStatusTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' @export
plotTimelineOriginatingAgency <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "Agency", "ChargeAgencyID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, width=8.5, height=5)
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

#' @export
plotTimelineIllnessDisorder <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  plotTimeline(measure, "BehavioralHealthEvaluationType", "BehavioralHealthEvaluationTypeID", jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
}

#' Generate all operational timeline plots
#'
#' @export
allTimelineOperationalDashboardPlots <- function(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure) {
  c(
    plotTimelineOriginatingAgency(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
    plotTimelineCaseStatus(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, measure),
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
