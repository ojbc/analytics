#' @import ggplot2
#' @import scales
#' @import lubridate
plotTimeline <- function(dataFrameName, dataFrameFieldName, xAxisLabel,
                 jurisdiction = DefaultJurisdictionLabel,
                 originatingAgency = DefaultOriginatingAgenciesLabel,
                 targetPopulationOnly = FALSE, periodFilterDays = 180, width=7, height=3) {

  callingFunctionStack <- as.list(sys.call(-1))[[1]]
  callingFunctionName <- ifelse(class(callingFunctionStack) == "name", callingFunctionStack,
                                callingFunctionStack[length(callingFunctionStack)])
  svgFileName <- paste0(callingFunctionName, ".svg")

  df <- filterDataFrame(get(dataFrameName), jurisdiction, originatingAgency, targetPopulationOnly) %>%
    filter(DaysAgo <= periodFilterDays)

  if (periodFilterDays > 60) {
    df <- filter(df, DaysAgo %% 7 == 0)
  }

  plot <- ggplot(data=summarize(group_by_(mutate(df, d=Sys.Date() - days(DaysAgo)),
                               "d", dataFrameFieldName), EpisodeCount=sum(EpisodeCount)),
       aes(x=d, y=EpisodeCount, group=get(dataFrameFieldName), colour=get(dataFrameFieldName))) + geom_line(size=1.2) +
  scale_x_date() +
  ggthemes::theme_hc() + ggthemes::scale_colour_hc() +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank(), legend.title = element_blank())

  svgPrint(plot, svgFileName, width, height)

  as.character(callingFunctionName)

}

#' Case Status Plot
#'
#' @export
plotTimelineCaseStatus <- function(jurisdiction = DefaultJurisdictionLabel,
                                 originatingAgency = DefaultOriginatingAgenciesLabel,
                                 targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "CaseStatusTypeDescription", "Case Status",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)

}

#' Pretrial Status Plot
#'
#' @export
plotTimelinePretrialStatus <- function(jurisdiction = DefaultJurisdictionLabel,
                                     originatingAgency = DefaultOriginatingAgenciesLabel,
                                     targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "PretrialStatusTypeDescription", "Pretrial Status",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, height=4)

}

#' Pretrial Status Plot
#'
#' @export
plotTimelineOriginatingAgency <- function(jurisdiction = DefaultJurisdictionLabel,
                                        originatingAgency = DefaultOriginatingAgenciesLabel,
                                        targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "AgencyTypeDescription", "Originating Agency",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, width=8.5, height=5)

}

#' Bed Type Plot
#'
#' @export
plotTimelineBedType <- function(jurisdiction = DefaultJurisdictionLabel,
                              originatingAgency = DefaultOriginatingAgenciesLabel,
                              targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "BedTypeDescription", "Bed Type",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays, height=4)

}

#' Gender Plot
#'
#' @export
plotTimelineGender <- function(jurisdiction = DefaultJurisdictionLabel,
                             originatingAgency = DefaultOriginatingAgenciesLabel,
                             targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "PersonSexDescription", "Gender",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)

}

#' Race Plot
#'
#' @export
plotTimelineRace <- function(jurisdiction = DefaultJurisdictionLabel,
                           originatingAgency = DefaultOriginatingAgenciesLabel,
                           targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "PersonRaceDescription", "Race",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)

}

#' Age Plot
#'
#' @export
plotTimelineAge <- function(jurisdiction = DefaultJurisdictionLabel,
                          originatingAgency = DefaultOriginatingAgenciesLabel,
                          targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "AgeRange", "Age",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)

}

#' Income Plot
#'
#' @export
plotTimelineIncome <- function(jurisdiction = DefaultJurisdictionLabel,
                             originatingAgency = DefaultOriginatingAgenciesLabel,
                             targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "IncomeLevelTypeDescription", "Income",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)

}

#' Housing Status Plot
#'
#' @export
plotTimelineHousingStatus <- function(jurisdiction = DefaultJurisdictionLabel,
                                    originatingAgency = DefaultOriginatingAgenciesLabel,
                                    targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "HousingStatusTypeDescription", "Housing Status",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)

}

#' Language Plot
#'
#' @export
plotTimelineLanguage <- function(jurisdiction = DefaultJurisdictionLabel,
                               originatingAgency = DefaultOriginatingAgenciesLabel,
                               targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "LanguageTypeDescription", "Language",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)

}

#' Education Plot
#'
#' @export
plotTimelineEducation <- function(jurisdiction = DefaultJurisdictionLabel,
                                originatingAgency = DefaultOriginatingAgenciesLabel,
                                targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationCount", "EducationLevelTypeDescription", "Education Level",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)

}

#' Charge Type Plot
#'
#' @export
plotTimelineChargeType <- function(jurisdiction = DefaultJurisdictionLabel,
                                 originatingAgency = DefaultOriginatingAgenciesLabel,
                                 targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationChargeCount", "ChargeTypeDescription", "Charge Type",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)

}

#' Illness/Disorder Plot
#'
#' @export
plotTimelineIllnessDisorder <- function(jurisdiction = DefaultJurisdictionLabel,
                                      originatingAgency = DefaultOriginatingAgenciesLabel,
                                      targetPopulationOnly = FALSE, periodFilterDays = 180) {

  plotTimeline("PopulationBehavioralHealthCount", "BehavioralHealthTypeDescription", "Illness/Disorder",
          jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)

}

#' Generate all operational timeline plots
#'
#' @export
allTimelineOperationalDashboardPlots <- function(jurisdiction = DefaultJurisdictionLabel,
                                               originatingAgency = DefaultOriginatingAgenciesLabel,
                                               targetPopulationOnly = FALSE, periodFilterDays = 180) {
  c(
    plotTimelineBedType(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelineOriginatingAgency(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelinePretrialStatus(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelineCaseStatus(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelineChargeType(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelineIllnessDisorder(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
  )
}

#' Generate all demographic timeline plots
#'
#' @export
allTimelineDemographicDashboardPlots <- function(jurisdiction = DefaultJurisdictionLabel,
                                               originatingAgency = DefaultOriginatingAgenciesLabel,
                                               targetPopulationOnly = FALSE, periodFilterDays = 180) {
  c(
    plotTimelineGender(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelineRace(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelineAge(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelineLanguage(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelineIncome(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelineHousingStatus(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays),
    plotTimelineEducation(jurisdiction, originatingAgency, targetPopulationOnly, periodFilterDays)
  )
}
