# Functions that produce the plots for the following configuration:
#   View:  Static
#   Metric: Population Count

#' @import dplyr
filterDataFrame <- function(dataFrame, jurisdiction, originatingAgency, targetPopulationOnly) {

  df <- dataFrame

  if (targetPopulationOnly) {
    df <- filter(df, PopulationTypeDescription == TargetPopulationLabel)
  }

  if (jurisdiction != AllCourtsLabel) {
    df <- filter(df, JurisdictionTypeDescription == jurisdiction)
  }

  if (originatingAgency != AllOriginatingAgenciesLabel) {
    df <- filter(df, AgencyTypeDescription == originatingAgency)
  }

  df

}

#' @import ggplot2
#' @import scales
plot <- function(dataFrameName, dataFrameFieldName, xAxisLabel, svgFileName,
                 jurisdiction = DefaultJurisdictionLabel,
                 originatingAgency = DefaultOriginatingAgenciesLabel,
                 targetPopulationOnly = FALSE) {

  df <- filterDataFrame(get(dataFrameName), jurisdiction, originatingAgency, targetPopulationOnly)

  plot <- ggplot(data=df, mapping=aes(x=get(dataFrameFieldName))) +
    geom_bar(aes(y=..count../sum(..count..), weight=EpisodeCount)) +
    scale_y_continuous(labels=percent) +
    coord_flip() +
    ylab("Percent") + xlab(xAxisLabel) +
    getTheme() + getColorScheme() + getFillScheme()

  svgPrint(plot, svgFileName)
  invisible()

}

#' Case Status Plot
#'
#' @export
plotCaseStatus <- function(jurisdiction = DefaultJurisdictionLabel,
                           originatingAgency = DefaultOriginatingAgenciesLabel,
                           targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "CaseStatusTypeDescription", "Case Status", "CaseStatusPlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Pretrial Status Plot
#'
#' @export
plotPretrialStatus <- function(jurisdiction = DefaultJurisdictionLabel,
                               originatingAgency = DefaultOriginatingAgenciesLabel,
                               targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "PretrialStatusTypeDescription", "Pretrial Status", "PretrialStatusPlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Pretrial Status Plot
#'
#' @export
plotOriginatingAgency <- function(jurisdiction = DefaultJurisdictionLabel,
                               originatingAgency = DefaultOriginatingAgenciesLabel,
                               targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "AgencyTypeDescription", "Originating Agency", "OriginatingAgencyPlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Bed Type Plot
#'
#' @export
plotBedType <- function(jurisdiction = DefaultJurisdictionLabel,
                        originatingAgency = DefaultOriginatingAgenciesLabel,
                        targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "BedTypeDescription", "Bed Type", "BedTypePlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Gender Plot
#'
#' @export
plotGender <- function(jurisdiction = DefaultJurisdictionLabel,
                        originatingAgency = DefaultOriginatingAgenciesLabel,
                        targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "PersonSexDescription", "Gender", "GenderPlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Race Plot
#'
#' @export
plotRace <- function(jurisdiction = DefaultJurisdictionLabel,
                        originatingAgency = DefaultOriginatingAgenciesLabel,
                        targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "PersonRaceDescription", "Race", "RacePlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Age Plot
#'
#' @export
plotAge <- function(jurisdiction = DefaultJurisdictionLabel,
                    originatingAgency = DefaultOriginatingAgenciesLabel,
                    targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "AgeRange", "Age", "AgePlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Income Plot
#'
#' @export
plotIncome <- function(jurisdiction = DefaultJurisdictionLabel,
                       originatingAgency = DefaultOriginatingAgenciesLabel,
                       targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "IncomeLevelTypeDescription", "Income", "IncomePlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Housing Status Plot
#'
#' @export
plotHousingStatus <- function(jurisdiction = DefaultJurisdictionLabel,
                              originatingAgency = DefaultOriginatingAgenciesLabel,
                              targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "HousingStatusTypeDescription", "Housing Status", "HousingStatusPlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Language Plot
#'
#' @export
plotLanguage <- function(jurisdiction = DefaultJurisdictionLabel,
                         originatingAgency = DefaultOriginatingAgenciesLabel,
                         targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "LanguageTypeDescription", "Language", "LanguagePlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Education Plot
#'
#' @export
plotEducation <- function(jurisdiction = DefaultJurisdictionLabel,
                         originatingAgency = DefaultOriginatingAgenciesLabel,
                         targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "EducationLevelTypeDescription", "Education Level", "EducationPlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Charge Type Plot
#'
#' @export
plotChargeType <- function(jurisdiction = DefaultJurisdictionLabel,
                           originatingAgency = DefaultOriginatingAgenciesLabel,
                           targetPopulationOnly = FALSE) {

  plot("CurrentPopulationChargeCount", "ChargeTypeDescription", "Charge Type", "ChargeTypePlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Illness/Disorder Plot
#'
#' @export
plotIllnessDisorder <- function(jurisdiction = DefaultJurisdictionLabel,
                           originatingAgency = DefaultOriginatingAgenciesLabel,
                           targetPopulationOnly = FALSE) {

  plot("CurrentPopulationBehavioralHealthCount", "BehavioralHealthTypeDescription", "Illness/Disorder", "IllnessDisorderPlot.svg",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Generate all static Current Population plots
#'
#' @export
allPlots <- function(jurisdiction = DefaultJurisdictionLabel,
                     originatingAgency = DefaultOriginatingAgenciesLabel,
                     targetPopulationOnly = FALSE) {
  plotBedType(jurisdiction, originatingAgency, targetPopulationOnly)
  plotOriginatingAgency(jurisdiction, originatingAgency, targetPopulationOnly)
  plotPretrialStatus(jurisdiction, originatingAgency, targetPopulationOnly)
  plotCaseStatus(jurisdiction, originatingAgency, targetPopulationOnly)
  plotChargeType(jurisdiction, originatingAgency, targetPopulationOnly)
  plotIllnessDisorder(jurisdiction, originatingAgency, targetPopulationOnly)
  plotGender(jurisdiction, originatingAgency, targetPopulationOnly)
  plotRace(jurisdiction, originatingAgency, targetPopulationOnly)
  plotAge(jurisdiction, originatingAgency, targetPopulationOnly)
  plotIncome(jurisdiction, originatingAgency, targetPopulationOnly)
  plotHousingStatus(jurisdiction, originatingAgency, targetPopulationOnly)
  plotLanguage(jurisdiction, originatingAgency, targetPopulationOnly)
  plotEducation(jurisdiction, originatingAgency, targetPopulationOnly)
}
