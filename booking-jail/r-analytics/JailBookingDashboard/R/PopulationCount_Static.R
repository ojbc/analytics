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
plot <- function(dataFrameName, dataFrameFieldName, xAxisLabel,
                 jurisdiction = DefaultJurisdictionLabel,
                 originatingAgency = DefaultOriginatingAgenciesLabel,
                 targetPopulationOnly = FALSE, width=5, height=2, horizontal = TRUE) {

  callingFunctionStack <- as.list(sys.call(-1))[[1]]
  callingFunctionName <- ifelse(class(callingFunctionStack) == "name", callingFunctionStack,
                                callingFunctionStack[length(callingFunctionStack)])
  svgFileName <- paste0(callingFunctionName, ".svg")

  df <- filterDataFrame(get(dataFrameName), jurisdiction, originatingAgency, targetPopulationOnly)

  plot <- ggplot(data=df, mapping=aes(x=get(dataFrameFieldName))) +
    geom_bar(aes(y=..count../sum(..count..), weight=EpisodeCount)) +
    scale_y_continuous(labels=percent)

  if (horizontal) {
    plot <- plot + coord_flip()
  }

  plot <- plot +
    xlab(xAxisLabel) +
    getTheme() + theme(axis.title.y = element_blank(), axis.title.x = element_blank()) + getColorScheme() + getFillScheme()

  if (!horizontal) {
    plot <- plot + theme(axis.text.x = element_text(angle = 60, hjust = 1))
  }

  svgPrint(plot, svgFileName, width, height)

  as.character(callingFunctionName)

}

#' Jail Utilization Plot
#'
#' @import dplyr
#' @export
plotStaticJailUtilization <- function(jurisdiction = DefaultJurisdictionLabel,
                   originatingAgency = DefaultOriginatingAgenciesLabel,
                   targetPopulationOnly = FALSE) {

  df <- CurrentPopulationCount %>%
    filterDataFrame(jurisdiction, originatingAgency, targetPopulationOnly) %>%
    summarize(value=n()) %>%
    mutate(variable="Part")

  util <- sum(df$value)

  df <- bind_rows(df, data.frame(variable=c("Whole"), value=c(JailCapacity-util))) %>%
    mutate(variable=as.factor(variable))

  plot <- ggplot(data=df,
                 aes(x=1, y=value, fill=variable)) +
    geom_bar(stat="identity") +
    geom_text(data=filter(df, variable=="Part"), aes(label=cumsum(value), hjust=1.5), position="stack") +
    geom_text(data=filter(df, variable=="Whole"), aes(label=JailCapacity, y=JailCapacity+75), position="stack") +
    scale_fill_grey(start=.6, end=.8) +
    coord_flip() + theme_void() + theme(legend.position="none")

  svgPrint(plot, "plotStaticJailUtilization.svg", width=10, height=.35)

  "plotStaticJailUtilization"

}

#' SMI Plot
#'
#' @import dplyr
#' @export
plotStaticSMI <- function(jurisdiction = DefaultJurisdictionLabel,
                    originatingAgency = DefaultOriginatingAgenciesLabel,
                    targetPopulationOnly = FALSE) {

  df <- CurrentPopulationBehavioralHealthCount %>%
    filterDataFrame(jurisdiction, originatingAgency, targetPopulationOnly) %>%
    group_by(SevereMentalIllnessIndicator) %>%
    summarize(value=n()) %>%
    mutate(SevereMentalIllnessIndicator=as.factor(SevereMentalIllnessIndicator))

  plot <- ggplot(data=df,
                 aes(x=1, y=value, fill=SevereMentalIllnessIndicator)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=cumsum(value), hjust=1.5), position="stack") +
    scale_fill_grey(start=.6, end=.8) +
    coord_flip() + theme_void() + theme(legend.position="none")

  svgPrint(plot, "plotStaticSMI.svg", width=5, height=.35)

  "plotStaticSMI"

}

#' Case Status Plot
#'
#' @export
plotStaticCaseStatus <- function(jurisdiction = DefaultJurisdictionLabel,
                           originatingAgency = DefaultOriginatingAgenciesLabel,
                           targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "CaseStatusTypeDescription", "Case Status",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Pretrial Status Plot
#'
#' @export
plotStaticPretrialStatus <- function(jurisdiction = DefaultJurisdictionLabel,
                               originatingAgency = DefaultOriginatingAgenciesLabel,
                               targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "PretrialStatusTypeDescription", "Pretrial Status",
       jurisdiction, originatingAgency, targetPopulationOnly, width=5, height=2.5)

}

#' Pretrial Status Plot
#'
#' @export
plotStaticOriginatingAgency <- function(jurisdiction = DefaultJurisdictionLabel,
                               originatingAgency = DefaultOriginatingAgenciesLabel,
                               targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "AgencyTypeDescription", "Originating Agency",
       jurisdiction, originatingAgency, targetPopulationOnly, height=3.5, width=5, horizontal = FALSE)

}

#' Bed Type Plot
#'
#' @export
plotStaticBedType <- function(jurisdiction = DefaultJurisdictionLabel,
                        originatingAgency = DefaultOriginatingAgenciesLabel,
                        targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "BedTypeDescription", "Bed Type",
       jurisdiction, originatingAgency, targetPopulationOnly, height=3.5, width=5, horizontal = FALSE)

}

#' Gender Plot
#'
#' @export
plotStaticGender <- function(jurisdiction = DefaultJurisdictionLabel,
                        originatingAgency = DefaultOriginatingAgenciesLabel,
                        targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "PersonSexDescription", "Gender",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Race Plot
#'
#' @export
plotStaticRace <- function(jurisdiction = DefaultJurisdictionLabel,
                        originatingAgency = DefaultOriginatingAgenciesLabel,
                        targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "PersonRaceDescription", "Race",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Age Plot
#'
#' @export
plotStaticAge <- function(jurisdiction = DefaultJurisdictionLabel,
                    originatingAgency = DefaultOriginatingAgenciesLabel,
                    targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "AgeRange", "Age",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Income Plot
#'
#' @export
plotStaticIncome <- function(jurisdiction = DefaultJurisdictionLabel,
                       originatingAgency = DefaultOriginatingAgenciesLabel,
                       targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "IncomeLevelTypeDescription", "Income",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Housing Status Plot
#'
#' @export
plotStaticHousingStatus <- function(jurisdiction = DefaultJurisdictionLabel,
                              originatingAgency = DefaultOriginatingAgenciesLabel,
                              targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "HousingStatusTypeDescription", "Housing Status",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Language Plot
#'
#' @export
plotStaticLanguage <- function(jurisdiction = DefaultJurisdictionLabel,
                         originatingAgency = DefaultOriginatingAgenciesLabel,
                         targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "LanguageTypeDescription", "Language",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Education Plot
#'
#' @export
plotStaticEducation <- function(jurisdiction = DefaultJurisdictionLabel,
                         originatingAgency = DefaultOriginatingAgenciesLabel,
                         targetPopulationOnly = FALSE) {

  plot("CurrentPopulationCount", "EducationLevelTypeDescription", "Education Level",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Charge Type Plot
#'
#' @export
plotStaticChargeType <- function(jurisdiction = DefaultJurisdictionLabel,
                           originatingAgency = DefaultOriginatingAgenciesLabel,
                           targetPopulationOnly = FALSE) {

  plot("CurrentPopulationChargeCount", "ChargeTypeDescription", "Charge Type",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Illness/Disorder Plot
#'
#' @export
plotStaticIllnessDisorder <- function(jurisdiction = DefaultJurisdictionLabel,
                           originatingAgency = DefaultOriginatingAgenciesLabel,
                           targetPopulationOnly = FALSE) {

  plot("CurrentPopulationBehavioralHealthCount", "BehavioralHealthTypeDescription", "Illness/Disorder",
       jurisdiction, originatingAgency, targetPopulationOnly)

}

#' Generate all static Current Population plots
#'
#' @export
allStaticOperationalDashboardPlots <- function(jurisdiction = DefaultJurisdictionLabel,
                     originatingAgency = DefaultOriginatingAgenciesLabel,
                     targetPopulationOnly = FALSE) {
  c(
    plotStaticBedType(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticOriginatingAgency(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticPretrialStatus(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticCaseStatus(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticChargeType(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticIllnessDisorder(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticSMI(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticJailUtilization(jurisdiction, originatingAgency, targetPopulationOnly)
  )
}

#' Generate all demographic Current Population plots
#'
#' @export
allStaticDemographicDashboardPlots <- function(jurisdiction = DefaultJurisdictionLabel,
                                               originatingAgency = DefaultOriginatingAgenciesLabel,
                                               targetPopulationOnly = FALSE) {
  c(
    plotStaticGender(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticRace(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticAge(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticLanguage(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticIncome(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticHousingStatus(jurisdiction, originatingAgency, targetPopulationOnly),
    plotStaticEducation(jurisdiction, originatingAgency, targetPopulationOnly)
  )
}
