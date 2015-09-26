# functions related to Incident data manipulation

library(lubridate)
library(dplyr)
library(stringr)

addIncidentID <- function(rawIncidentsDataFrame) {
  IncidentID <- 1:(nrow(rawIncidentsDataFrame))
  rawIncidentsDataFrame$IncidentID <- IncidentID
  rawIncidentsDataFrame
}

writeIncidents <- function(conn, rawIncidentsDataFrame) {
  
  currentMonth <- formatC(month(Sys.Date()), width=2, flag="0")
  currentYear <- year(Sys.Date())
  
  AdministrativeSegment <- rawIncidentsDataFrame %>%
    select(IncidentID, ORI, IncidentNumber=INCNUM, INCDATE, IncidentHour=V1007, ClearedExceptionallyTypeID=V1013, ReportDateIndicator=V1006) %>%
    mutate(IncidentDate=as.Date(ifelse(INCDATE==-5, NA, as.Date(as.character(INCDATE), format="%Y%m%d")), origin="1970-01-01"),
           MonthOfTape=currentMonth, YearOfTape=currentYear, CityIndicator=NA, SegmentActionTypeTypeID=9) %>%
    select(-INCDATE)
  
  # todo: write to db
  
  AdministrativeSegment
  
}