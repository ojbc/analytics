# functions related to Incident data manipulation

library(lubridate)
library(dplyr)
library(stringr)
library(RMySQL)

addAdministrativeSegmentID <- function(rawIncidentsDataFrame) {
  AdministrativeSegmentID <- 1:(nrow(rawIncidentsDataFrame))
  rawIncidentsDataFrame$AdministrativeSegmentID <- AdministrativeSegmentID
  rawIncidentsDataFrame
}

truncateIncidents <- function(conn) {
  dbClearResult(dbSendQuery(conn, "truncate AdministrativeSegment"))
}

writeIncidents <- function(conn, rawIncidentsDataFrame, currentMonth, currentYear, segmentActionTypeTypeID) {
  
  writeLines(paste0("Processing ", nrow(rawIncidentsDataFrame), " raw incidents"))
  
  AdministrativeSegment <- rawIncidentsDataFrame %>%
    select(AdministrativeSegmentID, ORI, IncidentNumber=INCNUM, INCDATE, IncidentHour=V1007, ClearedExceptionallyTypeID=V1013, ReportDateIndicator=V1006) %>%
    mutate(IncidentDate=as.Date(ifelse(INCDATE==-5, NA, as.Date(as.character(INCDATE), format="%Y%m%d")), origin="1970-01-01"),
           MonthOfTape=currentMonth, YearOfTape=currentYear, CityIndicator=NA, SegmentActionTypeTypeID=segmentActionTypeTypeID) %>%
    select(-INCDATE)
  
  writeLines(paste0("Writing ", nrow(AdministrativeSegment), " administrative segments to database"))
  
  dbWriteTable(conn=conn, name="AdministrativeSegment", value=data.table(AdministrativeSegment), append=TRUE, row.names = FALSE)
  
  AdministrativeSegment
  
}