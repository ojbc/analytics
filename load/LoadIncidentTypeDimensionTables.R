library(RMySQL)
library(data.table)
library(dplyr)

loadIncidentCategoryDimensionTable <- function(conn) {
  incidentCategory <- c("Quality of Life", "Traffic", "Violent Crime", "Unknown")
  IncidentCategory <- data.table(IncidentCategoryID=1:length(incidentCategory), IncidentCategoryDescription=incidentCategory)
  dbSendQuery(conn, "delete from IncidentCategory")
  dbWriteTable(conn, "IncidentCategory", IncidentCategory, append=TRUE, row.names=FALSE)
}

loadIncidentTypeDimensionTable <- function(conn) {
  incidentType <- c("Citizen Dispute", "Noise Complaint", "Animal Problem", "Traffic Accident", "Assault", "Stalking", "Unknown")
  IncidentType <- data.table(IncidentTypeID=1:length(incidentType), IncidentTypeDescription=incidentType, IncidentCategoryID=c(1,1,1,2,3,3,4))
  dbSendQuery(conn, "delete from IncidentType")
  dbWriteTable(conn, "IncidentType", IncidentType, append=TRUE, row.names=FALSE)
  IncidentType
}
