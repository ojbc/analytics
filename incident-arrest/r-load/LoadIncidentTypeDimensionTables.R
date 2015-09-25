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
# Copyright 2012-2015 Open Justice Broker Consortium

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
  dbSendQuery(conn, "delete from IncidentType2")
  dbWriteTable(conn, "IncidentType2", select(IncidentType, -IncidentCategoryID), append=TRUE, row.names=FALSE)
  IncidentType
}
