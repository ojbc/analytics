library(RMySQL)

source("LoadCodeTables.R")
source("FileLoadingFunctions.R")
source("IncidentFunctions.R")
source("CommonFunctions.R")

options(stringsAsFactors = FALSE)

conn <- dbConnect(MySQL(), host="localhost", dbname="nibrs_analytics", username="root")

tryCatch({
  
  #loadCodeTables("NIBRSCodeTables.xlsx", conn)
  rawIncidents <- loadIncidentFile(conn, "/opt/data/NIBRS/2013/ICPSR_36121/DS0001/Ohio.txt", maxRecords=300)
  rawIncidents <- addIncidentID(rawIncidents)
  adminSegment <- writeIncidents(conn, rawIncidents)
  
}, finally = {
  
  dbDisconnect(conn)
  
})