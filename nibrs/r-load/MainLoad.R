library(RMySQL)

source("LoadCodeTables.R")

options(stringsAsFactors = FALSE)

conn <- dbConnect(MySQL(), host="localhost", dbname="nibrs_analytics", username="root")

tryCatch({
  
  loadCodeTables("NIBRSCodeTables.xlsx", conn)
  
}, finally = {
  
  dbDisconnect(conn)
  
})