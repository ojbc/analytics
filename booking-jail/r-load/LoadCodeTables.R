# Script to load code tables into the VT analytical data store
# Typically, only need to do this if building a database from scratch

library(RMySQL)
library(dplyr)
library(xlsx)
library(data.table)

source("CommonFunctions.R")

loadCodeTables <- function(spreadsheetFile, conn) {
  
  codeTableSpreadsheet <- loadWorkbook(spreadsheetFile)
  codeTableCount <- codeTableSpreadsheet$getNumberOfSheets()
  
  for (i in 0:(codeTableCount-1)) {
    codeTableName <- codeTableSpreadsheet$getSheetName(i)
    ct <- read.xlsx2(spreadsheetFile, sheetName = codeTableName, header = TRUE)
    if (any(grepl("X_", colnames(ct)))) {
      ct <- select(ct, -starts_with("X_"))
    }
    ct <- data.table(ct)
    writeDataFrameToDatabase(conn=conn, x=ct, tableName=codeTableName, append=FALSE)
    assign(codeTableName, ct, envir=.GlobalEnv)
  }
}