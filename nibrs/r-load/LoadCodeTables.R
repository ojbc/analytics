library(RMySQL)
library(dplyr)
library(xlsx)
library(data.table)

loadCodeTables <- function(spreadsheetFile, conn) {
  
  codeTableSpreadsheet <- loadWorkbook(spreadsheetFile)
  codeTableCount <- codeTableSpreadsheet$getNumberOfSheets()
  
  TOC <- read.xlsx2(spreadsheetFile, sheetName = "TOC", header = TRUE)
  
  for (i in 0:(codeTableCount-1)) {
    codeTableName <- codeTableSpreadsheet$getSheetName(i)
    if ("TOC" != codeTableName) {
      ct <- read.xlsx2(spreadsheetFile, sheetName = codeTableName, header = TRUE)
      if (any(grepl("X_", colnames(ct)))) {
        ct <- select(ct, -starts_with("X_"))
      }
      ct <- data.table(ct)
      codeTableName <- filter(TOC, Tab==codeTableName)[1,"Table"]
      writeLines(paste0("Loading code table: ", codeTableName))
      dbClearResult(dbSendQuery(conn, paste0("truncate ", codeTableName)))
      dbWriteTable(conn=conn, value=ct, name=codeTableName, append=TRUE, row.names = FALSE)
      assign(codeTableName, ct, envir=.GlobalEnv)
    }
  }
  
}
