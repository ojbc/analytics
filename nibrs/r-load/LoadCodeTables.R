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
      names <- colnames(ct)
    }
  }
  
}
