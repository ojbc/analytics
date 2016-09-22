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

# Script to load code tables into the analytical data store
# Typically, only need to do this if building a database from scratch

#' @import RMySQL
#' @import openxlsx
#' @export
loadCodeTables <- function(conn, codeTableFileName, writeToDatabase=TRUE) {

  ret = list()

  spreadsheetFile <- system.file("raw", codeTableFileName, package=getPackageName())
  sheetNames <- getSheetNames(spreadsheetFile)

  for (codeTableName in sheetNames) {
    ct <- read.xlsx(spreadsheetFile, sheet = codeTableName, colNames = TRUE)
    if (any(grepl("X_", colnames(ct)))) {
      ct <- dplyr::select(ct, -starts_with("X_"))
    }
    ct <- dplyr::mutate_if(ct, is.numeric, "as.integer")
    if (writeToDatabase) {
      writeDataFrameToDatabase(conn=conn, x=ct, tableName=codeTableName, append=FALSE)
    }
    l <- list(ct)
    names(l) <- c(codeTableName)
    ret <- c(ret, l)
  }

  ret

}

#' Generate a code table spreadsheet template with random number of rows and sequential content, reading the
#' code table names from a specified database.  We assume that code tables follow the OJBC database conventions; specifically:
#' each code table ends in the suffix "Type", the PK column of that table
#' ends in a suffix "ID", and the text column ends in a suffix "Description".  Specify additional tables (that don't
#' end in Type) via the additionalTables parameter.
#' @param conn the database containing the tables
#' @param spreadsheetFile the spreadsheet into which the code table content is written
#' @param additionalTables names of additional tables in the database (other than those ending in Type) to create
#' @import RMySQL
#' @import openxlsx
#' @export
#' @title Generate a code table spreadsheet template with random number of rows and sequential content
generateCodeTableSpreadsheetTemplate <- function(conn, spreadsheetFile="CodeTables.xlsx", additionalTables=character()) {

  codeTableNames <- dbListTables(conn)
  codeTableNames <- c(additionalTables, codeTableNames[endsWith(codeTableNames, "Type")])

  dfs <- list()

  for (ctn in codeTableNames) {

    res <- dbSendQuery(stagingConnection, paste0("select * from ", ctn))
    columnInfo <- dbColumnInfo(res)
    dbClearResult(res)

    rowsToGenerate <- sample(1:12, size=1) # max of 12 rows in code tables

    columnVectors <- list()

    for (c in seq(nrow(columnInfo))) {
      cn <- columnInfo[c, 'name']
      cc <- columnInfo[c, 'Sclass']
      colVals <- list(rep(NA, times=rowsToGenerate)) # default is all null values for a particular column
      if (cn == paste0(ctn, "ID")) {
        colVals <- list(seq(rowsToGenerate))

      } else if (cn == paste0(ctn, "Description")) {
        colVals <- list(paste(ctn, seq(rowsToGenerate)))
      } else {
        if ("integer" == cc) {
          colVals <- list(sample(seq(rowsToGenerate), size=rowsToGenerate))
        } else if ("character" == cc) {
          colVals <- list(sample(LETTERS, size=rowsToGenerate))
        }
      }
      names(colVals) <- cn
      columnVectors <- c(columnVectors, colVals)
    }

    df <- list(data.frame(columnVectors))
    names(df) <- ctn

    dfs <- c(dfs, df)

  }

  write.xlsx(x=dfs, file=spreadsheetFile)

  wb <- loadWorkbook(spreadsheetFile)

  for (ctn in codeTableNames) {
    setColWidths(wb, ctn, widths="auto", cols=1:(ncol(dfs[[ctn]])))
  }

  saveWorkbook(wb, file=spreadsheetFile, overwrite=TRUE)

}
