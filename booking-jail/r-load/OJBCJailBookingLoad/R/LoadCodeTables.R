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
loadCodeTables <- function(conn, codeTableFileName) {

  spreadsheetFile <- system.file("raw", codeTableFileName, package=getPackageName())
  sheetNames <- getSheetNames(spreadsheetFile)

  for (codeTableName in sheetNames) {
    ct <- read.xlsx(spreadsheetFile, sheet = codeTableName, colNames = TRUE)
    if (any(grepl("X_", colnames(ct)))) {
      ct <- dplyr::select(ct, -starts_with("X_"))
    }
    ct <- data.table::data.table(ct)
    writeDataFrameToDatabase(conn=conn, x=ct, tableName=codeTableName, append=FALSE)
    assign(codeTableName, ct, envir=.GlobalEnv)
  }
}
