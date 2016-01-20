# Common functions for Vermont load

library(dplyr)
library(data.table)
library(stringr)
library(RMySQL)

writeDataFrameToDatabase <- function(conn, x, tableName, append = TRUE) {
  # replacing dbWriteTable with our own, because found it to be buggy and inconsistent across platforms...
  #dbWriteTable(conn, tableName, x, row.names=FALSE, append=append)
  executeSQL <- function(sql) {
    #writeLines(sql)
    tryCatch(
      dbClearResult(dbSendQuery(conn, sql)),
      error = function(e) {
        writeLines(paste0("Attempted SQL: ", sql))
        stop(e)
      })
  }
  formatValue <- function(value) {
    ret <- "NULL"
    if (!is.na(value)) {
      if (is.character(value) | is.factor(value)) {
        ret <- paste0("\"", value, "\"")
      } else if (class(value) == "Date") {
        ret <- paste0("\"", format(value, "%Y-%m-%d"), "\"")
      } else {
        ret <- as.character(value)
      }
    }
    ret
  }
  x <- as.data.frame(x)
  if (nrow(x) > 0) {
    if (!append) {
      executeSQL(paste0("delete from ", tableName))
    }
    colNames <- colnames(x)
    colCount <- length(colNames)
    for (r in 1:nrow(x)) {
      sql <- paste0("insert into ", tableName, " (", paste0(colNames, collapse=","), ") values (")
      for (c in 1:colCount) {
        sql <- paste0(sql, formatValue(x[r,c]), ifelse(c == colCount, ")", ","))
      }
      executeSQL(sql)
    }
  }
  invisible()
}

lookupMapping <- function(stagingDataFrame, stagingColumnName, mappingDataFrame) {
  if(any(duplicated(mappingDataFrame$Staging))) stop(paste0("Illegal mapping for ", stagingColumnName, ", duplicated values: ",
                                                            mappingDataFrame$Staging[duplicated(mappingDataFrame$Staging)]))
  byVector <- c("Staging")
  names(byVector) <- stagingColumnName
  df <- left_join(stagingDataFrame, mappingDataFrame, by=byVector)
  unknowns <- filter(df, is.na(ADS))
  if (nrow(unknowns) > 0) {
    print(paste("Unknown lookup values for", stagingColumnName, ":", paste0(unique(unknowns[,stagingColumnName]), collapse=",")))
  }
  as.character(df$ADS)
}

startsWithVectorMatch <- function(x, table) {
  ret <- logical()
  patternTable <- paste0("^", table)
  for (s in x) {
    sMatchesT <- FALSE
    for (t in patternTable) {
      #print(paste0("t=", t, ", s=", s, ", sMatchesT=", sMatchesT, ", grepl(t, s)=", grepl(t, s)))
      sMatchesT <- sMatchesT | grepl(t, s)
      if (sMatchesT) {
        break
      }
    }
    #print(paste0("Does ", s, " start with ", t, "? ->", sMatchesT))
    ret <- c(ret, sMatchesT)
  }
  ret
}

formatRValueAsSqlValue <- function(v, isCharacter = FALSE) {
  ret <- "null"
  if (!is.na(v)){
    if (isCharacter) {
      ret <- paste0("'", v, "'")
    } else {
      ret <- v
    }
  }
  ret
}

