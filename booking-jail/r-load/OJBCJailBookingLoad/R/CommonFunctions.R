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
# Copyright 2012-2016 Open Justice Broker Consortium

# Common functions for package

#' @import stringr
#' @import RMySQL
#' @importFrom readr write_delim
#' @export
writeDataFrameToDatabase <- function(conn, x, tableName, append = TRUE, viaBulk = FALSE, writeToDatabase=TRUE, forceConnectionType=NULL) {

  # replacing dbWriteTable with our own, because found it to be buggy and inconsistent across platforms...
  #dbWriteTable(conn, tableName, x, row.names=FALSE, append=append)

  executeSQL <- function(sql) {
    if (!writeToDatabase) {
      writeLines(sql)
    } else {
    tryCatch(
      dbClearResult(dbSendStatement(conn, sql)),
      error = function(e) {
        writeLines(paste0("Attempted SQL: ", sql))
        stop(e)
      })
    }
  }

  formatValue <- function(value) {
    ret <- "NULL"
    if (!is.na(value)) {
      if (is.character(value) | is.factor(value)) {
        ret <- paste0("'", value, "'")
      } else if (class(value) == "Date") {
        ret <- paste0("'", format(value, "%Y-%m-%d"), "'")
      } else {
        ret <- as.character(value)
      }
    }
    ret
  }

  if (viaBulk) {
    cc <- class(conn)
    if (!is.null(forceConnectionType)) {
      cc <- forceConnectionType
    }
    f <- NULL
    if (Sys.info()['sysname'] == 'Windows') {
      f <- gsub(x=tempfile(tmpdir='C:/dev', pattern=tableName), pattern='\\\\', replacement='/')
    } else {
      f <- tempfile(tmpdir = "/tmp", pattern = tableName)
    }
    if ('MySQLConnection'==cc) {
      if (nrow(x) > 0) {
        if (!append) {
          executeSQL(paste0("delete from ", tableName))
        }

        write_delim(x=x, path=f, na="\\N", delim="|", col_names=FALSE)
        cn <- colnames(x)
        dateCols <- as.vector(sapply(x, function(col) {inherits(col, "Date")}))
        cne <- cn
        cne[dateCols] <- paste0('@', cne[dateCols])
        setString <- ""
        fieldList <-  paste0("(", paste0(cne, collapse=','), ")")
        if (any(dateCols)) {
          setString <- paste0("set ", paste0(cn[dateCols], "=str_to_date(", cne[dateCols], ", '%Y-%m-%d')", collapse=","))
          fieldList <-  paste0("(", paste0(cne, collapse=','), ")")
        }

        sql <- paste0("load data infile '", f, "' into table ", tableName, " fields terminated by \"|\" ", fieldList, " ", setString)
        executeSQL(sql)
        if (writeToDatabase) {
          file.remove(f)
        }
      }
    } else if ('SQLServerConnection'==cc) {
      if (nrow(x) > 0) {
        if (!append) {
          executeSQL(paste0("delete from ", tableName))
        }

        x <- select_(x, .dots=dbListFields(conn, tableName))

        if (any(sapply(x, is.logical))) {
          x <- mutate_if(x, is.logical, "as.integer")
        }

        # To avoid the scientific notation. Ideally, we should use the options(scipen=999)
        # but in readr version 1.0.0, it does not for write_delim -HW.

        x <- mutate_if(x, function(col) is.numeric(col), function(v) ifelse(is.na(v), NA, trimws(format(v, scientific=FALSE))))

        write_delim(x=x, path=f, na="", delim="|", col_names=FALSE)

        sql <- paste0("BULK INSERT ", tableName, " FROM '" , f, "' WITH ( KEEPIDENTITY, FIELDTERMINATOR ='|', ROWTERMINATOR ='\n' ) ")
        executeSQL(sql)
        if (writeToDatabase) {
          file.remove(f)
        }
      }
    }else {
      stop(paste0("Bulk loading on unsupported database: ", cc))
    }
  }
  else {

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
  }
  invisible()
}

#' @import stringr
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

#' @import stringr
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

#' @import stringr
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
