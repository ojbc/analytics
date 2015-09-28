# Functions for loading ICPSR NIBRS extract files

library(readr)
library(dplyr)
library(stringr)

loadIncidentFile <- function(conn, file, maxRecords = -1) {
  columnSpecs <- getColumnSpecs("IncidentFileFormat.txt")
  read_fwf(file=file, col_positions = fwf_positions(start = columnSpecs$start, end = columnSpecs$end, col_names = columnSpecs$name),
           n_max = maxRecords)
}

loadArresteeFile <- function(conn, file, maxRecords = -1) {
  columnSpecs <- getColumnSpecs("ArresteeFileFormat.txt")
  read_fwf(file=file, col_positions = fwf_positions(start = columnSpecs$start, end = columnSpecs$end, col_names = columnSpecs$name),
           n_max = maxRecords)
}

getColumnSpecs <- function(fileName) {
  
  df <- read_delim(file=fileName, delim=" ", col_names=c("name", "pos", "type"), col_types="ccc")
  p <- "(.+)\\-(.+)"
  start <- gsub(pattern=p, x = df$pos, replacement="\\1")
  end <- gsub(pattern=p, x = df$pos, replacement="\\2")
  
  df$start <- as.integer(start)
  df$end <- as.integer(end)
  
  df
  
}