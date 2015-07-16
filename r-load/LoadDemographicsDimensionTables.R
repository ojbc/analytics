library(RMySQL)
library(data.table)
library(dplyr)

loadSexDimensionTable <- function(conn) {
  sexes <- c("Male","Female","Unknown")
  PersonSex <- data.table(PersonSexID=1:length(sexes), PersonSexDescription=sexes)
  dbSendQuery(conn, "delete from PersonSex")
  dbWriteTable(conn, "PersonSex", PersonSex, append=TRUE, row.names=FALSE)
  PersonSex
}

loadRaceDimensionTable <- function(conn) {
  races <- c("BLACK","ASIAN","WHITE","UNKNOWN","AMERICAN INDIAN")
  PersonRace <- data.table(PersonRaceID=1:length(races), PersonRaceDescription=races)
  dbSendQuery(conn, "delete from PersonRace")
  dbWriteTable(conn, "PersonRace", PersonRace, append=TRUE, row.names=FALSE)
  PersonRace
}

loadAgeDimensionTables <- function(conn) {
  
  # note: you can change top age, but it must be divisible by age range step
  topAge <- 85
  ageRangeStep <- 5
  ages <- 0:(topAge+1)
  ageCount <- length(ages)
  ageStrings <- as.character(ages)
  ageStrings[ageCount] <- "Unknown"
  ageStrings[ageCount-1] <- paste(ageStrings[ageCount-1],"+",sep="")
  ageRanges <- rep("", ageCount)
  ageRangesSort <- rep("", ageCount)
  currentBottom <- 0
  for (a in ages)
  {
    currentTop <- currentBottom+(ageRangeStep-1)
    currentTopS <- as.character(currentTop)
    if (currentTop == topAge) currentTopS <- paste(currentTopS,"+",sep="")
    ageRanges[a+1] <- paste(currentBottom, "-", currentTopS, sep="")
    ageRangesSort[a+1] <- paste(formatC(currentBottom, width=3, digits=3, flag="0"), formatC(currentTop, width=3, digits=3, flag="0"), sep = "")
    if ((a+1) %% 5 == 0) currentBottom <- currentBottom + ageRangeStep
  }
  ageRanges[ageCount] <- ageStrings[ageCount]
  ageRanges[ageCount-1] <- ageStrings[ageCount-1]
  ageRangesSort[ageCount] <- ageStrings[ageCount]
  ageRangesSort[ageCount-1] <- ageStrings[ageCount-1]
  
  # This would need refactoring if you ever wanted to have an age range step different than 5.  We chose that because that's what
  # the Census uses
  
  AgeRange5=unique(ageRanges)
  PersonAgeRange <- data.table(PersonAgeRangeID=1:length(AgeRange5), AgeRange5, AgeRange5Sort=AgeRange5)
  dbWriteTable(conn, "PersonAgeRange", PersonAgeRange, append=TRUE, row.names=FALSE)
  
  PersonAge <- data.table(PersonAgeID=(ages+1), AgeInYears=ageStrings, PersonAgeRangeID=match(ageRanges, AgeRange5))
  dbSendQuery(conn, "delete from PersonAge")
  dbSendQuery(conn, "delete from PersonAgeRange")
  dbWriteTable(conn, "PersonAge", PersonAge, append=TRUE, row.names=FALSE)
  
}