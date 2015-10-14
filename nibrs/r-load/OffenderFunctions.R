# functions related to Offender data manipulation

library(dplyr)
library(tidyr)

truncateOffender <- function(conn) {
  dbClearResult(dbSendQuery(conn, "truncate OffenderSegment"))
}

writeOffenders <- function(conn, rawIncidentsDataFrame, segmentActionTypeTypeID) {

  OffenderSegment <- cbind(
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V50061:V50063) %>%
      gather(V_OffenderSequenceNumber, OffenderSequenceNumber, V50061:V50063),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V50071:V50073) %>%
      gather(V_OffenderAge, AgeOfOffender, V50071:V50073) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V50081:V50083) %>%
      gather(V_OffenderSex, SexOfPersonTypeID, V50081:V50083) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V50091:V50093) %>%
      gather(V_OffenderRace, RaceOfPersonTypeID, V50091:V50093) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V50111:V50113) %>%
      gather(V_OffenderEthnicity, EthnicityOfPersonTypeID, V50111:V50113) %>%
      select(-AdministrativeSegmentID)
    
  ) %>% filter(OffenderSequenceNumber > 0) %>% select(-starts_with("V_")) %>%
    mutate(AgeOfOffender=ifelse(AgeOfOffender < 0, NA, AgeOfOffender),
           SexOfPersonTypeID=ifelse(SexOfPersonTypeID < 0, 9, SexOfPersonTypeID+1),
           RaceOfPersonTypeID=ifelse(RaceOfPersonTypeID < 0, 9, RaceOfPersonTypeID),
           EthnicityOfPersonTypeID=ifelse(EthnicityOfPersonTypeID < 0, 9, EthnicityOfPersonTypeID),
           SegmentActionTypeTypeID=segmentActionTypeTypeID)
  
  OffenderSegment$OffenderSegmentID = 1:nrow(OffenderSegment)
  
  writeLines(paste0("Writing ", nrow(OffenderSegment), " offender segments to database"))
  
  dbWriteTable(conn=conn, name="OffenderSegment", value=data.table(OffenderSegment), append=TRUE, row.names = FALSE)
    
  OffenderSegment
  
}