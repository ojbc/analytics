# functions related to Victim data manipulation

library(dplyr)
library(tidyr)

truncateVictim <- function(conn) {
  dbClearResult(dbSendQuery(conn, "truncate VictimSegment"))
  dbClearResult(dbSendQuery(conn, "truncate VictimOffenderAssociation"))
  dbClearResult(dbSendQuery(conn, "truncate VictimOffenseAssociation"))
  dbClearResult(dbSendQuery(conn, "truncate AggravatedAssaultHomicideCircumstances"))
}

writeAggravatedAssaultHomicideCircumstances <- function(conn, victimSegmentDataFrame, rawIncidentsDataFrame) {
  
  
  tempDf <- bind_rows(
    rawIncidentsDataFrame %>%
      select(
        AdministrativeSegmentID, VictimSequenceNumber = V40061, V40231, V40241, V40251
      ) %>%
      filter(VictimSequenceNumber > 0) %>%
      gather(V_Pivot, Pivot, V40231, V40241, V40251) %>%
      mutate(V_Pivot = as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(
        AdministrativeSegmentID, VictimSequenceNumber = V40062, V40232, V40242, V40252
      ) %>%
      filter(VictimSequenceNumber > 0) %>%
      gather(V_Pivot, Pivot, V40232, V40242, V40252) %>%
      mutate(V_Pivot = as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(
        AdministrativeSegmentID, VictimSequenceNumber = V40063, V40233, V40243, V40253
      ) %>%
      filter(VictimSequenceNumber > 0) %>%
      gather(V_Pivot, Pivot, V40233, V40243, V40253) %>%
      mutate(V_Pivot = as.character(V_Pivot)) %>%
      filter(Pivot > 0)
  )
  
  AggravatedAssaultHomicideCircumstances <- left_join(
    tempDf,
    select(
      victimSegmentDataFrame, AdministrativeSegmentID, VictimSequenceNumber, VictimSegmentID
    ),
    by = c("AdministrativeSegmentID", "VictimSequenceNumber")
  ) %>%
    select(VictimSegmentID, AggravatedAssaultHomicideCircumstancesTypeID = Pivot)
  
  missingSegmentIDs <- setdiff(victimSegmentDataFrame$VictimSegmentID, AggravatedAssaultHomicideCircumstances$VictimSegmentID)
  
  AggravatedAssaultHomicideCircumstances <- bind_rows(AggravatedAssaultHomicideCircumstances,
                                         data.frame(VictimSegmentID=missingSegmentIDs,
                                                    AggravatedAssaultHomicideCircumstancesTypeID=rep(x=99, times=length(missingSegmentIDs))))
  
  AggravatedAssaultHomicideCircumstances$AggravatedAssaultHomicideCircumstancesID <- 1:nrow(AggravatedAssaultHomicideCircumstances)
  
  writeLines(paste0("Writing ", nrow(AggravatedAssaultHomicideCircumstances), " AggravatedAssaultHomicideCircumstances association rows to database"))
  
  dbWriteTable(conn=conn, name="AggravatedAssaultHomicideCircumstances", value=data.table(AggravatedAssaultHomicideCircumstances), append=TRUE, row.names = FALSE)
  
  AggravatedAssaultHomicideCircumstances
  
}

writeVictimOffenderAssociation <- function(conn, victimSegmentDataFrame, rawIncidentsDataFrame) {
  
  tempDf <- cbind(
    bind_rows(
      rawIncidentsDataFrame %>%
        select(
          AdministrativeSegmentID, VictimSequenceNumber = V40061, V40311, V40331, V40351, V40371, V40391,
          V40411, V40431, V40451, V40471, V40491
        ) %>%
        filter(VictimSequenceNumber > 0) %>%
        gather(
          V_Pivot, OffenderSegmentID, V40311, V40331, V40351, V40371, V40391,
          V40411, V40431, V40451, V40471, V40491
        ) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(
          AdministrativeSegmentID, VictimSequenceNumber = V40062, V40312, V40332, V40352, V40372, V40392,
          V40412, V40432, V40452, V40472, V40492
        ) %>%
        filter(VictimSequenceNumber > 0) %>%
        gather(
          V_Pivot, OffenderSegmentID, V40312, V40332, V40352, V40372, V40392,
          V40412, V40432, V40452, V40472, V40492
        ) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(
          AdministrativeSegmentID, VictimSequenceNumber = V40063, V40313, V40333, V40353, V40373, V40393,
          V40413, V40433, V40453, V40473, V40493
        ) %>%
        filter(VictimSequenceNumber > 0) %>%
        gather(
          V_Pivot, OffenderSegmentID, V40313, V40333, V40353, V40373, V40393,
          V40413, V40433, V40453, V40473, V40493
        ) %>%
        mutate(V_Pivot = as.character(V_Pivot))
    ) %>% select(-V_Pivot),
    bind_rows(
      rawIncidentsDataFrame %>%
        select(
          AdministrativeSegmentID, VictimSequenceNumber = V40061, V40321, V40341, V40361, V40381, V40401,
          V40421, V40441, V40461, V40481, V40501
        ) %>%
        filter(VictimSequenceNumber > 0) %>%
        gather(
          V_Pivot, VictimOffenderRelationshipTypeID, V40321, V40341, V40361, V40381, V40401,
          V40421, V40441, V40461, V40481, V40501
        ) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(
          AdministrativeSegmentID, VictimSequenceNumber = V40062, V40322, V40342, V40362, V40382, V40402,
          V40422, V40442, V40462, V40482, V40502
        ) %>%
        filter(VictimSequenceNumber > 0) %>%
        gather(
          V_Pivot, VictimOffenderRelationshipTypeID, V40322, V40342, V40362, V40382, V40402,
          V40422, V40442, V40462, V40482, V40502
        ) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(
          AdministrativeSegmentID, VictimSequenceNumber = V40063, V40323, V40343, V40363, V40383, V40403,
          V40423, V40443, V40463, V40483, V40503
        ) %>%
        filter(VictimSequenceNumber > 0) %>%
        gather(
          V_Pivot, VictimOffenderRelationshipTypeID, V40323, V40343, V40363, V40383, V40403,
          V40423, V40443, V40463, V40483, V40503
        ) %>%
        mutate(V_Pivot = as.character(V_Pivot))
    ) %>% select(-V_Pivot, -VictimSequenceNumber, -AdministrativeSegmentID)
  ) %>% filter(OffenderSegmentID > 0)
  
  VictimOffenderAssociation <- left_join(
    tempDf,
    select(
      victimSegmentDataFrame, AdministrativeSegmentID, VictimSequenceNumber, VictimSegmentID
    ),
    by = c("AdministrativeSegmentID", "VictimSequenceNumber")
  ) %>%
    select(VictimSegmentID, OffenderSegmentID, VictimOffenderRelationshipTypeID)
  
  VictimOffenderAssociation$VictimOffenderAssociationID <-
    1:nrow(VictimOffenderAssociation)
  
  writeLines(paste0(
    "Writing ", nrow(VictimOffenderAssociation), " VictimOffenderAssociation association rows to database"
  ))
  
  dbWriteTable(
    conn = conn, name = "VictimOffenderAssociation", value = data.table(VictimOffenderAssociation), append =
      TRUE, row.names = FALSE
  )
  
  VictimOffenderAssociation
}

writeVictimOffenseAssociation <- function(conn, victimSegmentDataFrame, rawIncidentsDataFrame) {
  tempDf <- bind_rows(
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, VictimSequenceNumber=V40061, V40071, V40081, V40091, V40101, V40111,
             V40121, V40131, V40141, V40151, V40161) %>%
      filter(VictimSequenceNumber > 0) %>%
      gather(V_Pivot, Pivot, V40071, V40081, V40091, V40101, V40111,
             V40121, V40131, V40141, V40151, V40161) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, VictimSequenceNumber=V40062, V40072, V40082, V40092, V40102, V40112,
             V40122, V40132, V40142, V40152, V40162) %>%
      filter(VictimSequenceNumber > 0) %>%
      gather(V_Pivot, Pivot, V40072, V40082, V40092, V40102, V40112,
             V40122, V40132, V40142, V40152, V40162) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, VictimSequenceNumber=V40063, V40073, V40083, V40093, V40103, V40113,
             V40123, V40133, V40143, V40153, V40163) %>%
      filter(VictimSequenceNumber > 0) %>%
      gather(V_Pivot, Pivot, V40073, V40083, V40093, V40103, V40113,
             V40123, V40133, V40143, V40153, V40163) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0)
  )
  
  VictimOffenseAssociation <- left_join(tempDf,
                          select(victimSegmentDataFrame, AdministrativeSegmentID, VictimSequenceNumber, VictimSegmentID),
                          by=c("AdministrativeSegmentID", "VictimSequenceNumber")) %>%
    select(VictimSegmentID, OffenseSegmentID=Pivot)
  
  VictimOffenseAssociation$VictimOffenseAssociationID <- 1:nrow(VictimOffenseAssociation)
  
  writeLines(paste0("Writing ", nrow(VictimOffenseAssociation), " VictimOffenseAssociation association rows to database"))
  
  dbWriteTable(conn=conn, name="VictimOffenseAssociation", value=data.table(VictimOffenseAssociation), append=TRUE, row.names = FALSE)
  
  VictimOffenseAssociation
  
}

writeVictimTypeInjury <- function(conn, victimSegmentDataFrame, rawIncidentsDataFrame) {
  tempDf <- bind_rows(
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, VictimSequenceNumber=V40061, V40261, V40271, V40281, V40291, V40301) %>%
      filter(VictimSequenceNumber > 0) %>%
      gather(V_Pivot, Pivot, V40261, V40271, V40281, V40291, V40301) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, VictimSequenceNumber=V40062, V40262, V40272, V40282, V40292, V40302) %>%
      filter(VictimSequenceNumber > 0) %>%
      gather(V_Pivot, Pivot, V40262, V40272, V40282, V40292, V40302) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, VictimSequenceNumber=V40063, V40263, V40273, V40283, V40293, V40303) %>%
      filter(VictimSequenceNumber > 0) %>%
      gather(V_Pivot, Pivot, V40263, V40273, V40283, V40293, V40303) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0)
  )
  
  TypeInjury <- left_join(tempDf,
                          select(victimSegmentDataFrame, AdministrativeSegmentID, VictimSequenceNumber, VictimSegmentID),
                          by=c("AdministrativeSegmentID", "VictimSequenceNumber")) %>%
    select(VictimSegmentID, TypeInjuryTypeID=Pivot)
  
  missingSegmentIDs <- setdiff(victimSegmentDataFrame$VictimSegmentID, TypeInjury$VictimSegmentID)
  
  TypeInjury <- bind_rows(TypeInjury,
                          data.frame(VictimSegmentID=missingSegmentIDs,
                                     TypeInjuryTypeID=rep(x=1, times=length(missingSegmentIDs))))
  
  TypeInjury$TypeInjuryID <- 1:nrow(TypeInjury)
  
  writeLines(paste0("Writing ", nrow(TypeInjury), " TypeInjury association rows to database"))
  
  dbWriteTable(conn=conn, name="TypeInjury", value=data.table(TypeInjury), append=TRUE, row.names = FALSE)
  
  TypeInjury
  
}

writeVictims <- function(conn, rawIncidentsDataFrame, segmentActionTypeTypeID) {
  
  VictimSegment <- cbind(
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V40061:V40063) %>%
      gather(V_VictimSequenceNumber, VictimSequenceNumber, V40061:V40063),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V40171:V40173) %>%
      gather(V_TypeOfVictimTypeID, TypeOfVictimTypeID, V40171:V40173) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V4017A1:V4017A3) %>%
      gather(V_OfficerActivityCircumstanceTypeID, OfficerActivityCircumstanceTypeID, V4017A1:V4017A3) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V4017B1:V4017B3) %>%
      gather(V_OfficerAssignmentTypeTypeID, OfficerAssignmentTypeTypeID, V4017B1:V4017B3) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V40191:V40193) %>%
      gather(V_SexOfPersonTypeID, SexOfPersonTypeID, V40191:V40193) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V40181:V40183) %>%
      gather(V_AgeRaw, AgeRaw, V40181:V40183) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V40201:V40203) %>%
      gather(V_RaceOfPersonTypeID, RaceOfPersonTypeID, V40201:V40203) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V40211:V40213) %>%
      gather(V_EthnicityOfPersonTypeID, EthnicityOfPersonTypeID, V40211:V40213) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V40221:V40223) %>%
      gather(V_ResidentStatusOfPersonTypeID, ResidentStatusOfPersonTypeID, V40221:V40223) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V40251:V40253) %>%
      gather(V_AdditionalJustifiableHomicideCircumstancesID, AdditionalJustifiableHomicideCircumstancesID, V40251:V40253) %>%
      select(-AdministrativeSegmentID)
    
  )  %>% filter(VictimSequenceNumber > 0) %>% select(-starts_with("V_")) %>%
    mutate(AgeOfVictim=as.integer(ifelse(AgeRaw < 1, NA, AgeRaw)),
           AgeNeonateIndicator=ifelse(AgeRaw == 0.1, 1, 0),
           AgeFirstWeekIndicator=ifelse(AgeRaw == 0.2, 1, 0),
           AgeFirstYearIndicator=ifelse(AgeRaw == 0.5, 1, 0),
           SexOfPersonTypeID=ifelse(SexOfPersonTypeID < 0, 9, SexOfPersonTypeID+1),
           TypeOfVictimTypeID=ifelse(TypeOfVictimTypeID < 0, 9, TypeOfVictimTypeID),
           RaceOfPersonTypeID=ifelse(RaceOfPersonTypeID < 0, 9, RaceOfPersonTypeID),
           EthnicityOfPersonTypeID=ifelse(EthnicityOfPersonTypeID < 0, 9, EthnicityOfPersonTypeID),
           ResidentStatusOfPersonTypeID=ifelse(ResidentStatusOfPersonTypeID < 0, 9, ResidentStatusOfPersonTypeID),
           OfficerActivityCircumstanceTypeID=ifelse(OfficerActivityCircumstanceTypeID < 0, 99, OfficerActivityCircumstanceTypeID),
           OfficerAssignmentTypeTypeID=ifelse(OfficerAssignmentTypeTypeID < 0, 9, OfficerAssignmentTypeTypeID),
           AdditionalJustifiableHomicideCircumstancesID=ifelse(AdditionalJustifiableHomicideCircumstancesID < 0, 9, AdditionalJustifiableHomicideCircumstancesID),
           SegmentActionTypeTypeID=segmentActionTypeTypeID) %>%
    select(-AgeRaw)
  
  VictimSegment$VictimSegmentID = 1:nrow(VictimSegment)
  
  writeLines(paste0("Writing ", nrow(VictimSegment), " victim segments to database"))
  
  #dbWriteTable(conn=conn, name="VictimSegment", value=data.table(VictimSegment), append=TRUE, row.names = FALSE)
  
  VictimSegment
  
}