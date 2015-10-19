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

# functions related to Offense data manipulation

library(dplyr)
library(tidyr)

truncateOffenses <- function(conn) {
  dbClearResult(dbSendQuery(conn, "truncate OffenseSegment"))
  dbClearResult(dbSendQuery(conn, "truncate OffenderSuspectedOfUsing"))
  dbClearResult(dbSendQuery(conn, "truncate TypeCriminalActivity"))
  dbClearResult(dbSendQuery(conn, "truncate TypeOfWeaponForceInvolved"))
}

writeOffenderSuspectedOfUsing <- function(conn, offenseSegmentDataFrame, rawIncidentsDataFrame) {
  
  tempDf <- bind_rows(
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, OffenseCode=V20061, V20081:V20083) %>%
      filter(OffenseCode > 0) %>%
      gather(V_Pivot, Pivot, V20081:V20083) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, OffenseCode=V20062, V20091:V20093) %>%
      filter(OffenseCode > 0) %>%
      gather(V_Pivot, Pivot, V20091:V20093) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, OffenseCode=V20063, V20101:V20103) %>%
      filter(OffenseCode > 0) %>%
      gather(V_Pivot, Pivot, V20101:V20103) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0)
  )
  
  OffenderSuspectedOfUsing <- left_join(tempDf,
                                        select(offenseSegmentDataFrame, AdministrativeSegmentID, OffenseCode, OffenseSegmentID),
                                        by=c("AdministrativeSegmentID", "OffenseCode")) %>%
    select(OffenseSegmentID, OffenderSuspectedOfUsingTypeID=Pivot)
  
  missingSegmentIDs <- setdiff(offenseSegmentDataFrame$OffenseSegmentID, OffenderSuspectedOfUsing$OffenseSegmentID)
  
  OffenderSuspectedOfUsing <- bind_rows(OffenderSuspectedOfUsing,
                                        data.frame(OffenseSegmentID=missingSegmentIDs,
                                                   OffenderSuspectedOfUsingTypeID=rep(x=4, times=length(missingSegmentIDs))))
  
  OffenderSuspectedOfUsing$OffenderSuspectedOfUsingID <- 1:nrow(OffenderSuspectedOfUsing)
  
  writeLines(paste0("Writing ", nrow(OffenderSuspectedOfUsing), " OffenderSuspectedOfUsing association rows to database"))
  
  dbWriteTable(conn=conn, name="OffenderSuspectedOfUsing", value=data.table(OffenderSuspectedOfUsing), append=TRUE, row.names = FALSE)

  OffenderSuspectedOfUsing
  
}

writeTypeOfWeaponForceInvolved <- function(conn, offenseSegmentDataFrame, rawIncidentsDataFrame) {
  
  tempDf <- bind_rows(
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, OffenseCode=V20061, V20171:V20173) %>%
      filter(OffenseCode > 0) %>%
      gather(V_Pivot, Pivot, V20171:V20173) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, OffenseCode=V20062, V20181:V20183) %>%
      filter(OffenseCode > 0) %>%
      gather(V_Pivot, Pivot, V20181:V20183) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, OffenseCode=V20063, V20191:V20193) %>%
      filter(OffenseCode > 0) %>%
      gather(V_Pivot, Pivot, V20191:V20193) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0)
  )
  
  codeTranslationDf <- data.frame(
    icpsrCode=c(110,111,120,121,130,131,140,141,150,151,200,300,350,400,500,600,650,700,850,900,990),
    ourCode=  c(1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 17),
    AutomaticWeaponIndicator=c("N","Y","N","Y","N","Y","N","Y","N","Y","N","N","N","N","N","N","N","N","N","N","N")
  )
  
  TypeOfWeaponForceInvolved <- left_join(tempDf,
                                        select(offenseSegmentDataFrame, AdministrativeSegmentID, OffenseCode, OffenseSegmentID),
                                        by=c("AdministrativeSegmentID", "OffenseCode")) %>%
    select(OffenseSegmentID, TypeOfWeaponForceInvolvedTypeID=Pivot)
  
  TypeOfWeaponForceInvolved <- left_join(TypeOfWeaponForceInvolved, codeTranslationDf,
                                   by=c("TypeOfWeaponForceInvolvedTypeID"="icpsrCode")) %>%
    select(-TypeOfWeaponForceInvolvedTypeID) %>%
    rename(TypeOfWeaponForceInvolvedTypeID=ourCode)
  
  missingSegmentIDs <- setdiff(offenseSegmentDataFrame$OffenseSegmentID, TypeOfWeaponForceInvolved$OffenseSegmentID)
  
  TypeOfWeaponForceInvolved <- bind_rows(TypeOfWeaponForceInvolved,
                                        data.frame(OffenseSegmentID=missingSegmentIDs,
                                                   TypeOfWeaponForceInvolvedTypeID=rep(x=17, times=length(missingSegmentIDs)),
                                                   AutomaticWeaponIndicator=rep(x="N", times=length(missingSegmentIDs))))
  
  TypeOfWeaponForceInvolved$TypeOfWeaponForceInvolvedID <- 1:nrow(TypeOfWeaponForceInvolved)
  
  writeLines(paste0("Writing ", nrow(TypeOfWeaponForceInvolved), " TypeOfWeaponForceInvolved association rows to database"))
  
  dbWriteTable(conn=conn, name="TypeOfWeaponForceInvolved", value=data.table(TypeOfWeaponForceInvolved), append=TRUE, row.names = FALSE)

  TypeOfWeaponForceInvolved
  
}

writeTypeCriminalActivity <- function(conn, offenseSegmentDataFrame, rawIncidentsDataFrame) {
  
  tempDf <- bind_rows(
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, OffenseCode=V20061, V20141:V20143) %>%
      filter(OffenseCode > 0) %>%
      gather(V_Pivot, Pivot, V20141:V20143) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, OffenseCode=V20062, V20151:V20153) %>%
      filter(OffenseCode > 0) %>%
      gather(V_Pivot, Pivot, V20151:V20153) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0),
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, OffenseCode=V20063, V20161:V20163) %>%
      filter(OffenseCode > 0) %>%
      gather(V_Pivot, Pivot, V20161:V20163) %>%
      mutate(V_Pivot=as.character(V_Pivot)) %>%
      filter(Pivot > 0)
  )
  
  TypeCriminalActivity <- left_join(tempDf,
                                        select(offenseSegmentDataFrame, AdministrativeSegmentID, OffenseCode, OffenseSegmentID),
                                        by=c("AdministrativeSegmentID", "OffenseCode")) %>%
    select(OffenseSegmentID, TypeOfCriminalActivityTypeID=Pivot)
  
  missingSegmentIDs <- setdiff(offenseSegmentDataFrame$OffenseSegmentID, TypeCriminalActivity$OffenseSegmentID)
  
  TypeCriminalActivity <- bind_rows(TypeCriminalActivity,
                                        data.frame(OffenseSegmentID=missingSegmentIDs,
                                                   TypeOfCriminalActivityTypeID=rep(x=99, times=length(missingSegmentIDs))))
  
  TypeCriminalActivity$TypeCriminalActivityID <- 1:nrow(TypeCriminalActivity)
  
  writeLines(paste0("Writing ", nrow(TypeCriminalActivity), " TypeCriminalActivity association rows to database"))
  
  dbWriteTable(conn=conn, name="TypeCriminalActivity", value=data.table(TypeCriminalActivity), append=TRUE, row.names = FALSE)
  
  TypeCriminalActivity
  
}

writeOffenses <- function(conn, rawIncidentsDataFrame, segmentActionTypeTypeID) {
  
  OffenseSegment <- cbind(

    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V20061:V20063) %>%
      gather(V_OffenseCode, OffenseCode, V20061:V20063),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V20071:V20073) %>%
      gather(V_AttemptedCompleted, OffenseAttemptedCompleted, V20071:V20073) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V20111:V20113) %>%
      gather(V_LocationType, LocationTypeTypeID, V20111:V20113) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V20121:V20123) %>%
      gather(V_PremisesEntered, NumberOfPremisesEntered, V20121:V20123) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V20131:V20133) %>%
      gather(V_MethodOfEntry, MethodOfEntryTypeID, V20131:V20133) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V20201:V20203) %>%
      gather(V_BiasMotivation, BiasMotivationTypeID, V20201:V20203) %>%
      select(-AdministrativeSegmentID)
    
  ) %>%
    filter(OffenseCode != -8) %>% select(-starts_with("V_")) %>%
    mutate(MethodOfEntryTypeID=ifelse(MethodOfEntryTypeID < 0, 9, MethodOfEntryTypeID),
           NumberOfPremisesEntered=ifelse(NumberOfPremisesEntered < 0, NA, NumberOfPremisesEntered),
           LocationTypeTypeID=ifelse(LocationTypeTypeID < 0, 99, LocationTypeTypeID),
           SegmentActionTypeTypeID=segmentActionTypeTypeID, UCROffenseCodeTypeID=OffenseCode)
  
  OffenseSegment$OffenseSegmentID = 1:nrow(OffenseSegment)
  
  writeLines(paste0("Writing ", nrow(OffenseSegment), " offense segments to database"))
  
  dbWriteTable(conn=conn, name="OffenseSegment", value=data.table(select(OffenseSegment, -OffenseCode)), append=TRUE, row.names = FALSE)
  
  OffenseSegment
  
}