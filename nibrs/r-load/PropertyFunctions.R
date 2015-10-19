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

# functions related to Property data manipulation

library(dplyr)
library(tidyr)

truncateProperty <- function(conn) {
  dbClearResult(dbSendQuery(conn, "truncate PropertySegment"))
  dbClearResult(dbSendQuery(conn, "truncate SuspectedDrugType"))
}

writeProperty <- function(conn, rawIncidentsDataFrame, segmentActionTypeTypeID) {
  
  PropertySegment <- cbind(
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V30061:V30063) %>%
      gather(V_TypeLoss, TypePropertyLossEtcTypeID, V30061:V30063),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V30071:V30073) %>%
      gather(V_Desc, PropertyDescriptionTypeID, V30071:V30073) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V30081:V30083) %>%
      gather(V_Value, ValueOfProperty, V30081:V30083) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V30091:V30093) %>%
      gather(V_DateRecovered, DateRecovered, V30091:V30093) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V30101:V30103) %>%
      gather(V_StolenMV, NumberOfStolenMotorVehicles, V30101:V30103) %>%
      select(-AdministrativeSegmentID),
    
    rawIncidentsDataFrame %>%
      select(AdministrativeSegmentID, V30111:V30113) %>%
      gather(V_RecoveredMV, NumberOfRecoveredMotorVehicles, V30111:V30113) %>%
      select(-AdministrativeSegmentID)
    
  ) %>%
    filter(TypePropertyLossEtcTypeID != -8) %>% select(-starts_with("V_")) %>%
    mutate(TypePropertyLossEtcTypeID=ifelse(TypePropertyLossEtcTypeID < 0, 9, TypePropertyLossEtcTypeID),
           PropertyDescriptionTypeID=ifelse(PropertyDescriptionTypeID < 0, 99, PropertyDescriptionTypeID),
           ValueOfProperty=ifelse(ValueOfProperty < 0, NA, ValueOfProperty),
           DateRecovered=as.Date(
             ifelse(DateRecovered < 0, NA, as.Date(as.character(DateRecovered), format="%Y%m%d")), origin="1970-01-01"),
           NumberOfStolenMotorVehicles=ifelse(NumberOfStolenMotorVehicles < 0, NA, NumberOfStolenMotorVehicles),
           NumberOfRecoveredMotorVehicles=ifelse(NumberOfRecoveredMotorVehicles < 0, NA, NumberOfRecoveredMotorVehicles),
           SegmentActionTypeTypeID=segmentActionTypeTypeID)
  
  PropertySegment$PropertySegmentID = 1:nrow(PropertySegment)
  
  writeLines(paste0("Writing ", nrow(PropertySegment), " property segments to database"))
  
  dbWriteTable(conn=conn, name="PropertySegment", value=data.table(PropertySegment), append=TRUE, row.names = FALSE)
  
  PropertySegment
  
}

writeSuspectedDrugType <- function(conn, propertySegmentDataFrame, rawIncidentsDataFrame) {
  
  tempDf <- cbind(
    bind_rows(
      rawIncidentsDataFrame %>%
        select(AdministrativeSegmentID, PropertyDescription = V30071, V30121:V30123) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, SuspectedDrugTypeTypeID, V30121:V30123) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(AdministrativeSegmentID, PropertyDescription = V30072, V30161:V30163) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, SuspectedDrugTypeTypeID, V30161:V30163) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(AdministrativeSegmentID, PropertyDescription = V30073, V30201:V30203) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, SuspectedDrugTypeTypeID, V30201:V30203) %>%
        mutate(V_Pivot = as.character(V_Pivot))
    ) %>% select(-V_Pivot),
    bind_rows(
      rawIncidentsDataFrame %>%
        select(PropertyDescription = V30071, V30131:V30133) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, EstimatedDrugQuantityWholePart, V30131:V30133) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(PropertyDescription = V30072, V30171:V30173) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, EstimatedDrugQuantityWholePart, V30171:V30173) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(PropertyDescription = V30073, V30211:V30213) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, EstimatedDrugQuantityWholePart, V30211:V30213) %>%
        mutate(V_Pivot = as.character(V_Pivot))
    ) %>% select(-PropertyDescription, -V_Pivot),
    bind_rows(
      rawIncidentsDataFrame %>%
        select(PropertyDescription = V30071, V30141:V30143) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, EstimatedDrugQuantityFractionalPart, V30141:V30143) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(PropertyDescription = V30072, V30181:V30183) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, EstimatedDrugQuantityFractionalPart, V30181:V30183) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(PropertyDescription = V30073, V30221:V30223) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, EstimatedDrugQuantityFractionalPart, V30221:V30223) %>%
        mutate(V_Pivot = as.character(V_Pivot))
    ) %>% select(-PropertyDescription, -V_Pivot),
    bind_rows(
      rawIncidentsDataFrame %>%
        select(PropertyDescription = V30071, V30151:V30153) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, TypeMeasurement, V30151:V30153) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(PropertyDescription = V30072, V30191:V30193) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, TypeMeasurement, V30191:V30193) %>%
        mutate(V_Pivot = as.character(V_Pivot)),
      rawIncidentsDataFrame %>%
        select(PropertyDescription = V30073, V30231:V30233) %>%
        filter(PropertyDescription == 10) %>%
        gather(V_Pivot, TypeMeasurement, V30231:V30233) %>%
        mutate(V_Pivot = as.character(V_Pivot))
    ) %>% select(-PropertyDescription, -V_Pivot)
  )  %>% filter(SuspectedDrugTypeTypeID > 0) %>% rename(PropertyDescriptionTypeID=PropertyDescription)
  
  SuspectedDrugType <- left_join(tempDf,
                                        select(propertySegmentDataFrame, AdministrativeSegmentID,PropertyDescriptionTypeID, PropertySegmentID),
                                        by=c("AdministrativeSegmentID", "PropertyDescriptionTypeID")) %>%
    mutate(EstimatedDrugQuantity=as.numeric(ifelse(EstimatedDrugQuantityWholePart < 0, NA,
                                        EstimatedDrugQuantityWholePart + 
                                          (ifelse(EstimatedDrugQuantityFractionalPart < 0, 0,
                                                  as.numeric(EstimatedDrugQuantityFractionalPart/1000))))),
           TypeDrugMeasurementTypeID=as.integer(ifelse(TypeMeasurement < 0, 99, TypeMeasurement))) %>%
    select(PropertySegmentID, SuspectedDrugTypeTypeID, TypeDrugMeasurementTypeID, EstimatedDrugQuantity)
  
  SuspectedDrugType$SuspectedDrugTypeID <- 1:nrow(SuspectedDrugType)
  
  writeLines(paste0("Writing ", nrow(SuspectedDrugType), " SuspectedDrugType association rows to database"))
  
  dbWriteTable(conn=conn, name="SuspectedDrugType", value=data.table(SuspectedDrugType), append=TRUE, row.names = FALSE)
  
  SuspectedDrugType
  
}

