library(RMySQL)

source("LoadCodeTables.R")
source("FileLoadingFunctions.R")
source("IncidentFunctions.R")
source("OffenseFunctions.R")
source("PropertyFunctions.R")
source("OffenderFunctions.R")
source("VictimFunctions.R")
source("AgencyFunctions.R")
source("CommonFunctions.R")

options(stringsAsFactors = FALSE)

conn <- dbConnect(MySQL(), host="localhost", dbname="nibrs_analytics", username="root")

tryCatch({
  
  dbClearResult(dbSendQuery(conn, "set foreign_key_checks=0"))
  
  truncateIncidents(conn)
  truncateOffenses(conn)
  truncateProperty(conn)
  truncateOffender(conn)
  truncateVictim(conn)
  
  #loadCodeTables("NIBRSCodeTables.xlsx", conn)
  
  agencies <- loadAgencyFile("/opt/data/ICPSR_35158/DS0001/35158-0001-Data.txt")
  agencies <- addAgencyTable(conn, agencies)
  
  rawIncidents <- loadIncidentFile("/opt/data/NIBRS/2013/ICPSR_36121/DS0001/Ohio.txt", maxRecords=1000)
  rawIncidents <- addAdministrativeSegmentID(rawIncidents)
  
  currentMonth <- formatC(month(Sys.Date()), width=2, flag="0")
  currentYear <- year(Sys.Date())
  
  AdministrativeSegment <- writeIncidents(conn, rawIncidents, currentMonth, currentYear, 9, agencies)
  
  OffenseSegment <- writeOffenses(conn, rawIncidents, 9)
  OffenderSuspectedOfUsing <- writeOffenderSuspectedOfUsing(conn, OffenseSegment, rawIncidents)
  TypeCriminalActivity <- writeTypeCriminalActivity(conn, OffenseSegment, rawIncidents)
  TypeOfWeaponForceInvolved <- writeTypeOfWeaponForceInvolved(conn, OffenseSegment, rawIncidents)
  
  PropertySegment <- writeProperty(conn, rawIncidents, 9)
  SuspectedDrugType <- writeSuspectedDrugType(conn, PropertySegment, rawIncidents)
  
  OffenderSegment <- writeOffenders(conn, rawIncidents, 9)
  
  VictimSegment <- writeVictims(conn, rawIncidents, 9)
  TypeInjury <- writeVictimTypeInjury(conn, VictimSegment, rawIncidents)
  VictimOffenseAssociation <- writeVictimOffenseAssociation(conn, VictimSegment, rawIncidents)
  VictimOffenderAssociation <- writeVictimOffenderAssociation(conn, VictimSegment, rawIncidents)
  AggravatedAssaultHomicideCircumstances <- writeAggravatedAssaultHomicideCircumstances(conn, VictimSegment, rawIncidents)
  
}, finally = {
  
  dbDisconnect(conn)
  
})