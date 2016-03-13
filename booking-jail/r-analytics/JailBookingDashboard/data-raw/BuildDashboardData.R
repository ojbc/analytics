library(RMySQL)
library(dplyr)

adsConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")

populationCountSql <- paste0("select EpisodeCount, JurisdictionTypeDescription, AgencyTypeDescription, PretrialStatusTypeDescription, ",
                             "CaseStatusTypeDescription, LanguageTypeDescription, BedTypeDescription, ",
                             "AgeInYears, AgeRangeSort as AgeRange, PersonSexDescription, OccupationTypeDescription, ",
                             "EducationLevelTypeDescription, IncomeLevelTypeDescription, PopulationTypeDescription, ",
                             "HousingStatusTypeDescription, PersonRaceDescription, DaysAgo ",
                             "from DailyPopulation, JurisdictionType, AgencyType, PretrialStatusType, CaseStatusType, ",
                             "LanguageType, BedType, PersonAge, PersonAgeRange, PersonSex, OccupationType, ",
                             "EducationLevelType, IncomeLevelType, PopulationType, HousingStatusType, PersonRace ",
                             "where DailyPopulation.JurisdictionTypeID=JurisdictionType.JurisdictionTypeID and ",
                             "DailyPopulation.AgencyTypeID=AgencyType.AgencyTypeID and ",
                             "DailyPopulation.PretrialStatusTypeID=PretrialStatusType.PretrialStatusTypeID and ",
                             "DailyPopulation.CaseStatusTypeID=CaseStatusType.CaseStatusTypeID and ",
                             "DailyPopulation.LanguageTypeID=LanguageType.LanguageTypeID and ",
                             "DailyPopulation.BedTypeID=BedType.BedTypeID and ",
                             "DailyPopulation.PersonAgeID=PersonAge.PersonAgeID and ",
                             "PersonAge.PersonAgeRangeID=PersonAgeRange.PersonAgeRangeID and ",
                             "DailyPopulation.PersonSexID=PersonSex.PersonSexID and ",
                             "DailyPopulation.OccupationTypeID=OccupationType.OccupationTypeID and ",
                             "DailyPopulation.EducationLevelTypeID=EducationLevelType.EducationLevelTypeID and " ,
                             "DailyPopulation.IncomeLevelTypeID=IncomeLevelType.IncomeLevelTypeID and ",
                             "DailyPopulation.PopulationTypeID=PopulationType.PopulationTypeID and ",
                             "DailyPopulation.HousingStatusTypeID=HousingStatusType.HousingStatusTypeID and ",
                             "DailyPopulation.PersonRaceID=PersonRace.PersonRaceID")

PopulationCount <- dbGetQuery(adsConnection, populationCountSql)
CurrentPopulationCount <- filter(PopulationCount, DaysAgo==1)

populationChargeCountSql <- paste0("select EpisodeCount, ChargeTypeDescription, JurisdictionTypeDescription, ",
                                   "AgencyTypeDescription, PopulationTypeDescription, DaysAgo ",
                                   "from DailyPopulationCharges, JurisdictionType, AgencyType, PopulationType, ChargeType ",
                                   "where DailyPopulationCharges.JurisdictionTypeID=JurisdictionType.JurisdictionTypeID and ",
                                   "DailyPopulationCharges.AgencyTypeID=AgencyType.AgencyTypeID and ",
                                   "DailyPopulationCharges.PopulationTypeID=PopulationType.PopulationTypeID and ",
                                   "DailyPopulationCharges.ChargeTypeID=ChargeType.ChargeTypeID")

PopulationChargeCount <- dbGetQuery(adsConnection, populationChargeCountSql)
CurrentPopulationChargeCount <- filter(PopulationChargeCount, DaysAgo==1)

populationBehavioralHealthCountSql <- paste0("select EpisodeCount, ",
                                             "BehavioralHealthDescription as BehavioralHealthTypeDescription, JurisdictionTypeDescription, ",
                                             "AgencyTypeDescription, PopulationTypeDescription, DaysAgo ",
                                             "from DailyPopulationBehavioralHealth, JurisdictionType, AgencyType, PopulationType, BehavioralHealthType ",
                                             "where DailyPopulationBehavioralHealth.JurisdictionTypeID=JurisdictionType.JurisdictionTypeID and ",
                                             "DailyPopulationBehavioralHealth.AgencyTypeID=AgencyType.AgencyTypeID and ",
                                             "DailyPopulationBehavioralHealth.PopulationTypeID=PopulationType.PopulationTypeID and ",
                                             "DailyPopulationBehavioralHealth.BehavioralHealthTypeID=BehavioralHealthType.BehavioralHealthTypeID")

PopulationBehavioralHealthCount <- dbGetQuery(adsConnection, populationBehavioralHealthCountSql)
CurrentPopulationBehavioralHealthCount <- filter(PopulationBehavioralHealthCount, DaysAgo==1)

devtools::use_data(PopulationCount, CurrentPopulationCount,
                   PopulationChargeCount, CurrentPopulationChargeCount,
                   PopulationBehavioralHealthCount, CurrentPopulationBehavioralHealthCount,
                   overwrite = TRUE, internal = TRUE)

dbDisconnect(adsConnection)
