library(RMySQL)
library(dplyr)

maxDaysAgo <- " where DaysAgo <= 90"
# uncomment the following line to load a full two years of data
maxDaysAgo <- " "

adsConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")

populationCountSql <- paste0("select EpisodeCount, DaysAgo, ",
                             "(select JurisdictionTypeDescription from JurisdictionType where DailyPopulation.JurisdictionTypeID=JurisdictionType.JurisdictionTypeID), ",
                             "(select PretrialStatusTypeDescription from PretrialStatusType where DailyPopulation.PretrialStatusTypeID=PretrialStatusType.PretrialStatusTypeID),",
                             "(select CaseStatusTypeDescription from CaseStatusType where DailyPopulation.CaseStatusTypeID=CaseStatusType.CaseStatusTypeID),",
                             "(select LanguageTypeDescription from LanguageType where DailyPopulation.LanguageTypeID=LanguageType.LanguageTypeID),",
                             "(select BedTypeDescription from BedType where DailyPopulation.BedTypeID=BedType.BedTypeID),",
                             "(select AgeInYears from PersonAge where DailyPopulation.PersonAgeID=PersonAge.PersonAgeID),",
                             "(select AgeRangeSort as AgeRange from PersonAge, PersonAgeRange where DailyPopulation.PersonAgeID=PersonAge.PersonAgeID and PersonAge.PersonAgeRangeID=PersonAgeRange.PersonAgeRangeID),",
                             "(select PersonSexDescription from PersonSex where DailyPopulation.PersonSexID=PersonSex.PersonSexID),",
                             "(select OccupationTypeDescription from OccupationType where DailyPopulation.OccupationTypeID=OccupationType.OccupationTypeID),",
                             "(select EducationLevelTypeDescription from EducationLevelType where DailyPopulation.EducationLevelTypeID=EducationLevelType.EducationLevelTypeID),",
                             "(select IncomeLevelTypeDescription from IncomeLevelType where DailyPopulation.IncomeLevelTypeID=IncomeLevelType.IncomeLevelTypeID),",
                             "(select PopulationTypeDescription from PopulationType where DailyPopulation.PopulationTypeID=PopulationType.PopulationTypeID),",
                             "(select HousingStatusTypeDescription from HousingStatusType where DailyPopulation.HousingStatusTypeID=HousingStatusType.HousingStatusTypeID),",
                             "(select PersonRaceDescription from PersonRace where DailyPopulation.PersonRaceID=PersonRace.PersonRaceID),",
                             "(select AgencyTypeDescription from AgencyType where DailyPopulation.AgencyTypeID=AgencyType.AgencyTypeID) ",
                             "from DailyPopulation ", maxDaysAgo)

PopulationCount <- dbGetQuery(adsConnection, populationCountSql)
CurrentPopulationCount <- filter(PopulationCount, DaysAgo==1)

populationChargeCountSql <- paste0("select EpisodeCount, ChargeTypeDescription, JurisdictionTypeDescription, ",
                                   "AgencyTypeDescription, PopulationTypeDescription, DaysAgo ",
                                   "from DailyPopulationCharges, JurisdictionType, AgencyType, PopulationType, ChargeType ",
                                   "where DailyPopulationCharges.JurisdictionTypeID=JurisdictionType.JurisdictionTypeID and ",
                                   "DailyPopulationCharges.AgencyTypeID=AgencyType.AgencyTypeID and ",
                                   "DailyPopulationCharges.PopulationTypeID=PopulationType.PopulationTypeID and ",
                                   "DailyPopulationCharges.ChargeTypeID=ChargeType.ChargeTypeID", maxDaysAgo)

PopulationChargeCount <- dbGetQuery(adsConnection, populationChargeCountSql)
CurrentPopulationChargeCount <- filter(PopulationChargeCount, DaysAgo==1)

populationBehavioralHealthCountSql <- paste0("select EpisodeCount, ",
                                             "BehavioralHealthDescription as BehavioralHealthTypeDescription, JurisdictionTypeDescription, ",
                                             "AgencyTypeDescription, PopulationTypeDescription, DaysAgo, SevereMentalIllnessIndicator ",
                                             "from DailyPopulationBehavioralHealth, JurisdictionType, AgencyType, PopulationType, BehavioralHealthType ",
                                             "where DailyPopulationBehavioralHealth.JurisdictionTypeID=JurisdictionType.JurisdictionTypeID and ",
                                             "DailyPopulationBehavioralHealth.AgencyTypeID=AgencyType.AgencyTypeID and ",
                                             "DailyPopulationBehavioralHealth.PopulationTypeID=PopulationType.PopulationTypeID and ",
                                             "DailyPopulationBehavioralHealth.BehavioralHealthTypeID=BehavioralHealthType.BehavioralHealthTypeID",
                                             maxDaysAgo)

PopulationBehavioralHealthCount <- dbGetQuery(adsConnection, populationBehavioralHealthCountSql)
CurrentPopulationBehavioralHealthCount <- filter(PopulationBehavioralHealthCount, DaysAgo==1)

devtools::use_data(PopulationCount, CurrentPopulationCount,
                   PopulationChargeCount, CurrentPopulationChargeCount,
                   PopulationBehavioralHealthCount, CurrentPopulationBehavioralHealthCount,
                   overwrite = TRUE, internal = TRUE)

dbDisconnect(adsConnection)
