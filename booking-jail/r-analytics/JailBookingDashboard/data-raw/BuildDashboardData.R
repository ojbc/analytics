library(RMySQL)
library(dplyr)

maxDaysAgo <- " where DaysAgo <= 90"
# uncomment the following line to load a full two years of data
maxDaysAgo <- " "

adsConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")

populationCountSql <- paste0("select EpisodeCount, DaysAgo, ",
                             "(select JurisdictionTypeDescription from JurisdictionType where DailyPopulation.JurisdictionTypeID=JurisdictionType.JurisdictionTypeID) as JurisdictionTypeDescription, ",
                             "(select PretrialStatusTypeDescription from PretrialStatusType where DailyPopulation.PretrialStatusTypeID=PretrialStatusType.PretrialStatusTypeID) as PretrialStatusTypeDescription, ",
                             "(select CaseStatusTypeDescription from CaseStatusType where DailyPopulation.CaseStatusTypeID=CaseStatusType.CaseStatusTypeID) as CaseStatusTypeDescription, ",
                             "(select LanguageTypeDescription from LanguageType where DailyPopulation.LanguageTypeID=LanguageType.LanguageTypeID) as LanguageTypeDescription, ",
                             "(select BedTypeDescription from BedType where DailyPopulation.BedTypeID=BedType.BedTypeID) as BedTypeDescription, ",
                             "(select AgeInYears from PersonAge where DailyPopulation.PersonAgeID=PersonAge.PersonAgeID) as AgeInYears, ",
                             "(select AgeRangeSort as AgeRange from PersonAge, PersonAgeRange where DailyPopulation.PersonAgeID=PersonAge.PersonAgeID and PersonAge.PersonAgeRangeID=PersonAgeRange.PersonAgeRangeID) as AgeRange, ",
                             "(select PersonSexDescription from PersonSex where DailyPopulation.PersonSexID=PersonSex.PersonSexID) as PersonSexDescription, ",
                             "(select OccupationTypeDescription from OccupationType where DailyPopulation.OccupationTypeID=OccupationType.OccupationTypeID) as OccupationTypeDescription, ",
                             "(select EducationLevelTypeDescription from EducationLevelType where DailyPopulation.EducationLevelTypeID=EducationLevelType.EducationLevelTypeID) as EducationLevelTypeDescription, ",
                             "(select IncomeLevelTypeDescription from IncomeLevelType where DailyPopulation.IncomeLevelTypeID=IncomeLevelType.IncomeLevelTypeID) as IncomeLevelTypeDescription, ",
                             "(select PopulationTypeDescription from PopulationType where DailyPopulation.PopulationTypeID=PopulationType.PopulationTypeID) as PopulationTypeDescription, ",
                             "(select HousingStatusTypeDescription from HousingStatusType where DailyPopulation.HousingStatusTypeID=HousingStatusType.HousingStatusTypeID) as HousingStatusTypeDescription, ",
                             "(select PersonRaceDescription from PersonRace where DailyPopulation.PersonRaceID=PersonRace.PersonRaceID) as PersonRaceDescription, ",
                             "(select AgencyTypeDescription from AgencyType where DailyPopulation.AgencyTypeID=AgencyType.AgencyTypeID) as AgencyTypeDescription ",
                             "from DailyPopulation ", maxDaysAgo)

PopulationCount <- dbGetQuery(adsConnection, populationCountSql)
CurrentPopulationCount <- filter(PopulationCount, DaysAgo==1)

populationChargeCountSql <- paste0("select EpisodeCount, DaysAgo, ",
                             "(select JurisdictionTypeDescription from JurisdictionType where DailyPopulationCharges.JurisdictionTypeID=JurisdictionType.JurisdictionTypeID) as JurisdictionTypeDescription, ",
                             "(select ChargeTypeDescription from ChargeType where DailyPopulationCharges.ChargeTypeID=ChargeType.ChargeTypeID) as ChargeTypeDescription, ",
                             "(select PopulationTypeDescription from PopulationType where DailyPopulationCharges.PopulationTypeID=PopulationType.PopulationTypeID) as PopulationTypeDescription, ",
                             "(select AgencyTypeDescription from AgencyType where DailyPopulationCharges.AgencyTypeID=AgencyType.AgencyTypeID) as AgencyTypeDescription ",
                             "from DailyPopulationCharges ", maxDaysAgo)

PopulationChargeCount <- dbGetQuery(adsConnection, populationChargeCountSql)
CurrentPopulationChargeCount <- filter(PopulationChargeCount, DaysAgo==1)

populationBehavioralHealthCountSql <- paste0("select EpisodeCount, DaysAgo, SevereMentalIllnessIndicator, ",
                                   "(select JurisdictionTypeDescription from JurisdictionType where DailyPopulationBehavioralHealth.JurisdictionTypeID=JurisdictionType.JurisdictionTypeID) as JurisdictionTypeDescription, ",
                                   "(select BehavioralHealthDescription from BehavioralHealthType where DailyPopulationBehavioralHealth.BehavioralHealthTypeID=BehavioralHealthType.BehavioralHealthTypeID) as BehavioralHealthTypeDescription, ",
                                   "(select PopulationTypeDescription from PopulationType where DailyPopulationBehavioralHealth.PopulationTypeID=PopulationType.PopulationTypeID) as PopulationTypeDescription, ",
                                   "(select AgencyTypeDescription from AgencyType where DailyPopulationBehavioralHealth.AgencyTypeID=AgencyType.AgencyTypeID) as AgencyTypeDescription ",
                                   "from DailyPopulationBehavioralHealth ", maxDaysAgo)

PopulationBehavioralHealthCount <- dbGetQuery(adsConnection, populationBehavioralHealthCountSql)
CurrentPopulationBehavioralHealthCount <- filter(PopulationBehavioralHealthCount, DaysAgo==1)

devtools::use_data(PopulationCount, CurrentPopulationCount,
                   PopulationChargeCount, CurrentPopulationChargeCount,
                   PopulationBehavioralHealthCount, CurrentPopulationBehavioralHealthCount,
                   overwrite = TRUE, internal = TRUE)

dbDisconnect(adsConnection)
