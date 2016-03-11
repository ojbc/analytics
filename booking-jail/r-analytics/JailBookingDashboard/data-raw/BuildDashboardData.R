library(RMySQL)
library(dplyr)

adsConnection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_booking_analytics_demo", username="root")

populationCountSql <- paste0("select EpisodeCount, JurisdictionTypeDescription, AgencyTypeDescription, PretrialStatusTypeDescription, ",
                             "CaseStatusTypeDescription LanguageTypeDescription, BedTypeDescription, ",
                             "AgeInYears, PersonSexDescription, OccupationTypeDescription, ",
                             "EducationLevelTypeDescription, IncomeLevelTypeDescription, PopulationTypeDescription, ",
                             "HousingStatusTypeDescription, PersonRaceDescription ",
                             "from DailyPopulation, JurisdictionType, AgencyType, PretrialStatusType, CaseStatusType, ",
                             "LanguageType, BedType, PersonAge, PersonSex, OccupationType, ",
                             "EducationLevelType, IncomeLevelType, PopulationType, HousingStatusType, PersonRace ",
                             "where DailyPopulation.JurisdictionTypeID=JurisdictionType.JurisdictionTypeID and ",
                             "DailyPopulation.AgencyTypeID=AgencyType.AgencyTypeID and ",
                             "DailyPopulation.PretrialStatusTypeID=PretrialStatusType.PretrialStatusTypeID and ",
                             "DailyPopulation.CaseStatusTypeID=CaseStatusType.CaseStatusTypeID and ",
                             "DailyPopulation.LanguageTypeID=LanguageType.LanguageTypeID and ",
                             "DailyPopulation.BedTypeID=BedType.BedTypeID and ",
                             "DailyPopulation.PersonAgeID=PersonAge.PersonAgeID and ",
                             "DailyPopulation.PersonSexID=PersonSex.PersonSexID and ",
                             "DailyPopulation.OccupationTypeID=OccupationType.OccupationTypeID and ",
                             "DailyPopulation.EducationLevelTypeID=EducationLevelType.EducationLevelTypeID and " ,
                             "DailyPopulation.IncomeLevelTypeID=IncomeLevelType.IncomeLevelTypeID and ",
                             "DailyPopulation.PopulationTypeID=PopulationType.PopulationTypeID and ",
                             "DailyPopulation.HousingStatusTypeID=HousingStatusType.HousingStatusTypeID and ",
                             "DailyPopulation.PersonRaceID=PersonRace.PersonRaceID and ",
                             "DaysAgo=1")

PopulationCount <- dbGetQuery(adsConnection, populationCountSql)
