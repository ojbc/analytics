# functions related to Agency data manipulation

library(dplyr)
library(tidyr)
library(RMySQL)

addAgencyTable <- function(conn, rawAgencyDataFrame) {
  
  dbClearResult(dbSendQuery(conn, "truncate Agency"))
  
  Agency <- rawAgencyDataFrame %>%
    select(AgencyName=NAME,
           AgencyORI=ORI9,
           UCRPopulation=U_TPOP,
           CensusPopulation=LG_POPULATION,
           StateCode=FIPS_ST,
           CountyCode=FIPS_COUNTY,
           StateName=STATENAME,
           CountyName=COUNTYNAME) %>%
    mutate(CensusPopulation=ifelse(CensusPopulation==888888888,NA,CensusPopulation),
           UCRPopulation=ifelse(UCRPopulation==-1 | UCRPopulation==0,NA,UCRPopulation))
  
  Agency$AgencyID <- 1:nrow(Agency)
  
  writeLines(paste0("Writing ", nrow(Agency), " Agency rows to database"))
  
  dbWriteTable(conn=conn, name="Agency", value=data.table(Agency), append=TRUE, row.names = FALSE)
  
  Agency
  
}

