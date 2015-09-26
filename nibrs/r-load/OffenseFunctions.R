# functions related to Offense data manipulation

library(dplyr)
library(tidyr)

writeOffenses <- function(conn, rawIncidentsDataFrame) {
  
  OffenseSegment <- rawIncidentsDataFrame %>%
    select(IncidentID, V20061:V20073, V20111:V20133, V20201:V20203)
  
  OffenseSegment
  
}