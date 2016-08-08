library(RMySQL)
library(rgdal)
library(ggplot2)
library(dplyr)
library(ggthemes)

conn <- dbConnect(MySQL(), host="localhost", dbname="nibrs_analytics", username="root")

# this shapefile is better for states like Ohio that have counties extending into a body of water...

county_shp <- readOGR("/opt/data/Shapefiles/glin_oh_county_boundaries_2000", "oh_county_boundaries_2000")
county_shp_df <- fortify(county_shp)

countyData <- dbGetQuery(conn, "select CountyCode, sum(NumberOfRecoveredMotorVehicles) as recovered, sum(NumberOfStolenMotorVehicles) as stolen from PropertySegment, AdministrativeSegment, Agency where PropertySegment.AdministrativeSegmentID = AdministrativeSegment.AdministrativeSegmentID and AdministrativeSegment.AgencyID = Agency.AgencyID group by CountyCode")
countyData <- countyData %>%
  mutate(PercentRecovered=ifelse(is.na(recovered) | is.na(stolen) | stolen==0, NA, recovered/stolen)) %>%
  rename(county=CountyCode)

county_shp@data$id <- rownames(county_shp@data)
county_shp_df <- left_join(county_shp_df, county_shp@data)
county_shp_df <- left_join(county_shp_df, countyData)

ggplot(data=county_shp_df, aes(x=long, y=lat, group=group)) +
  geom_path(color="grey") +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", space = "Lab", na.value = "grey50", guide = "colourbar", name="% Recovered") +
  geom_polygon(aes(fill=PercentRecovered)) + coord_map(projection="mercator") +
  labs(title="Stolen Vehicle Recovery Success (NIBRS Data, 2013)") + theme_few() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank())

dbDisconnect(conn)
