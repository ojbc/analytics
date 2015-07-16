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

STATE = 'VT'

library(rgdal)
library(ggplot2)
library(ggmap)
library(dplyr)
library(RMySQL)
library(ggmap)

state_shp <- readOGR("/opt/data//Shapefiles/tl_2014_us_state", "tl_2014_us_state")
county_shp = readOGR("/opt/data/Shapefiles/tl_2014_us_county/", "tl_2014_us_county")

state_df <- state_shp@data
stateFips <- as.character(filter(state_df, STUSPS==STATE)$STATEFP)

county_shp <- subset(county_shp, STATEFP == stateFips)
county_shp_df <- fortify(county_shp)

state_shp <- subset(state_shp, STATEFP == stateFips)
state_shp_df <- fortify(state_shp)

conn <- dbConnect(MySQL(), host="localhost", dbname="ojbc_analytics_demo", username="root")
Incident <- dbFetch(dbSendQuery(conn, "select IncidentLocationLongitude, IncidentLocationLatitude, IncidentTypeDescription
                                from Incident, IncidentType where Incident.IncidentTypeID=IncidentType.IncidentTypeID"))


map <- ggplot() +
  geom_path(data = county_shp_df, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .3) +
  geom_path(data = state_shp_df, aes(x = long, y = lat, group = group), color = 'black', fill = 'white', size = .4) +
  geom_point(data=Incident, aes(x = IncidentLocationLongitude, y=IncidentLocationLatitude, colour=IncidentTypeDescription)) +
  coord_map(projection="mercator")

map

city <- get_map("Waterbury, Vermont", zoom=12, source = "google")
map <- ggmap(city) + 
  geom_point(data=Incident, aes(x = IncidentLocationLongitude, y=IncidentLocationLatitude, colour=IncidentTypeDescription), size=4)
map

dbDisconnect(conn)