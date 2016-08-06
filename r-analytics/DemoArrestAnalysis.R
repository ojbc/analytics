library(RMySQL)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggmap)
library(rgdal)

conn <-
  dbConnect(MySQL(), host = "localhost", dbname = "ojbc_analytics_historical", username =
              "root")

# set up analytic datasets

AllArrestees <-
  dbGetQuery(
    conn, paste0(
      "select DaysUntilNextArrest, AgeInYears, PersonSexDescription, ArrestLocationLatitude, ArrestLocationLongitude, ArrestingAgencyID ",
      "from Arrest, PersonAge, PersonSex ",
      "where ArresteeAgeID=PersonAgeID and ArresteeSexID=PersonSexID and ",
      "ArresteeSexID <> 3 and (DaysUntilNextArrest is null or DaysUntilNextArrest < 1000)",
      " and PersonAgeID < 85"
    )
  )

Incidents <- dbGetQuery(conn, paste0("select ReportingAgencyID, CountyID, IncidentLocationLatitude, IncidentLocationLongitude from Incident where",
                                     " IncidentLocationLatitude is not null"))

dbDisconnect(conn)

ReArrestees <- filter(AllArrestees,!is.na(DaysUntilNextArrest))

# plot of days until next arrest against age

ByAge <- ReArrestees %>%
  group_by(AgeInYears, PersonSexDescription) %>%
  summarise(days = mean(DaysUntilNextArrest), count = n())
# theme_economist() +
ggplot(data = ByAge, aes(x = AgeInYears, y = days)) + geom_point(aes(color =
                                                                       PersonSexDescription, size = count)) + 
  labs(x = "ReArresteesee Age", y = "Days Until Next ReArrestees", color =
         "Sex:") + scale_x_discrete(breaks = seq(0,85,5))

ReArrestees <- ReArrestees %>%
  mutate(Age = as.integer(AgeInYears), IsMale = ifelse(PersonSexDescription ==
                                                         'Male', 1, 0))

# OLS regression of days until next arrest against age and sex

ols <- lm(DaysUntilNextArrest ~ Age + IsMale, data = ReArrestees)
summary(ols)

# demo of probit regression on binomial outcome Y/N rearrest

AllArrestees <- AllArrestees %>%
  mutate(
    FutureArrest = !is.na(DaysUntilNextArrest), Age = as.integer(AgeInYears), IsMale =
      ifelse(PersonSexDescription == 'Male', T, F)
  )

probit <-
  glm(
    FutureArrest ~ Age + IsMale, family = binomial(link = "probit"), data =
      AllArrestees, control = list(maxit = 50)
  )
summary(probit)

# demo heat map

LocatedArrests <- filter(AllArrestees, !is.na(ArrestLocationLatitude) & !is.na(ArrestLocationLongitude) & ArrestingAgencyID==7)
city <- get_map("Bennington, Vermont", zoom = 13, source = "osm")
map <- ggmap(city) +
#  geom_point(
#    data = AllArrestees, aes(x = ArrestLocationLongitude, y = ArrestLocationLatitude), size = 1
#  )
  stat_density2d(aes(x = ArrestLocationLongitude, y = ArrestLocationLatitude, colour = "grey95",
                     fill = ..level.., alpha = ..level..), 
                 bins = 15, size = .01, geom="polygon", data = LocatedArrests) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(.25, .75), guide = FALSE)
map

Incidents <- filter(Incidents, !is.na(IncidentLocationLatitude) & !is.na(IncidentLocationLongitude) & CountyID==50011)
osm <- get_map("Fairfield, Vermont", zoom = 10, source = "osm")
county_shp = readOGR("/opt/data/Shapefiles/tl_2014_us_county/", "tl_2014_us_county")
county_shp <- subset(county_shp, GEOID == '50011')
county_shp_df <- fortify(county_shp)
map <- ggmap(osm) +
  geom_path(data = county_shp_df, aes(x = long, y = lat, group = group),
            color = 'black', fill = 'white', size = .3) +
  stat_density2d(aes(x = IncidentLocationLongitude, y = IncidentLocationLatitude, 
                     fill = ..level.., alpha = ..level..), 
                 bins = 15, geom="polygon", data = Incidents) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(.2, .3), guide = FALSE) +
  labs(fill="Incidents",
       title="Franklin County Incident Densities, Jan. 2013 - Sept. 2015")
map
