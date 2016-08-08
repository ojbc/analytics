library(ggplot2)
library(ggmap)
library(rgdal)
library(dplyr)

adamsCommissionerDistrictShp <- readOGR("/Users/scott/Downloads/AdamsCountyShapefiles/", "AC_Comm_Dist")
adamsCommissionerDistrictShp <- spTransform(adamsCommissionerDistrictShp, CRS("+proj=longlat +datum=WGS84"))
adamsCommissionerDistrictShpDf <- fortify(adamsCommissionerDistrictShp)

adamsShp <- readOGR("/Users/scott/Downloads/gz_2010_08_150_00_500k/", "gz_2010_08_150_00_500k")
adamsShpDf <- fortify(adamsShp)
adamsShp <- subset(adamsShp, COUNTY == "001")
adamsShpDf <- fortify(adamsShp)
ggplot() + geom_path(data=adamsShpDf, aes(x=long, y=lat, group=group)) + coord_equal()
adamsPopData <- read.csv2("/Users/scott/Downloads/DEC_10_SF1_P1/DEC_10_SF1_P1_with_ann.csv", header=TRUE, sep=",", skip = 1)
adamsShp@data$id <- rownames(adamsShp@data)
adamsShpDf <- left_join(adamsShpDf, adamsShp@data)
adamsShpDfWithPop <- left_join(adamsShpDf, adamsPopData, by=c("GEO_ID"="Id"))

map <- ggplot(data=adamsShpDfWithPop, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill=Total)) + coord_equal() +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444")
map + geom_path(data=adamsCommissionerDistrictShpDf, aes(x=long, y=lat, group=group), color="blue")

adamsPopData$Total <- sample(adamsPopData$Total)

adamsShpDfWithPop <- left_join(adamsShpDf, adamsPopData, by=c("GEO_ID"="Id"))

map <- ggplot(data=adamsShpDfWithPop, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill=Total)) + coord_equal() +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444")
map + geom_path(data=adamsCommissionerDistrictShpDf, aes(x=long, y=lat, group=group), color="blue")

adamsPopData$Total <- sample(adamsPopData$Total)

adamsShpDfWithPop <- left_join(adamsShpDf, adamsPopData, by=c("GEO_ID"="Id"))

map <- ggplot(data=adamsShpDfWithPop, aes(x=long, y=lat, group=group)) + geom_path() + geom_polygon(aes(fill=Total)) + coord_equal() +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444")
map + geom_path(data=adamsCommissionerDistrictShpDf, aes(x=long, y=lat, group=group), color="blue")

