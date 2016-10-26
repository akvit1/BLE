# clean the point data

setwd(file.path(".", "Raw_Data"))


packages<- c("tidyr", "geoR", "dplyr","maps", "maptools", "rgdal", "rgeos", "sp", "spatialEco")
sapply(packages, require, character.only = T)


crime<- read.csv("point_crime.csv")
vacant<- read.csv("point_vacancy.csv")
estate<- read.csv("real_estate.csv")

dim(crime)
dim(vacant)
dim(estate)


# clean the latitude and longitude
crime$Location.1 <- gsub("\\(", "", as.character(factor(crime$Location.1)))
crime$Location.1 <- gsub("\\)", "", as.character(factor(crime$Location.1)))
crime <- separate(crime, Location.1, into = c("lat", "long"), sep = ", ")

vacant$Location <- gsub("\\(", "", as.character(factor(vacant$Location)))
vacant$Location <- gsub("\\)", "", as.character(factor(vacant$Location)))
vacant <- separate(vacant, Location, into = c("lat", "long"), sep = ", ")

estate$Location <- gsub("\\(", "", as.character(factor(estate$Location)))
estate$Location <- gsub("\\)", "", as.character(factor(estate$Location)))
estate <- separate(estate, Location, into = c("lat", "long"), sep = ", ")

# get only data with intact longitude and latitude

crime <- filter(crime, !is.na(lat) & !is.na(long))
vacant <- filter(vacant, !is.na(lat) & !is.na(long))
estate <- filter(estate, !is.na(lat) & !is.na(long))

# convert all the lats and longs into numerics
crime$lat <- as.numeric(crime$lat)
crime$long<- as.numeric(crime$long)
vacant$lat <- as.numeric(vacant$lat)
vacant$long<- as.numeric(vacant$long)
estate$lat <- as.numeric(estate$lat)
estate$long<- as.numeric(estate$long)


dim(crime)
dim(vacant)
dim(estate)

# create a counter variable for each dataset
crime$counter <- rep(1,nrow(crime))
vacant$counter <- rep(1,nrow(vacant))
estate$counter <- rep(1,nrow(estate))

# save the files for processing in ArcGIS

write.csv(crime, file.path("..", "Analyzed_Data", "ArcGIS", "crime.csv"))
write.csv(vacant, file.path("..", "Analyzed_Data", "ArcGIS", "vacant.csv"))
write.csv(estate, file.path("..", "Analyzed_Data", "ArcGIS", "estate_.csv"))



############################################################################################
### The following code is to create shape files and merge things in R. However
# it is more convenient to do this in ArcGIS

if(FALSE) {

# convert the data frames into point shapefiles

crime_sp <- SpatialPointsDataFrame(coords = select(crime, c(long, lat)), data = crime,
                               proj4string = CRS("+proj=longlat +datum=WGS84"))

vacant_sp <- SpatialPointsDataFrame(coords = select(vacant, c(long, lat)), data = vacant,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 "))

estate_sp <- SpatialPointsDataFrame(coords = select(estate, c(long, lat)), data = estate,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 "))




### get the blocks shapefile ###

blocks <- readOGR(".", "blk2010")
blocks <- spTransform(blocks, CRS("+proj=longlat +datum=WGS84"))

# make it the baltimore city shapefile
balt<-blocks[blocks$CNTY2010==24510,]

# Save the baltimore shapefile NAD 1983 Maryland state plane projection
writeSpatialShape(balt, file.path("..", "Analyzed_Data", "ArcGIS","balt"))



# upload health data and census data


census_shp <- readShapeSpatial(file.path("./VS 14 Census Demographics", "VS14_Census"))
health_shp <- readShapeSpatial(file.path("./VS 14 Children & Family Health", "VS14_Health"))

# merge!

balt_crime <- point.in.polygon(crime_sp, balt)
class(crime_sp)
class(balt)

View(crime_sp)
View(balt)

plot(balt)
points(crime_sp)

}

setwd(file.path(".."))
