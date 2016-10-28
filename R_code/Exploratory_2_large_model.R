setwd(file.path(".", "Analyzed_Data", "ArcGIS"))


packages<- c("maps", "maptools", "rgdal", "rgeos", "downloader", "dplyr",
             "plyr", "geoR", "RColorBrewer", "classInt", "spatstat",
             "spdep", "sp", "usdm")
sapply(packages, require, character.only = T)



# get the block baltimore shapefile from ArcGIS
# All the shapefiles are in NAD 1983 Maryland state plane projection
blocks <- readShapePoly("balt_vac_crime_estate_cen_health_feet.shp")
# rename
names(blocks)[34] <- "vacancy"
names(blocks)[35] <- "crime"
names(blocks)[38] <- "estate"



# get the census tract shapefile from ArcGIS
tracts <- readShapePoly("track_health_cens_crim_est_vac.shp")

names(tracts)[110] <- "crime"
names(tracts)[113] <- "estate"
names(tracts)[114] <- "vacancy"


# get count variables like crime and vacancy per square mile
tracts$crime_mi <- tracts$crime/tracts$tract_area
tracts$vacancy_mi <- tracts$vacancy/tracts$tract_area
blocks$crime_mi <- blocks$crime/blocks$block_area
blocks$vacancy_mi <- blocks$vacancy/blocks$block_area


tracts_nb<-poly2nb(tracts,queen=FALSE, snap = 300)


# W is the weight matrix of just 1 and 0, showing if each census tract has a tract as its neighbor.
# select style = W to make it row standardized

tracts_nb_w<-nb2listw(tracts_nb,style="W")
W<-listw2mat(tracts_nb_w)

####################################################
# create neighborhood weights for blocks as well
blocks_nb <- poly2nb(blocks,queen=FALSE)

blocks_nb_w<-nb2listw(blocks_nb,style="W")
W2<-listw2mat(blocks_nb_w)




f7 <- LifeExp11 ~ liquor11 + fastfd11 + racdiv10 + femhhs10 +
    mhhi14 + crime_mi + StateTax 


m7_s = lagsarlm(f7, data=tracts, tracts_nb_w)
summary(m7_s)


p1 <- predict.sarlm(m7_s, listw = blocks_nb_w, newdata = blocks)

setwd(file.path("..", ".."))
      