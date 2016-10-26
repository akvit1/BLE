setwd(file.path(".", "Raw_Data"))



packages<- c("maps", "maptools", "rgdal", "rgeos", "dplyr", "spdep",
             "classInt", "RColorBrewer", "sp")
sapply(packages, require, character.only = T)



# get census and health census data, state plane maryland 1983 feet
census_shp <- readShapeSpatial(file.path("./VS 14 Census Demographics", "VS14_Census"))
health_shp <- readShapeSpatial(file.path("./VS 14 Children & Family Health", "VS14_Health"))



# figure out how to combine the shapefiles
#comb_shp <- full_join(census_shp, health_shp, by = "CSA2010")


# plot choropleth of life expectancy in Baltimore Census Tracts in 2014


exp <- health_shp$LifeExp14
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(exp, nclr,style="quantile")
colcode<-findColours(class,plotclr)
plot(health_shp)
plot(health_shp,col=colcode,add=TRUE)
title(main="Baltimore Life Expectancy per Neighborhood") 
mtext(side=3,line=.5, text="Life Expect")
legend(621406,144550,legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="FF Counts",cex=.7,bty="n")

# create border for the plots. allow distance of 300 between neighborhoods to account
# for an "island" in southern Baltimore

health_shp_nb<-poly2nb(health_shp,queen=FALSE, snap = 300)
plot(health_shp)


# plot centroids of each neighborhood to identify nbhood structure
plot(health_shp_nb,coordinates(health_shp),pch=19,cex=.6,add=TRUE)



# W is the weight matrix of just 1 and 0, showing if each census tract has a tract as its neighbor.

health_shp_nb_w<-nb2listw(health_shp_nb,style="B")
W<-listw2mat(health_shp_nb_w)

# a single moran's I test for Baltimore life expectancy


moran<-moran.test(health_shp$LifeExp14,listw=nb2listw(health_shp_nb,style="B" ))
moran

# make a correlogram of Moran's I, lagged by neighborhood distance
cor<-sp.correlogram(neighbours=health_shp_nb,var=health_shp$LifeExp14,order=8,method="I",style="B", 
                     zero.policy =TRUE)
plot(cor, main="Moran's I Correlogram of Baltimore Life Expectancy")

View(health_shp)

# do a Poisson regression now of life expectancy on
# liquor, fast food, and blood lead. Can use area or population as an offset. 
# Don't set an offset yet,
#

m1<-glm(LifeExp14~liquor14+fastfd13+ebll14,family=poisson,data=health_shp)

summary(m1)

# generate standardized residuals from this model

resid1<-(m1$y-m1$fitted.values)/sqrt(m1$fitted.values)

# look at overdispersion
X2.M1<-sum(resid1^2)

# chi square statistics of the models
X2.M1


# plot a residual correlogram
cor<-sp.correlogram(neighbours=health_shp_nb,var=resid1,order=8,method="I",style="B", 
                    zero.policy =TRUE)
plot(cor, main="Moran's I Correlogram of Life Expectancy Residuals")


setwd(file.path(".."))

