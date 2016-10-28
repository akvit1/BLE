# install all the necessary packages

list.of.packages <- c("tidyr", "geoR", "dplyr", "maps", "maptools", "rgdal", "rgeos", "sp", "spatialEco",
                      "plyr", "RColorBrewer", "classInt", "spatstat", "spdep", "sp", "usdm","readr",
                      "lubridate", "rmarkdown", "leaps", "MASS", "Metrics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



setwd(file.path(".", "Analyzed_Data", "ArcGIS"))

packages<- c("maps", "maptools", "rgdal", "rgeos", "downloader", "dplyr",
             "plyr", "geoR", "RColorBrewer", "classInt", "spatstat",
             "spdep", "sp", "usdm", "leaps", "MASS", "Metrics")
sapply(packages, require, character.only = T)



# get the block baltimore shapefile from ArcGIS
# All the shapefiles are in NAD 1983 Maryland state plane projection
blocks <- readShapePoly("balt_vac_crime_estate_cen_health_feet.shp")

# rename some of the columns that resulted from ArcGIS merging
names(blocks)[34] <- "vacancy"
names(blocks)[35] <- "crime"
names(blocks)[38] <- "estate"


# get the census tract shapefile from ArcGIS
tracts <- readShapePoly("track_health_cens_crim_est_vac.shp")

names(tracts)[110] <- "crime"
names(tracts)[113] <- "estate"
names(tracts)[114] <- "vacancy"


# add centroid coordinates to account for spatial correlation
t_coords<- as.data.frame(gCentroid(tracts, byid = TRUE))
b_coords<- as.data.frame(gCentroid(blocks, byid = TRUE))

tracts$X <- t_coords$x
tracts$Y <- t_coords$y
blocks$X <- b_coords$x
blocks$Y <- b_coords$y

# replace values in the blocks dataset with the
# mean tract values, in order to take care of the few blocks
# that received no values from the tracts shapefile 
# 9 blocks have zero values

# replace with Old Town Middle East data

block_names<-blocks$BLK2010[blocks$femhhs10 == 0]
block_names<- as.vector(block_names)

blocks@data[blocks@data$BLK2010 %in% block_names,]$"LifeExp11" <- tracts@data[51,]$"LifeExp11"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"liquor11" <- tracts@data[51,]$"liquor11"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"fastfd11" <- tracts@data[51,]$"fastfd11"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"tanf11" <- tracts@data[51,]$"tanf11"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"tpop10" <- tracts@data[51,]$"tpop10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"male10" <- tracts@data[51,]$"male10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"female10" <- tracts@data[51,]$"female10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"pwhite10" <- tracts@data[51,]$"pwhite10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"paa10" <- tracts@data[51,]$"paa10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"pasi10" <- tracts@data[51,]$"pasi10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"p2more10" <- tracts@data[51,]$"p2more10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"ppac10" <- tracts@data[51,]$"ppac10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"racdiv10" <- tracts@data[51,]$"racdiv10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"hhs10" <- tracts@data[51,]$"hhs10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"femhhs10" <- tracts@data[51,]$"femhhs10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"fam10" <- tracts@data[51,]$"fam10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"hhsize10" <- tracts@data[51,]$"hhsize10"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"mhhi14" <- tracts@data[51,]$"mhhi14"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"hh25inc14" <- tracts@data[51,]$"hh25inc14"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"hh40inc14" <- tracts@data[51,]$"hh40inc14"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"hh60inc14" <- tracts@data[51,]$"hh60inc14"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"hh75inc14" <- tracts@data[51,]$"hh75inc14"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"hhpov14" <- tracts@data[51,]$"hhpov14"
blocks@data[blocks@data$BLK2010 %in% block_names,]$"hhchpov14" <- tracts@data[51,]$"hhchpov14"



# plot 2011 life expectancy by census tract to get an idea
# of what the distribution in Baltimore is like 

plotvar0<-tracts$LifeExp11
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="pretty")
colcode<-findColours(class,plotclr)
plot(tracts)
plot(tracts,col=colcode,add=TRUE)
title(main="Baltimore 2011 Life Expetancy by Census Tract") 
legend(1390926, 579406, 
       legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Life Expectancy",cex=.7,bty="n")

# save this plot
png(file.path("..", "..", "Figures",  "Final_Figures", "2011_Life_Expect.png"),
    width = 1000, height = 1200, pointsize = 30)

plotvar0<-tracts$LifeExp11
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="pretty")
colcode<-findColours(class,plotclr)
plot(tracts)
plot(tracts,col=colcode,add=TRUE)
title(main="2011 Life Expetancy by Census Tract") 
mtext(side=3,line=.5, text="Baltimore, MD")
legend(1390926, 579406, 
       legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Life Expectancy",cex=.7,bty="n")

dev.off()

# get count variables like crime and vacancy per square mile, in order
# to offset the difference between census tract and block level
# crime counts
tracts$crime_mi <- tracts$crime/tracts$tract_area
tracts$vacancy_mi <- tracts$vacancy/tracts$tract_area
blocks$crime_mi <- blocks$crime/blocks$block_area
blocks$vacancy_mi <- blocks$vacancy/blocks$block_area

# In order to further standardize the variables between census tracts and
# blocks, the variables can be represented by deviation from the mean value of 
# the variable divided by the variance of that variable
tracts$crime_dev <- tracts$crime - mean(tracts$crime)
tracts$crime_dev <- tracts$crime_dev/var(tracts$crime_dev)

tracts$crime_mi_dev <- tracts$crime_mi - mean(tracts$crime_mi)
tracts$crime_mi_dev <- tracts$crime_mi_dev/var(tracts$crime_mi_dev)

tracts$StateTax_dev <- tracts$StateTax - mean(tracts$StateTax)
tracts$StateTax_dev <- tracts$StateTax_dev/var(tracts$StateTax_dev)

tracts$CityTax_dev <- tracts$CityTax - mean(tracts$CityTax)
tracts$CityTax_dev <- tracts$CityTax_dev/var(tracts$CityTax_dev)

tracts$vacancy_mi_dev <- tracts$vacancy_mi - mean(tracts$vacancy_mi)
tracts$vacancy_mi_dev <- tracts$vacancy_mi_dev/var(tracts$vacancy_mi_dev)

tracts$vacancy_dev <- tracts$vacancy - mean(tracts$vacancy)
tracts$vacancy_dev <- tracts$vacancy_dev/var(tracts$vacancy_dev)

tracts$LifeExp11_dev <- tracts$LifeExp11 - mean(tracts$LifeExp11)
tracts$LifeExp11_dev <- tracts$LifeExp11_dev/var(tracts$LifeExp11_dev)

tracts$liquor11_dev <- tracts$liquor11 - mean(tracts$liquor11)
tracts$liquor11_dev <- tracts$liquor11_dev/var(tracts$liquor11_dev)

tracts$fastfd11_dev <- tracts$fastfd11 - mean(tracts$fastfd11)
tracts$fastfd11_dev <- tracts$fastfd11_dev/var(tracts$fastfd11_dev)

tracts$racdiv10_dev <- tracts$racdiv10 - mean(tracts$racdiv10)
tracts$racdiv10_dev <- tracts$racdiv10_dev/var(tracts$racdiv10_dev)

tracts$femhhs10_dev <- tracts$femhhs10 - mean(tracts$femhhs10)
tracts$femhhs10_dev <- tracts$femhhs10_dev/var(tracts$femhhs10_dev)

tracts$mhhi14_dev <- tracts$mhhi14 - mean(tracts$mhhi14)
tracts$mhhi14_dev <- tracts$mhhi14_dev/var(tracts$mhhi14_dev)



## do the same at the block level


blocks$crime_mi_dev <- blocks$crime_mi - mean(blocks$crime_mi)
blocks$crime_mi_dev <- blocks$crime_mi_dev/var(blocks$crime_mi_dev)

blocks$crime_dev <- blocks$crime - mean(blocks$crime)
blocks$crime_dev <- blocks$crime_dev/var(blocks$crime_dev)

blocks$StateTax_dev <- blocks$StateTax - mean(blocks$StateTax)
blocks$StateTax_dev <- blocks$StateTax_dev/var(blocks$StateTax_dev)

blocks$CityTax_dev <- blocks$CityTax - mean(blocks$CityTax)
blocks$CityTax_dev <- blocks$CityTax_dev/var(blocks$CityTax_dev)

blocks$vacancy_mi_dev <- blocks$vacancy_mi - mean(blocks$vacancy_mi)
blocks$vacancy_mi_dev <- blocks$vacancy_mi_dev/var(blocks$vacancy_mi_dev)

blocks$vacancy_dev <- blocks$vacancy - mean(blocks$vacancy)
blocks$vacancy_dev <- blocks$vacancy_dev/var(blocks$vacancy_dev)

blocks$le11_dev <- blocks$LifeExp11 - mean(blocks$LifeExp11)
blocks$le11_dev <- blocks$le11_dev/var(blocks$le11_dev)

blocks$liquor11_dev <- blocks$liquor11 - mean(blocks$liquor11)
blocks$liquor11_dev <- blocks$liquor11_dev/var(blocks$liquor11_dev)

blocks$fastfd11_dev <- blocks$fastfd11 - mean(blocks$fastfd11)
blocks$fastfd11_dev <- blocks$fastfd11_dev/var(blocks$fastfd11_dev)

blocks$racdiv10_dev <- blocks$racdiv10 - mean(blocks$racdiv10)
blocks$racdiv10_dev <- blocks$racdiv10_dev/var(blocks$racdiv10_dev)

blocks$femhhs10_dev <- blocks$femhhs10 - mean(blocks$femhhs10)
blocks$femhhs10_dev <- blocks$femhhs10_dev/var(blocks$femhhs10_dev)

blocks$mhhi14_dev <- blocks$mhhi14 - mean(blocks$mhhi14)
blocks$mhhi14_dev <- blocks$mhhi14_dev/var(blocks$mhhi14_dev)


# Check which variables of interest might be collinear with each other
# look at the collinearity using vif, leave only those below 10. Looks like
# all the variables have VIF below 10. 
vars_s<- c("liquor11",   "fastfd11",   "tanf11",    
           "tpop10",     "paa10",      "pwhite10" , 
           "racdiv10",   "hhs10",      "femhhs10",  
           "fam10" ,     "hhsize10",   "mhhi14",    
           "hhpov14",    "hhchpov14",  "crime_mi_dev",     
           "CityTax_dev",    "StateTax_dev",   "vacancy_mi_dev",   
           "X",          "Y")  

df <- tracts@data[,(names(tracts) %in% vars_s)] 
vif(df)
vif
b<- vifstep(df, th=5)
b



# The final prediction model includes variables that have been previously shown
# to make sense based on the literature, and have the best predictive
# accuracy. To account for spatial autocorrelation
# the x and y coordinates are included as well. A generalized linear model is used. 

f1 <- LifeExp11 ~ femhhs10_dev + crime_mi_dev + racdiv10_dev + vacancy_mi_dev + X + Y 



# apply the model built on a census tract level to the the block level #
# Randomly select a training set and a testing set (training set has 30 tracts, test set
# has 25 tracts)

set.seed(123)
samp<- sample(nrow(tracts), 30)

train_set <- tracts[samp,]
test_set <- tracts[-samp,]


f1 <- LifeExp11 ~ femhhs10_dev + crime_mi_dev + racdiv10_dev + vacancy_mi_dev  + X + Y 

m1 = lm(f1, data=train_set)
summary(m1)

# test the model using the test set.
p0 <- predict(m1, newdata = test_set)

# look at the correlation between predicted life expectancy p1 and the actual life
# expectancy in the test_set
cor(p0, test_set@data$LifeExp11)
rmse(test_set@data$LifeExp11, p0)

# use the same model to predict life expectancy for each block
p1 <- predict(m1, newdata = blocks, type = "response", interval = "predict")
blocks$p1 <- p1[,1]


# the 95% confidence interval of the prediction
blocks$p1_var <- p1[,3]- p1[,2]

# the summary of the predicted life expectancy
summary(blocks$p1)
# the 95% confidence interval of the prediction
summary(blocks$p1_var)

# plot the map of the predicted life expectancy per block 

par(mfrow=c(1,2))


plotvar0<-blocks$p1
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="pretty")
colcode<-findColours(class,plotclr)
plot(blocks )
plot(blocks,col=colcode,add=TRUE)
title(main="Baltimore Predicted Life Expectancy by Block") 
mtext(side=3,line=.5, text="")
legend(1390926, 579406, legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Life Expectancy",cex=.7,bty="n")



# plot the 95% confidence interval as an estimate of the error

plotvar0<-blocks$p1_var
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="quantile")
colcode<-findColours(class,plotclr)
plot(blocks)
plot(blocks,col=colcode,add=TRUE)
title(main="Baltimore Predicted Life Expectancy Confidence Interval Width") 
mtext(side=3,line=.5, text="")
legend(1390926, 579406, legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="95% CI Width",cex=.7,bty="n")

par(mfrow=c(1,1))




# save the plots 

png(file.path("..", "..", "Figures", "Final_Figures", "Predicted_2011_Life_Expec.png"),
    width = 1000, height = 1200, pointsize = 30)

plotvar0<-blocks$p1
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="pretty")
colcode<-findColours(class,plotclr)
plot(blocks)
plot(blocks,col=colcode,add=TRUE)
title(main="Predicted 2011 Life Expectancy by Street Block") 
mtext(side=3,line=.5, text="")
mtext(side=3,line=.5, text="Baltimore, MD")
legend(1390926, 579406, legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Life Expectancy",cex=.7,bty="n")

dev.off()





png(file.path("..", "..", "Figures",  "Final_Figures", "Predicted_2011_Intervals.png"),
    width = 1200, height = 700, pointsize = 20)
# plot the 95% confidence interval as an estimate of the error
par(mfrow=c(1,2))

plotvar0<-p1[,2]
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="pretty")
colcode<-findColours(class,plotclr)
plot(blocks)
plot(blocks,col=colcode,add=TRUE)
title(main="Prediction 95% CI (Lower Bound)") 
mtext(side=3,line=.5, text="")
mtext(side=3,line=.5, text="Baltimore, MD")
legend(1386926, 579406, legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="95% CI Lower Bound",cex=.7,bty="n")


plotvar0<-p1[,3]
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="pretty")
colcode<-findColours(class,plotclr)
plot(blocks)
plot(blocks,col=colcode,add=TRUE)
title(main="Prediction 95% CI (Upper Bound)" )
mtext(side=3,line=.5, text="")
mtext(side=3,line=.5, text="Baltimore, MD")
legend(1386926, 579406, legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="95% CI Upper Bound",cex=.7,bty="n")

par(mfrow=c(1,1))


dev.off()


setwd(file.path(".."))
setwd(file.path(".."))
