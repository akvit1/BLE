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
# rename
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



tracts_e <- tracts@data[,-(1:9)]
tracts_e[, sapply(tracts_e, is.factor)]
# remove all the values that are factors, since none of them
# have evidence that they are associated with life expectancy
tracts_e<-tracts_e[, !sapply(tracts_e, is.factor)]
# get crime, estate, and vacanacy as numeric
tracts_e[, sapply(tracts_e, is.integer)]
tracts_e$crime <- as.numeric(tracts_e$crime)
tracts_e$estate <- as.numeric(tracts_e$estate)
tracts_e$vacancy <- as.numeric(tracts_e$vacancy)
tracts_e$long <- as.numeric(tracts_e$long)
unique(sapply(tracts_e, class))


# only inlude variables that are reasonable and associated with previous
# literature reviews. This includes:
# City Tax, State Tax, vacancy, crime, X, Y for point level data
# Liquor stores, Fast food stores, race (paa10, pwhite10, pasi10,
#p2more10, ppac10, phisp10, racdiv10, variables associated with income,
# and poverty)

# select only variables that are reasonable

tracts_e2<-tracts_e[,c(29,61,65,67, 71:80, 86:103, 106:107)]
names(tracts_e2)
t_names<-names(tracts_e2)
# look at the correlations between life expectancy in 2011
# and other variables
cors<- cor(tracts_e2$LifeExp11,tracts_e2, use="pairwise.complete.obs")
cors<- t(cors)
corsa<- abs(cors)
corsa


# replace values in the blocks dataset with the
# mean tract values, in order to take care of the few blocks
# that received no values from the tracts shapefile 
# 9 blocks have zero values

# replace with Old Town Middle East data
var_names<-names(tracts_e2[,c(1:30)])
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



png(file.path("..", "..", "Figures", "Exploratory_Figures","Life_Exp_Block.png"),
    width = 1200, height = 700, pointsize = 20)

# plot 2011 life expectancy by block
par(mfrow=c(1,2))
plotvar0<-blocks$LifeExp11
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="equal")
colcode<-findColours(class,plotclr)
plot(blocks)
plot(blocks,col=colcode,add=TRUE)
title(main="Baltimore 2011 Life Expectancy by Street Block") 
#mtext(side=3,line=.5, text="Life Expectancy")
legend(1390926, 579406, legend=names(attr(colcode, "table")), 
fill=attr(colcode,"palette"),title="Life Expectancy",cex=.7,bty="n")

#dev.off()

#png(file.path("..", "..", "Figures", "Exploratory_Figures","Life_Exp_Tract.png"))
# plot 2011 life expectancy by census tract

plotvar0<-tracts$LifeExp11
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="equal")
colcode<-findColours(class,plotclr)
plot(tracts)
plot(tracts,col=colcode,add=TRUE)
title(main="Baltimore 2011 Life Expetancy by Census Tract") 
#mtext(side=3,line=.5, text="Life Expectancy")
legend(1390926, 579406, 
legend=names(attr(colcode, "table")), 
fill=attr(colcode,"palette"),title="Life Expectancy",cex=.7,bty="n")

par(mfrow=c(1,1))

dev.off()


# now look at something that was the same between the blocks
# and the tracts, like life expectancy and household income
png(file.path("..", "..", "Figures", "Exploratory_Figures","Life_Expec_Histo.png"))

par(mfrow=c(2,2))
hist(tracts$LifeExp11)
hist(tracts$mhhi14)
hist(blocks$LifeExp11)
hist(blocks$mhhi14)
par(mfrow=c(1,1))
dev.off()

summary(blocks@data$LifeExp11)
summary(tracts@data$LifeExp11)

summary(tracts@data$mhhi14)
summary(blocks@data$mhhi14)

var(blocks@data$LifeExp11)
var(tracts@data$LifeExp11)

var(tracts@data$mhhi14)
var(blocks@data$mhhi14)

# not surprisingly, in this case the variance
# is very similar. 


# Test the assumption that point variables, such as crime, vacancy, and taxes
# increase with the area in the same way between blocks and tracts.
# If we can assume that, we can say that we can apply a model built
# using tracts onto a block map. 
png(file.path("..", "..", "Figures", "Exploratory_Figures","Linear_Increase.png"))


par(mfrow=c(2,2))

plot(tracts@data$tract_area, tracts@data$crime)
plot(blocks@data$block_area, blocks@data$crime)

plot(tracts@data$tract_area, tracts@data$vacancy)
plot(blocks@data$block_area, blocks@data$vacancy)

par(mfrow=c(1,1))

dev.off()

# same thing for taxes
par(mfrow=c(2,2))

plot(tracts@data$tract_area, tracts@data$StateTax)
plot(blocks@data$block_area, blocks@data$StateTax)

plot(tracts@data$tract_area, tracts@data$CityTax)
plot(blocks@data$block_area, blocks@data$CityTax)

par(mfrow=c(1,1))




###########################################################
# Look at the distribution of the crime counts and
# abandoned houses in the blocks and in 
#the tracts (CSA). Is it comparable?
png(file.path("..", "..", "Figures", "Exploratory_Figures","Crime_Vacant_Histo.png"))


par(mfrow=c(2,2))
hist(tracts$crime)
hist(tracts$vacancy)
hist(blocks$crime)
hist(blocks$vacancy)
par(mfrow=c(1,1))

dev.off()

############################################################
# get count variables like crime and vacancy per square mile
tracts$crime_mi <- tracts$crime/tracts$tract_area
tracts$vacancy_mi <- tracts$vacancy/tracts$tract_area
blocks$crime_mi <- blocks$crime/blocks$block_area
blocks$vacancy_mi <- blocks$vacancy/blocks$block_area


par(mfrow=c(2,2))
hist(tracts$crime_mi)
hist(tracts$vacancy_mi)
hist(blocks$crime_mi)
hist(blocks$vacancy_mi)
par(mfrow=c(1,1))


# if the crime and vacant house counts are divided by area,
# creating a a crime and vacant house density, the distribution
# of the densities is more comparable between the tracts and the
# blocks
 
summary(blocks@data$crime)
summary(tracts@data$crime)

summary(blocks@data$crime_mi)
summary(tracts@data$crime_mi)

var(blocks@data$crime)
var(tracts@data$crime)

var(blocks@data$crime_mi)
var(tracts@data$crime_mi)

summary(blocks@data$vacancy)
summary(tracts@data$vacancy)

summary(tracts@data$vacancy_mi)
summary(blocks@data$vacancy_mi)

var(blocks@data$vacancy)
var(tracts@data$vacancy)

var(tracts@data$vacancy_mi)
var(blocks@data$vacancy_mi)

# Now look at State and City Taxes

par(mfrow=c(2,2))
hist(tracts$StateTax)
hist(tracts$CityTax)
hist(blocks$StateTax)
hist(blocks$CityTax)
par(mfrow=c(1,1))

summary(blocks@data$StateTax)
summary(tracts@data$StateTax)

summary(tracts@data$CityTax)
summary(blocks@data$CityTax)

var(blocks@data$StateTax)
var(tracts@data$StateTax)

var(tracts@data$CityTax)
var(blocks@data$CityTax)

# the distributions within the blocks and within the tracts seem to be
# very different, making a comparison more difficult. Both City and State
# Taxes are not distributed in a similar way between blocks and tracts.
#The variance in blocks is much higher than the variance in tracts



### STANDARDIZE! ##
# divide things by variance
# get the deviation from the median

#tracts@data <- mutate(tracts@data, LifeExp11_dev = (LifeExp11 - mean(LifeExp11))/
#var(LifeExp11))


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


# look at the variances after standardization

par(mfrow=c(2,2))
hist(tracts$StateTax_dev)
hist(tracts$CityTax_dev)
hist(blocks$StateTax_dev)
hist(blocks$CityTax_dev)
par(mfrow=c(1,1))

summary(blocks@data$StateTax_dev)
summary(blocks@data$StateTax)
summary(tracts@data$StateTax_dev)
summary(tracts@data$StateTax)

var(tracts@data$crime_mi_dev)
var(blocks@data$crime_mi_dev)

var(tracts@data$vacancy_mi_dev)
var(blocks@data$vacancy_mi_dev)

var(blocks@data$StateTax_dev)
var(tracts@data$StateTax_dev)

var(tracts@data$CityTax_dev)
var(blocks@data$CityTax_dev)


#################################################
# Select the proper variables

# check for collinearity. Select variables you want to check

names(tracts_e2)

vars <- names(tracts_e2)
vars

df <- tracts@data[,(names(tracts) %in% vars)] 

# look at the collinearity using vif, leave only those below 10.
# many errors, take away variables that are redundant. 

vars_s<- c("liquor11",   "fastfd11",   "tanf11",    
"tpop10",     "paa10",      "pwhite10" , 
"racdiv10",   "hhs10",      "femhhs10",  
"fam10" ,     "hhsize10",   "mhhi14",    
"hhpov14",    "hhchpov14",  "crime_mi_dev",     
"CityTax_dev",    "StateTax_dev",   "vacancy_mi_dev",   
"X",          "Y", "femhhs10_dev", "liquor11_dev", "fastfd11_dev",
"racdiv10_dev", "mhhi14_dev")  

df <- tracts@data[,(names(tracts) %in% vars_s)] 
vif(df)
vif
b<- vifstep(df, th=10)
b

vars_s2<- c("tanf11", "pwhite10" , "hhs10",
            "femhhs10",  "hhpov14",   "crime_mi_dev", 
           "CityTax_dev",    "StateTax_dev",   "vacancy_mi_dev",  
           "X", "Y", "femhhs10_dev", "liquor11_dev",
           "racdiv10_dev", "mhhi14_dev")
            
df <- tracts@data[,(names(tracts) %in% vars_s2)] 
vif(df)
vif
b_f<- vifstep(df, th=5)
b_f            


############################################################
# do some regression at the tract level. Use the biggest possible model
# first. Look at standardized. 

f1 <- LifeExp11 ~ liquor11 + tanf11 + pwhite10 + racdiv10 + hhs10 + 
fam10 + hhpov14 + StateTax_dev + crime_mi_dev + vacancy_mi_dev + X + Y

m1 <- lm(f1, data = tracts)
summary(m1)

reg1<-regsubsets(f1, data = tracts, method = "backward")
summary(reg1)

# non standardized. 
f2 <- LifeExp11 ~ liquor11 + tanf11 + pwhite10 + racdiv10 + hhs10 + 
    fam10 + hhpov14 + StateTax + crime + vacancy + X + Y

m2 <- lm(f2, data = tracts)
summary(m2)

reg2<-regsubsets(f2, data = tracts, method = "backward")
summary(reg2)

# looks like temprary assistance for needy families (TANF) proportion
# liquor, percent of white people, household povery proportion
# And Y coordinate are some of the strongest predictors.
f3 <- LifeExp11 ~ liquor11 + tanf11 + pwhite10 + 
    hhpov14 + StateTax_dev + crime_mi_dev + vacancy_mi_dev

m3 <- lm(f3, data = tracts)
summary(m3)

reg3<-regsubsets(f3, data = tracts, method = "backward")
summary(reg3)

# non standardized. 
f4 <- LifeExp11 ~ liquor11 + tanf11 + pwhite10 + 
    hhpov14 + StateTax + crime + vacancy 

m4 <- lm(f4, data = tracts)
summary(m4)

reg4<-regsubsets(f4, data = tracts, method = "backward")
summary(reg4)

# of the point variables, vacancy and state tax seem to be the
# most important

f3s <- LifeExp11 ~ liquor11 + tanf11 + pwhite10 + 
    hhpov14 + StateTax_dev + crime_mi_dev + vacancy_mi_dev + X+ Y

m3s <- lm(f3s, data = tracts)
summary(m3s)


AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)
AIC(m3s)


# plot the residuals of m3

png(file.path("..", "..", "Figures",  "Exploratory_Figures", "Spatial_Non_Spatial_Residuals.png"),
    width = 1200, height = 700, pointsize = 20)

par(mfrow=c(1,2))
plotvar0<- residuals(m3)
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="equal")
colcode<-findColours(class,plotclr)
plot(tracts)
plot(tracts,col=colcode,add=TRUE)
title(main="M3 residuals") 
legend(1390926, 577406,legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Residuals",cex=.7,bty="n")



plotvar0<- residuals(m3s)
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="equal")
colcode<-findColours(class,plotclr)
plot(tracts)
plot(tracts,col=colcode,add=TRUE)
title(main="M3s residuals") 
legend(1390926, 577406,legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Residuals",cex=.7,bty="n")

par(mfrow=c(1,1))


dev.off()






# the residuals look spatially dependent
# calculate Moran's I

# create border for the plots. allow distance of 300 between neighborhoods to account
# for an "island" in southern Baltimore
tracts_nb<-poly2nb(tracts,queen=FALSE, snap = 300)
plot(tracts)

plot(tracts_nb,coordinates(tracts),pch=19,cex=.6,add=TRUE)

# W is the weight matrix of just 1 and 0, showing if each census tract has a tract as its neighbor.
# select style = W to make it row standardized

tracts_nb_w<-nb2listw(tracts_nb,style="W")
W<-listw2mat(tracts_nb_w)


###################################################

# look at the kinds of autocorrelation that exist
lm.LMtests(m4, tracts_nb_w, test = "all")
lm.LMtests(m3, tracts_nb_w, test = "all")
lm.LMtests(m3s, tracts_nb_w, test = "all")


# a single moran's I test for Baltimore life expectancy



moran<-moran.test(tracts$LifeExp11,listw=nb2listw(tracts_nb,style="W" ))
moran

# the moran I p-value is highly significant, suggesting that there is
# spatial clustering!

# make a correlogram of Moran's I, lagged by neighborhood distance
cor<-sp.correlogram(neighbours=tracts_nb,var=tracts$LifeExp11,order=8,method="I",style="W", 
                    zero.policy =TRUE)

plot(cor, main="Moran's I Correlogram of Baltimore Life Expectancy")

# save this corrallorgram

png(file.path("..", "..", "Figures", "Exploratory_Figures","M_I_corr.png"))
plot(cor, main="Moran's I Correlogram of Baltimore Life Expectancy")
dev.off()


# another way to test for spatial clustering. Plot mean adjacent residulas
# vs residuals

png(file.path("..", "..", "Figures",  "Exploratory_Figures", "Residual_Adj_Resid.png"),
    width = 1200, height = 700, pointsize = 20)

par(mfrow=c(1,2))

resnb <- sapply(tracts_nb, function(x) mean(m3$residuals[x]))
cor(residuals(m3), resnb)
plot(residuals(m3), resnb, xlab='Residuals', ylab='Mean adjacent residuals', main = "M3")

resnb <- sapply(tracts_nb, function(x) mean(m3s$residuals[x]))
cor(residuals(m3s), resnb)
plot(residuals(m3s), resnb, xlab='Residuals', ylab='Mean adjacent residuals', main = "M3s")

par(mfrow=c(1,1))
dev.off()

# again, looks like there is a trend

# to account for spatial autocorrelation, use
# spatial simultaneous autoregressive lag model
# estimation. (lagsarlm)


m3_s = lagsarlm(f3, data=tracts, tracts_nb_w)
summary(m3_s)

# compare the unlagged and lagged models
m3 <- lm(f3, data = tracts)
m3_s = lagsarlm(f3, data=tracts, tracts_nb_w)
anova(m3_s, m3)

# looks like the lagged model minimizes the AIC


# look at the Moran's I of the residulas
residuals(m3_s)



cor<-sp.correlogram(neighbours=tracts_nb,var=tracts$LifeExp11,order=8,method="I",style="W", 
                    zero.policy =TRUE)

plot(cor, main="Moran's I Correlogram of Baltimore Life Expectancy")


# looks like Moran's I is no longer significant if the rows
# are stanardized
moran_resid<-moran.test(residuals(m3_s),listw=nb2listw(tracts_nb,style="W" ))
moran_resid

cor<-sp.correlogram(neighbours=tracts_nb,var=residuals(m3_s),order=8,method="I",style="W", 
                    zero.policy =TRUE)
plot(cor, main="Moran's I Correlogram of Baltimore Life Expectancy Residuals")

# save the collalogram

png(file.path("..", "..", "Figures", "Exploratory_Figures", "M_I_R_corr.png"))
plot(cor, main="Moran's I Correlogram of Baltimore Life Expectancy Residuals")
dev.off()




#######################################################################
# apply the model built on a census tract level to a the block level #
# select a training set and a testing set





set.seed(123)
samp<- sample(nrow(tracts), 30)
train_set <- tracts[samp,]
test_set <- tracts[-samp,]

m1 <- lm(f1, data = train_set)
summary(m1)
f1.5 <- LifeExp11 ~ femhhs10_dev +
crime_mi_dev + racdiv10_dev + vacancy_mi_dev + X + Y + mhhi14_dev
m1.5 <- lm(f1.5, data = train_set)
summary(m1.5)

m2 <- lm(f2, data = train_set)
summary(m2)
m3 <- lm(f3, data = train_set)
summary(m3)
m3s <- lm(f3s, data = train_set)
summary(m3s)
m4 <- lm(f4, data = train_set)
summary(m4)

# a more basic model
f5 <- LifeExp11 ~ femhhs10_dev +
    crime_mi_dev + racdiv10_dev + vacancy_mi_dev + X + Y

m5 <- lm(f5, data = train_set)
summary(m5)



p1 <- predict(m1, newdata = test_set)

p1.5 <- predict(m1.5, newdata = test_set)

p2 <- predict(m2, newdata = test_set)
p3 <- predict(m3, newdata = test_set)
p3s <- predict(m3s, newdata = test_set)
p4 <- predict(m4, newdata = test_set)
p5 <- predict(m5, newdata = test_set)




# look at the correlation between predicted life expectancy p1 and the actual life
# expectancy in the test_set

AIC(m1)
AIC(m1.5)
AIC(m2)
AIC(m3)
AIC(m4)
AIC(m3s)
AIC(m5)

rmse(test_set@data$LifeExp11, p1)
rmse(test_set@data$LifeExp11, p1.5)
rmse(test_set@data$LifeExp11, p2)
rmse(test_set@data$LifeExp11, p3)
rmse(test_set@data$LifeExp11, p3s)
rmse(test_set@data$LifeExp11, p4)
rmse(test_set@data$LifeExp11, p5)

cor(p1, test_set@data$LifeExp11)
cor(p1.5, test_set@data$LifeExp11)
cor(p2, test_set@data$LifeExp11)
cor(p3, test_set@data$LifeExp11)
cor(p3s, test_set@data$LifeExp11)
cor(p4, test_set@data$LifeExp11)
cor(p5, test_set@data$LifeExp11)

# looks like p1 has the best predictive ability

# use the same model to predict life expectancy for each block
bp1 <- predict(m1.5, newdata = blocks, type = "response", interval = "predict")
length(bp1)
# make a test blocks dataset to practice
blocks_t<- blocks
blocks_t$bp1 <- bp1[,1]

# the 95% confidence interval of the prediction
blocks_t$bp1_var <- bp1[,3]- bp1[,2]

summary(blocks_t$bp1)
# the 95% confidence interval of the prediction
summary(blocks_t$bp1_var)


par(mfrow=c(1,2))
# plot the outcome of p2 prediction model!!

plotvar0<-blocks_t$bp1
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="pretty")
colcode<-findColours(class,plotclr)
plot(blocks_t)
plot(blocks_t,col=colcode,add=TRUE)
title(main="Baltimore Predicted Life Expectancy by Block") 
mtext(side=3,line=.5, text="")
legend(1390926, 579406, legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Life Expectancy",cex=.7,bty="n")



# plot the variance for comparison

plotvar0<-blocks_t$bp1_var
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="pretty")
colcode<-findColours(class,plotclr)
plot(blocks_t)
plot(blocks_t,col=colcode,add=TRUE)
title(main="Baltimore Predicted Life Expectancy Confidence Interval") 
mtext(side=3,line=.5, text="")
legend(1390926, 579406, legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Confidence Interval",cex=.7,bty="n")

par(mfrow=c(1,1))

# looks like one of the  parameters is creating large errors downtown. 

# see if any of the tract level parameters look out of the ordinary



# looks like downtown has a very high number of liquor stores
# relative to the rest of the tracts. Median is 1.034,Downtown is 8.532 
# for number of businesses that possess Class A liqour licences. 

png(file.path("..", "..", "Figures", "Exploratory_Figures","Liquor_Tracts.png"))

plotvar0<-tracts$liquor11
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="equal")
colcode<-findColours(class,plotclr)
plot(tracts)
plot(tracts,col=colcode,add=TRUE)
title(main="Baltimore Liquor Store Density") 
mtext(side=3,line=.5, text="")
legend(1390926, 579406, legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Liquor Stores",cex=.7,bty="n")

dev.off()

setwd(file.path("..", ".."))
