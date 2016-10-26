setwd(file.path(".", "Analyzed_Data/ArcGIS"))


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


# add centroid coordinates to account for spatial correlation
t_coords<- as.data.frame(gCentroid(tracts, byid = TRUE))
b_coords<- as.data.frame(gCentroid(blocks, byid = TRUE))

tracts$X <- t_coords$x
tracts$Y <- t_coords$y
blocks$X <- b_coords$x
blocks$Y <- b_coords$y



# replace any zero values in the blocks dataset with the
# mean values, in order to take care of the few blocks
# that received no values from the tracts shapefile 
# 9 blocks have zero values


blocks$liquor11[blocks$liquor11 == 0] <- mean(tracts$liquor11)
blocks$fastfd11[blocks$fastfd11 == 0] <- mean(tracts$fastfd11)
blocks$racdiv10[blocks$racdiv10 == 0] <- mean(tracts$racdiv10)
blocks$femhhs10[blocks$femhhs10 == 0] <- mean(tracts$femhhs10)
blocks$mhhi14[blocks$mhhi14 == 0] <- mean(tracts$mhhi14)
blocks$paa10[blocks$paa10 == 0] <- mean(tracts$paa10)
blocks$LifeExp11[blocks$LifeExp11 == 0] <- mean(tracts$LifeExp11)


#blocks$crime[blocks$crime == 0] <- mean(blocks$crime)
#blocks$vacancy[blocks$vacancy == 0] <- mean(blocks$vacancy)
#blocks$StateTax[blocks$StateTax == 0] <- mean(blocks$StateTax)

# in case it is needed, narrow down the variables in the data.frame to only what you need:


if(FALSE){

# select only the necessary columns
vars <- c("BLK2010", "SQ_MILES", "crime", "vacancy", 
          "CityTax", "StateTax", "estate", "CSA2010",
          "birthwt10", "birthwt11",
          "birthwt12", "birthwt13", "birthwt14", 
          "ebll10", "ebll11", "ebll12", "ebll13", "ebll14",
          "LifeExp11", "LifeExp12", "LifeExp13", "LifeExp14",
          "tpop10", "male10", "female10", "paa10", "pwhite10",
          "mhhi14", "hhpov14")
          

blocks_c <- blocks[,(names(blocks) %in% vars)] 


vars_t <- c("crime","vacancy",  
          "CityTax", "StateTax", "estate", "CSA2010",
          "birthwt10", "birthwt11",
          "birthwt12", "birthwt13", "birthwt14", 
          "ebll10", "ebll11", "ebll12", "ebll13", "ebll14",
          "LifeExp11", "LifeExp12", "LifeExp13", "LifeExp14",
          "tpop10", "male10", "female10", "paa10", "pwhite10",
          "mhhi14", "hhpov14")

tracts_c<- tracts[,(names(tracts) %in% vars_t)]

}



par(mfrow=c(1,2))
# plot 2011 life expectancy by block


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

###########################################################
# look at the types of data that is avaliable in each block

plot(blocks@data$BLK2010, blocks@data$StateTax)

plot(blocks_t$BLK2010, blocks_t$crime, col = "red")
plot(blocks_t$BLK2010, blocks_t$vacancy, col = "green")

summary(blocks@data$StateTax)

# state tax has some extreme outliers, and an enormous range

summary(blocks@data$crime)
summary(blocks@data$vacancy)

summary(tracts@data$crime)
summary(tracts@data$vacancy)


# the distributions within the blocks and within the tracts seem to be
# very different, making a comparison more difficult. 


# look at crime vs. area
plot(tracts@data$tract_area, tracts@data$crime)
plot(blocks@data$block_area, blocks@data$crime)
cor(tracts@data$tract_area, tracts@data$crime)
cor(blocks@data$block_area, blocks@data$crime)
# there doesn't seem to be an obvious relationship between these values
# and area


############################################################
# get count variables like crime and vacancy per square mile
tracts$crime_mi <- tracts$crime/tracts$tract_area
tracts$vacancy_mi <- tracts$vacancy/tracts$tract_area
blocks$crime_mi <- blocks$crime/blocks$block_area
blocks$vacancy_mi <- blocks$vacancy/blocks$block_area

############################################################
# do some regression at the tract level first 

f1 <- LifeExp14 ~ crime_mi + CityTax + StateTax + vacancy_mi
m1 <- lm(f1, data = tracts)
summary(m1)

# looks like city tax and crime might be related with life expectancy
f2 <- LifeExp14 ~ crime_mi + CityTax
m2 <- lm(f2, data = tracts)
summary(m2)

f3 <- LifeExp14 ~ crime_mi + CityTax + paa10 + hhpov14
m3 <- lm(f3, data = tracts)
summary(m3)

# looks like crime and precent African American correlate
# the most with life expecancy
f4 <- LifeExp14 ~ paa10 + crime_mi
m4 <- lm(f4, data = tracts)
summary(m4)



# plot the residuals of m4

plotvar0<- residuals(m4)
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="equal")
colcode<-findColours(class,plotclr)
plot(tracts)
plot(tracts,col=colcode,add=TRUE)
title(main="M4 residuals") 
legend(1390926, 577406,legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Residuals",cex=.7,bty="n")


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

####################################################
# create neighborhood weights for blocks as well
#blocks_nb <- poly2nb(blocks,queen=FALSE)
#plot(blocks)
#plot(blocks_nb,coordinates(blocks),pch=19,cex=.6,add=TRUE)
#blocks_nb_w<-nb2listw(blocks_nb,style="W")
#W2<-listw2mat(blocks_nb_w)
###################################################

# look at the kinds of autocorrelation that exist
lm.LMtests(m4, tracts_nb_w, test = "all")


# a single moran's I test for Baltimore life expectancy


moran<-moran.test(tracts$LifeExp14,listw=nb2listw(tracts_nb,style="W" ))
moran

# the moran I p-value is highly significant, suggesting that there is
# spatial clustering!

# make a correlogram of Moran's I, lagged by neighborhood distance
cor<-sp.correlogram(neighbours=tracts_nb,var=tracts$LifeExp14,order=8,method="I",style="W", 
                    zero.policy =TRUE)

plot(cor, main="Moran's I Correlogram of Baltimore Life Expectancy")

# save this corrallorgram

png(file.path("..", "..", "Figures", "M_I_corr.png"))
plot(cor, main="Moran's I Correlogram of Baltimore Life Expectancy")
dev.off()


# another way to test for spatial clustering. Plot mean adjacent residulas
# vs residuals

resnb <- sapply(tracts_nb, function(x) mean(m4$residuals[x]))
cor(residuals(m4), resnb)
plot(residuals(m4), resnb, xlab='Residuals', ylab='Mean adjacent residuals')

# again, looks like there is a trend

# to account for spatial autocorrelation, use
# spatial simultaneous autoregressive lag model
# estimation. (lagsarlm)


m4_s = lagsarlm(f4, data=tracts, tracts_nb_w)
summary(m4_s)

# compare the unlagged and lagged models
m4 <- lm(f4, data = tracts)
m4_s = lagsarlm(f4, data=tracts, tracts_nb_w)
anova(m4_s, m4)

# looks like the lagged model minimizes the AIC


# look at the Moran's I of the residulas
residuals(m4_s)

# looks like Moran's I is no longer significant if the rows
# are stanardized
moran_resid<-moran.test(residuals(m4_s),listw=nb2listw(tracts_nb,style="W" ))
moran_resid

cor<-sp.correlogram(neighbours=tracts_nb,var=residuals(m4_s),order=8,method="I",style="W", 
                    zero.policy =TRUE)
plot(cor, main="Moran's I Correlogram of Baltimore Life Expectancy Residuals")

# save the collalogram
png(file.path("..", "..", "Figures", "M_I_R_corr.png"))
plot(cor, main="Moran's I Correlogram of Baltimore Life Expectancy Residuals")
dev.off()


# plot the residuals
resnb <- sapply(tracts_nb, function(x) mean(m4_s$residuals[x]))
cor(residuals(m4_s), resnb)
plot(residuals(m4_s), resnb, xlab='Residuals', ylab='Mean adjacent residuals')

# plot the residuals of m4_s and m4 side by side
# looks like doing a lagged model accounts for spatial autocorrelation

par(mfrow=c(1,2))

plotvar0<- residuals(m4_s)
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="equal")
colcode<-findColours(class,plotclr)
plot(tracts)
plot(tracts,col=colcode,add=TRUE)
title(main="M4_s residuals") 
legend(1390926, 577406,legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Residuals",cex=.7,bty="n")

plotvar0<- residuals(m4)
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="equal")
colcode<-findColours(class,plotclr)
plot(tracts)
plot(tracts,col=colcode,add=TRUE)
title(main="M4 residuals")
legend(1390926, 577406,legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Residuals",cex=.7,bty="n")

par(mfrow=c(1,1))





### looking at other potential variables ###
View(tracts)


f5 <- LifeExp11 ~ crime_mi + femhhs10  + liquor11  + CityTax

m5_s = lagsarlm(f5, data=tracts, tracts_nb_w)
summary(m5_s)




# chec for collinearity. Select variables you want to check

vars <- c("paa10", "racdiv10", "crime_mi", "vacancy_mi", 
          "CityTax", "StateTax", "liquor11",
          "fastfd11", "mhhi14", "hhpov14",
          "femhhs10")


df <- tracts@data[,(names(tracts) %in% vars)] 

# look at the collinearity using vif, leave only those below 10
vif(df)
b<- vifstep(df, th=10)
b

# only variables with VIF less then 10 remain, which include: liquor11, fastfd11, racdiv10, femhhs10,
#mhhi14, hhpov14, crime, city tax, and vacancy




f6 <- LifeExp11 ~  fastfd11 + racdiv10 + femhhs10 +
    mhhi14 + crime + StateTax + vacancy

m6_s = lagsarlm(f6, data=tracts, tracts_nb_w)
summary(m6_s)



AIC(m4_s)
AIC(m5_s)
AIC(m6_s)


### lots of different variables. Look into which predictors are the best!!



### STANDARDIZE! ##
# divide things by variance
# get the deviation from the median

View(tracts)
View(blocks)

summary(tracts@data$LifeExp11)
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

tracts$crime_mi_dev <- tracts$crime_mi - mean(tracts$crime_mi)
tracts$crime_mi_dev <- tracts$crime_mi_dev/var(tracts$crime_mi_dev)

tracts$crime_dev <- tracts$crime - mean(tracts$crime)
tracts$crime_dev <- tracts$crime_dev/var(tracts$crime_dev)

tracts$StateTax_dev <- tracts$StateTax - mean(tracts$StateTax)
tracts$StateTax_dev <- tracts$StateTax_dev/var(tracts$StateTax_dev)

tracts$vacancy_mi_dev <- tracts$vacancy_mi - mean(tracts$vacancy_mi)
tracts$vacancy_mi_dev <- tracts$vacancy_mi_dev/var(tracts$vacancy_mi_dev)

## do the same at the block level

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

blocks$crime_mi_dev <- blocks$crime_mi - mean(blocks$crime_mi)
blocks$crime_mi_dev <- blocks$crime_mi_dev/var(blocks$crime_mi_dev)

blocks$crime_dev <- blocks$crime - mean(blocks$crime)
blocks$crime_dev <- blocks$crime_dev/var(blocks$crime_dev)

blocks$StateTax_dev <- blocks$StateTax - mean(blocks$StateTax)
blocks$StateTax_dev <- blocks$StateTax_dev/var(blocks$StateTax_dev)

blocks$vacancy_mi_dev <- blocks$vacancy_mi - mean(blocks$vacancy_mi)
blocks$vacancy_mi_dev <- blocks$vacancy_mi_dev/var(blocks$vacancy_mi_dev)




#######################################################################
# apply the model built on a census tract level to a the block level #
# select a training set and a testing set

set.seed(123)
samp<- sample(nrow(tracts), 30)

train_set <- tracts[samp,]
test_set <- tracts[-samp,]


#f7 <- LifeExp11 ~ liquor11_dev + fastfd11_dev + racdiv10_dev + femhhs10_dev +
   # mhhi14_dev + crime_mi_dev + vacancy_mi_dev


f7 <- LifeExp11 ~ femhhs10_dev +
    crime_mi_dev + racdiv10_dev + vacancy_mi_dev + X + Y

m7 = lm(f7, data=train_set)
summary(m7)


p1 <- predict(m7, newdata = test_set)

# look at the correlation between predicted life expectancy p1 and the actual life
# expectancy in the test_set
cor(p1, test_set@data$LifeExp11)



# use the same model to predict life expectancy for each block
p2 <- predict(m7, newdata = blocks, type = "response", interval = "predict")
length(p2)
# make a test blocks dataset to practice
blocks_t<- blocks
blocks_t$p2 <- p2[,1]

# the 95% confidence interval of the prediction
blocks_t$p2_var <- p2[,3]- p2[,2]

summary(blocks_t$p2)
# the 95% confidence interval of the prediction
summary(blocks_t$p2_var)


plot(blocks_t$BLK2010, blocks_t$p2)
points(blocks_t$BLK2010, blocks_t$crime_mi, col = "red")
points(blocks_t$BLK2010, blocks_t$vacancy_mi, col = "green")


par(mfrow=c(1,2))
# plot the outcome of p2 prediction model!!

plotvar0<-blocks_t$p2
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



# plot life expectancy by census tract for comparison

plotvar0<-blocks_t$p2_var
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="quantile")
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

summary(tracts$liquor11)

plotvar0<-tracts$liquor11_dev
nclr<-5
plotclr<-brewer.pal(nclr,"YlOrBr")
class<-classIntervals(plotvar0,nclr,style="equal")
colcode<-findColours(class,plotclr)
plot(tracts)
plot(tracts,col=colcode,add=TRUE)
title(main="Baltimore Predicted Life Expectancy Confidence Interval") 
mtext(side=3,line=.5, text="")
legend(1390926, 579406, legend=names(attr(colcode, "table")), 
       fill=attr(colcode,"palette"),title="Confidence Interval",cex=.7,bty="n")




#########################################################################
# a sarlm prediction model that takes a very long time


f7 <- LifeExp11 ~ liquor11 + fastfd11 + racdiv10 + femhhs10 +
    mhhi14 + crime_mi + StateTax


m7_s = lagsarlm(f7, data=tracts, tracts_nb_w)
summary(m7_s)


p1 <- predict.sarlm(m7_s, listw = blocks_nb_w, newdata = blocks)




########################################################
# No training dataset



f7 <- LifeExp11 ~ liquor11  + fastfd11 + racdiv10 + femhhs10 +
    mhhi14 + crime + vacancy + paa10

m8 = lm(f7, data=tracts)
summary(m8)
p3 <- predict(m8, newdata = blocks, type = "response")

summary(p3)



par(mfrow=c(1,2))


# object = m6_s
# weights = tracts_nb_w

predict.sarlm(m6_s, listw = tracts_nb_w)


a<- predict.sarlm(tracts)

View(blocks)


setwd(file.path(".."))
