## @knitr Get_raw_data

packages<- c("downloader", "readr", "lubridate")
sapply(packages, require, character.only = T)

Census_Demographics_2010_2014 <- "https://data.baltimorecity.gov/api/views/t7sb-aegk/rows.csv?accessType=DOWNLOAD"
Children_and_Family_Health_2010_2014 <- "https://data.baltimorecity.gov/api/views/rtbq-mnni/rows.csv?accessType=DOWNLOAD"
Housing_and_Community_2010_2014 <- "https://data.baltimorecity.gov/api/views/mvvs-32jm/rows.csv?accessType=DOWNLOAD"
Real_Estate_Taxes <- "https://data.baltimorecity.gov/api/views/27w9-urtv/rows.csv?accessType=DOWNLOAD"
Vital_13_Codebook <- "https://data.baltimorecity.gov/api/views/bded-bhdg/rows.csv?accessType=DOWNLOAD"
# download the tabular data

download(url = Census_Demographics_2010_2014, destfile = file.path("Raw_Data", "demographics.csv"))
download_date <- now()
download(Children_and_Family_Health_2010_2014, destfile = file.path("Raw_Data", "fam_health.csv"))
download_date[2] <- now()
download(Housing_and_Community_2010_2014, destfile = file.path("Raw_Data", "housing.csv"))
download_date[3] <- now()
download(Real_Estate_Taxes, destfile = file.path("Raw_Data", "real_estate.csv"))
download_date[4] <- now()


# get the codebook

download(Vital_13_Codebook, destfile = file.path("Raw_Data", "vital_13_codebook.csv"))
download_date[5] <- now()

# download the shapefiles

shape_Census_Demographics_2010_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Census.zip"
shape_Children_and_Family_Health_2010_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Health.zip"
shape_Crime_2010_2014 <- "http://bniajfi.org/wp-content/uploads/2016/04/VS-14-Crime.zip"
#real_property<-"http://gisdata.baltimorecity.gov/datasets/b41551f53345445fa05b554cd77b3732_0.zip"
shape_blocks_2010 <- "http://www.mdp.state.md.us/msdc/census/cen2010/maps/tiger10/blk2010.zip"
point_crime <- "https://data.baltimorecity.gov/api/views/u2bu-g36y/rows.csv?accessType=DOWNLOAD"
point_vacancy <- "https://data.baltimorecity.gov/api/views/qqcv-ihn5/rows.csv?accessType=DOWNLOAD"
    
    


download(url = shape_Census_Demographics_2010_2014, destfile = file.path("Raw_Data", "demographics_shape.zip"))
download_date[6] <- now()
download(shape_Children_and_Family_Health_2010_2014, destfile = file.path("Raw_Data", "fam_health_shape.zip"))
download_date[7] <- now()
download(shape_Crime_2010_2014, destfile = file.path("Raw_Data", "crime_shape.zip"))
download_date[8] <- now()
#download(real_property, destfile = file.path("Raw_Data", "real_property.zip"))
download_date[9] <- now()
download(shape_blocks_2010, destfile = file.path("Raw_Data", "shape_blocks_2010.zip"))
download_date[10] <- now()

download(point_crime, destfile = file.path("Raw_Data", "point_crime.csv"))
download_date[11] <- now()
download(point_vacancy, destfile = file.path("Raw_Data", "point_vacancy.csv"))
download_date[12] <- now()


# unzip the shapefiles
unzip(file.path("Raw_Data", "demographics_shape.zip"), exdir = file.path("Raw_Data"))
unzip(file.path("Raw_Data", "fam_health_shape.zip"), exdir = file.path("Raw_Data"))
unzip(file.path("Raw_Data", "crime_shape.zip"), exdir = file.path("Raw_Data"))
#unzip(file.path("Raw_Data", "real_property.zip"), exdir = file.path("Raw_Data"))
unzip(file.path("Raw_Data", "shape_blocks_2010.zip"), exdir = file.path("Raw_Data"))



