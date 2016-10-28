# install all the necessary packages

list.of.packages <- c("tidyr", "geoR", "dplyr", "maps", "maptools", "rgdal", "rgeos", "sp", "spatialEco",
                      "plyr", "RColorBrewer", "classInt", "spatstat", "spdep", "sp", "usdm","readr",
                      "lubridate", "rmarkdown", "leaps", "MASS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


if (!dir.exists("Raw_Data"))
{
    dir.create("Raw_Data")
}


if (!dir.exists("Analyzed_Data"))
{
    dir.create("Analyzed_Data")
}

if (!dir.exists(file.path("Analyzed_Data","ArcGIS")))
{
    dir.create(file.path("Analyzed_Data", "ArcGIS"))
}


if (!dir.exists("R_code"))
{
    dir.create("R_code")
}

if (!dir.exists("Figures"))
{
    dir.create("Figures")
}

if (!dir.exists(file.path("Figures","Exploratory_Figures")))
{
    dir.create(file.path("Figures","Exploratory_Figures"))
}

if (!dir.exists(file.path("Figures","Final_Figures")))
{
    dir.create(file.path("Figures","Final_Figures"))
}


if (!dir.exists("Text"))
{
    dir.create("Text")
}
