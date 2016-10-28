---
output: pdf_document
---
##Baltimore Life Expectancy Project
Anton Kvit, 10/28/2016
Note: This code was written using a Windows machine, and may have difficulties when read on Mac machines

####Make sure the following R packages are installed:

tidyr, geoR, dplyr, maps, maptools, rgdal, rgeos, sp, spatialEco,
plyr, RColorBrewer, classInt, spatstat, spdep, sp, usdm,readr,
lubridate, rmarkdown.

####Baltimore_Life_Expectancy_Analysis.Rmd
This file contains the final write up and the sources to the rest of the code, and produces the final write up as Baltimore_Life_Expectancy_Analysis.pdf, contained in the Text folder. 

####Supplementary_Analysis.Rmd
This file contains the code and figures produced in all of the exploratory analysis. These are summarized in the Supplementary_Analysis.pdf, contained in the Text folder. 

####create_directories.R
This file contains code to create all the necessary directories

####Get_raw_data.R
This file contains code to get all the raw data and shapefiles from various sources

####Clean_Point_Data.R
This file contains code to clean the raw data, and prepare data for merging using ArcGIS 10.4.1

####Exploratory_1.R
This file contains code for initial exploratory analysis, some Moran's I plots to determine whether there is spatial autocorrelation in the model, and some analyses to determine which parameters should be used in the final model. This file contains all the exploratory analysis mentioned in the final write up, and the other two exploratory.R files are included just in case. 

####Exploratory_2_large_model.R
This file contains code for a spatial simultaneous autorgressive lag model to account for spatial
autocorrelation, that ended up being too big to be handeled by the machine this code was written on

####Exploratory_3_Moran_I.R
This file contains code for a very early exploratory analysis, looking at spatial autocorrelation

####Final_Analysis.R
This file contains code for the final model as well as the final figures used in the write up

\newpage

###To reproduce the results of the Baltimore Life Expectancy Analysis:
Final_Analysis.R can be run in order to reproduce the final results and figures used in the write-up. It will use the shapefiles from the Analyzed_Data/ArcGIS folder, that were obtained from merging files in ArcGIS 10.4.1. The steps taken for this merging are outlined in the appendix at the end of the write-up.

Note: Due to the large size of the ArcGIS shapefiles and raw data (~1 GB), they were not uploaded to GitHub, but are available via a DropBox link.

Exploratory_1.R can be run in order to reproduce the additional exploratory figures, that are described in the Supplementary_Analysis.pdf. 

create_directories.R, Get_raw_data.R, and Clean_Point_Data.R can be run in order to download all the data necessary for this analysis, and prepare it for merging in ArcGIS 10.4.1
