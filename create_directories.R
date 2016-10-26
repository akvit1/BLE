

if (!dir.exists("Raw_Data"))
{
    dir.create("Raw_Data")
}


if (!dir.exists("Analyzed_Data"))
{
    dir.create("Analyzed_Data")
}

if (!dir.exists(file.path("Analyzed_Data/ArcGIS")))
{
    dir.create(file.path("Analyzed_Data/ArcGIS"))
}


if (!dir.exists("R_code"))
{
    dir.create("R_code")
}

if (!dir.exists("Figures"))
{
    dir.create("Figures")
}

if (!dir.exists("Figures/Exploratory_Figures"))
{
    dir.create("Figures/Exploratory_Figures")
}

if (!dir.exists("Figures/Final_Figures"))
{
    dir.create("Figures/Final_Figures")
}


if (!dir.exists("Text"))
{
    dir.create("Text")
}
