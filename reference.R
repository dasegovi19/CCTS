### libraries
library(tidyverse)
library(shiny)
library(shinydashboard)





##maps

library(tmap)
library(sf)



#table
library("kableExtra")


# Connect
library(rsconnect)
rsconnect::setAccountInfo(name='davidsegovia123',
                          token='7E3F32AEAA1425661EAC1C87E870A16A',
                          secret='oZCK4PLIQ8nt81C2LSmCgJGjMAeCg6bTuu2i0zGH')


library(here)


# datasets
df_final <- read_csv("df_final copy.csv")
Asian_groups <- read_csv("Asian by groups copy.csv") # asian breakdown by selected groups
AIAN_by_groups <- read_csv("AIAN by groups copy.csv") # native american breakdown by groups
Chicago <- read_csv("Chicago copy.csv") # Chicago break down
chi_shape <- here("geo_export_f70e301e-6f53-4352-8c66-52b53fdc49ed.shp") %>%
  st_read() # Chicago map

# make character to join with map
df_final$cca_num <- as.character(df_final$cca_num) 









#maps 

chicagomap <- left_join(chi_shape, df_final, by= c("area_num_1" = "cca_num"))


#App
choices = c("Total Hispanic or Latino Population" = "HispanicOrLatino_Population", 
            "Total African American/Black Population" = "Black_Population", 
            "Total Asian Population" = "Asian_Population", 
            "Total Native American Population" = "NativeAmerican_Population")


choices2= c("% that are Foreign Born"= "ForeignBorn_percent", 
"% with Limited English Proficiency"= "LimitedEnglishProfiency_Percent", 
"% below the Federal Poverty Level" = "PovertyRate_Percent", 
"% unemployed" = "UnemploymentRate_Percent", 
"% who have food stamps" = "FoodStamps_Percent", 
"% with Severe Rent Burden" = "SeverelyRentBurdened_Percent", 
"% who don't have insurance" ="UninsuredRate_Percent")



