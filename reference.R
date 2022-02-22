### libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(scales) 
library(plotly) # delete






##maps

library(tmap)
library(sf)




#table
library("kableExtra")


# Connect
library(rsconnect)
rsconnect::setAccountInfo(name=
                          token=
                          secret=)


library(here)


# datasets
df_final <- read_csv("df_final_2 copy.csv")
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
            "% unemployed " = "UnemploymentRate_Percent", 
            "% who have food stamps" = "FoodStamps_Percent", 
            "% with Severe Rent Burden" = "SeverelyRentBurdened_Percent", 
            "% who don't have insurance" ="UninsuredRate_Percent",
            "Child Opportunity Index(Composite Index scored from 1-5)" = "Child_Opportunity_Index2.0",
            "% who are Infants(0-4 Years)" = "Infants_0_4Years_Percent",
            "% who are Juveniles(5-17 Years)" = "Juveniles_5_17Years_Percent",
            "% who are Young Adults(18-39 Years)" = "YoungAdults_18_39Years_Percent",
            "% who are Middle Aged Adults(40-64 Years)" =  "MiddleAgedAdults_40_64Years_Percent",
            "% who are Seniors(65+ Years)" = "Seniors_65_and_older_Percent")


labels = c('Hispanic or Latino','Black or African American','White','Native American', 'Asian','Native Hawaiian/Pacific Islander',
             'Other race or 2+ races')
  
values = c(819518,787551,863622,3332,189857, 529, 81979)
  


#rsconnect::configureApp("CCTS", size="xlarge")




