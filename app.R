source("reference.R")


ui <-  dashboardPage(
  skin = "blue",
  dashboardHeader(title = "UIC's Center for Clinical and Translational Sciences (CCTS) recruitment and retention resource page",
                  titleWidth = 1200),  # close header
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Maps/Tables", tabName = "map_tab", icon = icon("map")),
      menuItem("Breakdown of Chicago", tabName = "table_tab1", icon = icon("tablet")),
      menuItem("Breakdown of Asian Population", tabName = "table_tab2", icon = icon("tablet")),
      menuItem("Breakdown of Native American Pop", tabName = "table_tab3", icon = icon("tablet")),
      menuItem("Sources", tabName = "source_tab", icon = icon("tablet"))
    ) # close side bar Menu
  ), # Close SideBar
  
  dashboardBody(
    tabItems(
      
      
      ## map/table item
      tabItem(
        tabName = "map_tab",
        titlePanel("Welcome to the CCTS resource page. These tables, charts, and maps examine underrepresented groups in Chicago and the socioeconomic characteristics that exist in each community area."),
        fluidRow(
          box(
            title = "Data Source: 2020 Census",
            status = "primary",
            width = 3,
            selectInput(
              inputId = "choices",
              label = "Select Under-represented Group",
              choices = choices,
              selected = NULL),# close input
          ),# close box
          box(
            title = "Under-represented groups in this population map are limited to Hispanic/Latino, African Americans, Native Americans, and Asian Americans.",
            width = 9,
            tmapOutput("demographicmap")
          )#close 2nd box
        ),#close fluid row
        
        fluidRow(
          box(
            title = "Data Sources: (1) American Community Survey 5 year estimates 2015-2019, 
            (2) diversitydatakids.org",
            status = "primary",
            width = 3,
            selectInput(
              inputId = "choices2",
              label = "Select indicator",
              choices = choices2,
              selected = NULL),# close input
          ),# close box
          box(
            title = "This map allows you to select the social determinants of health and age groups in each community area.",
            width = 9,
            tmapOutput("healthmap")
          )#close 2nd box
        ),#close fluid row, 
        
        ## table
        fluidRow(
          box(
            title = "Please select a region to view the values from the 2 maps in table format",
            status = "primary",
            width = 3,
            selectInput(
              inputId= "region_id",
              label= "Select region",
              choices = df_final$region %>% 
                as.list(),
              selected = "Far North Side") # close input
          ), # close 1st box
          box(
            title = "",
            width = 20,
            tableOutput("table3")
          ) # close 2nd box
        ), # close 2nd fluid row
        
        
        
      ),# close map tab
      
      
      
      
      
      ## start census tab
      tabItem(
        tabName = "table_tab1",
        titlePanel(" This pie chart and table details races and ethnicities within Chicago, including individuals who identify as two or more races."),
        
        ## table/plotly
        fluidRow(
          box(
            title = "Chicago breakdown",
            status = "primary",
            width = 3,
          ), # close 1st box
          
          box(
            width = 20,
            plotlyOutput("plot")),
          box(
            title = "Chicago's Race/Ethnicity from the 2020 Census",
            width = 20,
            tableOutput("chicagotable")
          ) # close 2nd box
        ) # close fluid row
      ), # close table1 tab
      
      
      ## start Asian tab
      tabItem(
        tabName = "table_tab2",
        titlePanel(" This bargraph breaks down the Asian American community in Chicago into selected groups. Check out the table below, which also includes the margin of error."),
        
        ### bargraph
        fluidRow(
          box(
            title = "Asian Breakdown into selected groups",
            width = 12,
            plotOutput("bargraph")
          ) # close box
        ), # close fluid row
        
        ###
        ## table
        fluidRow(
          box(
            title = "Data source: American Community Survey 2015-2019",
            status = "primary",
            width = 3,
          ), # close 1st box
          box(
            title = "Data source: American Community Survey 2015-2019 ",
            width = 20,
            tableOutput("asiantable")
          ) # close 2nd box
        ) # close fluid row
        
      ), # close Asian tab
      
      ## start Native American tab
      tabItem(
        tabName = "table_tab3",
        titlePanel(" This bargraph also breaks down the Native American community into selected groups. Check out the table below, which includes the estimated value with the margin of error."),
        
        ### bargraph
        fluidRow(
          box(
            title = "Native American Breakdown into selected groups",
            width = 12,
            plotOutput("bargraph2")
          ) # close box
        ), # close fluid row
        
        ## table
        fluidRow(
          box(
            title = "",
            status = "primary",
            width = 3,
          ), # close 1st box
          box(
            title = "Data source: American Community Survey 2015-2019",
            width = 20,
            tableOutput("nativeamericantable")
          ) # close 2nd box
        ) # close fluid row
        
      ), # close table3 tab     
      
      
      
      
      tabItem(
        tabName = "source_tab",
        titlePanel(" Sources"),
        
        
        p(tags$a(href="https://www.chicagohealthatlas.org/", 
                 " Chicago Health Atlas")), 
        
        p(tags$a(href="https://www.census.gov/programs-surveys/acs/", 
                 "American Community Survey 5 year estimates 2015-2019")), 
        
        p(tags$a(href="https://www.diversitydatakids.org/", 
                 "Diversity Data Kids: Child Opportunity Index")),
        
        p(tags$a(href="https://www.census.gov/programs-surveys/decennial-census/decade/2020/2020-census-main.html", 
                 "2020 U.S. Census"))
        
        
        
      )
    ) # closes tab Items
  ) # closes dashboard Body
) # closes dashboard Page



server <- function(input, output, session) {
  
  
  
  
  
  ### 1st map
  output$demographicmap <- renderTmap({
    tm_shape(chicagomap) +
      tm_polygons(choices[1], zindex = 401)
  })
  
  observe({
    tmapProxy("demographicmap", session, {
      if(input$choices == "HispanicOrLatino_Population")
        tm_remove_layer(401) +
        tm_shape(chicagomap) +
        tm_polygons(col = "HispanicOrLatino_Population", id= "Name", zindex = 401, title = "Total Hispanic/Latino Population", popup.vars = c("Total # of Hispanics/Latinos: " = "HispanicOrLatino_Population"))
      
      
      else if(input$choices == "Black_Population") {
        tm_remove_layer(401) +
          tm_shape(chicagomap) +
          tm_polygons(col = "Black_Population", id= "Name", zindex = 401, title = "Total African American Population", popup.vars = c("Total # of African Americans: " = "Black_Population"))
        
      }
      
      else if(input$choices == "Asian_Population") {
        tm_remove_layer(401) +
          tm_shape(chicagomap) +
          tm_polygons(col = "Asian_Population", id= "Name", zindex = 401, title = "Total Asian Population", popup.vars = c("Total # of Asian Americans: " = "Asian_Population"))
        
      }
      
      
      else if(input$choices == "NativeAmerican_Population") {
        tm_remove_layer(401) +
          tm_shape(chicagomap) +
          tm_polygons(col = "NativeAmerican_Population", id= "Name", zindex = 401, title = "Total Native American Population", popup.vars = c("Total # of Native Americans: " = "NativeAmerican_Population"))
        
      }
      
    })
  })
  
  
  
  
  output$healthmap <- renderTmap({
    tm_shape(chicagomap) +
      tm_polygons(choices2[1], zindex = 401)
  })
  
  observe({
    tmapProxy("healthmap", session, {
      if(input$choices2 == "ForeignBorn_percent") {
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "ForeignBorn_percent", id = "Name", zindex = 401, title = "% of residents who were not U.S. citizens at the time of birth (includes both naturalized citizens and those who are not currently citizens)", popup.vars = c( "% that are Foreign Born: " = "ForeignBorn_percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
        
        
      } else if(input$choices2 == "LimitedEnglishProfiency_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "LimitedEnglishProfiency_Percent", id = "Name", zindex = 401, title = "% of residents 5 years and older who do not speak English 'very well'", popup.vars = c( "% that have limited English Proficiency: " = "LimitedEnglishProfiency_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      else if(input$choices2 == "PovertyRate_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "PovertyRate_Percent", id = "Name", zindex = 401, title = "% of residents in families below the Federal Poverty Level", popup.vars = c( "% of residents in poverty: " = "PovertyRate_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      else if(input$choices2 == "UnemploymentRate_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "UnemploymentRate_Percent", id = "Name", zindex = 401, title = "% of residents in 16+ actively seeking employment", popup.vars = c( "% of residents unemployed: " = "UnemploymentRate_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      else if(input$choices2 == "FoodStamps_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "FoodStamps_Percent", id = "Name", zindex = 401, title = "% of households actively receiving Supplemental Nutrition Assistance Program (SNAP) benefits over the past 12 months ", popup.vars = c( "% of households with Food Stamps: " = "FoodStamps_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      else if(input$choices2 == "SeverelyRentBurdened_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "SeverelyRentBurdened_Percent", id = "Name", zindex = 401, title = "% of Households spending more than 50% of income on rent ", popup.vars = c( "% of renter-occupied housing units: " = "SeverelyRentBurdened_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      else if(input$choices2 == "UninsuredRate_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "UninsuredRate_Percent", id = "Name", zindex = 401, title = "% of residents without health insurance ", popup.vars = c( "% of residents without health insurance: " = "UninsuredRate_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      else if(input$choices2 == "Child_Opportunity_Index2.0")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "Child_Opportunity_Index2.0", id = "Name", zindex = 401, title = "A composite index score  that measures neighborhood resources and conditions that are important for children's healthy development: scored as Very Low (5), Low (4), Moderate (3), High (2), and Very High (1) ", 
                      popup.vars = c( "Composite index score: " = "Child_Opportunity_Index2.0"))
        
        
        
      }
      
      else if(input$choices2 == "Infants_0_4Years_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "Infants_0_4Years_Percent", id = "Name", zindex = 401, title = "% of residents that are infants(between 0-4 years)", popup.vars = c( "% that are infants: " = "Infants_0_4Years_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      else if(input$choices2 == "Juveniles_5_17Years_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "Juveniles_5_17Years_Percent", id = "Name", zindex = 401, title = "% of residents that are juveniles(between 5-17 years)", popup.vars = c( "% that are juveniles: " = "Juveniles_5_17Years_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      else if(input$choices2 == "YoungAdults_18_39Years_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "YoungAdults_18_39Years_Percent", id = "Name", zindex = 401, title = "% of residents that are Young Adults(between 18-39 years)", popup.vars = c( "% that are young adults: " = "YoungAdults_18_39Years_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      
      else if(input$choices2 == "MiddleAgedAdults_40_64Years_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "MiddleAgedAdults_40_64Years_Percent", id = "Name", zindex = 401, title = "% of residents that are Middlle-aged Adults(between 40-64 years)", popup.vars = c( "% that are middle aged adults: " = "MiddleAgedAdults_40_64Years_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      
      else if(input$choices2 == "Seniors_65_and_older_Percent")  {
        
        tm_remove_layer(401) + 
          tm_shape(chicagomap) + 
          tm_polygons(col = "Seniors_65_and_older_Percent", id = "Name", zindex = 401, title = "% of residents that are Senior Adults(65+ years)", popup.vars = c( "% that are senior adults: " = "Seniors_65_and_older_Percent"),
                      legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) + 
          tm_layout(legend.format = list(fun = function(x) { paste0(formatC(x, digits = 0, format = "f"), "%") })) 
        
        
        
      }
      
      
      
      
      
    })
    
    
  })
  
  
  
  ### end map
  
  
  
  ### table
  
  
  
  output$asiantable <- function() {
    
    dfasian <- Asian_groups %>%
      mutate_each(funs(prettyNum(., big.mark=",")))
    
    
    knitr::kable(dfasian, 
                 col.names = c("Asian group", "Total", "Margin of Error")) %>%
      kableExtra::kable_styling(fixed_thead = TRUE)
  }
  
  
  
  output$nativeamericantable <- function() {
    
    dfaian <- AIAN_by_groups %>%
      mutate_each(funs(prettyNum(., big.mark=",")))
    
    knitr::kable(dfaian, 
                 col.names = c("Native American group", "Total", "Margin of Error")) %>%
      kableExtra::kable_styling(fixed_thead = TRUE) %>%
      pack_rows("American Indian Tribes specified", 2, 43, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("Alaska Native Tribes Specified", 46, 52, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("Alaska Native Tribes Not Specified", 53, 54, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("American Indian or Alaska Native Tribes Not Specified", 54, 55, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("Two or more American Indian or Alaska Native Tribes", 55, 55, label_row_css = "background-color: #666; color: #fff;") 
  }
  
  
  output$chicagotable <- function() {
    
    dfchicago <- Chicago %>%
      mutate_each(funs(prettyNum(., big.mark=","))) ## adds commas
    
    
    knitr::kable(dfchicago, 
                 col.names = c("Race/Ethnicity", "Total")) %>%
      kableExtra::kable_styling(fixed_thead = TRUE)  %>%
      pack_rows("Total population that doesn't identify as Hispanic or Latino", 3, 3, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("Population of one race", 4, 10, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("Population of two or more races", 11, 11, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("Population of two races", 12, 27, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("Population of three races", 28, 48, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("Population of four races", 49, 64, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("Population of five races", 65, 71, label_row_css = "background-color: #666; color: #fff;") %>%
      pack_rows("Population of six races", 72, 73, label_row_css = "background-color: #666; color: #fff;")
    
    
    
  }
  
  
  
  
  
  ### end table
  
  ### last table
  
  ### table
  output$table3 <- function() {
    df <- df_final %>%
      filter(region == input$region_id) %>%
      select(Name, region, Pop_Total, White_Population, Percent_White, HispanicOrLatino_Population, Percent_HispanicOrLatino, Black_Population, Percent_Black, Asian_Population, Percent_Asian, 
             NativeAmerican_Population, Percent_NativeAmerican, ForeignBorn_percent, LimitedEnglishProfiency_Percent, PovertyRate_Percent, UnemploymentRate_Percent,
             FoodStamps_Percent, SeverelyRentBurdened_Percent, UninsuredRate_Percent, Child_Opportunity_Index2.0, Infants_0_4Years_Percent, Juveniles_5_17Years_Percent, YoungAdults_18_39Years_Percent,
             MiddleAgedAdults_40_64Years_Percent, Seniors_65_and_older_Percent) %>%
      mutate_each(funs(prettyNum(., big.mark=",")))
    
    df[, 5] <- paste0(as.matrix(df[,5]), '%')
    df[, 7] <- paste0(as.matrix(df[,7]), '%')
    df[, 9] <- paste0(as.matrix(df[,9]), '%')
    df[, 11] <- paste0(as.matrix(df[,11]), '%')
    df[, 13] <- paste0(as.matrix(df[,13]), '%')
    df[, 14] <- paste0(as.matrix(df[,14]), '%')
    df[, 15] <- paste0(as.matrix(df[,15]), '%')
    df[, 16] <- paste0(as.matrix(df[,16]), '%')
    df[, 17] <- paste0(as.matrix(df[,17]), '%')
    df[, 18] <- paste0(as.matrix(df[,18]), '%')
    df[, 19] <- paste0(as.matrix(df[,19]), '%')
    df[, 20] <- paste0(as.matrix(df[,20]), '%')
    df[, 22] <- paste0(as.matrix(df[,22]), '%')
    df[, 23] <- paste0(as.matrix(df[,23]), '%')
    df[, 24] <- paste0(as.matrix(df[,24]), '%')
    df[, 25] <- paste0(as.matrix(df[,25]), '%')
    df[, 26] <- paste0(as.matrix(df[,26]), '%')
    
    
    knitr::kable(df, 
                 col.names = c("Chicago Community Area", "region", "Total Population", "Total White", "Percent White", "Total Latino/Hispanic", "Percent Hispanic", 
                               "Total African American", "Percent African American", "Total Asian", "Percent Asian", "Total American Indian and Alaska Native", 
                               "Percent American Indian and Alaska Native", "Percent who are Foreign Born", "Percent with Limited English Proficiency", 
                               "Percent below the Federal Poverty Level", "Percent Unemployed", "Percent with Food Stamps", "Percent with severe rent burdened", 
                               "Percent uninsured", "Child Opportunity Index Composite Index scored from 1-5", "Percent who are Infants 0-4 Years", "Percent who are Juveniles 5-17 Years", "Percent who are Young Adults 18-39 Years", 
                               "Percent who are Middle Aged Adults 40-64","Percent who are Seniors 65+ Years")) %>%
      kableExtra::kable_styling(fixed_thead = TRUE, font_size = 12) %>%
      
      scroll_box(width = "100%", height = "200%")
    
  }
  
  
  ### end table
  
  
  ## start bargraph
  
  Asian_groups2 = Asian_groups %>%
    slice(2:nrow(Asian_groups)) %>%
    mutate(`Label (Grouping)` = fct_reorder(`Label (Grouping)`, `Chicago city, Illinois!!Estimate`)) 
  
  
  output$bargraph <- renderPlot({
    ggplot(Asian_groups2, aes( y = `Chicago city, Illinois!!Estimate`, x = `Label (Grouping)`, fill =`Label (Grouping)`)) + 
      geom_bar(stat = "identity") + geom_text(aes(x = Asian_groups2$`Label (Grouping)`, y = Asian_groups2$`Chicago city, Illinois!!Estimate`, 
                                                  label = scales::comma(`Chicago city, Illinois!!Estimate`)), size = 5, hjust=-0.1 , vjust= 0.4 , family= "Arial") + coord_flip()  + 
      
      scale_y_continuous(expand = c(0,0), breaks = c(20000, 40000, 60000)) +
      
      labs( x= "Asian group", 
            y= "Total Population Number in Chicago",
            title = "Asian breakdown by Groups 2015-2019") +
      
      
      theme_classic() +
      theme( 
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
    
  })
  
  ## end bargraph
  
  
  ### bargraph 2
  
  AIAN_by_groups2=  AIAN_by_groups  %>%
    slice(-(1:2), -(44:46), -53) %>%
    filter(`Chicago city, Illinois!!Estimate` >0) %>%
    mutate(`Label (Grouping)` = fct_reorder(`Label (Grouping)`, `Chicago city, Illinois!!Estimate`)) 
  
  
  output$bargraph2 <- renderPlot({
    ggplot(AIAN_by_groups2, aes( y = `Chicago city, Illinois!!Estimate`, x = `Label (Grouping)`, fill =`Label (Grouping)`)) + 
      geom_bar(stat = "identity") + geom_text(aes(x = AIAN_by_groups2$`Label (Grouping)`, y = AIAN_by_groups2$`Chicago city, Illinois!!Estimate`, 
                                                  label = scales::comma(`Chicago city, Illinois!!Estimate`)), size = 4, hjust=-0.1 , vjust= 0.3 , family= "Arial") + coord_flip()  + 
      
      scale_y_continuous(expand = c(0,0), limits=c(0,4000), breaks = c(1000, 2000, 3000)) +
      
      labs( x= "Native American group", 
            y= "Total Population Number in Chicago",
            title = "Native American breakdown by Groups 2015-2019") +
      
      
      theme_classic() +
      theme( 
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
    
  })
  
  ## end bargraph 2
  
  
  # pie chart
  
  
  
  output$plot <- renderPlotly({
    
    plot_ly(type='pie', labels=labels, values=values, 
            textinfo='label+percent',
            insidetextorientation='radial')
    
    
  })
  
  
  
  ### end pie chart
  
  
  
  
  
  
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)






