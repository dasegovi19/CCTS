source("reference.R")


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "UIC's Center for Clinical and Translational Sciences (CCTS) recruitment and retention resource page",
                  titleWidth = 1200),  # close header
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Maps", tabName = "map_tab", icon = icon("map")),
      menuItem("Characteristics by Region", tabName = "table_tab4", icon = icon("calendar")),
      menuItem("Breakdown of Chicago", tabName = "table_tab1", icon = icon("tablet")),
      menuItem("Breakdown of Asian Population", tabName = "table_tab2", icon = icon("list-alt")),
      menuItem("Breakdown of Native American Population", tabName = "table_tab3", icon = icon("bar-chart-o"))
     
    ) # close side bar Menu
  ), # Close SideBar
  
  dashboardBody(
    tabItems(
      
      
      ## map item
      tabItem(
        tabName = "map_tab",
        titlePanel("Hello! Welcome to this resource page. We hope that these tables and maps help you understand where under-represented groups are located in Chicago, their break-down according to race, and the economic/health factors in each community area. "),
        fluidRow(
          box(
            title = "These data points are based from the 2020 Census",
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
            title = "Values are based from the American Community Survey 5 year estimates 2015-2019(%)",
            status = "primary",
            width = 3,
            selectInput(
              inputId = "choices2",
              label = "Select indicator",
              choices = choices2,
              selected = NULL),# close input
          ),# close box
          box(
            title = "This map allows you to select among the social determinants of health data in each community area. Keep in mind that the values are based off percentage(%).",
            width = 9,
            tmapOutput("healthmap")
          )#close 2nd box
        )#close fluid row
        
      ),# close map tab
      
      
      ## start table tab
      tabItem(
        tabName = "table_tab1",
        titlePanel(" These tables break down the race/ethnicity of Chicago and includes individuals who identify as 2 or more races."),
        ## table
        fluidRow(
          box(
            title = "Values were obtained from the 2020 Census",
            width = 20,
            tableOutput("chicagotable")
          ) # close 2nd box
        ) # close fluid row
      ), # close table1 tab
      
      
      ## start table2 tab
      tabItem(
        tabName = "table_tab2",
        titlePanel(" These tables break down the Asian American community into selected groups."),
        ## table
        fluidRow(
          box(
            title = "Values were obtained from the ACS 5-Year estimates 2015-2019",
            width = 20,
            tableOutput("asiantable")
          ) # close 2nd box
        ) # close fluid row
      ), # close table2 tab
      
      ## start table3 tab
      tabItem(
        tabName = "table_tab3",
        titlePanel(" These tables break down the Native American community into selected groups."),
        ## table
        fluidRow(
          box(
            title = "Values were obtained from the ACS 5-Year estimates 2015-2019",
            width = 20,
            tableOutput("nativeamericantable")
          ) # close 2nd box
        ) # close fluid row
        
      ), # close table3 tab     
      
   
      
       ### 4th table tab
      tabItem(
        tabName = "table_tab4",
        titlePanel(" These tables break down the individual characteristics you saw from the maps but in table format and is categorized by the region each community area is located."),
        ## table
        fluidRow(
          box(
            title = "Select Region",
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
            title = "Please select a region to get started",
            width = 20,
            tableOutput("table3")
          ) # close 2nd box
        ) # close 2nd fluid row
      ) # close  tab
      
      
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
    choices <- input$choices
    tmapProxy("demographicmap", session, {
      tm_remove_layer(401) +
        tm_shape(chicagomap) +
        tm_polygons(choices, id= "Name", zindex = 401)
    })
  })
  
  ## 2nd map
  
  output$healthmap <- renderTmap({
    tm_shape(chicagomap) +
      tm_polygons(choices2[1], zindex = 401)
  })
  
  observe({
    choices2 <- input$choices2
    tmapProxy("healthmap", session, {
      tm_remove_layer(401) +
        tm_shape(chicagomap) +
        tm_polygons(choices2, id="Name", zindex = 401)
      
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
             FoodStamps_Percent, SeverelyRentBurdened_Percent, UninsuredRate_Percent) %>%
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
    
    
    knitr::kable(df, 
                 col.names = c("Chicago Community Area", "region", "Total Population", "Total White", "Percent White", "Total Latino/Hispanic", "Percent Hispanic", 
                               "Total African American", "Percent African American", "Total Asian", "Percent Asian", "Total American Indian and Alaska Native", 
                               "Percent American Indian and Alaska Native", "Percent who are Foreign Born", "Percent with Limited English Proficiency", 
                               "Percent below the Federal Poverty Level", "Percent Unemployed", "Percent with Food Stamps", "Percent with severe rent burdened", 
                               "Percent uninsured")) %>%
      kableExtra::kable_styling(fixed_thead = TRUE, font_size = 12) %>%
      
      scroll_box(width = "100%", height = "200%")
    
  }
  
  
  ### end table
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)




