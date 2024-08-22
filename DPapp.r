################################################################################
########### Code to Create a Shiny App of Bellevue Demographics ################
################################################################################

# List of required package names
packages <- c("leaflet","plotly","rsconnect","shiny","tmaptools")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the required libraries
library(leaflet)        # For creating leaflet maps
library(plotly)         # For loading/observing plotly figures
library(rsconnect)      # For publishing Shiny Apps
library(shiny)          # For creating interactive data dashboards
library(tmaptools)      # For looking at color palettes

# Double check to make sure the workspace is loaded
load("DPAppData.RData")










################################################################################
############ Second Step: Use the Created Plots to Populate the App ############
################################################################################

# Create a key for the input slider we're going to create below when mapping
# the percentage of the CTs population that identifies with each racial/ethnic
# group.
groups <- c("Total Population"="Total Population",
            "Percent Non-Hispanic White"="White",
            "Percent Non-Hispanic Black"="Black",
            "Percent Hispanic/Latino"="Latino",
            "Percent Non-Hispanic Asian"="Asian",
            "Percent Non-Hispanic Other"="Other",
            "Dissimilarity Index Score (2020)*"="Dissimilarity")

# Create the frontend user interface of the shiny app. Note that there are 
# separate tabs for each of the plots included with the app.
ui <- fluidPage(
  titlePanel(div(span("Demographic Profile of the City of Bellevue", style = "color:#006598"),
                 br(),
                 span(em(h5("Bellevue welcomes the world. Our diversity is our 
                            strength. We embrace the future while respecting our 
                            past.")), style = "color:#006598")
  ),
  
  ),  
  sidebarLayout(                                                                         
    sidebarPanel(
      width=2,                                                                           
      tags$div("This dashboard presents data about the Bellevue community. Use           
               the tabs on the right side of the dashboard to explore different 
               aspects of the City's population. Some of the tabs contain 
               additional ways to sort and filter the data. The data sources 
               used to create the visualizations in this dashboard are each 
               provided separately."),                                                   
      br(),                                                                              
      tags$div("Where possible the 90% margins of error are represented by red
               bars around the estimates. These indicate the likely range of 
               uncertainty around estimates."),                                                   
      br(),                                                                              
      tags$div("This dashboard was built using the",
               tags$a(href="http://shiny.rstudio.com","Shiny"),
               "framework for the",
               tags$a(href="https://www.r-project.org/","R"), 
               "programming language.")                                                  
    ),
    mainPanel(                                                                           
      tabsetPanel(                                                                       
        type="tabs",                                                                     
        tabPanel("Race/Ethnicity (1/2)",                                                       
                 h3("Racial & Ethnic Composition of Bellevue Population by Census Tract (2018-2022)"),   
                 selectInput(inputId="var",                                              
                             label="Choose a Group to Display (Click Tract Area for Details)",
                             choices=groups),
                 leafletOutput("map",height="600"),                                     
                 p(strong("Source: "),                                                   
                   tags$a(href="https://data.census.gov/","2018-2022 American Community Survey."), 
                   "5-Year Data Profiles (Table DP05).",
                   "*Note: Dissimilarity Index scores are based on 2020 Census 
                   data & measure the within Census Tract racial/ethnic integration 
                   across Census Blocks. Scores can range from 0 - which would 
                   indicate complete group integration - to a score of 1 - which 
                   would indicate complete segregation.")),
        
        tabPanel("Race/Ethnicity (2/2)",                                                       
                 h3("Racial & Ethnic Composition of Bellevue Population (2010-2022)"),   
                 plotlyOutput("racets",height="600"),
                 p(strong("Source: "),                                                   
                   tags$a(href="https://data.census.gov/","2010-2019 & 2021-2022 American Community Survey."), 
                   "1-Year Data Profiles (Table DP05).")),
        
        tabPanel("Total Population",                                                     
                 h3("Count of Bellevue Residents Over Time (1968-2024)"),
                 plotlyOutput("popts",height="600"),
                 p(strong("Source: "), 
                   tags$a(href=paste("https://ofm.wa.gov/washington-data-research/",
                                     "population-demographics/population-estimates/",
                                     "historical-estimates-april-1-population-and-housing-state-counties-and-cities",
                                     sep=""),
                          "Washington State Office of Financial Management."), 
                   "April 1 Postcensal Estimates of Population (1960-Present); ",
                   tags$a(href="https://eastsideheritagecenter.org/gift-shop/bellevue-timeline",
                          "Bellevue Timeline."))),
        
        tabPanel("Age & Sex",                                                            
                 h3("Count of Bellevue Residents by Age & Sex (2022)"),
                 plotlyOutput("age",height="600"),
                 p(strong("Source: "), 
                   tags$a(href="https://data.census.gov/","2022 American Community Survey."), 
                   "1-Year Detailed Tables (Table B01001).")),
        
        tabPanel("Age & Race/Ethnicity",                                                 
                 h3("Count of Bellevue Residents by Age & Race/Ethnicity (2018-2022)"),
                 plotlyOutput("rcage",height="600"),
                 p(strong("Source: "), 
                   tags$a(href="https://data.census.gov/","2018-2022 American Community Survey."), 
                   "5-Year Detailed Tables (Table B01001B-I).")),
        
        tabPanel("Age Over Time",                                                 
                 h3("Percentage of Bellevue Residents by Age Group (2010-2022)"),
                 plotlyOutput("ageTS",height="600"),
                 p(strong("Source: "), 
                   tags$a(href="https://data.census.gov/","American Community Survey."), 
                   "2010-2019 & 2021-2022 1-Year Subject Tables & Data Profiles (Tables S0101 & DP05).")),
        
        tabPanel("Place of Birth/Citizenship",                                           
                 h3("Count of Bellevue Residents by Place of Birth/Citizenship (2000-2022)"),
                 plotlyOutput("pob",height="600"),
                 p(strong("Source: "), 
                   tags$a(href="https://data.census.gov/","U.S. Census Bureau."), 
                   "2000 Decennial Census (Table DP2);", 
                   tags$a(href="https://data.census.gov/","American Community Survey."), 
                   "2010-2019 & 2021-2022 1-Year Data Profiles (Table DP02).")),
        
        tabPanel("English Speaking Ability (1/2)",                                      
                 h3("Percentage of Bellevue Residents by Primary Language Spoken at
                    Home & English Proficiency (2000-2022)"),
                 plotlyOutput("bvspoke",height="600"),
                 p(strong("Source: "), 
                   tags$a(href="https://data.census.gov/","U.S. Census Bureau."), 
                   "2000 Decennial Census (Table DP2);", 
                   tags$a(href="https://data.census.gov/","American Community Survey."), 
                   "2010-2019 & 2021-2022 1-Year Data Profiles (Table DP02).")),
        
        tabPanel("English Speaking Ability (2/2)",                                      
                 h3("Percentage of Residents by Primary Language Spoken at
                    Home & English Proficiency (2022)"),
                 plotlyOutput("spokecomp1",height="600"),
                 p(strong("Source: "), 
                   tags$a(href="https://data.census.gov/","2022 American Community Survey."), 
                   "1-Year Data Profiles (Table DP02).")),
        
        tabPanel("Household Type & Age",                                                
                 h3("Count of Bellevue Households by Household Type & Head of 
                    Household Age (2022)"),
                 plotlyOutput("hhead",height="600"),
                 p(strong("Source: "), 
                   tags$a(href="https://data.census.gov/","2022 American Community Survey."), 
                   "1-Year Detailed Tables (Table B25011).")),
        
        tabPanel("Top 10 Languages",                                                    
                 h3("Count of Bellevue Area Residents by Primary Language & 
                    English Proficiency (2018-2022)"),
                 plotlyOutput("pumslang",height="600"),
                 p(strong("Source: "), 
                   tags$a(href="https://data.census.gov/","2018-2022 American Community Survey."), 
                   "5% PUMS Microdata Sample for Greater Bellevue PUMA.")),
        
      )
    )
  )
)



# Now create the backend server functionality to populate the shiny app
# palette_explorer()                      # Uncomment to explore palette options
server <- function(input,output){                                   
  group_to_map <- reactive({                                        
    filter(bvue_tracts_map,measure==input$var)                      
  })
  output$map <- renderLeaflet({                                     
    leaflet(options=leafletOptions(zoomControl=FALSE)) %>%
      setView(-122.156,47.585, zoom=11) %>%
      addProviderTiles('CartoDB.Positron') 
  })
  observeEvent(input$var, {                                         
    pal <- colorNumeric("inferno",reverse = TRUE,group_to_map()$`e`)
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data=group_to_map(),
                  color= ~pal(`e`),
                  weight=0.5,
                  fillOpacity=0.5,
                  smoothFactor=0.2,
                  popup=~paste0("Estimate: ",`e`,
                                "<br>",
                                "MOE: ±",`moe`)) %>%
      addLegend(
        position="bottomright",
        pal=pal,
        values=group_to_map()$`e`,
        title="Estimate"
      )
  })
  
  output$racets <- renderPlotly({                                   
    racetsplotly
  })
  
  output$popts <- renderPlotly({                                   
    popplotly
  })
  
  output$age <- renderPlotly({                                     
    ageplotly
  })
  
  output$rcage <- renderPlotly({                                   
    rcageplotly
  })
  
  output$ageTS <- renderPlotly({                                   
    ageTSplotly
  })
  
  output$pob <- renderPlotly({                                     
    pobplotly
  })
  
  output$bvspoke <- renderPlotly({                                 
    bvspokeplotly
  })
  
  output$spokecomp1 <- renderPlotly({                              
    spokecompplotly
  })
  
  output$hhead <- renderPlotly({                                   
    hheadplotly
  })
  
  output$pumslang <- renderPlotly({                                
    langplotly
  })
}



# Now connect the ui to the server function to create the App
shinyApp(ui=ui,server=server)

# rsconnect::deployApp()




















