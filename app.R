#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Before this data was used in R it was edited in Excel. 
# All of the columns that weren't the monthly numbers for KHW or THERM, Census Block, Building Type and Subtype, 
# Total population, Average Stories and Average building age were deleted. 
# To do this right click on the column that needs to be deleted and click the delete option.  
# After this click on the column for CENSUS.BLOCK to select the whole column. 
# Right click the selected column and select the option "Format Cells". Then select the "Number" option and click "Ok". 



library(shiny)
library(shinydashboard)
library(tigris)
library(ggplot2)
library(leaflet)
library(mapview)
library(DT)
library(stringr)

#------------------------------------------------------------------------- Save and Modify the data -----------------------------------------------------------------------------

chiData <- read.csv(file = "Energy_Usage_2010.csv", sep = ",", header = TRUE)

colnames(chiData)[1] <- "Area"
colnames(chiData)[21] <- "THERM.APRIL.2010"
chiData <- subset(chiData, !is.na(chiData$CENSUS.BLOCK))

# Make numbers instead of characters
chiData$KWH.JANUARY.2010 <- as.numeric(gsub(",","",chiData$KWH.JANUARY.2010))
chiData$KWH.FEBRUARY.2010 <- as.numeric(gsub(",","",chiData$KWH.FEBRUARY.2010))
chiData$KWH.MARCH.2010 <- as.numeric(gsub(",","",chiData$KWH.MARCH.2010))
chiData$KWH.APRIL.2010 <- as.numeric(gsub(",","",chiData$KWH.APRIL.2010))
chiData$KWH.MAY.2010 <- as.numeric(gsub(",","",chiData$KWH.MAY.2010))
chiData$KWH.JUNE.2010 <- as.numeric(gsub(",","",chiData$KWH.JUNE.2010))
chiData$KWH.JULY.2010 <- as.numeric(gsub(",","",chiData$KWH.JULY.2010))
chiData$KWH.AUGUST.2010 <- as.numeric(gsub(",","",chiData$KWH.AUGUST.2010))
chiData$KWH.SEPTEMBER.2010 <- as.numeric(gsub(",","",chiData$KWH.SEPTEMBER.2010))
chiData$KWH.OCTOBER.2010 <- as.numeric(gsub(",","",chiData$KWH.OCTOBER.2010))
chiData$KWH.NOVEMBER.2010 <- as.numeric(gsub(",","",chiData$KWH.NOVEMBER.2010))
chiData$KWH.DECEMBER.2010 <- as.numeric(gsub(",","",chiData$KWH.DECEMBER.2010))
chiData$TOTAL.KWH <- as.numeric(gsub(",","",chiData$TOTAL.KWH))

chiData$THERM.JANUARY.2010 <- as.numeric(gsub(",","",chiData$THERM.JANUARY.2010))
chiData$THERM.FEBRUARY.2010 <- as.numeric(gsub(",","",chiData$THERM.FEBRUARY.2010))
chiData$THERM.MARCH.2010 <- as.numeric(gsub(",","",chiData$THERM.MARCH.2010))
chiData$THERM.APRIL.2010 <- as.numeric(gsub(",","",chiData$THERM.APRIL.2010))
chiData$THERM.MAY.2010 <- as.numeric(gsub(",","",chiData$THERM.MAY.2010))
chiData$THERM.JUNE.2010 <- as.numeric(gsub(",","",chiData$THERM.JUNE.2010))
chiData$THERM.JULY.2010 <- as.numeric(gsub(",","",chiData$THERM.JULY.2010))
chiData$THERM.AUGUST.2010 <- as.numeric(gsub(",","",chiData$THERM.AUGUST.2010))
chiData$THERM.SEPTEMBER.2010 <- as.numeric(gsub(",","",chiData$THERM.SEPTEMBER.2010))
chiData$THERM.OCTOBER.2010 <- as.numeric(gsub(",","",chiData$THERM.OCTOBER.2010))
chiData$THERM.NOVEMBER.2010 <- as.numeric(gsub(",","",chiData$THERM.NOVEMBER.2010))
chiData$THERM.DECEMBER.2010 <- as.numeric(gsub(",","",chiData$THERM.DECEMBER.2010))
chiData$TOTAL.THERMS <- as.numeric(gsub(",","",chiData$TOTAL.THERMS))

#------------------------------------------------------------------------- Save data for Near West Side -----------------------------------------------------------------------------

near_west <- subset(chiData, chiData$Area == "Near West Side")
energy <- subset(cook, cook$GEOID10 %in% near_west$CENSUS.BLOCK)

# Energy data by month
nwJanE <- near_west
nwJanE <- nwJanE[, -c(1:2)]
nwJanE <- nwJanE[, -2]
nwJanE <- nwJanE[, -c(3:27)]

nwFebE <- near_west
nwFebE <- nwFebE[, -c(1:2)]
nwFebE <- nwFebE[, -c(2:3)]
nwFebE <- nwFebE[, -c(3:26)]

nwMarE <- near_west
nwMarE <- nwMarE[, -c(1:2)]
nwMarE <- nwMarE[, -c(2:4)]
nwMarE <- nwMarE[, -c(3:25)]

nwAprE <- near_west
nwAprE <- nwAprE[, -c(1:2)]
nwAprE <- nwAprE[, -c(2:5)]
nwAprE <- nwAprE[, -c(3:24)]

nwMayE <- near_west
nwMayE <- nwMayE[, -c(1:2)]
nwMayE <- nwMayE[, -c(2:6)]
nwMayE <- nwMayE[, -c(3:23)]

nwJunE <- near_west
nwJunE <- nwJunE[, -c(1:2)]
nwJunE <- nwJunE[, -c(2:7)]
nwJunE <- nwJunE[, -c(3:22)]

nwJulE <- near_west
nwJulE <- nwJulE[, -c(1:2)]
nwJulE <- nwJulE[, -c(2:8)]
nwJulE <- nwJulE[, -c(3:21)]

nwAugE <- near_west
nwAugE <- nwAugE[, -c(1:2)]
nwAugE <- nwAugE[, -c(2:9)]
nwAugE <- nwAugE[, -c(3:20)]

nwSepE <- near_west
nwSepE <- nwSepE[, -c(1:2)]
nwSepE <- nwSepE[, -c(2:10)]
nwSepE <- nwSepE[, -c(3:19)]

nwOctE <- near_west
nwOctE <- nwOctE[, -c(1:2)]
nwOctE <- nwOctE[, -c(2:11)]
nwOctE <- nwOctE[, -c(3:18)]

nwNovE <- near_west
nwNovE <- nwNovE[, -c(1:2)]
nwNovE <- nwNovE[, -c(2:12)]
nwNovE <- nwNovE[, -c(3:17)]

nwDecE <- near_west
nwDecE <- nwDecE[, -c(1:2)]
nwDecE <- nwDecE[, -c(2:13)]
nwDecE <- nwDecE[, -c(3:16)]

nwTotE <- near_west
nwTotE <- nwTotE[, -c(1:2)]
nwTotE <- nwTotE[, -c(2:14)]
nwTotE <- nwTotE[, -c(3:15)]

# Gas data by month
nwJanG <- near_west
nwJanG <- nwJanG[, -c(1:2)]
nwJanG <- nwJanG[, -c(2:15)]
nwJanG <- nwJanG[, -c(3:14)]

nwFebG <- near_west
nwFebG <- nwFebG[, -c(1:2)]
nwFebG <- nwFebG[, -c(2:16)]
nwFebG <- nwFebG[, -c(3:13)]

nwMarG <- near_west
nwMarG <- nwMarG[, -c(1:2)]
nwMarG <- nwMarG[, -c(2:17)]
nwMarG <- nwMarG[, -c(3:12)]

nwAprG <- near_west
nwAprG <- nwAprG[, -c(1:2)]
nwAprG <- nwAprG[, -c(2:18)]
nwAprG <- nwAprG[, -c(3:11)]

nwMayG <- near_west
nwMayG <- nwMayG[, -c(1:2)]
nwMayG <- nwMayG[, -c(2:19)]
nwMayG <- nwMayG[, -c(3:10)]

nwJunG <- near_west
nwJunG <- nwJunG[, -c(1:2)]
nwJunG <- nwJunG[, -c(2:20)]
nwJunG <- nwJunG[, -c(3:9)]

nwJulG <- near_west
nwJulG <- nwJulG[, -c(1:2)]
nwJulG <- nwJulG[, -c(2:21)]
nwJulG <- nwJulG[, -c(3:8)]

nwAugG <- near_west
nwAugG <- nwAugG[, -c(1:2)]
nwAugG <- nwAugG[, -c(2:22)]
nwAugG <- nwAugG[, -c(3:7)]

nwSepG <- near_west
nwSepG <- nwSepG[, -c(1:2)]
nwSepG <- nwSepG[, -c(2:23)]
nwSepG <- nwSepG[, -c(3:6)]

nwOctG <- near_west
nwOctG <- nwOctG[, -c(1:2)]
nwOctG <- nwOctG[, -c(2:24)]
nwOctG <- nwOctG[, -c(3:5)]

nwNovG <- near_west
nwNovG <- nwNovG[, -c(1:2)]
nwNovG <- nwNovG[, -c(2:25)]
nwNovG <- nwNovG[, -c(3:4)]

nwDecG <- near_west
nwDecG <- nwDecG[, -c(1:2)]
nwDecG <- nwDecG[, -c(2:26)]
nwDecG <- nwDecG[, -3]

nwTotG <- near_west
nwTotG <- nwTotG[, -c(1:2)]
nwTotG <- nwTotG[, -c(2:27)]



# Define UI for application that draws a histogram
ui <- navbarPage(

    title = "Will Davidson CS 424 Project 3", 
    
    #------------------------------------------------------------------------- Near West Side Page-----------------------------------------------------------------------------
    tabPanel(title = "Near West Side", 
              fluidRow(
                
                column(4, 
                       box(title = "Near West Side", solidHeader = TRUE, status = "primary", width = 8,
                           leafletOutput("nearWest"), 
                           fluidRow(actionButton("reset", label = "Reset Map"))
                       )
                       ), 
                column (2, box(title = "Inputs", solidHeader = TRUE, status = "primary", width = 8,
                               fluidRow(selectInput("choices", label = "Map View", choices = c("Electricity", "Gas", "Building Age", "Building Type", "Building Height", "Total Population"), selected = "Electricity")), 
                               fluidRow(selectInput("month", label = "Month View", choices = c("Total", "January", "February", "March", "April", "May", "June", "July", 
                                                                                       "August", "September", "October", "November", "December"), selected = "Total")), 
                               fluidRow(selectInput("types", label = "Building Types", choices = c("All", "Commercial", "Industrial", "Residential"), selected = "All"))
                )), 
                column (4, 
                        box(title = "Data Tables", solidHeader = TRUE, status = "primary", width = 8,
                            dataTableOutput("nearWestTable")
                        ))
              )), 
    
    #------------------------------------------------------------------------- Comparison Page -----------------------------------------------------------------------------
    
    tabPanel(title = "Comparison", 
             
             column(4, 
                    box(title = "Map 1", solidHeader = TRUE, status = "primary", width = 8,
                        leafletOutput("map1"), 
                        fluidRow(actionButton("reset1", label = "Reset Map"))
                    )
             ), 
             column (2, box(title = "Inputs Map 1", solidHeader = TRUE, status = "primary", width = 8,
                            fluidRow(selectInput("choices1", label = "Map View", choices = c("Electricity", "Gas", "Building Age", "Building Type", "Building Height", "Total Population"), selected = "Electricity")), 
                            fluidRow(selectInput("month1", label = "Month View", choices = c("Total", "January", "February", "March", "April", "May", "June", "July", 
                                                                                            "August", "September", "October", "November", "December"), selected = "Total")), 
                            fluidRow(selectInput("types1", label = "Building Types", choices = c("All", "Commercial", "Industrial", "Residential"), selected = "All"))
             )),
             column(4, 
                    box(title = "Map 2", solidHeader = TRUE, status = "primary", width = 8,
                        leafletOutput("map2"), 
                        fluidRow(actionButton("reset2", label = "Reset Map"))
                    )
             ), 
             column (2, box(title = "Inputs Map 2", solidHeader = TRUE, status = "primary", width = 8,
                            fluidRow(selectInput("choices2", label = "Map View", choices = c("Electricity", "Gas", "Building Age", "Building Type", "Building Height", "Total Population"), selected = "Electricity")), 
                            fluidRow(selectInput("month2", label = "Month View", choices = c("Total", "January", "February", "March", "April", "May", "June", "July", 
                                                                                             "August", "September", "October", "November", "December"), selected = "Total")), 
                            fluidRow(selectInput("types2", label = "Building Types", choices = c("All", "Commercial", "Industrial", "Residential"), selected = "All"))
             ))
             
             ), 
    
    #------------------------------------------------------------------------- Chicago Page -----------------------------------------------------------------------------
    
    tabPanel(title = "Chicago", 
             column(4, 
                    box(title = "Chicago", solidHeader = TRUE, status = "primary", width = 8,
                        leafletOutput("chiMap"), 
                        fluidRow(actionButton("reset3", label = "Reset Map"))
                    )
             )
             
    ), 
    
    #------------------------------------------------------------------------- About Page -----------------------------------------------------------------------------
    
    tabPanel(title = "About", 
              p("This data presented in the app comes from https://data.cityofchicago.org/Environment-Sustainable-Development/Energy-Usage-2010/8yq3-m6wp "),
              p("This data is of the amount of electricity and gas used in by a certain census block from 2010. "),
              p("The data also includes the total polulation, average stories and age of the buildings in each census block"),
              p("This app was written by Will Davidson on 4/24/2021."))

    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #------------------------------------------------------------------------- Near West Side Map -----------------------------------------------------------------------------
  
    output$nearWest <- renderLeaflet({
      west <- mapview(energy)@map
      
      if(input$reset || !input$reset) {
        west
      }
    })
    
    #------------------------------------------------------------------------- Near West Side Data Tables -----------------------------------------------------------------------------
    
    output$nearWestTable <- DT::renderDataTable(
      # Electricity selection
      if(input$choices == "Electricity") {
        
        # All Selection
        if(input$types == "All") {
          
          # Months selection
          if (input$month == "January") {
            DT::datatable({nwJanE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          } 
          else if (input$month == "February") {
            DT::datatable({nwFebE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "March") {
            DT::datatable({nwMarE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "April") {
            DT::datatable({nwAprE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "May") {
            DT::datatable({nwMayE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "June") {
            DT::datatable({nwJunE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "July") {
            DT::datatable({nwJulE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "August") {
            DT::datatable({nwAugE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "September") {
            DT::datatable({nwSepE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "October") {
            DT::datatable({nwOctE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "November") {
            DT::datatable({nwNovE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "December") {
            DT::datatable({nwDecE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "Total") {
            DT::datatable({nwTotE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
        }
        # Commercial Selection
        else if (input$types == "Commercial") {
          # Months Selection
          if (input$month == "January") {
            cJanE <- subset(nwJanE, nwJanE$BUILDING.TYPE == "Commercial")
            DT::datatable({cJanE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          } 
          else if (input$month == "February") {
            cFebE <- subset(nwFebE, nwFebE$BUILDING.TYPE == "Commercial")
            DT::datatable({cFebE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "March") {
            cMarE <- subset(nwMarE, nwMarE$BUILDING.TYPE == "Commercial")
            DT::datatable({cMarE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "April") {
            cAprE <- subset(nwAprE, nwAprE$BUILDING.TYPE == "Commercial")
            DT::datatable({cAprE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "May") {
            cMayE <- subset(nwMayE, nwMayE$BUILDING.TYPE == "Commercial")
            DT::datatable({cMayE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "June") {
            cJunE <- subset(nwJunE, nwJunE$BUILDING.TYPE == "Commercial")
            DT::datatable({cJunE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "July") {
            cJulE <- subset(nwJulE, nwJulE$BUILDING.TYPE == "Commercial")
            DT::datatable({cJulE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "August") {
            cAugE <- subset(nwAugE, nwAugE$BUILDING.TYPE == "Commercial")
            DT::datatable({cAugE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "September") {
            cSepE <- subset(nwSepE, nwSepE$BUILDING.TYPE == "Commercial")
            DT::datatable({cSepE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "October") {
            cOctE <- subset(nwOctE, nwOctE$BUILDING.TYPE == "Commercial")
            DT::datatable({cOctE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "November") {
            cNovE <- subset(nwNovE, nwNovE$BUILDING.TYPE == "Commercial")
            DT::datatable({cNovE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "December") {
            cDecE <- subset(nwDecE, nwDecE$BUILDING.TYPE == "Commercial")
            DT::datatable({cDecE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "Total") {
            cTotE <- subset(nwTotE, nwTotE$BUILDING.TYPE == "Commercial")
            DT::datatable({cTotE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
        }
        # Industrial Selection
        else if (input$types == "Industrial") {
          # Months Selection
          if (input$month == "January") {
            cJanE <- subset(nwJanE, nwJanE$BUILDING.TYPE == "Industrial")
            DT::datatable({cJanE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          } 
          else if (input$month == "February") {
            cFebE <- subset(nwFebE, nwFebE$BUILDING.TYPE == "Industrial")
            DT::datatable({cFebE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "March") {
            cMarE <- subset(nwMarE, nwMarE$BUILDING.TYPE == "Industrial")
            DT::datatable({cMarE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "April") {
            cAprE <- subset(nwAprE, nwAprE$BUILDING.TYPE == "Industrial")
            DT::datatable({cAprE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "May") {
            cMayE <- subset(nwMayE, nwMayE$BUILDING.TYPE == "Industrial")
            DT::datatable({cMayE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "June") {
            cJunE <- subset(nwJunE, nwJunE$BUILDING.TYPE == "Industrial")
            DT::datatable({cJunE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "July") {
            cJulE <- subset(nwJulE, nwJulE$BUILDING.TYPE == "Industrial")
            DT::datatable({cJulE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "August") {
            cAugE <- subset(nwAugE, nwAugE$BUILDING.TYPE == "Industrial")
            DT::datatable({cAugE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "September") {
            cSepE <- subset(nwSepE, nwSepE$BUILDING.TYPE == "Industrial")
            DT::datatable({cSepE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "October") {
            cOctE <- subset(nwOctE, nwOctE$BUILDING.TYPE == "Industrial")
            DT::datatable({cOctE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "November") {
            cNovE <- subset(nwNovE, nwNovE$BUILDING.TYPE == "Industrial")
            DT::datatable({cNovE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "December") {
            cDecE <- subset(nwDecE, nwDecE$BUILDING.TYPE == "Industrial")
            DT::datatable({cDecE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "Total") {
            cTotE <- subset(nwTotE, nwTotE$BUILDING.TYPE == "Industrial")
            DT::datatable({cTotE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
        }
        # Residential Selection
        else if (input$types == "Residential") {
          # Months Selection
          if (input$month == "January") {
            cJanE <- subset(nwJanE, nwJanE$BUILDING.TYPE == "Residential")
            DT::datatable({cJanE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          } 
          else if (input$month == "February") {
            cFebE <- subset(nwFebE, nwFebE$BUILDING.TYPE == "Residential")
            DT::datatable({cFebE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "March") {
            cMarE <- subset(nwMarE, nwMarE$BUILDING.TYPE == "Residential")
            DT::datatable({cMarE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "April") {
            cAprE <- subset(nwAprE, nwAprE$BUILDING.TYPE == "Residential")
            DT::datatable({cAprE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "May") {
            cMayE <- subset(nwMayE, nwMayE$BUILDING.TYPE == "Residential")
            DT::datatable({cMayE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "June") {
            cJunE <- subset(nwJunE, nwJunE$BUILDING.TYPE == "Residential")
            DT::datatable({cJunE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "July") {
            cJulE <- subset(nwJulE, nwJulE$BUILDING.TYPE == "Residential")
            DT::datatable({cJulE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "August") {
            cAugE <- subset(nwAugE, nwAugE$BUILDING.TYPE == "Residential")
            DT::datatable({cAugE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "September") {
            cSepE <- subset(nwSepE, nwSepE$BUILDING.TYPE == "Residential")
            DT::datatable({cSepE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "October") {
            cOctE <- subset(nwOctE, nwOctE$BUILDING.TYPE == "Residential")
            DT::datatable({cOctE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "November") {
            cNovE <- subset(nwNovE, nwNovE$BUILDING.TYPE == "Residential")
            DT::datatable({cNovE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "December") {
            cDecE <- subset(nwDecE, nwDecE$BUILDING.TYPE == "Residential")
            DT::datatable({cDecE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "Total") {
            cTotE <- subset(nwTotE, nwTotE$BUILDING.TYPE == "Residential")
            DT::datatable({cTotE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
        }
        
      }
      
      # Gas Selection
      else if (input$choices == "Gas") {
        
        # All selection
        if(input$types == "All") {
          
          # Months Selection
          if (input$month == "January") {
            DT::datatable({nwJanG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          } 
          else if (input$month == "February") {
            DT::datatable({nwFebG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "March") {
            DT::datatable({nwMarG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "April") {
            DT::datatable({nwAprG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "May") {
            DT::datatable({nwMayG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "June") {
            DT::datatable({nwJunG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "July") {
            DT::datatable({nwJulG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "August") {
            DT::datatable({nwAugG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "September") {
            DT::datatable({nwSepG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "October") {
            DT::datatable({nwOctG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "November") {
            DT::datatable({nwNovG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "December") {
            DT::datatable({nwDecG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "Total") {
            DT::datatable({nwTotG}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
        }
        
        # Commercial Selection
        else if (input$types == "Commercial") {
          
          # Months Selection
          if (input$month == "January") {
            cJanE <- subset(nwJanG, nwJanG$BUILDING.TYPE == "Commercial")
            DT::datatable({cJanE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          } 
          else if (input$month == "February") {
            cFebE <- subset(nwFebG, nwFebG$BUILDING.TYPE == "Commercial")
            DT::datatable({cFebE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "March") {
            cMarE <- subset(nwMarG, nwMarG$BUILDING.TYPE == "Commercial")
            DT::datatable({cMarE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "April") {
            cAprE <- subset(nwAprG, nwAprG$BUILDING.TYPE == "Commercial")
            DT::datatable({cAprE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "May") {
            cMayE <- subset(nwMayG, nwMayG$BUILDING.TYPE == "Commercial")
            DT::datatable({cMayE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "June") {
            cJunE <- subset(nwJunG, nwJunG$BUILDING.TYPE == "Commercial")
            DT::datatable({cJunE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "July") {
            cJulE <- subset(nwJulG, nwJulG$BUILDING.TYPE == "Commercial")
            DT::datatable({cJulE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "August") {
            cAugE <- subset(nwAugG, nwAugG$BUILDING.TYPE == "Commercial")
            DT::datatable({cAugE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "September") {
            cSepE <- subset(nwSepG, nwSepG$BUILDING.TYPE == "Commercial")
            DT::datatable({cSepE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "October") {
            cOctE <- subset(nwOctG, nwOctG$BUILDING.TYPE == "Commercial")
            DT::datatable({cOctE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "November") {
            cNovE <- subset(nwNovG, nwNovG$BUILDING.TYPE == "Commercial")
            DT::datatable({cNovE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "December") {
            cDecE <- subset(nwDecG, nwDecG$BUILDING.TYPE == "Commercial")
            DT::datatable({cDecE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "Total") {
            cTotE <- subset(nwTotG, nwTotG$BUILDING.TYPE == "Commercial")
            DT::datatable({cTotE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
        }
        
        # Industrial Selection
        else if (input$types == "Industrial") {
          
          # Months Selection
          if (input$month == "January") {
            cJanE <- subset(nwJanG, nwJanG$BUILDING.TYPE == "Industrial")
            DT::datatable({cJanE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          } 
          else if (input$month == "February") {
            cFebE <- subset(nwFebG, nwFebG$BUILDING.TYPE == "Industrial")
            DT::datatable({cFebE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "March") {
            cMarE <- subset(nwMarG, nwMarG$BUILDING.TYPE == "Industrial")
            DT::datatable({cMarE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "April") {
            cAprE <- subset(nwAprG, nwAprG$BUILDING.TYPE == "Industrial")
            DT::datatable({cAprE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "May") {
            cMayE <- subset(nwMayG, nwMayG$BUILDING.TYPE == "Industrial")
            DT::datatable({cMayE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "June") {
            cJunE <- subset(nwJunG, nwJunG$BUILDING.TYPE == "Industrial")
            DT::datatable({cJunE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "July") {
            cJulE <- subset(nwJulG, nwJulG$BUILDING.TYPE == "Industrial")
            DT::datatable({cJulE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "August") {
            cAugE <- subset(nwAugG, nwAugG$BUILDING.TYPE == "Industrial")
            DT::datatable({cAugE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "September") {
            cSepE <- subset(nwSepG, nwSepG$BUILDING.TYPE == "Industrial")
            DT::datatable({cSepE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "October") {
            cOctE <- subset(nwOctG, nwOctG$BUILDING.TYPE == "Industrial")
            DT::datatable({cOctE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "November") {
            cNovE <- subset(nwNovG, nwNovG$BUILDING.TYPE == "Industrial")
            DT::datatable({cNovE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "December") {
            cDecE <- subset(nwDecG, nwDecG$BUILDING.TYPE == "Industrial")
            DT::datatable({cDecE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "Total") {
            cTotE <- subset(nwTotG, nwTotG$BUILDING.TYPE == "Industrial")
            DT::datatable({cTotE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
        }
        
        # Residential Selection
        else if (input$types == "Residential") {
          
          # Months Selection
          if (input$month == "January") {
            cJanE <- subset(nwJanG, nwJanG$BUILDING.TYPE == "Residential")
            DT::datatable({cJanE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          } 
          else if (input$month == "February") {
            cFebE <- subset(nwFebG, nwFebG$BUILDING.TYPE == "Residential")
            DT::datatable({cFebE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "March") {
            cMarE <- subset(nwMarG, nwMarG$BUILDING.TYPE == "Residential")
            DT::datatable({cMarE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "April") {
            cAprE <- subset(nwAprG, nwAprG$BUILDING.TYPE == "Residential")
            DT::datatable({cAprE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "May") {
            cMayE <- subset(nwMayG, nwMayG$BUILDING.TYPE == "Residential")
            DT::datatable({cMayE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "June") {
            cJunE <- subset(nwJunG, nwJunG$BUILDING.TYPE == "Residential")
            DT::datatable({cJunE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "July") {
            cJulE <- subset(nwJulG, nwJulG$BUILDING.TYPE == "Residential")
            DT::datatable({cJulE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "August") {
            cAugE <- subset(nwAugG, nwAugG$BUILDING.TYPE == "Residential")
            DT::datatable({cAugE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "September") {
            cSepE <- subset(nwSepG, nwSepG$BUILDING.TYPE == "Residential")
            DT::datatable({cSepE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "October") {
            cOctE <- subset(nwOctG, nwOctG$BUILDING.TYPE == "Residential")
            DT::datatable({cOctE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "November") {
            cNovE <- subset(nwNovG, nwNovG$BUILDING.TYPE == "Residential")
            DT::datatable({cNovE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "December") {
            cDecE <- subset(nwDecG, nwDecG$BUILDING.TYPE == "Residential")
            DT::datatable({cDecE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
          else if (input$month == "Total") {
            cTotE <- subset(nwTotG, nwTotG$BUILDING.TYPE == "Residential")
            DT::datatable({cTotE}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE)
          }
        }
      }
      
      )
    
    #------------------------------------------------------------------------- Map 1 Comparison -----------------------------------------------------------------------------
    
    output$map1 <- renderLeaflet({
      m1 <- subset(chiData, chiData$Area == "Near West Side")
      m1 <- subset(cook, cook$GEOID10 %in% m1$CENSUS.BLOCK)
      mapOne <- mapview(m1)@map
      
      if (input$reset1 || ! input$reset1) {
      
        mapOne
      }
      
    })
    
    #------------------------------------------------------------------------- Map 2 Comparison -----------------------------------------------------------------------------
    
    output$map2 <- renderLeaflet({
      m2 <- subset(chiData, chiData$Area == "Loop")
      m2 <- subset(cook, cook$GEOID10 %in% m2$CENSUS.BLOCK)
      mapTwo <- mapview(m2)@map
      
      if (input$reset2 || ! input$reset2) {
        
        mapTwo
      }
      
    })
    
    #------------------------------------------------------------------------- Chicago Map -----------------------------------------------------------------------------
    
    output$chiMap <- renderLeaflet({
      chi <- subset(cook, cook$GEOID10 %in% chiData$CENSUS.BLOCK)
      mapChi <- mapview(chi)@map
      
      if (input$reset3 || ! input$reset3) {
        
        mapChi
      }
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
