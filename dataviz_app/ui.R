library(shiny)
library(leaflet)

shinyUI(fluidPage(
  tags$head(includeCSS("styles.css")),
  # Our fullscreen part of the UI for the map
  fixedPanel(id = "fullscreen",
             top = 0, left = 0, 
             width = "100%", height = "100%", 
             # Here is the map output in the UI
             leafletOutput("nymap", 
                           width = "100%", height = "100%")
  ), 
  # And the tabbed part with the inputs and plot
  absolutePanel(id = "controls", 
                draggable = TRUE, 
                top = 30, left = 50, 
                width = 400, height = "auto",
                h4("AirBnB Listings"),
                selectInput("month", label = "Month", 
                            choices = c("January","February","March","April","May","June","July","August","September","October","November","December"), 
                            selected = "January"),
                numericInput("year", 
                             label = "Year", 
                             value = 2016, 
                             min = 2016, 
                             max = 2017),
                tabsetPanel(
                  tabPanel("AirBnB Listings",
                           numericInput("price", 
                                        label = "Minimum Price", 
                                        value = 70, 
                                        min = 0, 
                                        max = 10000),
                           numericInput("quality", 
                                        label = "Minimum Rating", 
                                        value = 70, 
                                        min = 0, 
                                        max = 100),
                           numericInput("bedrooms", 
                                        label = "Minimum Bedrooms", 
                                        value = 1, 
                                        min = 0, 
                                        max = 20),
                           numericInput("bathrooms", 
                                        label = "Minimum Bathrooms", 
                                        value = 1, 
                                        min = 0, 
                                        max = 20),
                          
                           actionButton('submit','Submit')),
                  tabPanel("AirBnB Insights",
                           
                           radioButtons("in_radio", "Select Listing Type",selected=character(0),
                                              choiceNames = list("Commercial Listing",
                                                "Household Listing"),
                                        choiceValues = list("CL","HL")
                                              ),
            
                           radioButtons("radio", "Parameters for analysis", selected = character(0),
                                        choiceNames = list("Cost","Review","Adjusted Listing Score"),
                                        choiceValues = list("cost", "review", "score")
                                        ),
                           
                           actionButton('submit2','Submit')
                           
                           ),
                  tabPanel("Trend Over the Years",
                           
                           actionButton('submit3','click')))
                
                
  )))
