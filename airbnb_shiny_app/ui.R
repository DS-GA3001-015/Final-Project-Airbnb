library(shiny)
library(leaflet)
library(data.table)

total_multiple = data.table::fread("./data/total_multiple.csv")
total_multiple = as.data.frame(total_multiple)
total_multiple = total_multiple[order(-total_multiple$sum_total),]
names = unique(total_multiple[1:2094,"pk"])

shinyUI(fluidPage(
  tags$head(includeCSS("styles.css")),
  # Our fullscreen part of the UI for the map
  # And the tabbed part with the inputs and plot
  fixedPanel(id = "fullscreen",
             top = 0, left = 0, 
             width = "100%", height = "100%", 
             # Here is the map output in the UI
             leafletOutput("nymap", 
                           width = "100%", height = "100%")),
  
    absolutePanel(id = "controls",
                draggable = TRUE, 
                top = 30, left = 50, 
                width = 400, height = "auto",
                h4("AirBnB Listings"),
                selectInput("month", label = "Month", 
                            choices = c("July","August","September","October","November","December"),
                            #choices = c("January","February","March","April","May","June","July","August","September","October","November","December"), 
                            selected = "July"),
                numericInput("year", 
                             label = "Year", 
                             value = 2017, 
                             min = 2017, 
                             max = 2017),
                tabsetPanel(
                  tabPanel("AirBnB Listings",
                           sliderInput("price", 
                                       label = "Minimum Price", 
                                       value = 70, 
                                       min = 0, 
                                       max = 1000),
                           sliderInput("quality", 
                                        label = "Minimum Rating", 
                                        value = 70, 
                                        min = 0, 
                                        max = 100),
                           sliderInput("bedrooms", 
                                        label = "Minimum Bedrooms", 
                                        value = 1, 
                                        min = 0, 
                                        max = 20),
                           sliderInput("bathrooms", 
                                        label = "Minimum Bathrooms", 
                                        value = 1, 
                                        min = 0, 
                                        max = 20),
                           checkboxInput('click',"Multiple Listings",value=FALSE),
                          
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
                  tabPanel("Location of Multiple Listings",
                           selectInput("multiple", label = "Select Owner", 
                                       choices = names, 
                                       selected = "Kara"),
                           actionButton('submit4','click')
                           )
                  #tabPanel("Trend Over the Years",
                           
                           #actionButton('submit3','click to view next month'))
           
                  ))
))
