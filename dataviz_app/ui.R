library(shiny)
library(leaflet)
library(data.table)

total_multiple = data.table::fread("../all Listing Data/total_multiple.csv")
total_multiple = as.data.frame(total_multiple)
total_multiple = total_multiple[order(-total_multiple$entire_home_apt),]
names = unique(total_multiple[1:1094,"pk"])

shinyUI(fluidPage(
  tags$head(includeCSS("styles.css")),
  # Our fullscreen part of the UI for the map
  # And the tabbed part with the inputs and plot
  sidebarLayout(
  sidebarPanel(id = "controls",
                draggable = TRUE, 
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
                           ),
                  tabPanel("Trend Over the Years",
                           
                           actionButton('submit3','click'))
                  )),
  
  mainPanel(# Here is the map output in the UI
    leafletOutput("nymap", height=500)),fluid = TRUE
  )))
