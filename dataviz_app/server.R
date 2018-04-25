library(shiny)
library(leaflet)
library(sp)
library(data.table)
library(zoo)

shinyServer(function(input, output, session) {
  t1=proc.time()
  output$nymap <- renderLeaflet({
    map <- leaflet() %>%
      addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
      setView(lng = -74.0156491, lat = 40.7022541, zoom = 10)
    map
  })
  
  
  data_replace_mean<- function(data){
    numeric_col = c("price","calculated_host_listings_count","availability_365",
                    "review_scores_rating","review_scores_accuracy","review_scores_cleanliness","review_scores_checkin",
                    "review_scores_communication","review_scores_location","review_scores_value","availability_30")
    
    for(i in numeric_col){
      #data[, i := na.aggregate(i,FUN=median)]
      data[is.na(data[,i]), i] <- median(data[,i], na.rm = TRUE)
    }
    
    data$is_commercial <- NA
    
    # for(i in 1:nrow(data)){
    #   if((data[i,"calculated_host_listings_count"] > 1)  &  (data[i,"availability_365"]/365 < 0.3) & 
    #      (data[i,"review_scores_rating"]>50)){
    #         data[i,"is_commercial"] = "Commercial Listing"  
    #   }
    #   else{
    #         data[i,"is_commercial"] = "Household Listing"
    #   }
    # }
    
    data$is_commercial = ifelse(((data$calculated_host_listings_count > 1)  &  (data$availability_365 / 365 < 0.3) & 
                                  (data$review_scores_rating > 50)),"Commercial Listing","Household Listing")
    
    print("ROWS")
    temp1 = subset(data,is_commercial=="Commercial Listing")
    print(nrow(temp1))
    
    temp2 = subset(data,is_commercial=="Household Listing")
    print(nrow(temp2))
    return(data)
  }
  
  file_change <- reactive({
    
    file_to_read <- paste("../data/",as.character(input$month),"_",as.character(input$year),"_listings.csv",sep="")
    # col_keep <- c("character","NULL","NULL","NULL","NULL","character","NULL","NULL","NULL","NULL","NULL","NULL",
    #               "NULL","NULL","NULL","NULL","character","NULL","character","NULL","NULL","NULL","NULL","NULL",
    #               "NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL",
    #               "character","character","character","character","NULL","NULL","character","NULL","NULL",
    #               "numeric","numeric","NULL","character","character","NULL","numeric","numeric","NULL",
    #               "NULL","NULL","NULL","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL",
    #               "NULL","NULL","numeric","numeric","numeric","numeric","NULL","NULL","NULL","NULL",
    #               "numeric","numeric","numeric","numeric","numeric","numeric","numeric","NULL","NULL",
    #               "NULL","NULL","NULL","NULL","NULL","numeric","NULL")
    
    listings_col_to_keep = c("id","name","host_id","host_name","neighbourhood_cleansed",
                             "neighbourhood_group_cleansed","city","state","smart_location","latitude","longitude",
                             "property_type","room_type","bathrooms","bedrooms","price","review_scores_rating",
                             "review_scores_accuracy","review_scores_cleanliness","review_scores_checkin",
                             "review_scores_communication","review_scores_location","review_scores_value",
                             "reviews_per_month","host_listings_count","host_total_listings_count",
                             "calculated_host_listings_count","availability_365","availability_30",
                             "number_of_reviews","summary")
    
    #df <- read.csv(file_to_read,colClasses = col_keep)
    df = data.table::fread(file_to_read,select = listings_col_to_keep)
    df = as.data.frame(df)
    print("File Read")
    print(proc.time()-t1)
    #print("DONE READ")
    df$price <- gsub("[$|,]", "", df$price)
    df$price <- as.numeric(df$price)
    
    df <- data_replace_mean(df)
    
    print("Processing COmplete")
    print(proc.time()-t1)
    
    df$avg_review_value <- ((df$review_scores_accuracy + df$review_scores_cleanliness + 
                               df$review_scores_checkin + df$review_scores_communication + 
                               df$review_scores_location + df$review_scores_value)/6)
    
    df$avg_review_value[is.na(df$avg_review_value) ]<- 0
    df$avg_review_value = df$avg_review_value*10
    
    print("File Read COmplete")
    print(proc.time()-t1)
    
    #df = as.data.table(df)
    return(df)
  })
  
  observeInputs <- observeEvent(input$submit,{
    df = file_change()
    #print(input$price)
    print("1st function call")
    print(proc.time()-t1)
    df_subset = subset(df,(df$price > input$price) & (df$review_scores_rating > input$quality) & 
                         (df$bedrooms > input$bedrooms) & (df$bathrooms > input$bathrooms) )
    map <- leafletProxy("nymap", session) %>%
      clearShapes() %>%
      clearControls() %>%
      addCircles(lng = df_subset$longitude,lat = df_subset$latitude)
    print(proc.time()-t1)
  })
  
  observeEvent(input$submit2,{
    df = file_change()
    print("Second call")
    print(proc.time()-t1)
    if(is.null(input$in_checkboxgroup)){
      df_subs=df
    }
    else if(input$in_checkboxgroup=="CL"){
      df_subs = subset(df,is_commercial=="Commercial Listing")
      #print(nrow(df_subs))
      }
    else if(input$in_checkboxgroup=="HL"){
      df_subs = subset(df,is_commercial=="Household Listing")
      #print(nrow(df_subs))
      }
    else{
      df_subs = df
      #print(nrow(df_subs))
    }
    print("second call first")
    print(proc.time()-t1)
    
    neighbourhood_costs <- data.frame(aggregate(df_subs$price, by=list(df_subs$neighbourhood_cleansed), FUN=mean))
    names(neighbourhood_costs) <- c("neighbourhood", "avg_cost")
    
    neighbourhood_reviews <- data.frame(aggregate(df_subs$avg_review_value, by=list(df_subs$neighbourhood_cleansed), FUN=mean))
    names(neighbourhood_reviews) <- c("neighbourhood", "avg_review")
    
    neighbourhood_scores <- data.frame(aggregate(df_subs$review_scores_rating, by=list(df_subs$neighbourhood_cleansed), FUN=mean))
    names(neighbourhood_scores) <- c("neighbourhood", "avg_score")
    
    neighbourhood_merged <- merge(neighbourhood_scores, neighbourhood_reviews, by="neighbourhood")
    neighbourhood_merged <- merge(neighbourhood_merged, neighbourhood_costs, by="neighbourhood")
    
    airbnb_neighbourhoods <- geojsonio::geojson_read("../data/neighbourhoods.geojson", what = "sp")
    pal <- colorNumeric("viridis", NULL)
    airbnb_neighbourhoods <- sp::merge(airbnb_neighbourhoods, neighbourhood_merged, by="neighbourhood")
    #print(class(airbnb_neighbourhoods))
    
    print("second call last")
    print(proc.time()-t1)
    
    if(input$radio=="score"){
      print("SCORE")
      map <- leafletProxy(mapId = "nymap", session = session,data = airbnb_neighbourhoods) %>%
        clearShapes() %>%
        clearControls() %>% 
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1 ,fillColor = ~pal(airbnb_neighbourhoods$avg_score)) %>%
        leaflet::addLegend(pal = pal, values = ~(airbnb_neighbourhoods$avg_score), opacity = 1.0)
    }
    else if(input$radio=="review"){
      print("REVIEW")
      map <- leafletProxy(mapId = "nymap", session = session,data = airbnb_neighbourhoods) %>%
        clearShapes() %>%
        clearControls() %>% 
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1 ,fillColor = ~pal(airbnb_neighbourhoods$avg_review)) %>%
        leaflet::addLegend(pal = pal, values = ~(airbnb_neighbourhoods$avg_review), opacity = 1.0)
    }
    else if(input$radio=="cost"){
      print("COST")
      map <- leafletProxy(mapId = "nymap", session = session,data = airbnb_neighbourhoods) %>%
        clearShapes() %>%
        clearControls() %>% 
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1 ,fillColor = ~pal(airbnb_neighbourhoods$avg_cost)) %>%
        leaflet::addLegend(pal = pal, values = ~(airbnb_neighbourhoods$avg_cost), opacity = 1.0)
    }
  })
  
})
