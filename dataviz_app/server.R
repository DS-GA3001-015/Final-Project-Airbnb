library(shiny)
library(leaflet)
library(sp)
library(data.table)
library(zoo)
library(geojsonio)
library(geojson)

shinyServer(function(input, output, session) {
  #t1=proc.time()
  output$nymap <- renderLeaflet({
    map <- leaflet() %>%
      addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
      setView(lng = -73.7856491, lat = 40.7022541, zoom = 10)
    map
  })
  
  
  data_replace_mean<- function(data){
    numeric_col = c("price","calculated_host_listings_count","availability_365","availability_30",
                    "review_scores_rating","review_scores_accuracy","review_scores_cleanliness","review_scores_checkin",
                    "review_scores_communication","review_scores_location","review_scores_value","availability_30")
    
    for(i in numeric_col){
      #data[, i := na.aggregate(i,FUN=median)]
      data[is.na(data[,i]), i] <- median(data[,i], na.rm = TRUE)
    }
    
    data$is_commercial <- NA
    
    # for(i in 1:nrow(data)){
    #   if((data[i,"calculated_host_listings_count"] > 1)  &  (data[i,"availability_30"]/30 < 0.3) & 
    #      (data[i,"review_scores_rating"]>50)){
    #         data[i,"is_commercial"] = "Commercial Listing"  
    #   }
    #   else{
    #         data[i,"is_commercial"] = "Household Listing"
    #   }
    # }
    
    data$is_commercial = ifelse(((data$calculated_host_listings_count > 1)  &  (data$availability_365 / 365 < 0.3) & 
                                  (data$review_scores_rating > 50)),"Commercial Listing","Household Listing")
    
    #print("ROWS")
    #temp1 = subset(data,is_commercial=="Commercial Listing")
    #rint(nrow(temp1))
    
    #temp2 = subset(data,is_commercial=="Household Listing")
    #print(nrow(temp2))
    return(data)
  }
  
  file_change <- reactive({
    
    wd=getwd()
    #print(wd)
    file_to_read <- paste("../all Listing Data/",as.character(input$year),"_",as.character(input$month),"_listings.csv",sep="")
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
    #print("File Read")
    #print(proc.time()-t1)
    #print("DONE READ")
    df$price <- gsub("[$|,]", "", df$price)
    df$price <- as.numeric(df$price)
    
    df <- data_replace_mean(df)
    
    #print("Processing COmplete")
    #print(proc.time()-t1)
    
    df$avg_review_value <- ((df$review_scores_accuracy + df$review_scores_cleanliness + 
                               df$review_scores_checkin + df$review_scores_communication + 
                               df$review_scores_location + df$review_scores_value)/6)
    
    df$avg_review_value[is.na(df$avg_review_value) ]<- 0
    df$avg_review_value = df$avg_review_value*10
    
    #print("File Read COmplete")
    #print(proc.time()-t1)
    
    #df = as.data.table(df)
    return(df)
  })
  
  
  observeInputs <- observeEvent(input$submit,{
    df = file_change()
    #print(input$price)
    #print(input$click)
    #print("1st function call")
    #print(proc.time()-t1)
    relative_hosts = data.table::fread("../all Listing Data/relative_hosts.csv")
    if(input$year==2016){
      relative_hosts = subset(relative_hosts,select=c("host_id","sum_2016"))
    }
    else{
      relative_hosts = subset(relative_hosts,select=c("host_id","sum_2017"))
    }
    colnames(relative_hosts) = c("host_id","sum_listings")
    if(input$click==TRUE)
    relative_hosts = subset(relative_hosts,sum_listings>1)
    
    
    df = base::merge(df,relative_hosts,by="host_id")
    
    
    df_subset = subset(df,(df$price > input$price) & (df$review_scores_rating > input$quality) & 
                         (df$bedrooms > input$bedrooms) & (df$bathrooms > input$bathrooms) )
    print(nrow(df_subset))
    host_info <- paste("Host Id:",df$host_id,"<br/>",
                                         "Host Name:",df$host_name,"<br/>",
                                         "Host has ",df$sum_listings,"other listings.","<br/>"
                                         )
    
    map <- leafletProxy("nymap", session) %>%
      clearShapes() %>%
      clearControls() %>%
      clearGroup(group = c("2016","2017"))%>%
      addCircles(lng = df_subset$longitude,lat = df_subset$latitude,popup = host_info)
    #print(proc.time()-t1)
  })
  
  observeEvent(input$submit2,{
    df = file_change()
    #print("Second call")
    #print(proc.time()-t1)
    #sample = paste(input$in_checkboxgroup,sep = ", ")
    if(is.null(input$in_radio)){
      df_subs=df
    }
    else if(input$in_radio=="CL"){
      df_subs = subset(df,is_commercial=="Commercial Listing")
      print(nrow(df_subs))
      }
    else if(input$in_radio=="HL"){
      df_subs = subset(df,is_commercial=="Household Listing")
      print(nrow(df_subs))
      }
    
    #print("second call first")
    #print(proc.time()-t1)
    
    neighbourhood_costs <- data.frame(aggregate(df_subs$price, by=list(df_subs$neighbourhood_cleansed), FUN=mean))
    names(neighbourhood_costs) <- c("neighbourhood", "avg_cost")
    
    neighbourhood_reviews <- data.frame(aggregate(df_subs$avg_review_value, by=list(df_subs$neighbourhood_cleansed), FUN=mean))
    names(neighbourhood_reviews) <- c("neighbourhood", "avg_review")
    
    neighbourhood_scores <- data.frame(aggregate(df_subs$review_scores_rating, by=list(df_subs$neighbourhood_cleansed), FUN=mean))
    names(neighbourhood_scores) <- c("neighbourhood", "avg_score")
    
    neighbourhood_merged <- merge(neighbourhood_scores, neighbourhood_reviews, by="neighbourhood")
    neighbourhood_merged <- merge(neighbourhood_merged, neighbourhood_costs, by="neighbourhood")
    airbnb_neighbourhoods <- geojsonio::geojson_read("../all Listing Data/neighbourhoods.geojson", what = "sp")
    pal <- colorNumeric("viridis", NULL)
    
    airbnb_neighbourhoods <- sp::merge(airbnb_neighbourhoods, neighbourhood_merged, by="neighbourhood")
    #print(class(airbnb_neighbourhoods))
    
    #print("second call last")
    #print(proc.time()-t1)
    
    if(input$radio=="score"){
      #print("SCORE")
      map <- leafletProxy(mapId = "nymap", session = session,data= airbnb_neighbourhoods) %>%
        clearShapes() %>%
        clearControls() %>% 
        clearGroup(group = c("2016","2017"))%>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1 ,fillColor = pal(airbnb_neighbourhoods$avg_score)) %>%
        leaflet::addLegend(pal = pal, values = ~airbnb_neighbourhoods$avg_score, opacity = 1.0)
    }
    else if(input$radio=="review"){
      #print("REVIEW")
      map <- leafletProxy(mapId = "nymap", session = session,data = airbnb_neighbourhoods) %>%
        clearShapes() %>%
        clearControls() %>% 
        clearGroup(group = c("2016","2017"))%>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1 ,fillColor = pal(airbnb_neighbourhoods$avg_review)) %>%
        leaflet::addLegend(pal = pal, values = ~airbnb_neighbourhoods$avg_review, opacity = 1.0)
    }
    else if(input$radio=="cost"){
      #print("COST")
      map <- leafletProxy(mapId = "nymap", session = session,data = airbnb_neighbourhoods) %>%
        clearShapes() %>%
        clearControls() %>% 
        clearGroup(group = c("2016","2017"))%>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1 ,fillColor = pal(airbnb_neighbourhoods$avg_cost)) %>%
        leaflet::addLegend(pal = pal, values = ~airbnb_neighbourhoods$avg_cost, opacity = 1.0)
    }
  })
  
  v <- reactiveValues(count_button = 0)
  
  month_val <- reactiveValues(cur_month = "")
  
  observeEvent(input$submit3,{
    
    #invalidateLater(2000)
    v$count_button = v$count_button + 1
    
    file_read_short=c("2016_January","2016_February","2016_March","2016_April","2016_May","2016_June",
       "2016_July","2016_August","2016_September","2016_October","2016_November","2016_December",
       "2017_January","2017_February","2017_March","2017_April","2017_May","2017_June",
       "2017_July","2017_August","2017_September","2017_October","2017_November","2017_December")
    #invalidateLater(2000,session = NULL)
    #for(i in file_read_short){
      i = file_read_short[v$count_button]
      #print(v$count_button)
      #print(i)
      wd=getwd()
      month_val$cur_month = isolate(as.character(i))
      
      #print(month_val$cur_month)
      #print("DONE")
      file_to_read <- paste("../all Listing Data/",as.character(i),"_listings.csv",sep="")
      listings_col_to_keep = c("id","name","host_id","host_name","neighbourhood_cleansed",
                               "neighbourhood_group_cleansed","city","state","smart_location","latitude","longitude",
                               "property_type","room_type","bathrooms","bedrooms","price","review_scores_rating",
                               "review_scores_accuracy","review_scores_cleanliness","review_scores_checkin",
                               "review_scores_communication","review_scores_location","review_scores_value",
                               "reviews_per_month","host_listings_count","host_total_listings_count",
                               "calculated_host_listings_count","availability_365","availability_30",
                               "number_of_reviews","summary")
      read_df = data.table::fread(file_to_read,select = listings_col_to_keep )
      read_df = as.data.frame(read_df)
      #read_df = subset(read_df,property_type=="Apartment")
      #read_df = head(read_df,n=100)
      #print(nrow(read_df))
      pal = colorFactor("Set1", domain = read_df$room_type) # Grab a palette
      color_pts = pal(read_df$room_type)
      #t1=proc.time()
      map <- leafletProxy(mapId = "nymap", session = session) %>%
              clearShapes()%>%
              clearControls()%>%
              clearGroup(group = c("2016","2017"))%>%
              addCircles(lat = read_df$latitude,lng = read_df$longitude,color=color_pts) %>%
              addLegend(pal = pal, values = read_df$room_type,title = month_val$cur_month)
      map
      
      #print(proc.time()-t1)
    #}
  })
  
  observeEvent(input$submit4,{
    name = input$multiple
    keyid = as.numeric(unlist(strsplit(name,"-"))[1])
    #print(keyid)
    multiple = data.table::fread("../all Listing Data/total_multiple.csv")
    multiple = as.data.frame(multiple)
    mul_16 = subset(multiple,(host_id == keyid) & (year==2016))
    mul_17 = subset(multiple,(host_id == keyid) & (year==2017))
    
    #print(nrow(mul_16))
    #print(nrow(mul_17))
    
    pal = colorFactor("Set1", domain = mul_16$room_type) # Grab a palette
    color_room = pal(mul_16$room_type)
    
    map <- leafletProxy(mapId = "nymap", session = session) %>%
      setView(lng = -73.7856491, lat = 40.7022541, zoom = 10) %>%
      clearShapes()%>%
      clearControls()%>%
      clearMarkers()%>%
      clearGroup(group = c("2016","2017"))%>%
      addCircles(data =mul_16,group = "2016",color=color_room,popup = paste("Host has ",mul_16$sum_total," listings",sep="")) %>%
      addCircles(data =mul_17, group = "2017",color=color_room,popup = paste("Host has ",mul_17$sum_total," listings",sep="")) %>%
      addLayersControl(overlayGroups = c("2016","2017")) %>%
      addLegend(pal=pal,values = mul_16$room_type)
    
    map
  })
  
  #output$current_month = renderText({print(month_val$cur_month)
  # month_val$cur_month})
  
})
