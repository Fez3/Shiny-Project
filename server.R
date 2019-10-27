
shinyServer(
            
            function(input, output, session) {
              observe({
                dest <- unique(flights[origin == input$origin, dest])
                states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
                                                  , what = "sp")
                bins <- c(0, 20, 50, 100, 300, 1000, 2000, 3000, Inf)
                pal <- colorBin("YlOrRd", domain = as.numeric(unlist(x[1000,])), bins = bins)
              updateSelectizeInput(session, "dest", 
                                   choices=unique(flights[flights$origin==input$origin,'dest']),
                                   selected=unique(flights[flights$origin==input$origin,'dest']),
             # updateSliderInput(session, "Date", datenow)
                                   
              )})
              date_now<-reactive(ifelse( length(row.names(x[x$TIME==as.character(input$Date), ]))==1,as.character(input$Date),as.character(x$TIME[which.min(abs(input$Date-x$TIME))]) ))
              flights_delay <- reactive({
                flights %>%
                  filter(origin == input$origin & dest == input$dest & month==input$month) %>%
                  group_by(carrier) %>%
                  summarise(n=n(), arrival=mean(arr_delay), departure=mean(dep_delay))
                
                
              })
              output$delay <- renderPlot(
                flights_delay() %>% 
                  gather(key = type, value = delay, departure, arrival) %>%
                  ggplot(aes(x = carrier, y = delay, fill = type)) +
                  geom_col(position = "dodge") + ggtitle("Average delay")
              )
              output$count <- renderLeaflet(
                
                
                leaflet(states) %>%
                  setView(-96, 37.8, 4) %>%
                  addProviderTiles("MapBox", options = providerTileOptions(
                    id = "mapbox.light",
                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
                %>% addPolygons(fillColor = ~pal(as.numeric(unlist(x[x$TIME==date_now(),!(colnames(x)=="TIME") ]))),
                                weight = 2,
                                opacity = 1,
                                color = "white",
                                dashArray = "3",
                                fillOpacity = 0.7,
                                highlight = highlightOptions(
                                  weight = 5,
                                  color = "#666",
                                  dashArray = "",
                                  fillOpacity = 0.7,
                                  bringToFront = TRUE))
                   
                
              )
              
              output$dataf <- renderTable(
                flights_delay() 
              )
              output$slidertime <- renderText(date_now() )
            })