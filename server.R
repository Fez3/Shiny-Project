
shinyServer(
            
            function(input, output, session) {
              observe({
               
                
                
              })
              #updateSelectizeInput(session, "year",choices=x$YEAR, server=TRUE )
               observeEvent(input$disease, {
                 data=eval(parse(text=selected_disease()))
                 x=select(data, -c("YEAR","WEEK"))
                 min=as.Date(min(data$TIME))
                 max=as.Date(max(data$TIME))
                 value=as.Date(min(data$TIME))
                
                 #pal <- colorBin("YlOrRd", domain = as.numeric(unlist(x[50,])), bins = 7)
                 updateSliderInput(session, "Date", min=min,max=max,value=value)
                 
                 
             })
              date_now<-reactive(ifelse( length(row.names(x[x$TIME==as.character(input$Date), ]))==1,as.character(input$Date),as.character(x$TIME[which.min(abs(input$Date-x$TIME))]) ))
              #updata<-reactive(data=eval(parse(text=selected_disease())))             
              selected_disease<-reactive({s=input$disease
                paste(tolower(s),
                      sep="", collapse="_")  
                                         })
              plotdata<-reactive({
                data=eval(parse(text=selected_disease()))
                plotdata<-data%>%mutate(., "month"=format(TIME,"%B"),"total_reported_cases"=rowSums(select(data, -c("YEAR","WEEK","TIME")))) 
                plotdata$month = factor(plotdata$month, levels = month.name)
                
                return(plotdata)
              
              })
              divisor<-reactive(sum(colSums(select(data, -c("YEAR","WEEK","TIME"))) ))  
              output$count <- renderLeaflet({
                dis=selected_disease()
                data=eval(parse(text=selected_disease()))
                x=select(data, -c("YEAR","WEEK"))
                bins <- c(0, 20, 50, 100, 300, 1000, 2000, 3000, Inf)
                pal <- colorBin("YlOrRd", domain = as.numeric(unlist(x[x$TIME==date_now(),])), bins = bins)
                m=leaflet(states) %>%
                  #setView(-96, 37.8, 4) %>%
                  addProviderTiles("MapBox", options = providerTileOptions(
                    id = "mapbox.light",
                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))%>% addPolygons(fillColor = ~pal(as.numeric(unlist(x[x$TIME==date_now(),!(colnames(x)=="TIME") ]))),
                                weight = 2,
                                opacity = 1,
                                layerId = 7,
                                color = "white",
                                dashArray = "3",
                                fillOpacity = 0.7,
                                highlight = highlightOptions(
                                  weight = 5,
                                  color = "#666",
                                  dashArray = "",
                                  fillOpacity = 0.7,
                                  bringToFront = TRUE))
                return(m)
                   
                
              })
              
              output$peak<-renderPlot(
                plotdata() %>% group_by(., month)%>%summarise(.,total=sum(total_reported_cases)) %>% ggplot(., aes(x=month,y=total/divisor())) +geom_bar(stat="identity") +scale_y_continuous(labels=scales::percent) +
                  ylab("relative frequencies") 
              )
            
              output$heat<-renderPlot(
                plotdata() %>% group_by(., YEAR, month)%>%summarise(.,total=sum(total_reported_cases)) %>% levelplot(total ~ YEAR * month, ., 
                                                                                                               panel = panel.levelplot.points, cex = 1.2, main = list(input$disease,side=1,line=0.5)
                ) + 
                  layer_(panel.2dsmoother(..., n = 200))
              )
              output$slidertime <- renderText(date_now() )
            })