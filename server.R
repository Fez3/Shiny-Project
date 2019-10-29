
shinyServer(
            
            function(input, output, session) {
              observe({
                click<-input$map_marker_click
                if(is.null(click))
                  return()
                text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
                text2<-paste("You've selected point ", click$id)
                map$clearPopups()
                map$showPopup( click$lat, click$lng, text)
                output$Click_text<-renderText({
                  text2
                })
                
              })
              
             
              
              #updateSelectizeInput(session, "year",choices=x$YEAR, server=TRUE )
               observeEvent(input$disease, {
                 data=lst_data[[selected_disease() ]]
                 x=select(data, -c("YEAR","WEEK"))
                 min=as.Date(min(data$TIME))
                 max=as.Date(max(data$TIME))
                 value=as.Date(min(data$TIME))
                
                 #pal <- colorBin("YlOrRd", domain = as.numeric(unlist(x[50,])), bins = 7)
                 updateSliderInput(session, "Date", min=min,max=max,value=value)
                 
                 
             })
              date_now<-reactive(ifelse( length(row.names(lst_data[[selected_disease() ]][lst_data[[selected_disease() ]]$TIME==as.character(input$Date), ]))==1,as.character(input$Date),as.character(lst_data[[selected_disease() ]]$TIME[which.min(abs(input$Date-lst_data[[selected_disease() ]]$TIME))]) ))
              #updata<-reactive(data=eval(parse(text=selected_disease())))             
             
              
               selected_disease<-reactive({s=input$disease
                return(
                  
                  paste(tolower(s),
                      sep="", collapse="_"))  
                                         })
            
               
               
               
                 plotdata<-reactive({
                word=selected_disease()
                data=lst_data[[word ]]
                plotdata<-data%>%mutate(., "month"=format(TIME,"%B"),"total_reported_cases"=rowSums(select(data, -c("YEAR","WEEK","TIME")))) 
                plotdata$month = factor(plotdata$month, levels = month.name)
                
                return(plotdata)
              
              })
             
               divisor<-reactive(sum(colSums(select(data, -c("YEAR","WEEK","TIME"))) )) 
               
               
               
               
              output$count <- renderLeaflet({
                req(input$disease)
                dis=selected_disease()
                data = lst_data[[dis]]
                
                x=select(data, -c("YEAR","WEEK"))
                bins <- c(0, 20, 50, 100, 300, 1000, 2000, 3000, Inf)
                pal = colorBin("YlOrRd", domain = as.numeric(unlist(x[x$TIME==date_now(),])), bins = bins)
               
                map=leaflet(states) %>%
                  setView(-96, 37.8, 4) %>% #addProviderTiles("Esri.WorldStreetMap") %>%
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
                return(map)
                   
                
              })
              
              output$peak<-renderGvis ({
               
                plt<-plotdata() %>% group_by(., month)%>%summarise(.,total=sum(total_reported_cases)/divisor()) 
                gvisColumnChart(plt, "month", "total",options=list(width=600, height=400,legend="none",title="Total Reported Cases by Season"))
                 # ggplot(plt, aes(x=month,y=total)) +geom_bar(stat="identity") +scale_y_continuous(labels=scales::percent) +
                #ylab("relative frequencies") 
              })
              
              output$heat<-renderPlot ({
               
                plotdata() %>% group_by(., YEAR, month)%>%summarise(.,total=sum(total_reported_cases)) %>% levelplot(total ~ YEAR * month, ., col.regions=cm.colors(12),
                                                                                                               panel = panel.levelplot.points, cex = 1.2, main = list(input$disease,side=1,line=0.5)
                ) + 
                  layer_(panel.2dsmoother(..., n = 200))
              })
              output$slidertime <- renderText(date_now() )
             #output$byState<- renderGvis(plot(gvisScatterChart(chlamydia,options=my_options)))
            })