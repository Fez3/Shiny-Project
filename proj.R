library(dplyr)
library(ggplot2)
library(gganimate)
library(leaflet)
library(geojsonio)
library(maps)
library(htmlwidgets)
library(htmltools)
library(rgdal)
setwd("C:/Users/fezro/OneDrive/Desktop/Git_Projects/Shiny Project")
data=read.csv("./infectious_disease_predictability/Data/flu_clean.csv",stringsAsFactors=FALSE)

data=read.csv("./infectious_disease_predictability/Data/HEPATITIS_A_Cases_1966-2014_20160707103116.csv",stringsAsFactors=FALSE)
data[data=='-']<-0
data[]<-lapply(data, function(x){as.numeric(x)})
data<-select(data, -c("AMERICAN.SAMOA", "NORTHERN.MARIANA.ISLANDS", "GUAM","PAC.TRUST.TERR","NEW.YORK.CITY","UPSTATE.NEW.YORK", "VIRGIN.ISLANDS" ))
dummy<-select(data, "PUERTO.RICO" )
data<-select(data, -"PUERTO.RICO") %>% mutate(., "PUERTO.RICO"=dummy[,])
data$WEEK<-sprintf("%02d", data$WEEK )
data<-data%>%mutate(., "TIME"=as.Date(paste(YEAR,WEEK,1,sep="" ), '%Y%U%u'))
#data[which(length(data$WEEK)==1),"TIME"]<-as.Date(paste(data$YEAR,0,data$WEEK,1,sep="" ), '%Y%U%u')

stt=toupper( state.name)
states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
                                  , what = "sp")
colnames(data)<-c("YEAR", "WEEK", as.character(states$name), "TIME")



m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
m %>% addPolygons()
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)


pal <- colorBin("YlOrRd", domain = as.numeric(unlist(x[1000,])), bins = bins)



m %>% addPolygons(
  fillColor = ~pal(as.numeric(unlist(x[1000,]))),
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

leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))%>% debug( addPolygons(
  fillColor = ~pal(as.numeric(unlist(x[1000,]))),
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
    bringToFront = TRUE)) )

)


######
dat <- structure(list(STATE_CODE = structure(1:52, .Label = colnames(data)[3:54], 
                                             class = "factor"), stateinfection = data[,3:54] ), row.names = c(NA, -8L), class = c("data.table", "data.frame"), .Names = c("STATE_CODE", "stateinfection"))

states@data <- merge(states@data, dat)
######

    