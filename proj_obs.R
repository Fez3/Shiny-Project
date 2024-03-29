library(plyr)
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
data=read.csv("./infectious_disease_predictability/Data/INFLUENZA_Cases_1919-1951_20151014132002.csv",stringsAsFactors=FALSE)
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

m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)
x=select(data, -c("YEAR","WEEK"))
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
colStates <- map("state", fill = TRUE,
                 plot = FALSE,
                 region = c("florida", "louisiana", "mississippi",
                            "alabama", "georgia", "tennesse"))

m<- m %>% addPolygons(data=colStates,
                      fillColor = heat.colors(6, alpha = 1),
                      stroke = FALSE)



library(data.table)
#keys <- colnames(data)[!grepl('value',colnames(data))]
keys<-"WEEK"
X <- as.data.table(dat)
X[,lapply(.SD,mean),keys]
dtt=list()
for(j in 1:53){
 pal <- colorBin("YlOrRd", domain = as.numeric(unlist(x[j,])), bins = bins)
  
  

  m %>% addPolygons(
    fillColor = ~pal(as.numeric(unlist(x[j,]))),
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
  m
 Sys.sleep(0.5)
}
######
dat <- structure(list(STATE_CODE = structure(1:52, .Label = colnames(data)[3:54], 
                                             class = "factor"), stateinfection = data[,3:54] ), row.names = c(NA, -8L), class = c("data.table", "data.frame"), .Names = c("STATE_CODE", "stateinfection"))

states@data <- merge(states@data, dat)
######



#Build data.frame with 10 obs + 3 cols
power <- data.frame(
  "Latitude" = c(33.515556, 38.060556, 47.903056, 49.71, 49.041667, 31.934167, 54.140586, 54.140586, 48.494444, 48.494444),
  "Longitude" = c(129.837222, -77.789444, 7.563056, 8.415278, 9.175, -82.343889, 13.664422, 13.664422, 17.681944, 17.681944),
  "start" = do.call(
    "as.Date",
    list(
      x = c("15-Sep-1971", "1-Dec-1971", "1-Feb-1972", "1-Feb-1972", "1-Feb-1972", "1-Feb-1972", "1-Apr-1972", "1-Apr-1972", "24-Apr-1972", "24-Apr-1972"),
      format = "%d-%b-%Y"
    )
  )
)

# set start same as end
#  adjust however you would like
power$end <- power$start + 30


# use geojsonio to convert our data.frame
#  to GeoJSON which timeline expects
power_geo <- geojson_json(power,lat="Latitude",lon="Longitude", pretty = T)

# create a leaflet map on which we will build
leaf <- leaflet() %>%
  addTiles()

# add leaflet-timeline as a dependency
#  to get the js and css
leaf$dependencies[[length(leaf$dependencies)+1]] <- htmlDependency(
  name = "leaflet-timeline",
  version = "1.0.0",
  src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
  script = "javascripts/leaflet.timeline.js",
  stylesheet = "stylesheets/leaflet.timeline.css"
)

# use the new onRender in htmlwidgets to run
#  this code once our leaflet map is rendered
#  I did not spend time perfecting the leaflet-timeline
#  options
leaf %>%
  setView(44.0665,23.74667,2) %>%
  onRender(sprintf(
    '
        function(el,x){
        var power_data = %s;

        var timelineControl = L.timelineSliderControl({
          formatOutput: function(date) {
            return new Date(date).toString();
          }
        });

        var timeline = L.timeline(power_data, {
        pointToLayer: function(data, latlng){
        var hue_min = 120;
        var hue_max = 0;
        var hue = hue_min;
        return L.circleMarker(latlng, {
        radius: 10,
        color: "hsl("+hue+", 100%%, 50%%)",
        fillColor: "hsl("+hue+", 100%%, 50%%)"
        });
        },
        steps: 1000,
        duration: 10000,
        showTicks: true
        });
        timelineControl.addTo(HTMLWidgets.find(".leaflet").getMap());
        timelineControl.addTimelines(timeline);
        timeline.addTo(HTMLWidgets.find(".leaflet").getMap());
        }
        ',
    power_geo
  ))

#pointToLayer: function(data, latlng){
#        var hue_min = 120;
#        var hue_max = 0;
#        var hue = hue_min;
#        return L.circleMarker(latlng, {
#        radius: 10,
#        color: "hsl("+hue+", 100%%, 50%%)",
#        fillColor: "hsl("+hue+", 100%%, 50%%)"
#        });
#}

data=polio
data<-data%>%mutate(., "month"=format(TIME,"%B"),"total_reported_cases"=rowSums(select(data, -c("YEAR","WEEK","TIME")))) 
data$month = factor(data$month, levels = month.name)
cnstnt=sum(data$total_reported_cases)
data %>% group_by(., month)%>%summarise(.,total=sum(total_reported_cases)) %>% ggplot(., aes(x=month, y=total/cnstnt)) +geom_bar(stat="identity")+scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") 

require(lattice)
library(latticeExtra) 
data=influenza
data<-data%>%mutate(., "month"=format(TIME,"%B"),"total_reported_cases"=rowSums(select(data, -c("YEAR","WEEK","TIME")))) 
data$month = factor(data$month, levels = month.name)

data %>% group_by(., YEAR, month)%>%summarise(.,total=sum(total_reported_cases)) %>% levelplot(total ~ YEAR * month, ., 
          panel = panel.levelplot.points, cex = 1.2
) + 
  layer_(panel.2dsmoother(..., n = 200))




sidebarUserPanel("Aaron Festinger", image = img(src="360_borat_lebanon0109.jpg", align = "right"),
                 sidebarMenu(
                   menuItem("Map", tabName = "map", icon = icon("map")),
                   menuItem("Data", tabName = "data", icon = icon("database"))))

tabItems(
  tabItem(tabName = "map",
          fluidRow(box(leafletOutput("count"), 
                       height = 300)
#                   ,box(plotOutput("peak"), 
#                       height = 300))),
#  tabItem(tabName = "data",
#          fluidRow(box(plotOutput("heat")))))

library(plyr)
state="TEXAS"
stateplot=data.frame(whooping_cough[,"TIME"])
colnames(stateplot)[1]<-"TIME"
colnames(stateplot)
for(disease in diseases){
  a=toupper(disease)
  assign(eval(a),data.frame(eval(parse(text=disease))[,c("TIME",state)]))
  colnames(eval(disease))<-eval(disease)
  if(length(a)<5519) a[length(a)+1:5518]<-0
  stateplot[[disease]]<-a

  
}
for(disease in diseases){
       assign(toupper(disease),data.frame(eval(parse(text=disease))[,c("TIME",eval(state))]))}


plot(MUMPS, MEASLES, pch = 22, 
     col = rep(c('forestgreen','blue'),each = 2),main="Capital Allocation Lines", 
     xlab="Standard Deviation", ylab="Expected Return")

plot(c(MUMPS$TIME,MEASLES$TIME, INFLUENZA$TIME, CHLAMYDIA$TIME, HEPATITIS_A$TIME, POLIO$TIME, GONORRHEA$TIME, WHOOPING_COUGH$TIME ), c(MUMPS$TEXAS, MEASLES$TEXAS, INFLUENZA$TEXAS, CHLAMYDIA$TEXAS,HEPATITIS_A$TEXAS, POLIO$TEXAS, GONORRHEA$TEXAS, WHOOPING_COUGH$TEXAS), pch = 22, 
     col = rep(c('forestgreen','blue','yellow','orange','red','purple','black','grey'),each = 2),main="Capital Allocation Lines", 
     xlab="Standard Deviation", ylab="Expected Return")
