library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(gganimate)
library(leaflet)
library(geojsonio)
library(maps)
library(htmlwidgets)
library(htmltools)
library(rgdal)
library(zoo)
library(latticeExtra)
library(shinydashboard)


diseases=c("chlamydia","gonorrhea", "hepatitis_a","influenza","measles","mumps","polio","whooping_cough")
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
Diseases=sapply(diseases,simpleCap)

data=read.csv("./infectious_disease_predictability/Data/INFLUENZA_Cases_1919-1951_20151014132002.csv",stringsAsFactors=FALSE)
data[data=='-']<-0
data[is.na(data)]<-0
data[]<-lapply(data, function(x){as.numeric(x)})
data<-select(data, -c("AMERICAN.SAMOA", "NORTHERN.MARIANA.ISLANDS", "GUAM","PAC.TRUST.TERR","NEW.YORK.CITY","UPSTATE.NEW.YORK", "VIRGIN.ISLANDS" ))
dummy<-select(data, "PUERTO.RICO" )
data<-select(data, -"PUERTO.RICO") %>% mutate(., "PUERTO.RICO"=dummy[,])
data$WEEK<-sprintf("%02d", data$WEEK )
data<-data%>%mutate(., "TIME"=as.Date(paste(YEAR,WEEK,1,sep="" ), '%Y%U%u'))
data$TIME=na.locf(data$TIME)
influenza=data
data=read.csv("./infectious_disease_predictability/Data/CHLAMYDIA_Cases_2006-2014_20160707103149.csv",stringsAsFactors=FALSE)
data[data=='-']<-0
data[is.na(data)]<-0
data[]<-lapply(data, function(x){as.numeric(x)})
data<-select(data, -c("AMERICAN.SAMOA", "NORTHERN.MARIANA.ISLANDS", "GUAM","PAC.TRUST.TERR","NEW.YORK.CITY","UPSTATE.NEW.YORK", "VIRGIN.ISLANDS" ))
dummy<-select(data, "PUERTO.RICO" )
data<-select(data, -"PUERTO.RICO") %>% mutate(., "PUERTO.RICO"=dummy[,])
data$WEEK<-sprintf("%02d", data$WEEK )
data<-data%>%mutate(., "TIME"=as.Date(paste(YEAR,WEEK,1,sep="" ), '%Y%U%u'))
data$TIME=na.locf(data$TIME)
chlamydia=data
data=read.csv("./infectious_disease_predictability/Data/GONORRHEA_Cases_1972-2014_20160707103202.csv",stringsAsFactors=FALSE)
data[data=='-']<-0
data[is.na(data)]<-0
data[]<-lapply(data, function(x){as.numeric(x)})
data<-select(data, -c("AMERICAN.SAMOA", "NORTHERN.MARIANA.ISLANDS", "GUAM","PAC.TRUST.TERR","NEW.YORK.CITY","UPSTATE.NEW.YORK", "VIRGIN.ISLANDS" ))
dummy<-select(data, "PUERTO.RICO" )
data<-select(data, -"PUERTO.RICO") %>% mutate(., "PUERTO.RICO"=dummy[,])
data$WEEK<-sprintf("%02d", data$WEEK )
data<-data%>%mutate(., "TIME"=as.Date(paste(YEAR,WEEK,1,sep="" ), '%Y%U%u'))
data$TIME=na.locf(data$TIME)
gonorrhea=data
data=read.csv("./infectious_disease_predictability/Data/HEPATITIS_A_Cases_1966-2014_20160707103116.csv",stringsAsFactors=FALSE)
data[data=='-']<-0
data[is.na(data)]<-0
data[]<-lapply(data, function(x){as.numeric(x)})
data<-select(data, -c("AMERICAN.SAMOA", "NORTHERN.MARIANA.ISLANDS", "GUAM","PAC.TRUST.TERR","NEW.YORK.CITY","UPSTATE.NEW.YORK", "VIRGIN.ISLANDS" ))
dummy<-select(data, "PUERTO.RICO" )
data<-select(data, -"PUERTO.RICO") %>% mutate(., "PUERTO.RICO"=dummy[,])
data$WEEK<-sprintf("%02d", data$WEEK )
data<-data%>%mutate(., "TIME"=as.Date(paste(YEAR,WEEK,1,sep="" ), '%Y%U%u'))
data$TIME=na.locf(data$TIME)
hepatitis_a=data
data=read.csv("./infectious_disease_predictability/Data/MEASLES_Cases_1909-2001_20150923120449.csv",stringsAsFactors=FALSE)
data[data=='-']<-0
data[is.na(data)]<-0
data[]<-lapply(data, function(x){as.numeric(x)})
data<-select(data, -c("NEW.YORK.CITY" ))
data$WEEK<-sprintf("%02d", data$WEEK )
data<-data%>%mutate(., "TIME"=as.Date(paste(YEAR,WEEK,1,sep="" ), '%Y%U%u'))
data$TIME=na.locf(data$TIME)
measles=data
data=read.csv("./infectious_disease_predictability/Data/MUMPS_Cases_1967-2014_20160707103045.csv",stringsAsFactors=FALSE)
data[data=='-']<-0
data[is.na(data)]<-0
data[]<-lapply(data, function(x){as.numeric(x)})
data<-select(data, -c("AMERICAN.SAMOA", "NORTHERN.MARIANA.ISLANDS", "GUAM","PAC.TRUST.TERR","NEW.YORK.CITY","UPSTATE.NEW.YORK", "VIRGIN.ISLANDS" ))
dummy<-select(data, "PUERTO.RICO" )
data<-select(data, -"PUERTO.RICO") %>% mutate(., "PUERTO.RICO"=dummy[,])
data$WEEK<-sprintf("%02d", data$WEEK )
data<-data%>%mutate(., "TIME"=as.Date(paste(YEAR,WEEK,1,sep="" ), '%Y%U%u'))
data$TIME=na.locf(data$TIME)
mumps=data
data=read.csv("./infectious_disease_predictability/Data/POLIOMYELITIS_Cases_1921-1971_20150923114821.csv",stringsAsFactors=FALSE)
data[data=='-']<-0
data[is.na(data)]<-0
data[]<-lapply(data, function(x){as.numeric(x)})
data<-select(data, -c("NEW.YORK.CITY" ))
data$WEEK<-sprintf("%02d", data$WEEK )
data<-data%>%mutate(., "TIME"=as.Date(paste(YEAR,WEEK,1,sep="" ), '%Y%U%u'))
data$TIME=na.locf(data$TIME)
polio=data
data=read.csv("./infectious_disease_predictability/Data/WHOOPING_COUGH_[PERTUSSIS]_Cases_1909-2014_20160607104756.csv",stringsAsFactors=FALSE)
data[data=='-']<-0
data[is.na(data)]<-0
data[]<-lapply(data, function(x){as.numeric(x)})
data<-select(data, -c("AMERICAN.SAMOA", "NORTHERN.MARIANA.ISLANDS", "GUAM","PAC.TRUST.TERR","NEW.YORK.CITY","UPSTATE.NEW.YORK", "VIRGIN.ISLANDS" ))
dummy<-select(data, "PUERTO.RICO" )
data<-select(data, -"PUERTO.RICO") %>% mutate(., "PUERTO.RICO"=dummy[,])
data$WEEK<-sprintf("%02d", data$WEEK )
data<-data%>%mutate(., "TIME"=as.Date(paste(YEAR,WEEK,1,sep="" ), '%Y%U%u'))
data$TIME=na.locf(data$TIME)
whooping_cough=data






lst_data = list(
  
  chlamydia = chlamydia,
  gonorrhea = gonorrhea,
  hepatitis_a=hepatitis_a,
  influenza = influenza,
  measles=measles,
  mumps=mumps,
  polio=polio,
  whooping_cough=whooping_cough
)

stt=toupper( state.name)
states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
                                  , what = "sp")





