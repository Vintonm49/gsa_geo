# Geospatial Example in R
# LTC Melanie Vinton
# November 2016

# Data downloaded from GSA Inventory of Owned Leased Properties website

library(plyr)
library(dplyr)
library(ggmap)
library(leaflet)
library(sp)
library(rgdal)

VAprop<-read.csv("VA_Govt_Properties.csv")
names(VAprop)

VAprop$Address<-paste(VAprop$STREET_ADDRESS,","," ",VAprop$CITY,","," ","VA", sep="")

# Get Lat/Lons
VAprop2<-VAprop %>% mutate_geocode(Address)

# Map locations
test1<- leaflet() %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(data=VAprop2, lng=~lon, lat=~lat, color = "red", radius = 2,
                   popup=as.character(paste("Latitude:",VAprop2$lat,"<br>",
                                            "Longitude:",VAprop2$lon,"<br>",
                                            "Address:", VAprop2$Address)))
test1

# Munge to fix bad addresses
levels(VAprop$STREET_ADDRESS) <- sub("&","and",levels(VAprop$STREET_ADDRESS))
levels(VAprop$STREET_ADDRESS) <- sub("^8TH&S CRTHSE ROAD$","8th and Courthouse Road",levels(VAprop$STREET_ADDRESS))
levels(VAprop$CITY) <- sub("DULLES AIRPORT","DULLES",levels(VAprop$CITY))
VAprop$Address<-NULL
VAprop$Address<-paste(VAprop$STREET_ADDRESS,","," ",VAprop$CITY,","," ","VA", sep="")
VAprop3<-VAprop %>% mutate_geocode(Address)

test2<- leaflet() %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(data=VAprop3, lng=~lon, lat=~lat, color = "red", radius = 2,
                   popup=as.character(paste("Latitude:",VAprop3$lat,"<br>",
                                            "Longitude:",VAprop3$lon,"<br>",
                                            "Address:", VAprop3$Address)))
test2



# Add Congressional Districts
VArep<-ddply(VAprop3, c("REPRESENTATIVE","CONGRESSIONAL_DISTRICT_CODE"), summarize, reptot=length(LOCATION_CODE))
VArep

levels(VAprop3$CONGRESSIONAL_DISTRICT_CODE) <- sub(1198,"No value",levels(VAprop3$CONGRESSIONAL_DISTRICT_CODE))

VArep<-ddply(VAprop3, c("REPRESENTATIVE","CONGRESSIONAL_DISTRICT_CODE"), summarize, reptot=length(LOCATION_CODE))
VArep


# Add Congressional Districts to Map
districts<-readOGR(dsn='Congressional_2015_us_5m', layer='cb_2015_us_cd114_5m')
dist51VA<-districts[districts$STATEFP==51,]
plot(dist51VA)

names(dist51VA)
names(VAprop3)
VAprop3<-rename(VAprop3,GEOID = CONGRESSIONAL_DISTRICT_CODE)
names(VAprop3)

coordinates(VAprop3)<-~lon+lat
proj4string(VAprop3)<-CRS(proj4string(dist51VA))

dist5101<-dist51VA[dist51VA$GEOID==5101,]
Prop5101<-VAprop3[dist5101,]
Prop5101df<-as.data.frame(Prop5101)
levels(Prop5101df$GEOID) <- sub("No value",5101,levels(Prop5101df$GEOID))
levels(Prop5101df$REPRESENTATIVE) <- sub("undefined","Wittman, Robert J. (R)",levels(Prop5101df$REPRESENTATIVE))
VAprop_imp<-rbind(Prop5101df,Prop5102df,Prop5103df,Prop5104df,Prop5105df,Prop5106df,Prop5107df,Prop5108df,Prop5109df,Prop5110df,Prop5111df)

distreps<-ddply(VAprop_imp, c("GEOID","REPRESENTATIVE"), summarize, reptot=length(LOCATION_CODE))

dist51VA3<-merge(dist51VA,distreps, by = "GEOID")
distpal<-colorQuantile(palette="Purples", domain = dist51VA3$reptot,n=3)

test3<- leaflet() %>% 
  setView(lng = -77.552580, lat = 37.552488, zoom = 7) %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  addPolygons(data=dist51VA3, fillOpacity=0, weight = 1, color="black",
              group = "Districts") %>%
  addPolygons(data=dist51VA3, fillOpacity=.2, fillColor= ~distpal(reptot), 
              color = 'purple',weight = .9, popup=~REPRESENTATIVE,
              group = "Districts Colored")%>%
  addCircleMarkers(data=VAprop_imp, lng=~lon, lat=~lat, color = "red", radius = 2,
                   group = "Properties",
                   popup=as.character(paste("US Rep:",VAprop_imp$REPRESENTATIVE,"<br>",
                                            "City:", VAprop_imp$CITY))) %>%
  addLayersControl(overlayGroups = c("Districts","Districts Colored", "Properties"),
                   options=layersControlOptions(collapsed=FALSE))
test3








