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

# the next 5 lines are repeated for each of the 11 districts in VA
dist5101<-dist51VA[dist51VA$GEOID==5101,]
Prop5101<-VAprop3[dist5101,]
Prop5101df<-as.data.frame(Prop5101)
levels(Prop5101df$GEOID) <- sub("No value",5101,levels(Prop5101df$GEOID))
levels(Prop5101df$REPRESENTATIVE) <- sub("undefined","Wittman, Robert J. (R)",levels(Prop5101df$REPRESENTATIVE))

dist5102<-dist51VA[dist51VA$GEOID==5102,]
Prop5102<-VAprop3[dist5102,]
Prop5102df<-as.data.frame(Prop5102)
levels(Prop5102df$GEOID) <- sub("No value",5102,levels(Prop5102df$GEOID))
levels(Prop5102df$REPRESENTATIVE) <- sub("undefined","Rigell, Scott (R)",levels(Prop5102df$REPRESENTATIVE))

dist5103<-dist51VA[dist51VA$GEOID==5103,]
Prop5103<-VAprop3[dist5103,]
Prop5103df<-as.data.frame(Prop5103)
levels(Prop5103df$GEOID) <- sub("No value",5103,levels(Prop5103df$GEOID))
levels(Prop5103df$REPRESENTATIVE) <- sub("undefined","Scott, Robert C. (D)",levels(Prop5103df$REPRESENTATIVE))

dist5104<-dist51VA[dist51VA$GEOID==5104,]
Prop5104<-VAprop3[dist5104,]
Prop5104df<-as.data.frame(Prop5104)
levels(Prop5104df$GEOID) <- sub("No value",5104,levels(Prop5104df$GEOID))
levels(Prop5104df$REPRESENTATIVE) <- sub("undefined","Forbes, J. Randy (R)",levels(Prop5104df$REPRESENTATIVE))

dist5105<-dist51VA[dist51VA$GEOID==5105,]
Prop5105<-VAprop3[dist5105,]
Prop5105df<-as.data.frame(Prop5105)
levels(Prop5105df$GEOID) <- sub("No value",5105,levels(Prop5105df$GEOID))
levels(Prop5105df$REPRESENTATIVE) <- sub("undefined","Hurt, Robert (R)",levels(Prop5105df$REPRESENTATIVE))

dist5106<-dist51VA[dist51VA$GEOID==5106,]
Prop5106<-VAprop3[dist5106,]
Prop5106df<-as.data.frame(Prop5106)
levels(Prop5106df$GEOID) <- sub("No value",5106,levels(Prop5106df$GEOID))
levels(Prop5106df$REPRESENTATIVE) <- sub("undefined","Goodlatte, Bob (R)",levels(Prop5106df$REPRESENTATIVE))

dist5107<-dist51VA[dist51VA$GEOID==5107,]
Prop5107<-VAprop3[dist5107,]
Prop5107df<-as.data.frame(Prop5107)
levels(Prop5107df$GEOID) <- sub("No value",5107,levels(Prop5107df$GEOID))
levels(Prop5107df$REPRESENTATIVE) <- sub("undefined","Brat, Dave (R)",levels(Prop5107df$REPRESENTATIVE))

dist5108<-dist51VA[dist51VA$GEOID==5108,]
Prop5108<-VAprop3[dist5108,]
Prop5108df<-as.data.frame(Prop5108)
levels(Prop5108df$GEOID) <- sub("No value",5108,levels(Prop5108df$GEOID))
levels(Prop5108df$REPRESENTATIVE) <- sub("undefined","Beyer, Don (D)",levels(Prop5108df$REPRESENTATIVE))

dist5109<-dist51VA[dist51VA$GEOID==5109,]
Prop5109<-VAprop3[dist5109,]
Prop5109df<-as.data.frame(Prop5109)
levels(Prop5109df$GEOID) <- sub("No value",5109,levels(Prop5109df$GEOID))
levels(Prop5109df$REPRESENTATIVE) <- sub("undefined","Griffith, Morgan (R)",levels(Prop5109df$REPRESENTATIVE))

dist5110<-dist51VA[dist51VA$GEOID==5110,]
Prop5110<-VAprop3[dist5110,]
Prop5110df<-as.data.frame(Prop5110)
levels(Prop5110df$GEOID) <- sub("No value",5110,levels(Prop5110df$GEOID))
levels(Prop5110df$REPRESENTATIVE) <- sub("undefined","Comstock, Barbara (R)",levels(Prop5110df$REPRESENTATIVE))

dist5111<-dist51VA[dist51VA$GEOID==5111,]
Prop5111<-VAprop3[dist5111,]
Prop5111df<-as.data.frame(Prop5111)
levels(Prop5111df$GEOID) <- sub("No value",5111,levels(Prop5111df$GEOID))
levels(Prop5111df$REPRESENTATIVE) <- sub("undefined","Connolly, Gerald E. Gerry (D)",levels(Prop5111df$REPRESENTATIVE))

# Bind the resulting 11 dataframes
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








