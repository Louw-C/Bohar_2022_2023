require(tidyverse)
require(actel)
require(raster)
require(gdistance)
require(rgdal)
require(rgeos)
require(adehabitatHR)

#Set your working directory (remember to change \ to \\)
setwd("E:\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023")
getwd()

#Upload your BIOMETRICS (fish and their tags)
Biometrics<-read.csv("E:\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023\\Data\\Bohar_test\\Biometrics.csv")
names(Biometrics)
Biometrics


#sort out time and date (combine and make sure of format)
#unite date and time in two separate columns into a new merged one - date.time
Biometrics<-Biometrics %>% unite('Release.date.time', Release.date,Release.time, remove=FALSE)
#Sort out the format of the date and time
Biometrics<-Biometrics %>% mutate(Release.date.time=ymd_hm(Release.date.time))
Biometrics <- Biometrics %>% 
  mutate(Release.date1 = parse_date_time(Release.date, 
                                          orders = "%Y-%m-%d %H:%M"))
#Save the formatted data frame
write.csv(Biometrics,"E:\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023\\Data.csv", row.names=FALSE)

#Upload DEPLOYMENTS
Deployments<-read.csv("E:\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023\\Data\\Bohar_test\\Deployments.csv")
names(Deployments)
#Sort out the format of the date and time
Deployments<-Deployments %>% mutate(Start=ymd_hm(Start))
Deployments<- Deployments %>% 
  mutate(Start = parse_date_time(Start, orders = "%Y-%m-%d %H:%M"))
Deployments<-Deployments %>% mutate(Stop=ymd_hm(Stop))
Deployments<- Deployments %>% 
  mutate(Stop = parse_date(Stop, orders = "%Y-%m-%d %H:%M"))

Deployments<-Deployments %>% mutate(Stop=mdy(Stop))
#Save the formatted data frame
write.csv(Deployments,"E:http://127.0.0.1:11467/graphics/plot_zoom_png?width=1383&height=794\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023\\Data\\Deployments.csv", row.names=FALSE)


#Upload SPATIAL
Spatial<-read.csv("E:\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023\\Data\\Spatial.csv")
names(Spatial)

#time zone to use - Pacific/Guam
getwd()
setwd("E:\\Palau/Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023\\Data\\Bohar_test")
createWorkspace("Bohar_test")

Bohar.results<-explore(tz = "Pacific/Guam", report=TRUE, save.detections = TRUE)
plotDetections(Bohar.results,'NA-60864',y.axis=c("stations"), 
               ylab=c("Station names"))
Bohar.results

ID60861 <- filter(database,tag.ID == 60861)
ggplot(data= ID60861, aes(x = date.local, y=site.name, color = species)) +
  geom_point()+ ggtitle("Tag.ID 60861")

############Move onto Natalie's code - data exploration

Bohar_all <- read.csv(file.choose()) 
Bohar_all %>% head(5)

#Loaded the tag meta data that Natalie developed
Bohar_meta <- read.csv(file.choose()) 
#Look at the first 5 rows of data
Bohar_meta %>% head(5)
#This code selects certain columns only (make sure to state that dplyr:: should be used)
Bohar_meta <- Bohar_meta %>% dplyr::select(tag.ID,species,FL,sex,
                            site,Latitude,Longitude,
                            date.local,time.local,tag.group)
#Check the first 5 rows of data
Bohar_meta %>% head(5)
colnames(Bohar_meta) <- c("tag.ID","species","FL","sex",
                        "deploy.site","deploy.lat","deploy.lon",
                        "deploy.date","deploy.time",
                        "tag.group")
#This merges two data frames with a common ID - here we used tag.ID
database <- merge(Bohar_all,Bohar_meta,by = "tag.ID")
database %>% head(5)

#Make sure tag.ID is a factor
database$tag.ID.f <- as.factor(database$tag.ID)

#only look at Mutiaur 2 - and see what tags were detected across time
#Filter whole database to only Mutiaur 2
Mut2 <- database %>% filter(Station_Name == "Mutiaur 2")
#Filter all species to just look at bohar
Mut2<-Mut2 %>% filter(species=="Lutjanus bohar")

#Plot detection of tags across time at Mutiaur 2
Mut2.plot <- ggplot(data= Mut2, aes(x = Date_Local, y=tag.ID.f)) +
geom_point()+ ggtitle("Mutiaur 2") + theme(axis.text.x=element_text(angle=90))
Mut2.plot

#only look at Mutiaur 6 - and see what tags were detected across time
#Filter whole database to only Mutiaur 6
Mut6 <- database %>% filter(Station_Name == "Mutiaur 6")
#Filter all species to just look at bohar
Mut6<-Mut6 %>% filter(species=="Lutjanus bohar")

#Plot detection of tags across time at Mutiaur 6
Mut6.plot <- ggplot(data= Mut6, aes(x = Date_Local, y=tag.ID.f)) +
  geom_point()+ ggtitle("Mutiaur 6") + theme(axis.text.x=element_text(angle=90))
Mut6.plot

#Look at detections only at Mutiaur
Mut <- database %>% filter(Station_Name == "Mutiaur 6"|
                             Station_Name == "MUtiaur 1"|
                             Station_Name == "Mutiaur 2"|
                             Station_Name == "Mutiaur 3"|
                             Station_Name == "Mutiaur 4"|
                             Station_Name ==  "Mutiaur 5")
#Filter all species to just look at bohar
Mut<-Mut %>% filter(species=="Lutjanus bohar")
#Look at specific time series
Mut<-Mut %>% 
  filter(Date_Local >= as.Date("2023-01-01"), Date_Local <= as.Date("2023-01-09"))
#Plot detection of tags across time at Mutiaur 6
Mut.plot <- ggplot(data= Mut, aes(x = Date_Local, y=tag.ID.f, color=Station_Name)) +
  geom_point()+ ggtitle("Mutiaur") + theme(axis.text.x=element_text(angle=90))
Mut.plot

#Look at all bohar across all receivers
All<-database %>% filter(species=="Lutjanus bohar")

All.plot <- ggplot(data= All, aes(x = Date_Local, y=tag.ID.f)) +
  facet_grid(Station_Name~.)+
  geom_point()+ ggtitle("All detections") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0))
All.plot

All.plot <- ggplot(data= All, aes(x = tag.ID.f, y=Station_Name))+
  geom_point()+ ggtitle("All detections") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0))
All.plot
