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
All %>% head(5)

All.plot <- ggplot(data= All, aes(x = Date_Local, y=tag.ID.f)) +
  facet_grid(Station_Name~.)+
  geom_point()+ ggtitle("All detections") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0))
All.plot

All.plot <- ggplot(data= All, aes(x = Date_Local, y=tag.ID.f, color=Station_Name))+
  geom_jitter()+ ggtitle("All detections") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0))
All.plot

All.plot <- ggplot(data= All, aes(x = tag.ID.f, y=Station_Name))+
  geom_point()+ ggtitle("All detections") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0))
All.plot

#Zoom in on a single full moon series (January 2023)
All<-database %>% filter(species=="Lutjanus bohar")
All_Jan<-All %>% 
  filter(Date_Local >= as.Date("2023-01-01"), Date_Local <= as.Date("2023-01-09"))
#Colours
All_Jan.plot <- ggplot(data= All_Jan, aes(x = Date_Local, y=tag.ID.f, color=Station_Name))+
  geom_jitter()+ ggtitle("All detections_January 2023") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0))
All_Jan.plot

#Facet grid
All_Jan.plot <- ggplot(data= All_Jan, aes(x = Date_Local, y=tag.ID.f))+
  facet_grid(Station_Name~.)+
  geom_jitter()+ ggtitle("All detections_January 2023") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0))
All_Jan.plot

