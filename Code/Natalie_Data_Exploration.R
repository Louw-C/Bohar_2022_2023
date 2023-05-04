############Move onto Natalie's code - data exploration

Bohar_all <- read.csv(file.choose()) 
Bohar_all %>% head(5)

#Loaded the tag meta data that Natalie developed (Revised the column names)
Bohar_meta <- read.csv(file.choose()) 
#Look at the first 5 rows of data
Bohar_meta %>% head(5)

#This merges two data frames with a common ID - here we used tag.ID
Bohar_database <- merge(Bohar_all,Bohar_meta,by = "Tag_ID")
Bohar_database %>% head(5)
#Make sure tag.ID is a factor
Bohar_database$Tag_ID.f <- as.factor(Bohar_database$Tag_ID)
Bohar_database$Station_Name <- as.factor(Bohar_database$Station_Name)

#Develop plots to look at overall patterns - look at bohar and grey reefs
Overview<-Bohar_database %>% filter(Species=="Lutjanus bohar" |
                                      Species==" Carcharhinus amblyrhynchos")
Overview %>% head(5)


Overview.plot1 <- ggplot(data= Overview, aes(x = Date_Local, y=Tag_ID.f, color=Species)) +
  facet_grid(Station_Name~.)+
  geom_point()+ ggtitle("All detections") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0))
Overview.plot1

Overview.plot2 <- ggplot(data= Overview, aes(x = Date_Local, y=Tag_ID.f, color=Station_Name))+
  facet_grid(Species~.)+
  geom_point()+ ggtitle("All detections") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0))
Overview.plot2

Overview.plot3 <- ggplot(data= Overview, aes(x = Tag_ID.f, y=Station_Name, colour=Species))+
  geom_point()+ ggtitle("All detections") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0))+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Overview.plot3

#Pick out a single fish
#Bohar 14021
Bohar_14021<-Bohar_database %>% filter(Tag_ID.f=="14021")
Bohar_14021.plot1<- ggplot(data= Bohar_14021, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_14021") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
                                                ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_14021.plot1

#Bohar 46650 
Bohar_46650<-Bohar_database %>% filter(Tag_ID.f=="46650")

Bohar_46650.plot1<- ggplot(data= Bohar_46650, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_46650") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
                                                ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_46650.plot1
#Such an interesting pattern between Rebotel and Mutiaur - linked with full moon

###Bohar_51648
Bohar_51648<-Bohar_database %>% filter(Tag_ID.f=="51648")
Bohar_51648.plot1<- ggplot(data= Bohar_51648, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_51648") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
                                                ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_51648.plot1

#Bohar 51647
Bohar_51647<-Bohar_database %>% filter(Tag_ID.f=="51647")
Bohar_51647.plot1<- ggplot(data= Bohar_51647, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_51648") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
                                                ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_51647.plot1

#Bohar51646
Bohar_51646<-Bohar_database %>% filter(Tag_ID.f=="51646")
Bohar_51646.plot1<- ggplot(data= Bohar_51646, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_51646") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_51646.plot1

#Bohar 51645
Bohar_51645<-Bohar_database %>% filter(Tag_ID.f=="51645")
Bohar_51645.plot1<- ggplot(data= Bohar_51645, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_51645") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_51645.plot1

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

