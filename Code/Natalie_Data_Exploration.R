############Move onto Natalie's code - data exploration
require(gridExtra)

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

#Bohar 51644
Bohar_51644<-Bohar_database %>% filter(Tag_ID.f=="51644")
Bohar_51644.plot1<- ggplot(data= Bohar_51644, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_51644") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_51644.plot1

#Bohar 51643
Bohar_51643<-Bohar_database %>% filter(Tag_ID.f=="51643")
Bohar_51643.plot1<- ggplot(data= Bohar_51643, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_51643") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_51643.plot1

#Bohar 51642
Bohar_51642<-Bohar_database %>% filter(Tag_ID.f=="51642")
Bohar_51642.plot1<- ggplot(data= Bohar_51642, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_51642") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_51642.plot1


#Bohar 60864
Bohar_60864<-Bohar_database %>% filter(Tag_ID.f=="60864")
Bohar_60864.plot1<- ggplot(data= Bohar_60864, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_60864") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_60864.plot1

#Bohar 60863
Bohar_60863<-Bohar_database %>% filter(Tag_ID.f=="60863")
Bohar_60863.plot1<- ggplot(data= Bohar_60863, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_60863") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_60863.plot1

#Bohar 60861
Bohar_60861<-Bohar_database %>% filter(Tag_ID.f=="60861")
Bohar_60861.plot1<- ggplot(data= Bohar_60861, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_60861") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_60861.plot1

#Bohar 60860
Bohar_60860<-Bohar_database %>% filter(Tag_ID.f=="60860")
Bohar_60860.plot1<- ggplot(data= Bohar_60860, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_60860") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_60860.plot1

#Bohar 46655
Bohar_46655<-Bohar_database %>% filter(Tag_ID.f=="46655")
Bohar_46655.plot1<- ggplot(data= Bohar_46655, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_46655") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_46655.plot1

#Bohar 46654
Bohar_46654<-Bohar_database %>% filter(Tag_ID.f=="46654")
Bohar_46654.plot1<- ggplot(data= Bohar_46654, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_46654") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_46654.plot1

#Bohar 46653
Bohar_46653<-Bohar_database %>% filter(Tag_ID.f=="46653")
Bohar_46653.plot1<- ggplot(data= Bohar_46653, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_46653") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_46653.plot1

#Bohar 46652
Bohar_46652<-Bohar_database %>% filter(Tag_ID.f=="46652")
Bohar_46652.plot1<- ggplot(data= Bohar_46652, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_46652") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_46652.plot1

#Bohar 46649
Bohar_46649<-Bohar_database %>% filter(Tag_ID.f=="46649")
Bohar_46649.plot1<- ggplot(data= Bohar_46649, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_46649") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_46649.plot1

#Bohar 46647
Bohar_46647<-Bohar_database %>% filter(Tag_ID.f=="46647")
Bohar_46647.plot1<- ggplot(data= Bohar_46647, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_46647") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_46647.plot1

#Bohar 14047
Bohar_14047<-Bohar_database %>% filter(Tag_ID.f=="14047")
Bohar_14047.plot1<- ggplot(data= Bohar_14047, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_14047") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_14047.plot1

#Bohar 14020
Bohar_14020<-Bohar_database %>% filter(Tag_ID.f=="14020")
Bohar_14020.plot1<- ggplot(data= Bohar_14020, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_14020") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_14020.plot1


#Bohar 14011
Bohar_14011<-Bohar_database %>% filter(Tag_ID.f=="14011")
Bohar_14011.plot1<- ggplot(data= Bohar_14011, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_14011") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_14011.plot1

#Bohar 60855
Bohar_60855<-Bohar_database %>% filter(Tag_ID.f=="60855")
Bohar_60855.plot1<- ggplot(data= Bohar_60855, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_60855") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_60855.plot1

#Bohar 60856
Bohar_60856<-Bohar_database %>% filter(Tag_ID.f=="60856")
Bohar_60856.plot1<- ggplot(data= Bohar_60856, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_60856") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_60856.plot1

#Bohar 60859
Bohar_60859<-Bohar_database %>% filter(Tag_ID.f=="60859")
Bohar_60859.plot1<- ggplot(data= Bohar_60859, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_60859") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_60859.plot1


#Bohar 60862
Bohar_60862<-Bohar_database %>% filter(Tag_ID.f=="60862")
Bohar_60862.plot1<- ggplot(data= Bohar_60862, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_60862") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_60862.plot1

#Bohar 60857
Bohar_60857<-Bohar_database %>% filter(Tag_ID.f=="60857")
Bohar_60857.plot1<- ggplot(data= Bohar_60857, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_60857") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_60857.plot1

#Bohar 60858
Bohar_60858<-Bohar_database %>% filter(Tag_ID.f=="60858")
Bohar_60858.plot1<- ggplot(data= Bohar_60858, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_60858") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_60858.plot1

#Bohar 46646
Bohar_46646<-Bohar_database %>% filter(Tag_ID.f=="46646")
Bohar_46646.plot1<- ggplot(data= Bohar_46646, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_46646") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_46646.plot1

#Bohar 46648
Bohar_46648<-Bohar_database %>% filter(Tag_ID.f=="46648")
Bohar_46648.plot1<- ggplot(data= Bohar_46648, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_46648") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_46648.plot1

#Bohar 46651
Bohar_46651<-Bohar_database %>% filter(Tag_ID.f=="46651")
Bohar_46651.plot1<- ggplot(data= Bohar_46651, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_46651") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_46651.plot1

#Bohar 14010
Bohar_14010<-Bohar_database %>% filter(Tag_ID.f=="14010")
Bohar_14010.plot1<- ggplot(data= Bohar_14010, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_14010") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_14010.plot1

#Bohar 14026
Bohar_14026<-Bohar_database %>% filter(Tag_ID.f=="14026")
Bohar_14026.plot1<- ggplot(data= Bohar_14026, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_14026") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_14026.plot1

#Bohar 5960
Bohar_5960<-Bohar_database %>% filter(Tag_ID.f=="5960")
Bohar_5960.plot1<- ggplot(data= Bohar_5960, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Bohar_5960") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                                axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Bohar_5960.plot1

grid.arrange(Bohar_14011.plot1,Bohar_14020.plot1,Bohar_14047.plot1, 
             Bohar_46647.plot1,Bohar_46649.plot1,Bohar_46652.plot1,
             nrow = 3,ncol=2)

grid.arrange(Bohar_46653.plot1,Bohar_46654.plot1,Bohar_46655.plot1,
             Bohar_60860.plot1,Bohar_60861.plot1,Bohar_60863.plot1,
             nrow = 3,ncol=2)

grid.arrange(Bohar_60864.plot1,Bohar_14021.plot1,Bohar_51648.plot1,
             Bohar_51647.plot1,Bohar_51646.plot1,Bohar_51645.plot1,
             nrow = 3,ncol=2)

grid.arrange(Bohar_14026.plot1,Bohar_14010.plot1,Bohar_46651.plot1,
             Bohar_46648.plot1,Bohar_46646.plot1, Bohar_60858.plot1,
             nrow = 3,ncol=2)

grid.arrange(Bohar_60857.plot1,Bohar_60862.plot1,Bohar_60859.plot1,
             Bohar_60856.plot1,Bohar_60855.plot1,Bohar_14021.plot1,
             nrow = 3,ncol=2)

grid.arrange(Bohar_51644.plot1,Bohar_60864.plot1,Bohar_46650.plot1,
             Bohar_51647.plot1,Bohar_51646.plot1,Bohar_51642.plot1,
             nrow = 3,ncol=2)

grid.arrange(Bohar_51648.plot1,Bohar_51643.plot1,
             nrow = 3,ncol=2)



######Plot Sharks########

#Grey 49996 - no detections?


#Grey 49991
Grey_49991<-Bohar_database %>% filter(Tag_ID.f=="49991")
Grey_49991.plot1<- ggplot(data= Grey_49991, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_49991") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_49991.plot1

#Grey 63786
Grey_63786<-Bohar_database %>% filter(Tag_ID.f=="63786")
Grey_63786.plot1<- ggplot(data= Grey_63786, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_63786") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_63786.plot1

#Grey 49993
Grey_49993<-Bohar_database %>% filter(Tag_ID.f=="49993")
Grey_49993.plot1<- ggplot(data= Grey_49993, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_49993") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_49993.plot1

#Grey 63782
Grey_63782<-Bohar_database %>% filter(Tag_ID.f=="63782")
Grey_63782.plot1<- ggplot(data= Grey_63782, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_63782") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_63782.plot1

#Grey 63788 - no detections?

#Grey 49994
Grey_49994<-Bohar_database %>% filter(Tag_ID.f=="49994")
Grey_49994.plot1<- ggplot(data= Grey_49994, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_49994") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_49994.plot1

#Grey 63784
Grey_63784<-Bohar_database %>% filter(Tag_ID.f=="63784")
Grey_63784.plot1<- ggplot(data= Grey_63784, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_63784") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_63784.plot1


#Grey 49998
Grey_49998<-Bohar_database %>% filter(Tag_ID.f=="49998")
Grey_49998.plot1<- ggplot(data= Grey_49998, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_49998") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_49998.plot1

#Grey 49999
Grey_49999<-Bohar_database %>% filter(Tag_ID.f=="49999")
Grey_49999.plot1<- ggplot(data= Grey_49999, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_49999") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_49999.plot1

#Grey 49992
Grey_49992<-Bohar_database %>% filter(Tag_ID.f=="49992")
Grey_49992.plot1<- ggplot(data= Grey_49992, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_49992") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_49992.plot1

#Grey 63781
Grey_63781<-Bohar_database %>% filter(Tag_ID.f=="63781")
Grey_63781.plot1<- ggplot(data= Grey_63781, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_63781") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_63781.plot1

#Grey 63789
Grey_63789<-Bohar_database %>% filter(Tag_ID.f=="63789")
Grey_63789.plot1<- ggplot(data= Grey_63789, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_63789") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_63789.plot1

#Grey 49995
Grey_49995<-Bohar_database %>% filter(Tag_ID.f=="49995")
Grey_49995.plot1<- ggplot(data= Grey_49995, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_49995") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_49995.plot1

#Grey 49997
Grey_49997<-Bohar_database %>% filter(Tag_ID.f=="49997")
Grey_49997.plot1<- ggplot(data= Grey_49997, aes(x = Date_Local, y=Station_Name))+
  geom_jitter()+ ggtitle("Grey_49997") + theme(axis.text.x=element_text(angle=90),strip.text.y = element_text(angle = 0),
                                               axis.title.x=element_blank()) +
  ylab("Station name")+
  scale_y_discrete(limits = c("Blue corner", "Movement A", "Movement 8", "Movement 7", 
                              "Movement 6","Movement 5","Movement 4","Mutiaur 3", "Mutiaur 4",
                              "Mutiaur 5","Mutiaur 2", "Mutiaur 1","Mutiaur 6",
                              "Movement 3", "Movement 2", "Movement 1","Rebotel","Movement B",
                              "Ulong channel","Siaes corner","Back reef 1"))
Grey_49997.plot1

grid.arrange(Grey_49997.plot1,Grey_49995.plot1,Grey_63789.plot1,
             Grey_63781.plot1, Grey_49992.plot1, Grey_49999.plot1,
             nrow = 3,ncol=2)

grid.arrange(Grey_49998.plot1,Grey_63784.plot1,Grey_49994.plot1,
             Grey_63782.plot1, Grey_49993.plot1, Grey_63786.plot1,
             nrow = 3,ncol=2)

Grey_49991.plot1

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

