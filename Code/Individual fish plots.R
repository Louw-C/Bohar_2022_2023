####Individual fish plots#####

#Load detection data
#Make sure to edit the column headings:
#Date_Local
#Time_Local
#Reciever
#Tag_ID (just the tag ID - remove A69-1602-)
#Station_Name
#Latitude
#Longitude
#####Remember to add in coordinates for Movement A and B
#####Fix Mutiaur spelling - wrong in VUE (MUtiaur)
#####Check Siaes spelling and site name spelling throughout
Bohar_all <- read.csv(file.choose()) 
Bohar_all %>% head(5)

#Loaded the tag meta data that Natalie developed
#Revise column names to:
#Tag_Date_Local
#Tag_Time_Local
#Tag_ID (just the tag ID - remove A69-1602-)
#Serial_Number
#Tag_Group
#Species
#FL
#Tag_Location
#Latitude
#Longitude

Bohar_meta <- read.csv(file.choose()) 
#Look at the first 5 rows of data
Bohar_meta %>% head(5)

#This merges two data frames with a common ID - here we used tag.ID
Bohar_database <- merge(Bohar_all,Bohar_meta,by = "Tag_ID")
Bohar_database %>% head(5)
#Make sure tag.ID is a factor
Bohar_database$Tag_ID.f <- as.factor(Bohar_database$Tag_ID)
Bohar_database$Station_Name <- as.factor(Bohar_database$Station_Name)
Bohar_database$Date_Local <- as.factor(Bohar_database$Date_Local)

#Subset to only look at Bohar and sharks
Overview<-Bohar_database %>% filter(Species=="Lutjanus bohar" |
                                      Species==" Carcharhinus amblyrhynchos")
Overview %>% head(5)

#Fullmoon dates
Fullmoon<-data.frame(date=as.Date(c("2022-11-08", "2022-12-07", "2023-01-07", 
                                    "2023-02-06", "2023-03-07", "2023-04-06")))

#Individual fish plots
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

