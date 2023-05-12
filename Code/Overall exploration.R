####Overall exploration
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

