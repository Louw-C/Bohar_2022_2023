###Individual receiver plots####

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

#Subset to only look at Bohar and sharks
Receiver_Overview<-Bohar_database %>% filter(Species=="Lutjanus bohar" |
                                      Species==" Carcharhinus amblyrhynchos")
Receiver_Overview %>% head(5)



#only look at Mutiaur 2 - and see what tags were detected across time
#Filter whole database to only Mutiaur 2
Mut2 <- Receiver_Overview %>% filter(Station_Name == "Mutiaur 2")
Mut2 %>% head(5)

#Plot detection of tags across time at Mutiaur 2
Mut2.plot <- ggplot(data= Mut2, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Mutiaur 2") + theme(axis.text.x=element_text(angle=90))
Mut2.plot

#only look at Mutiaur 6 - and see what tags were detected across time
#Filter whole database to only Mutiaur 6
Mut6 <- Receiver_Overview %>% filter(Station_Name == "Mutiaur 6")

#Plot detection of tags across time at Mutiaur 6
Mut6.plot <- ggplot(data= Mut6, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Mutiaur 6") + theme(axis.text.x=element_text(angle=90))
Mut6.plot


#only look at Mutiaur 1 - and see what tags were detected across time
#Filter whole database to only Mutiaur 1
Mut1 <- Receiver_Overview %>% filter(Station_Name == "Mutiaur 1")

#Plot detection of tags across time at Mutiaur 6
Mut1.plot <- ggplot(data= Mut1, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Mutiaur 1") + theme(axis.text.x=element_text(angle=90))
Mut1.plot

#only look at Mutiaur 3 - and see what tags were detected across time
#Filter whole database to only Mutiaur 3
Mut3 <- Receiver_Overview %>% filter(Station_Name == "Mutiaur 3")

#Plot detection of tags across time at Mutiaur 3
Mut3.plot <- ggplot(data= Mut3, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Mutiaur 3") + theme(axis.text.x=element_text(angle=90))
Mut3.plot

#only look at Mutiaur 5 - and see what tags were detected across time
#Filter whole database to only Mutiaur 5
Mut5 <- Receiver_Overview %>% filter(Station_Name == "Mutiaur 5")

#Plot detection of tags across time at Mutiaur 5
Mut5.plot <- ggplot(data= Mut5, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Mutiaur 5") + theme(axis.text.x=element_text(angle=90))
Mut5.plot

#only look at Mutiaur 4 - and see what tags were detected across time
#Filter whole database to only Mutiaur 4
Mut4 <- Receiver_Overview %>% filter(Station_Name == "Mutiaur 4")

#Plot detection of tags across time at Mutiaur 6
Mut4.plot <- ggplot(data= Mut4, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Mutiaur 4") + theme(axis.text.x=element_text(angle=90))
Mut4.plot

#only look at Blue corner - and see what tags were detected across time
#Filter whole database to only Blue corner
Blue.corner <- Receiver_Overview %>% filter(Station_Name == "Blue corner")

#Plot detection of tags across time at Blue corner
Blue.corner.plot <- ggplot(data= Blue.corner, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Blue corner") + theme(axis.text.x=element_text(angle=90))
Blue.corner.plot

#only look at Movement A - and see what tags were detected across time
#Filter whole database to only Movement A 
MovA <- Receiver_Overview %>% filter(Station_Name == "Movement A")

#Plot detection of tags across time at Movement A
MovA.plot <- ggplot(data= MovA, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Movement A ") + theme(axis.text.x=element_text(angle=90))
MovA.plot

#only look at Movement 8 - and see what tags were detected across time
#Filter whole database to only Movement 8 
Mov8 <- Receiver_Overview %>% filter(Station_Name == "Movement 8")

#Plot detection of tags across time at Movement 8
Mov8.plot <- ggplot(data= Mov8, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Movement 8") + theme(axis.text.x=element_text(angle=90))
Mov8.plot

#only look at Movement 7 - and see what tags were detected across time
#Filter whole database to only Movement 7 
Mov7 <- Receiver_Overview %>% filter(Station_Name == "Movement 7")

#Plot detection of tags across time at Movement 7
Mov7.plot <- ggplot(data= Mov7, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Movement 7") + theme(axis.text.x=element_text(angle=90))
Mov7.plot

#only look at Movement 6 - and see what tags were detected across time
#Filter whole database to only Movement 6 
Mov6 <- Receiver_Overview %>% filter(Station_Name == "Movement 6")

#Plot detection of tags across time at Movement 6
Mov6.plot <- ggplot(data= Mov6, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Movement 6") + theme(axis.text.x=element_text(angle=90))
Mov6.plot

#only look at Movement 5 - and see what tags were detected across time
#Filter whole database to only Movement 5 
Mov5 <- Receiver_Overview %>% filter(Station_Name == "Movement 5")

#Plot detection of tags across time at Movement 5
Mov5.plot <- ggplot(data= Mov5, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Movement 5") + theme(axis.text.x=element_text(angle=90))
Mov5.plot

#only look at Movement 4 - and see what tags were detected across time
#Filter whole database to only Movement 4 
Mov4 <- Receiver_Overview %>% filter(Station_Name == "Movement 4")

#Plot detection of tags across time at Movement 4
Mov4.plot <- ggplot(data= Mov4, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Movement 4") + theme(axis.text.x=element_text(angle=90))
Mov4.plot

#only look at Movement 3 - and see what tags were detected across time
#Filter whole database to only Movement 3 
Mov3 <- Receiver_Overview %>% filter(Station_Name == "Movement 3")

#Plot detection of tags across time at Movement 3
Mov3.plot <- ggplot(data= Mov3, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Movement 3") + theme(axis.text.x=element_text(angle=90))
Mov3.plot

#only look at Movement 2 - and see what tags were detected across time
#Filter whole database to only Movement 2 
Mov2 <- Receiver_Overview %>% filter(Station_Name == "Movement 2")

#Plot detection of tags across time at Movement 2
Mov2.plot <- ggplot(data= Mov2, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Movement 2") + theme(axis.text.x=element_text(angle=90))
Mov2.plot

#only look at Movement 1 - and see what tags were detected across time
#Filter whole database to only Movement 1 
Mov1 <- Receiver_Overview %>% filter(Station_Name == "Movement 1")

#Plot detection of tags across time at Movement 1
Mov1.plot <- ggplot(data= Mov1, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Movement 1") + theme(axis.text.x=element_text(angle=90))
Mov1.plot

#only look at Movement Rebotel - and see what tags were detected across time
#Filter whole database to only Rebotel 
Rebotel <- Receiver_Overview %>% filter(Station_Name == "Rebotel")

#Plot detection of tags across time at Rebotel
Rebotel.plot <- ggplot(data= Rebotel, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Rebotel") + theme(axis.text.x=element_text(angle=90))
Rebotel.plot

#only look at Movement Movement B - and see what tags were detected across time
#Filter whole database to only Movement B 
MovB <- Receiver_Overview %>% filter(Station_Name == "Movement B")

#Plot detection of tags across time at Movement B
MovB.plot <- ggplot(data= MovB, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Rebotel") + theme(axis.text.x=element_text(angle=90))
MovB.plot

#only look at Movement Ulong - and see what tags were detected across time
#Filter whole database to only Ulong
Ulong <- Receiver_Overview %>% filter(Station_Name == "Ulong channel")

#Plot detection of tags across time at Ulong
Ulong.plot <- ggplot(data= Ulong, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Ulong") + theme(axis.text.x=element_text(angle=90))
Ulong.plot

#only look at Movement Siaes corner - and see what tags were detected across time
#Filter whole database to only Siaes corner
Siaes <- Receiver_Overview %>% filter(Station_Name == "Siaes corner")

#Plot detection of tags across time at Siaes corner
Siaes.plot <- ggplot(data= Siaes, aes(x = Date_Local, y=Tag_ID)) +
  geom_jitter()+ ggtitle("Siaes corner") + theme(axis.text.x=element_text(angle=90))
Siaes.plot

grid.arrange(Mut1.plot, Mut2.plot,Mut3.plot,
             Mut4.plot,Mut5.plot,Mut6.plot,
             nrow = 3,ncol=2)

grid.arrange(Ulong.plot, MovB.plot,Rebotel.plot,
             Mov1.plot,Mov2.plot,Mov3.plot,
             nrow = 3,ncol=2)

grid.arrange(Mov4.plot,Mov5.plot,Mov6.plot,
             Mov7.plot,Mov8.plot,MovA.plot,
             nrow = 3,ncol=2)

grid.arrange(Siaes.plot,Blue.corner.plot,
             nrow = 3,ncol=2)

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
