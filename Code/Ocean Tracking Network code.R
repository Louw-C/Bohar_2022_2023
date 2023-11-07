#Visualising and analyzing telemetry data - Ocean Tracking Network

Bohar<-read.csv("E:\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023\\Data\\Shark City_April 2023_Louw.csv")
names(Bohar)

#unite date and time in two separate columns into a new merged one - date.time
Bohar<-Bohar %>% unite('DateTime', Date_Local,Time_Local, remove=FALSE)
#Sort out the format of the date and time
Bohar<-Bohar %>% mutate(DateTime=ymd_hms(DateTime))

#Basic plot to look at spatial setup of array
Bohar %>%  ggplot(aes(Latitude,Longitude))+geom_point()

#Code to group and then look at means etc. 
Bohar %>% group_by(Transmitter) %>% 
  summarise(mean=mean(Latitude)) 

#Plot to look at mean longitude of tagged fish across months. This can be group to species as well.
#See when and where tagged fish were found.
Mean_Longitude<-Bohar %>% 
  group_by(m=month(DateTime), Transmitter) %>% 
  summarise(mean=mean(Longitude)) %>% 
  ggplot(aes(m %>% factor,mean,))+
  geom_point()

Mean_Longitude

#Mapping

BoharS<-Bohar
coordinates(BoharS)<-~Latitude+Longitude
BoharS
proj4string(BoharS)<-CRS("+proj=utm +zone=53 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
proj4string(BoharS)

BoharS1<-spTransform(BoharS,CRS("+init=epsg:28992"))
BoharS1


x=.5
SC<-getNOAA.bathy
BoharS1<-BoharS1 %>% .name_repair(BoharS1)
