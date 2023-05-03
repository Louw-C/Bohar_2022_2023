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

Map1<-Bohar %>% 
  group_by(Transmitter) %>% 
  summarise(n=n()) %>% 
  left_join(Bohar) %>% 
  dplyr::select(Transmitter, Latitude, Longitude) %>% 
  filter(Transmitter=="A69-1602-51651"| Transmitter=="A69-1602-51644") %>% 
  droplevels()

Map1
