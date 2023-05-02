require(tidyverse)
require(actel)
require(raster)
require(gdistance)
require(rgdal)

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


