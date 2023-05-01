require(tidyverse)

#Set your working directory (remember to change \ to \\)
setwd("E:\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023")
getwd()

#Upload your BIOMETRICS (fish and their tags)
Biometrics<-read.csv("E:\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023\\Data\\Biometrics.csv")
names(Biometrics)

#sort out time and date (combine and make sure of format)
#unite date and time in two separate columns into a new merged one - date.time
Biometrics<-Biometrics %>% unite('Release.date.time', Release.date,Release.time, remove=FALSE)
#Sort out the format of the date and time
Biometrics<-Biometrics %>% mutate(Release.date.time=mdy_hm(Release.date.time))

#Upload DEPLOYMENTS
Deployments<-read.csv("E:\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023\\Data\\Deployments.csv")
names(Deployments)
#Sort out the format of the date and time
Deployments<-Deployments %>% mutate(Start=mdy(Start))
Deployments<-Deployments %>% mutate(Stop=mdy(Stop))

#Upload SPATIAL
Spatial<-read.csv("E:\\Palau\\Research\\Coastal Fisheries\\Natalie Tagging\\2022 Bohar tagging\\Bohar_2022_2023\\Data\\Spatial.csv")
names(Spatial)
