##########################################################################
# TASK:merging and cleaning
#a) Casualties0514.csv
#b) Accidents0514.csv
#c) Vehicles0514.csv
#
# Author: Andres Cambronero
# Project: STAT503-Final Project
# Date Started: Sept 7, 2017
# Latest Update: July 20, 2018
##########################################################################


rm(list=ls())

#load packages
library(data.table)
library(stringr)
library(dplyr)
library(ade4)


#set working directory
setwd("~/Desktop/Michigan/503_multivariate/project/data_cleaning")


#load data
casualties<-read.csv("Casualties0514.csv", header = TRUE, sep = ",")
accidents<-read.csv("Accidents0514.csv", header= TRUE, sep=",")
vehicles<-read.csv("Vehicles0514.csv", header = TRUE, sep=",")


#make all column names lower case
names(casualties)<-tolower(names(casualties))
names(accidents)<-tolower(names(accidents))
names(vehicles)<-tolower(names(vehicles))


#merge by index
temp<-merge(casualties, accidents, by=c("accident_index"), all = TRUE)
uk_acc<-merge(temp, vehicles, by=c("accident_index"), all = TRUE)


#######################################
#parse years and keep only 2014
#######################################
# parse the date cell
parse_date = function(x){
  x = gsub(" ", "", x)
  str_sub(x,-4,-1)
}

# this is a wrapper to an apply call
get_year = function(x){
  sapply(x, parse_date)
}

uk_acc =
  uk_acc %>% 
  mutate(date = get_year(date))

#subset to 2014
uk_acc<-subset(uk_acc, date== "2014")















#######################################
# Delete NAs and -1 in all the dataset
#######################################
#drop observations with -1s
uk_acc = uk_acc[-unique(which(uk_acc == -1, arr.ind = TRUE)[,1]),]

# get rid of rows with missing values
uk_acc = uk_acc[complete.cases(uk_acc),]


#######################################
# RECODE TIME
#######################################
uk_acc$time<-as.character(uk_acc$time)

#all entries are length 5
length_char<-c()
for (i in 1:length(unique(uk_acc$time))){
  length_char<-nchar(unique(uk_acc$time)[i])
}

#keep only first two charaacters (hours)
uk_acc$time<-substring(uk_acc$time, 1,2)


#make numeric
uk_acc$time<-as.numeric(uk_acc$time)


#make hours three levels
# level 1 (20:00-4:00 late night)
# level 2 (4:00-12:00 morning)
# level 3 (12:00-20:00 afternoon)


uk_acc$time<-ifelse(uk_acc$time>4 & uk_acc$time<12,2,
             ifelse(uk_acc$time>=12 & uk_acc$time<20,3,1))



#change to factors
uk_acc$time<-as.factor(uk_acc$time)


#######################################
#drop variables that we will not use
#######################################
uk_acc$age_band_of_casualty<-NULL
uk_acc$age_band_of_driver<-NULL
uk_acc$car_passenger<-NULL
uk_acc$bus_or_coach_passenger<-NULL
uk_acc$casualty_home_area_type<-NULL
uk_acc$driver_home_area_type<-NULL
uk_acc$casualty_type<-NULL     
uk_acc$casualty_severity<-NULL
uk_acc$driver_imd_decile<-NULL
uk_acc$hit_object_in_carriageway<-NULL 
uk_acc$journey_purpose_of_driver<-NULL
uk_acc$junction_detail<-NULL
uk_acc$junction_location<-NULL
uk_acc$location_easting_osgr<-NULL
uk_acc$lsoa_of_accident_location<-NULL
uk_acc$pedestrian_location<-NULL
uk_acc$pedestrian_movement<-NULL
uk_acc$pedestrian_road_maintenance_worker<-NULL
uk_acc$police_force<-NULL
uk_acc$propulsion_code<-NULL
uk_acc$vehicle_leaving_carriageway<-NULL
uk_acc$vehicle_location.restricted_lane<-NULL
uk_acc$vehicle_reference.x<-NULL
uk_acc$vehicle_reference.y<-NULL
uk_acc$x1st_road_number<-NULL
uk_acc$x2nd_road_class<-NULL
uk_acc$x2nd_road_number<-NULL
uk_acc$engine_capacity_.cc.<-NULL
uk_acc$casualty_reference<-NULL
uk_acc$location_northing_osgr<-NULL
uk_acc$local_authority_.district.<-NULL
uk_acc$local_authority_.highway.<-NULL
uk_acc$date<-NULL
uk_acc$casualty_class<-NULL #cant find in codebook
uk_acc$number_of_casualties<-NULL #cant find in codebook
uk_acc$number_of_vehicles<-NULL #cant find in codebook



##################################################
# CHECK CLASS OF VARIABLES IS CORRECT
##################################################
#original class
sapply(uk_acc, class)

#change all to factors
for (i in 1:ncol(uk_acc)){
  uk_acc[,i] <- as.factor(uk_acc[,i])
}

#check classes
sapply(uk_acc, class)


#change the ones that are necessary
uk_acc$age_of_casualty<-as.numeric(uk_acc$age_of_casualty)
uk_acc$longitude<-as.numeric(uk_acc$longitude)
uk_acc$latitude<-as.numeric(uk_acc$latitude)
uk_acc$age_of_driver<-as.numeric(uk_acc$age_of_driver)
uk_acc$age_of_vehicle<-as.numeric(uk_acc$age_of_vehicle)




###################################################
# CREATE SUBSET MAINTAINING ORIGINAL PROPORTIONS
###################################################
#ORIGINAL PROPORTIONS
#fatal
length(which(uk_acc$accident_severity==1))/nrow(uk_acc)

#serious
length(which(uk_acc$accident_severity==2))/nrow(uk_acc)

#slight
length(which(uk_acc$accident_severity==3))/nrow(uk_acc)


#subset with correct proportions for subset of 2400 observations
set.seed(1)
imb_fatal<-uk_acc[sample(which(uk_acc$accident_severity==1), replace=FALSE,18),]

set.seed(1)
imb_serious<-uk_acc[sample(which(uk_acc$accident_severity==2),replace=FALSE,290),]

set.seed(1)
imb_slight<-uk_acc[sample(which(uk_acc$accident_severity==3), 2092, replace=FALSE),]

#rbind the subsets
imbalanced_3000<-rbind(imb_fatal, imb_serious, imb_slight)



###################################################
# CREATE SUBSET MAINTAINING BALANCED PROPORTIONS
###################################################
set.seed(1)
bal_fatal<-uk_acc[sample(which(uk_acc$accident_severity==1),800, replace=FALSE),]

set.seed(1)
bal_serious<-uk_acc[sample(which(uk_acc$accident_severity==2),800, replace=FALSE),]

set.seed(1)
bal_slight<-uk_acc[sample(which(uk_acc$accident_severity==3),800, replace=FALSE),]


#rbind the subsets
balanced_3000<-rbind(bal_fatal, bal_serious, bal_slight)



########################################################
#Check that coding of variables matches the codebook
########################################################
names(balanced_3000)==names(imbalanced_3000)

#sex_of_casualty
unique(balanced_3000$sex_of_casualty)
unique(imbalanced_3000$sex_of_casualty)

#age_of_casualty 
unique(balanced_3000$age_of_casualty)
unique(imbalanced_3000$age_of_casualty)

           
#longitude (hard to check)                
#latitude (hard to check)     

#accident_severity   
unique(balanced_3000$accident_severity)
unique(imbalanced_3000$accident_severity)


#day_of_week  
unique(balanced_3000$day_of_week)
unique(imbalanced_3000$day_of_week)


#x1st_road_class    
unique(balanced_3000$x1st_road_class)
unique(imbalanced_3000$x1st_road_class)


#road_type 
unique(balanced_3000$road_type)
unique(imbalanced_3000$road_type)


#speed_limit (technically missing but coding seems obvious)
unique(balanced_3000$speed_limit)
unique(imbalanced_3000$speed_limit)


#junction_control     
unique(balanced_3000$junction_control)
unique(imbalanced_3000$junction_control)


#pedestrian_crossing.human_control 
unique(balanced_3000$pedestrian_crossing.human_control)
unique(imbalanced_3000$pedestrian_crossing.human_control)


#pedestrian_crossing.physical_facilities
unique(balanced_3000$pedestrian_crossing.human_control)
unique(imbalanced_3000$pedestrian_crossing.human_control)


#light_conditions    
unique(balanced_3000$light_conditions)
unique(imbalanced_3000$light_conditions)


#weather_conditions  
unique(balanced_3000$weather_conditions)
unique(imbalanced_3000$weather_conditions)


#road_surface_conditions   (very similar to special_conditions_at_site (below)) 
unique(balanced_3000$road_surface_conditions)
unique(imbalanced_3000$road_surface_conditions)


#special_conditions_at_site  (very similar to road_surface_conditions (above)) 
unique(balanced_3000$special_conditions_at_site) 
unique(imbalanced_3000$special_conditions_at_site)


#carriageway_hazards    
unique(balanced_3000$carriageway_hazards)
unique(imbalanced_3000$carriageway_hazards)


#urban_or_rural_area 
unique(balanced_3000$urban_or_rural_area)
unique(imbalanced_3000$urban_or_rural_area)


#did_police_officer_attend_scene_of_accident
unique(balanced_3000$did_police_officer_attend_scene_of_accident)
unique(imbalanced_3000$did_police_officer_attend_scene_of_accident)

#vehicle_type   
unique(balanced_3000$vehicle_type)
unique(imbalanced_3000$vehicle_type)


#towing_and_articulation  
unique(balanced_3000$towing_and_articulation)
unique(imbalanced_3000$towing_and_articulation)


#vehicle_manoeuvre   
unique(balanced_3000$vehicle_manoeuvre)
unique(imbalanced_3000$vehicle_manoeuvre)


#skidding_and_overturning                  
unique(balanced_3000$skidding_and_overturning)
unique(imbalanced_3000$skidding_and_overturning)


#hit_object_off_carriageway     
unique(balanced_3000$hit_object_off_carriageway)
unique(imbalanced_3000$hit_object_off_carriageway)


#x1st_point_of_impact  
unique(balanced_3000$x1st_point_of_impact)
unique(imbalanced_3000$x1st_point_of_impact)


#was_vehicle_left_hand_drive.  
unique(balanced_3000$was_vehicle_left_hand_drive.)
unique(imbalanced_3000$was_vehicle_left_hand_drive.)


#sex_of_driver
unique(balanced_3000$sex_of_driver)
unique(imbalanced_3000$sex_of_driver)


#age_of_driver   
unique(balanced_3000$age_of_driver)
unique(imbalanced_3000$age_of_driver)


#age_of_vehicle
unique(balanced_3000$age_of_vehicle)
unique(imbalanced_3000$age_of_vehicle)





#####################################
# MAKE TRAINING AND TEST DATASETS
#####################################
#tr and test for imbalanced dataset
classes = lapply(levels(imbalanced_3000$accident_severity), function(x) which(imbalanced_3000$accident_severity==x))
set.seed(1)
train = lapply(classes, function(class) sample(class, 0.8*length(class), replace = F))
train = unlist(train)
imbalanced_train = imbalanced_3000[train,]
imbalanced_test = imbalanced_3000[-train,]



#tr and test for balanced dataset
classes = lapply(levels(balanced_3000$accident_severity), function(x) which(balanced_3000$accident_severity==x))
set.seed(1)
train = lapply(classes, function(class) sample(class, 0.8*length(class), replace = F))
train = unlist(train)
balanced_train = balanced_3000[train,]
balanced_test = balanced_3000[-train,]


######################
# write out files
######################
write.csv(imbalanced_3000, "imbalanced_uk_acc.csv", row.names = FALSE)
write.csv(balanced_3000, "balanced_uk_acc.csv", row.names = FALSE)

#save(imbalanced_3000, file="imbalanced_3000.rda")
#save(balanced_3000, file="balanced_3000.rda")


#imbalanced training and test
write.csv(imbalanced_train, "imbalanced_train.csv", row.names = FALSE)
write.csv(imbalanced_test, "imbalanced_test.csv", row.names = FALSE)

#save(imbalanced_train, file="imbalanced_train.rda")
#save(imbalanced_test, file="imbalanced_test.rda")


#balanced training and test
write.csv(balanced_train, "balanced_train.csv", row.names = FALSE)
write.csv(balanced_test, "balanced_test.csv", row.names = FALSE)

#save(balanced_train, file="balanced_train.rda")
#save(balanced_test, file="balanced_test.rda")

