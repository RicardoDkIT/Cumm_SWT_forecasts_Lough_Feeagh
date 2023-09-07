# Script to access Lake Feeagh data
# Modified: 07 September 2023
# Ricardo Paiz

##Packages######################################################################

library(rerddap)
library(tidyverse)
library(lubridate)
library(dplyr)

#Getting access to the Irish data site: ERDDAP.marine.ie########################

Sys.setenv(RERDDAP_DEFAULT_URL="http://erddap.marine.ie/erddap/")
buoy <- 'IMINewportBuoys' #dataset id -buoy
info(buoy)$variables #View variables


#Set start and stop dates to pull data from##################################### 
#Default: data is pulled from four weeks from now to today######################

start <- Sys.Date()-28 #number of days from now
stop <- Sys.Date()
samp<-tabledap('IMINewportBuoys' , paste0('time>=',start), paste0('time<=',stop))

#Getting water temperatures measured at Feeagh and dates########################

fgh <- samp[,c(6,37,10:21)] 
print(summary(fgh))

#c(6) corresponds to datetime.
#c(37) corresponds to the measurement of water temperature at 0.9m
#c(10:21) correspond to the measurements of water temperature at 2,5,8,11,14,16,18,20,22,27,32,42, respectively

#Convert all columns to a numeric format but the time column####################

Number_of_Cols <- ncol(fgh)

for(i in 2:Number_of_Cols){
  fgh[,i] <- as.numeric(as.character(fgh[,i]))
  print(paste0(i,' converted from factor to numeric'))
}

#Convert time to Datetime#######################################################

fgh$time <- gsub('T',' ',fgh$time) #Remove the T 
fgh$time <- gsub('Z','',fgh$time) #Remove the Z

#Formatting to datetime (checking in case there is other format in the dataset)
if(!is.na(as.POSIXct(fgh$time[nrow(fgh)], format = '%d/%m/%Y %H:%M:%S'))){
fmt = '%Y-%m-%d %H:%M:%S'
}else if(!is.na(as.POSIXct(fgh$time[1], format = '%Y-%m-%d %H:%M:%S'))){
fmt = '%Y-%m-%d %H:%M:%S'
}

str(fgh)

#Checking for NAs in time column (provided - external)##########################

fgh$time <- as.POSIXct(fgh$time, format = fmt, tz = 'UTC') #Format into datetime
date.na = sum(is.na(fgh$time))
print(paste0('Start: ', range(fgh$time, na.rm = T)[1], '; End: ', range(fgh$time, na.rm = T)[2]))
print(paste('No. of NAs in time:',date.na))

#Check timestep (provided - external)###########################################

print(dim(fgh))
print(summary(fgh$time))
dates = seq(from = range(fgh$time)[1],to = range(fgh$time)[2], by =120)
dif <- as.numeric(fgh[2:nrow(fgh),'time']-fgh[1:(nrow(fgh)-1),'time'])
sam <- fgh[order(fgh[,'time']),]
if(length(dates) != nrow(fgh)){
df = data.frame(time = dates,
test =rep(NA,length(dates)), 
stringsAsFactors=FALSE) 
df = merge(df,sam, by ='time', all.x = T)
df$test <- NULL
fgh <- df
print('Filled in missing dates with NAs')
}
dif2 <- fgh[2:nrow(fgh),'time']-fgh[1:(nrow(fgh)-1),'time']
if(max(dif)>2 | min(dif) < 2){
par(mfrow=c(2,1))
plot(dif, main = 'Time Difference - Raw', ylab = 'sec')
plot(dif2, main = 'Time Difference - Corrected', ylab = 'sec')
print('Timestep has been corrected')
}
sam <- NULL
if(date.na != 0){
snd.na = sum(is.na(fgh$Sonde_Temperature))
anem.na = sum(is.na(fgh$Anemometer))
if(date.na == snd.na & date.na == anem.na){
dates = seq(fgh$time[1], fgh$time[nrow(fgh)], by = 120)
}
}

#Replace Nan with NA (provided - external)######################################
for(i in 2:ncol(fgh)){
n = which(is.nan(fgh[,i]))
#print(n) ##To check the file
#}
if(length(n) == 0){
print(paste('No NaN in',colnames(fgh)[i]))
next
}else{
fgh[n,i] <- NA
print(paste('NaN replaced with NAs in',colnames(fgh)[i]))
}
}

str(fgh)
print(summary(fgh))


#Filling a dataframe with daily averagese#######################################

no_days <- as.numeric(stop - start)

mat <- matrix(1:(ncol(fgh)*no_days), ncol = ncol(fgh))                      

data_daily <- as.data.frame(mat)                       
data_daily

colnames(data_daily) <- c("datetime","0.9","2","5","8","11","14","16","18","20","22","27","32","42")

for(o in 1:(no_days)){

  data_daily[o,1] <- as.character(start + o -1, format = '%Y-%m-%d %H:%M:%S' )
  print(data_daily[o,1])
  
}

for(t in 1:(no_days)){
  
  day_calc <- fgh[fgh$time >= start + t -1 & fgh$time  < start + t ,]

  print(day_calc[1,1])
  
  for(z in 2:ncol(fgh)){
    
    mean_calc <- mean(day_calc[,z], na.rm = TRUE)
    
    print(mean_calc)
    
    data_daily[t,z] <- mean_calc
    
  }
  
  print(paste0("Values above correspond to daily averages computed for day number ", t))
}

#Some QC/QA - Ricardo############################################################

#Here two conditions are set: (1) a water temperature measured at depth X should be equal or lower 
#than shallower temperatures measured (+ 2% degrees C)
#Also, if the water temperature measured at depth X is equal or lower than shallower temperatures then it has to be within
# a certain range (it should/cant be too low!). This range will be dictated by
# the 95percentile variation of the antecedent (shallower) temperature measurement considering the period 2004-2023
#surface sensor (0.9 m) is assumed to be reading correctly!

output_daily <- data_daily

# sensor at 2 m
output_daily[,3][output_daily[,3] > output_daily[,2]*1.02] <- NA

output_daily[,3][output_daily[,3] < output_daily[,2]-0.5] <- NA #additional check - temperatures cant vary more than 0.5 C

# sensor at 5 m
output_daily[,4][output_daily[,4] > output_daily[,2]*1.02] <- NA
output_daily[,4][output_daily[,4] > output_daily[,3]*1.02] <- NA

output_daily[,4][output_daily[,4] < output_daily[,3]-0.7] <- NA #additional check - temperatures cant vary more than 0.95 percentile of variations 2004-2023
   
# sensor at 8 m
output_daily[,5][output_daily[,5] > output_daily[,2]*1.02] <- NA
output_daily[,5][output_daily[,5] > output_daily[,3]*1.02] <- NA
output_daily[,5][output_daily[,5] > output_daily[,4]*1.02] <- NA

output_daily[,5][output_daily[,5] < output_daily[,4]-0.8] <- NA #additional check - temperatures cant vary more than 0.95 percentile of variations 2004-2023

# sensor at 11 m

output_daily[,6][output_daily[,6] > output_daily[,2]*1.02] <- NA
output_daily[,6][output_daily[,6] > output_daily[,3]*1.02] <- NA
output_daily[,6][output_daily[,6] > output_daily[,4]*1.02] <- NA
output_daily[,6][output_daily[,6] > output_daily[,5]*1.02] <- NA

output_daily[,6][output_daily[,6] < output_daily[,5]-1] <- NA #additional check - temperatures cant vary more than 0.95 percentile of variations 2004-2023

# sensor at 14 m

output_daily[,7][output_daily[,7] > output_daily[,2]*1.02] <- NA
output_daily[,7][output_daily[,7] > output_daily[,3]*1.02] <- NA
output_daily[,7][output_daily[,7] > output_daily[,4]*1.02] <- NA
output_daily[,7][output_daily[,7] > output_daily[,5]*1.02] <- NA
output_daily[,7][output_daily[,7] > output_daily[,6]*1.02] <- NA

output_daily[,7][output_daily[,7] < output_daily[,6]-1.1] <- NA #additional check - temperatures cant vary more than 0.95 percentile of variations 2004-2023

# sensor at 16 m

output_daily[,8][output_daily[,8] > output_daily[,2]*1.02] <- NA
output_daily[,8][output_daily[,8] > output_daily[,3]*1.02] <- NA
output_daily[,8][output_daily[,8] > output_daily[,4]*1.02] <- NA
output_daily[,8][output_daily[,8] > output_daily[,5]*1.02] <- NA
output_daily[,8][output_daily[,8] > output_daily[,6]*1.02] <- NA
output_daily[,8][output_daily[,8] > output_daily[,7]*1.02] <- NA

output_daily[,8][output_daily[,8] < output_daily[,7]-0.8] <- NA #additional check - temperatures cant vary more than 0.95 percentile of variations 2004-2023. This was 0.8 but 1.1 (previous percetile was greater)

# sensor at 18 m
output_daily[,9][output_daily[,9] > output_daily[,2]*1.02] <- NA
output_daily[,9][output_daily[,9] > output_daily[,3]*1.02] <- NA
output_daily[,9][output_daily[,9] > output_daily[,4]*1.02] <- NA
output_daily[,9][output_daily[,9] > output_daily[,5]*1.02] <- NA
output_daily[,9][output_daily[,9] > output_daily[,6]*1.02] <- NA
output_daily[,9][output_daily[,9] > output_daily[,7]*1.02] <- NA
output_daily[,9][output_daily[,9] > output_daily[,8]*1.02] <- NA

output_daily[,9][output_daily[,9] < output_daily[,8]-0.8] <- NA #additional check - temperatures cant vary more than 0.95 percentile of variations 2004-2023. This was 0.8 but 1.1 (previous percetile was greater)

# sensor at 20 m

output_daily[,10][output_daily[,10] > output_daily[,2]*1.02] <- NA
output_daily[,10][output_daily[,10] > output_daily[,3]*1.02] <- NA
output_daily[,10][output_daily[,10] > output_daily[,4]*1.02] <- NA
output_daily[,10][output_daily[,10] > output_daily[,5]*1.02] <- NA
output_daily[,10][output_daily[,10] > output_daily[,6]*1.02] <- NA
output_daily[,10][output_daily[,10] > output_daily[,7]*1.02] <- NA
output_daily[,10][output_daily[,10] > output_daily[,8]*1.02] <- NA
output_daily[,10][output_daily[,10] > output_daily[,9]*1.02] <- NA

output_daily[,10][output_daily[,10] < output_daily[,9]-0.8] <- NA #additional check - temperatures cant vary more than 0.95 percentile of variations 2004-2023. This was 0.8 but 1.1 (previous percetile was greater)

# sensor at 22 m

output_daily[,11][output_daily[,11] > output_daily[,2]*1.02] <- NA
output_daily[,11][output_daily[,11] > output_daily[,3]*1.02] <- NA
output_daily[,11][output_daily[,11] > output_daily[,4]*1.02] <- NA
output_daily[,11][output_daily[,11] > output_daily[,5]*1.02] <- NA
output_daily[,11][output_daily[,11] > output_daily[,6]*1.02] <- NA
output_daily[,11][output_daily[,11] > output_daily[,7]*1.02] <- NA
output_daily[,11][output_daily[,11] > output_daily[,8]*1.02] <- NA
output_daily[,11][output_daily[,11] > output_daily[,9]*1.02] <- NA
output_daily[,11][output_daily[,11] > output_daily[,10]*1.02] <- NA

output_daily[,11][output_daily[,11] < output_daily[,10]-0.8] <- NA #additional check - temperatures cant vary more than 0.95 percentile of variations 2004-2023. This was 0.8 but 1.1 (previous percetile was greater)

# sensor at 27 m

output_daily[,12][output_daily[,12] > output_daily[,2]*1.02] <- NA
output_daily[,12][output_daily[,12] > output_daily[,3]*1.02] <- NA
output_daily[,12][output_daily[,12] > output_daily[,4]*1.02] <- NA
output_daily[,12][output_daily[,12] > output_daily[,5]*1.02] <- NA
output_daily[,12][output_daily[,12] > output_daily[,6]*1.02] <- NA
output_daily[,12][output_daily[,12] > output_daily[,7]*1.02] <- NA
output_daily[,12][output_daily[,12] > output_daily[,8]*1.02] <- NA
output_daily[,12][output_daily[,12] > output_daily[,9]*1.02] <- NA
output_daily[,12][output_daily[,12] > output_daily[,10]*1.02] <- NA
output_daily[,12][output_daily[,12] > output_daily[,11]*1.02] <- NA

output_daily[,12][output_daily[,12] < output_daily[,11]-2.0] <- NA #additional check - temperatures cant vary more than 0.96 percentile of variations 2004-2023. 

# sensor at 32 m

output_daily[,13][output_daily[,13] > output_daily[,2]*1.02] <- NA
output_daily[,13][output_daily[,13] > output_daily[,3]*1.02] <- NA
output_daily[,13][output_daily[,13] > output_daily[,4]*1.02] <- NA
output_daily[,13][output_daily[,13] > output_daily[,5]*1.02] <- NA
output_daily[,13][output_daily[,13] > output_daily[,6]*1.02] <- NA
output_daily[,13][output_daily[,13] > output_daily[,7]*1.02] <- NA
output_daily[,13][output_daily[,13] > output_daily[,8]*1.02] <- NA
output_daily[,13][output_daily[,13] > output_daily[,9]*1.02] <- NA
output_daily[,13][output_daily[,13] > output_daily[,10]*1.02] <- NA
output_daily[,13][output_daily[,13] > output_daily[,11]*1.02] <- NA
output_daily[,13][output_daily[,13] > output_daily[,12]*1.02] <- NA

output_daily[,13][output_daily[,13] < output_daily[,12]-1.0] <- NA #additional check - temperatures cant vary more than 0.96 percentile of variations 2004-2023. 
# sensor at 42 m

output_daily[,14][output_daily[,14] > output_daily[,2]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,3]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,4]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,5]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,6]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,7]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,8]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,9]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,10]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,11]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,12]*1.02] <- NA
output_daily[,14][output_daily[,14] > output_daily[,13]*1.02] <- NA

output_daily[,14][output_daily[,14] < output_daily[,13]-2.0] <- NA #additional check - temperatures cant vary more than 0.96 percentile of variations 2004-2023. This was 0.9 but 2.2 (previous percetile was greater)




#FLARE target configuration#####################################################

num_rows <- ((ncol(output_daily)-1)*no_days)
data_matrix <- matrix(NA, nrow = num_rows, ncol = 5)
FLARE_dataset <- data.frame(data_matrix)

colnames(FLARE_dataset) <- c("datetime","site_id","depth","observation","variable")

#Column1: datetime##############################################################

col_datetime <- output_daily[ , "datetime"]

rep_datetime <- rep(col_datetime, num_rows/no_days) 

FLARE_dataset$datetime <- rep_datetime

#Column 2: site_id##############################################################

site_id <- c("feea")

filling_site_id <- rep(site_id, lenght.out = num_rows)

FLARE_dataset$site_id<- filling_site_id

#Column 3: depth################################################################

rep_depth_values <- rep(c(0.9,2,5,8,11,14,16,18,20,22,27,32,42), each = no_days, times = 1) 

FLARE_dataset$depth <- rep_depth_values

#Column 4: observation##########################################################

values_0.9 <- output_daily[ , "0.9"]
values_2 <- output_daily[ , "2"]
values_4 <- output_daily[ , "5"]
values_8 <- output_daily[ , "8"]
values_11 <- output_daily[ , "11"]
values_14 <- output_daily[ , "14"]
values_16 <- output_daily[ , "16"]
values_18 <- output_daily[ , "18"]
values_20 <- output_daily[ , "20"]
values_22 <- output_daily[ , "22"]
values_27 <- output_daily[ , "27"]
values_32 <- output_daily[ , "32"]
values_42 <- output_daily[ , "42"]

observation_values  <- c(values_0.9, values_2, values_4, values_8, values_11, values_14, values_16, values_18,
                         values_20,values_22,values_27, values_32, values_42)

observation_values[is.nan(observation_values)] <-  NA

FLARE_dataset$observation <- observation_values 

#Column 5: variable#############################################################

temp_variable <- c("temperature")

filling_variable <- rep(temp_variable, lenght.out = num_rows)

FLARE_dataset$variable <- filling_variable

#Updating the observations file for Feeagh######################################

Past_feea_observations <- read.csv("Observations_feea.csv", header = TRUE)

merged_observations <- bind_rows(FLARE_dataset, Past_feea_observations) %>%
  arrange(depth)

updated_observations <- distinct(merged_observations, datetime, depth, .keep_all = TRUE)

updated_observations <- updated_observations %>% arrange(datetime) %>% arrange(depth)

updated_observations$observation <- round(updated_observations$observation, 2)

##Remove rows that are not in the same timeformat

time_format <- "\\d{4}-\\d{2}-\\d{2}$"

matching_rows <- grep(time_format, updated_observations$datetime)

updated_observations <- updated_observations[-matching_rows, ]

write.csv(updated_observations, "Observations_feea.csv", row.names = FALSE, quote = FALSE)

#Last line######################################################################
