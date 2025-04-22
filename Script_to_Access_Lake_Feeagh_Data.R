# Script to compute cummpulative water temperature in degrees C per day in Lough Feeagh
# This script is largely based on the Ricardo Paiz script that access Feeagh data through rerddap
# Modified: 12 March 2025
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


#Filling a dataframe with daily averages#######################################

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


##Code to compute cummulative temperature based solely on sonde temperature
##This was decided as other sensors could be missreading

Past_sonde_observations <- read.csv("Past_sonde_observations_since_2023.csv", header = TRUE)

# Ensure datetime is in the correct date format first
Past_sonde_observations$datetime <- as.Date(Past_sonde_observations$datetime, format = "%m/%d/%Y")

# Now convert to the desired format "%m/%d/%Y" if needed
Past_sonde_observations$datetime <- format(Past_sonde_observations$datetime, "%m/%d/%Y")

Base_data <- output_daily[, c(1, 2)]
Base_data$datetime <- format(as.Date(Base_data$datetime), "%m/%d/%Y")
colnames(Base_data) <- colnames(Past_sonde_observations)

# Add the rows from Base_data to Past_sonde_observations
merged_data <- rbind(Base_data, Past_sonde_observations)

merged_data <- merged_data[!duplicated(merged_data$datetime), ]

merged_data$Temp_for_cumm_T <- merged_data$observation

# Set Temp_for_cumm_T to observation, with 0 for 1st September
merged_data$Temp_for_cumm_T <- merged_data$observation
#merged_data$Temp_for_cumm_T[format(as.Date(merged_data$datetime, format = "%m/%d/%Y"), "%m-%d") == "09-01"] <- 0

# Replace NAs in Temp_for_cumm_T with the previous available observation value
library(zoo)
merged_data$Temp_for_cumm_T <- na.locf(merged_data$Temp_for_cumm_T, na.rm = FALSE)


# Function to calculate calendar day starting from 1st September
get_calendar_day <- function(date) {
  year <- as.numeric(format(date, "%Y"))  # Convert year to numeric
  start_date <- as.Date(paste0(year, "-09-01"))  # 1st September of the current year
  end_date <- as.Date(paste0(year + 1, "-08-31"))  # 31st August of the next year
  
  # If the date is before 1st September, adjust to the previous year's cycle
  if (date < start_date) {
    start_date <- as.Date(paste0(year - 1, "-09-01"))
    end_date <- as.Date(paste0(year, "-08-31"))
  }
  
  # Calculate the calendar day
  calendar_day <- as.numeric(difftime(date, start_date, units = "days")) + 1
  return(calendar_day)
}

# Apply the function to the 'datetime' column in merged_data
merged_data$calendar_day <- sapply(as.Date(merged_data$datetime, format = "%m/%d/%Y"), get_calendar_day)
merged_data <- merged_data %>%
  mutate(datetime = as.Date(datetime, format = "%m/%d/%Y")) %>%
  arrange(datetime)

write.csv(merged_data[, 1:2], "Past_sonde_observations_since_2023.csv", row.names = FALSE)

###################################3
NEWmerged_data <-merged_data

NEWmerged_data$datetime <- as.Date(merged_data$datetime, format = "%m/%d/%Y")

# Filter merged_data to only include data from 1st September 2023 onwards
NEWmerged_data <- subset(NEWmerged_data, datetime >= as.Date("2023-09-01"))


# Ensure datetime is in Date format
NEWmerged_data$datetime <- as.Date(NEWmerged_data$datetime, format = "%m/%d/%Y")

# Create a 'month_day' column to extract month and day from the 'datetime' column
NEWmerged_data$month_day <- format(NEWmerged_data$datetime, "%m-%d")

# Identify which rows are 1st September in any year
NEWmerged_data$sep_first <- NEWmerged_data$month_day == "09-01"

# Create a new cumulative temperature column based on 1st September resets
NEWmerged_data$cumm_temp <- with(NEWmerged_data, ave(Temp_for_cumm_T, cumsum(sep_first), FUN = cumsum))

# Start from the last row (Forecast_Cum_Temp)
Forecast_Cum_Temp <- tail(NEWmerged_data, 1)

# Generate 21 consecutive days starting from the next day of the last row's datetime
new_dates <- seq(from = as.Date(Forecast_Cum_Temp$datetime) + 1, by = "days", length.out = 21)

# Create the new data for the 21 rows
Forecast_Cum_Temp <- data.frame(
  datetime = new_dates,
  observation = rep(Forecast_Cum_Temp$observation, 21),
  Temp_for_cumm_T = rep(Forecast_Cum_Temp$Temp_for_cumm_T, 21),
  calendar_day = 1:21,  # Calendar day increments by 1
  month_day = format(new_dates, "%m-%d"),  # Extract month and day
  sep_first = rep(FALSE, 21),  # Not relevant, set to FALSE
  cumm_temp = cumsum(rep(Forecast_Cum_Temp$Temp_for_cumm_T, 21)) + Forecast_Cum_Temp$cumm_temp  # Cumulative sum
)

# View the result
head(Forecast_Cum_Temp)

# Select the first and last columns from Forecast_Cum_Temp
selected_columns <- Forecast_Cum_Temp[, c(1, ncol(Forecast_Cum_Temp))]


colnames(selected_columns)[1] <- "Forecast_date"
colnames(selected_columns)[2] <- "Cummulative_SWT_in_degree_C"

# Add a third column "Forecast_generated_on" with the datetime of the first row
selected_columns$Forecast_generated_on <- as.Date(Forecast_Cum_Temp$datetime[1])

# Create the file name with the desired format
file_name <- paste("Last_forecast_generated_Feeagh_Cummulative_SWT.csv", sep = "")

# Save the selected columns to a CSV file
write.csv(selected_columns, file = file_name, row.names = FALSE)

#Last line######################################################################
