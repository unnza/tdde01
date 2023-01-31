##################################
#         LAB 3 Assignment 1     #
##################################

#Implement a kernel method to predict the hourly temperatures for a date and place in Sweden. 
#To do so, you are provided with the files stations.csv and temps50k.csv. 
#These files contain information about weather stations and temperature measurements in the stations at different days and times. 
#The data have been kindly provided by the Swedish Meteorological and Hydrological Institute (SMHI).
#You are asked to provide a temperature forecast for a date and place in Sweden. 
#The forecast should consist of the predicted temperatures from 4 am to 24 pm in an interval of 2 hours.

#Use a kernel that is the sum of three Gaussian kernels:
#● The first to account for the physical distance from a station to the point of interest. 
#  For this purpose, use the function distHaversine from the R package geosphere.
#● The second to account for the distance between the day a temperature measurement was made and the day of interest.
#● The third to account for the distance between the hour of the day a temperature meaurement was made and the hour of interest.

#Choose an appropriate smoothing coefficient or width for each of the three kernels above. 
#No cross-validation should be used. 
#Instead, choose manually a width that gives large kernel values to closer points and small values to distant points. 
#Show this with a plot of the kernel value as a function of distance. 

#Help: Note that the file temps50k.csv may contain temperture measurements that are posterior to the day and hour of your forecast. 
#You must filter such measurements out, i.e. they cannot be used to compute the forecast.
#Finally, repeat the exercise above by combining the three kernels into one by multiplying them, instead of summing them up. 
#Compare the results obtained in both cases and elaborate on why they may differ.
#The only R package that is allowed to solve this assignment is the geosphere package (specifically, the function distHaversine).
#Feel free to use the template below to solve the assignment.

set.seed(1234567890)
#install.packages('geosphere')
library(geosphere)
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
times <- c("04:00:00", "06:00:00", "08:00:00","10:00:00","12:00:00", "14:00:00",
           "16:00:00","18:00:00","20:00:00","22:00:00","00:00:00")

filter_posterior_date <- function(data, current_date){
  filtered_data <- data[!as.Date(data$date) >= as.Date(current_date),]
  return(filtered_data)
} 
filter_posterior_hour <- function(data, current_time, current_date){
  filtered_data <- data[!as.Date(data$date) == as.Date(current_date) &
                           as.numeric(difftime(strptime(data$time, format = "%H:%M:%S"),
                                               strptime(current_time, format = "%H:%M:%S")) > 0),]
  return(filtered_data)
}


distance_kernel <- function(data,current_longitude,current_latitude,h){
  distance = c()
  distance_pred = data.frame(data$longitude, data$latitude)
  current_distance = data.frame(current_longitude, current_latitude)
  for (i in 1:nrow(distance_pred)){
  distance[i] <- distHaversine(distance_pred[i,], current_distance)
  }
  u = distance/h
  gaussian_kernel = exp(-u^2)
  plot(distance, gaussian_kernel, main="Distance Kernel")
  return(gaussian_kernel)
}
day_kernel <- function(data,current_date,h){
  current_date <- as.Date(current_date)
  date_interest <- as.Date(data$date)
  day = c()
  for (i in 1:length(date_interest)){
    day[i] <- as.numeric(current_date-date_interest[i])
  }
 print(day)
  u = day/h
  gaussian_kernel = exp(-u^2)
  plot(day, gaussian_kernel, main="Day Kernel")
  return(gaussian_kernel)
}
time_kernel <- function(data,current_time,h){
  time_interest <- (data$time)
  time <- abs(difftime(strptime(time_interest, format = "%H:%M:%S"),strptime(current_time, format = "%H:%M:%S")))
  time_numeric = as.numeric(time)
  u = time_numeric/h
  gaussian_kernel = exp(-u^2)
  plot(time_numeric, gaussian_kernel, main="Time Kernel")
  return(gaussian_kernel)
}

h_distance <- 100000  #meters (swedish miles hehe), classification boundary interval, the radius of each circle. Smooting coefficient.
h_date <- 30 #Days
h_time <- 120 #min, from instructions
  
  current_latitude<- 58.4274 # current location
  current_longitude<- 14.826
  current_date <- "2016-10-01" # current date

temp_sum <- vector(length=length(times))
temp_prod <- vector(length=length(times))
data <- as.data.frame(st)
data <- filter_posterior_date(data, current_date)

for (i in 1:length(times)){
  data <- filter_posterior_hour(data, times[i], current_date)
  dist_kernel_temp <- distance_kernel(data, current_longitude, current_latitude, h_distance)
  day_kernel_temp <- day_kernel(data, current_date, h_date)
  time_temp <- time_kernel(data,times[i],h_time)
  
  sum = 0
  sum = day_kernel_temp + dist_kernel_temp +time_temp
  temp_sum[i]  <- sum(sum%*%data$air_temperature)/sum(sum)
  
  prod = 0
  prod = day_kernel_temp * dist_kernel_temp * time_temp
  temp_prod[i]  <- sum(prod%*%data$air_temperature)/sum(prod)

}
plot(temp_sum,xaxt="n", type="o",col = "deeppink3", ylim = c(0,30), xlab="hours", ylab = "temperature")
points(temp_prod, col="turquoise", type="o",)
legend(x = "topright", legend = c("Sum of kernels","Product of kernels"),col = c("deeppink3", "turquoise"), pch = "o")
axis(1, at=1:length(times), labels=times)






