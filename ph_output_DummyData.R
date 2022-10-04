# Create List
rm(list = ls())

# Define avg daily kWh output per month
data.month <- c(120.88, 201.76, 306.59, 429.67, 433.2, 455.07, 449.03, 411.79, 
               358.03, 234.82, 132.66, 96.82)

# Define Days per Month
days.per.month <- c(31,28,31,30,31,30,31,31,30,31,30,31)

# Append Days to Month
data.day <- c()
for(i in 1:11){
  data.day <- append(data.day, seq(from = data.month[i], to = data.month[i+1],
                                   length = days.per.month[i]))
  }
data.day <- append(data.day, seq(from = data.month[12], to = data.month[1],
                         length = days.per.month[12]))

# Create Daily Hours Sequence
hour.ind <- seq(from = 1, to = 24, length = 24)

# normally distributed daily production: Peak at 13:30, Start at 5:30, End at 21:30
data.hour.dnorm <- dnorm(hour.ind, mean = 13.5, sd = 2.5)
data.hour.dnorm.corr <- data.hour.dnorm*5.7
data.hour.dnorm.corr.sum <- sum(data.hour.dnorm.corr)
data.hour.dnorm.corr.perc <- data.hour.dnorm.corr/data.hour.dnorm.corr.sum

# Appened distribution to Date.Hour list
data.hour <- c()
for(i in 1:365){
    data.hour <- append(data.hour, data.day[i]*data.hour.dnorm.corr.perc)
}

# Plotting  
#par(mfrow = c(1,2))
#plot(data.hour, type = "l", main = "year")
#plot(data.hour[1:744], type = "l", main = "January")

# Randomize data
data.hour.rand <- c()
for(i in 1:8760){
  data.hour.rand <- append(data.hour.rand, rnorm(1, mean = data.hour[i], sd = 0.5))
}

# Set negative values to 0
data.hour.rand[data.hour.rand<0] <- 0

# Plotting
#summary(data.hour.rand)
#par(mfrow = c(1,1))
#plot(data.hour.rand[1:72], type = "l")

# round data.hour.rand to 2 digits
phOutput = round(data.hour.rand, digits = 4)


# Creating dateTime list
start <- as.POSIXct("2022-01-01")
interval <- 60
end <- start + as.difftime(365, units="days")
dateTime = seq(from=start, by=interval*60, to=end - 1)

# Add dateTime list to Output
output <- as.data.frame(dateTime)

# Adding date.hour to Output
output["output (kWh)"] <- as.data.frame(phOutput)

# Write Output to CSV
output
write.csv(output,"pv_output_dummyData.csv", row.names = FALSE)