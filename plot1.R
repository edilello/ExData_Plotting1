######################################################
# Exploratory Data Analysis, Project 1
#
# The archive exdata-data-household_power_consumption.zip
# must be placed in the working directory.
# Read data from required file, and generate a plot.
#####################################################

library(lubridate)

# unzip and read data
unzip("exdata-data-household_power_consumption.zip",exdir = "./")

# We are only interested in the period from  2007-02-01 to 2007-02-02,
# so get the variable names from the header and then read lines
# one by on until the date field matches the the interval
inputFile <- "household_power_consumption.txt"
con  <- file(inputFile, open = "r") # open file connection

first_date <- as.Date("01022007","%d%m%Y")
last_date <- as.Date("02022007","%d%m%Y")

go_on <- 1;

# read header 
names <-strsplit(readLines(con, n = 1, warn = FALSE), ";")[[1]]
lines_read <-1;

# Initialize empty data frame
data<-data.frame("DateTime" = as.POSIXct(character(),
                            format = "%d/%m/%Y %H:%M:%S"),  # Date and Time
                 "Global_active_power" = numeric(),         # Global active Power    
                 "Global_reactive_power" = numeric(),       # Global reactive Power
                 "Voltage" = numeric(),                     # Voltage
                 "Global_intensity" = numeric(),            # Global intensity
                 "Sub_metering_1" = numeric(),              # Sub_metering_1
                 "Sub_metering_2" = numeric(),              # Sub_metering_2
                 "Sub_metering_3" = numeric()               # Sub_metering_3
)

# while there is data, and the date is in the specified interval
while ( length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0 && go_on) {
    
    # read line, remove semicolons
    oneLine <- strsplit(oneLine, ";")
    curr_date <- as.Date(dmy(oneLine[[1]][1])) #parse date
    
    # id date is inside the required interval
    if (curr_date >= first_date && curr_date <= last_date) {
        
        # add row to data frame
        data[lines_read,1] <- as.POSIXct(
            paste(oneLine[[1]][1],oneLine[[1]][2]),format = "%d/%m/%Y %H:%M:%S")
        
        data[lines_read,2:8] <- as.numeric(oneLine[[1]][3:9])
        
        lines_read <- lines_read+1;
    } 
    
    if (curr_date > last_date) go_on <- 0;
} 
close(con)

# plot 1
data[data$Global_active_power == "?","Global_active_power"] <- NA
hist(data$Global_active_power,
     col= "red", main = "Global active power", 
     xlab = "Global active power (kilowatts)")

dev.copy(png, file = "plot1.png") ## Copy my plot to a PNG file 
dev.off() ## close the PNG device

