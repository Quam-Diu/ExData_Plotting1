plot4 <- function(){
        ## A) Parameters
        ## This script has no parameters
        
        ## B) Objetive
        ## To produce a set of four plots (arranged in 2 columns and 2 rows) and save them to a png file
        ## The data is supposed to be in a directory called /data, but if the file is missing a message is printed
        ## Also, to run properly, the following libraries should be installed and loaded
        ##      - library(dplyr)
        ##      - library(lubridate)
        ## See Readme for more details
        
        ## C) Procedure
        
        ## c.1 Initial default values
        fileName <- "data/household_power_consumption.txt"
        filterDates <- c("01/02/2007","02/02/2007")
        
        ## c.2 Checks if the file exists in the specified directory
        if(file.exists(fileName)){
                ## c.2.1 Calculates the required memory to process the data
                requiredMemory = 2*file.info(fileName)$size/(1024^2)
                print(paste("The data requires ", requiredMemory , " MB of memory"))
                
                # c.2.2 Reads the data using the dplyr library and filters the dates
                uciData  <- read.table(fileName, sep=";", na.strings = "?", header=TRUE)
                uci_tbldf <- tbl_df(uciData)
                uci_tbldf <- mutate(uci_tbldf, Time = dmy_hms(paste(Date, Time, sep=" ")), Date = as.Date(Date, "%d/%m/%Y"))
                feb07_tbldf <- filter(uci_tbldf, Date %in% as.Date(filterDates, "%d/%m/%Y"))
                
                ## c.2.3 Creates the graph and saves it to a file
                png(filename = "plot4.png", width = 480, height = 480, units = "px")                
                par(mfrow = c(2,2), cex.lab=0.65, cex.axis=0.65)
                ## a) Plot 1
                with(febData, plot(Time, Global_active_power, type="l", ylab = "Global Active Power", xlab=""))
                ## b) Plot 2
                with(febData, plot(Time, Voltage, type="l", ylab = "Voltage", xlab="datetime"))
                ## c) Plot 3
                with(febData, plot(Time, Sub_metering_1, type="l", ylab = "Energy sub metering", xlab=""))
                with(febData, lines(Time, Sub_metering_2, col="red"))
                with(febData, lines(Time, Sub_metering_3, col="blue"))
                legend("topright", lty=1, lwd= 2, box.lty=0, cex=0.6, pt.cex=1, inset=0.01, c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"))
                ## d) Plot 4
                with(febData, plot(Time, Global_reactive_power, type="l", xlab="datetime"))
                dev.off()
                
        } else {
                print("Download the file and put it into the /data directory.")
                print("Download link: " %s+% "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
        } 
}