plot2 <- function () {
  
  ## read the file
  fulldata <- read.table("household_power_consumption.txt", header=TRUE, sep=";")
  
  ## convert the Date column to date format
  fulldata$Date <- as.Date(fulldata$Date, format = "%d/%m/%Y")
  
  ## select the data range needed
  library(dplyr)
  mydata <- filter(fulldata, Date>="2007-02-01" & Date<="2007-02-02")
  
  ## format the data frame
  mydata$Time <- strptime(paste(mydata$Date, mydata$Time), format = "%Y-%m-%d %H:%M:%S")
  for (i in 3:8) {
    mydata[,i] <- as.numeric(levels(mydata[,i]))[mydata[,i]]
  }

  ## plot and output to png format
  png(file="plot4.png", width=480, height=480)
  par(mfrow=c(2,2))
  
  ## plot 1
  with(mydata, plot(Time, Global_active_power, 
                    ylab = "Global Active Power", 
                    xlab="", 
                    type="s")
  )
  
  ## plot 2
  with(mydata, plot(Time, Voltage, 
                    ylab = "Voltage", 
                    xlab="datetime", 
                    type="s"
                    )
      )
  
  ## plot 3
  with(mydata, plot(Time, Sub_metering_1, 
                    ylab = "Energy sub metering", 
                    xlab="", 
                    type="s"
  )
  )
  lines(mydata$Time, mydata$Sub_metering_2, col="red", type="s")
  lines(mydata$Time, mydata$Sub_metering_3, col="blue", type="s")
  legend("topright", pch=0, col = c("black", "red", "blue"), legend = names(mydata[7:9]))
  
  ## plot 4
  with(mydata, plot(Time, Global_reactive_power, 
                    ylab = "Global_reactive_power", 
                    xlab="datetime", 
                    type="s"
  )
  )
  dev.off()
}