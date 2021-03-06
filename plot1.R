plot1 <- function () {
  
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
  png(file="plot1.png", width=480, height=480)  
  with(mydata, hist(Global_active_power, 
                    col="red", 
                    main = "Global Active Power", 
                    xlab = "Global Active Power (kilowatts)",
                    breaks = 12
                    )
       )
  dev.off()
}