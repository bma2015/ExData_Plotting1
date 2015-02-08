plot2 <- function(filePath) {
     data <- read.csv2(filePath)
     
     data[,"Timestamp"] <- as.POSIXct(paste(data$Date, data$Time),
                                      format="%d/%m/%Y %H:%M:%S")
     graphData <- data[which(data$Timestamp >= as.POSIXct("2007-02-01") &
                                  data$Timestamp < as.POSIXct("2007-02-03")),]
     t <- graphData$Timestamp
     y <- as.numeric(levels(
          graphData$Global_active_power))[graphData$Global_active_power]
     
     png(filename = "plot2.png", width = 480, height = 480)
     plot(t, y, xlab = "", ylab = "Global Active Power (kilowatts)", type = "n")
     lines(t, y, ylab = "Global Active Power (kilowatts)")
     dev.off()
}