plot3 <- function(filePath) {
     data <- read.csv2(filePath)
     
     data[,"Timestamp"] <- as.POSIXct(paste(data$Date, data$Time),
                                      format="%d/%m/%Y %H:%M:%S")
     graphData <- data[which(data$Timestamp >= as.POSIXct("2007-02-01") &
                                  data$Timestamp < as.POSIXct("2007-02-03")),]
     t <- graphData$Timestamp
     y1 <- as.numeric(levels(
          graphData$Sub_metering_1))[graphData$Sub_metering_1]
     y2 <- as.numeric(levels(
          graphData$Sub_metering_2))[graphData$Sub_metering_2]
     y3 <- as.numeric(levels(
          graphData$Sub_metering_3))[graphData$Sub_metering_3]
     
     maxESM <- max(y1,y2,y3)
     
     png(filename = "plot3.png", width = 480, height = 480)
     plot(t, y1, ylim = c(0,maxESM), xlab = "",
          ylab = "Energy sub metering", type = "n")
     lines(t, y1, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering")
     par(new=T)
     plot(t, y2, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering",
          type = "n")
     lines(t, y2, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering",
           col = "red")
     par(new=T)
     plot(t, y3, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering",
          type = "n")
     lines(t, y3, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering",
           col = "blue")
     legend("topright", col = c("black", "red", "blue"), lty = 1,
            legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
     dev.off()
}