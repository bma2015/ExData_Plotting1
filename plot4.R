plot4 <- function(filePath) {
     data <- read.csv2(filePath)
     
     data[,"Timestamp"] <- as.POSIXct(paste(data$Date, data$Time),
                                      format="%d/%m/%Y %H:%M:%S")
     graphData <- data[which(data$Timestamp >= as.POSIXct("2007-02-01") &
                                  data$Timestamp < as.POSIXct("2007-02-03")),]
     t <- graphData$Timestamp
     
     png(filename = "plot4.png", width = 480, height = 480)
     par(mfcol = c(2,2))
     
     #(1,1) plot
     gab <- as.numeric(levels(
          graphData$Global_active_power))[graphData$Global_active_power]
     plot(t, gab, xlab = "", ylab = "Global Active Power", type = "n")
     lines(t, gab, xlab = "", ylab = "Global Active Power")
     
     #(2,1) plot
     
     sm1 <- as.numeric(levels(
          graphData$Sub_metering_1))[graphData$Sub_metering_1]
     sm2 <- as.numeric(
          levels(graphData$Sub_metering_2))[graphData$Sub_metering_2]
     sm3 <- as.numeric(
          levels(graphData$Sub_metering_3))[graphData$Sub_metering_3]
     
     maxESM <- max(sm1,sm2,sm3)
     
     plot(t, sm1, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering",
          type = "n")
     lines(t, sm1, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering")
     par(new=T)
     plot(t, sm2, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering",
          type = "n")
     lines(t, sm2, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering",
           col = "red")
     par(new=T)
     plot(t, sm3, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering",
          type = "n")
     lines(t, sm3, ylim = c(0,maxESM), xlab = "", ylab = "Energy sub metering",
           col = "blue")
     legend("topright", col = c("black", "red", "blue"), lty = 1, bty = "n",
            legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
            cex = .9)
     
     #(1,2) plot
     
     voltage <- as.numeric(levels(
          graphData$Voltage))[graphData$Voltage]
     minV <- min(voltage)
     maxV <- max(voltage)
     plot(t, voltage, ylim = c(minV,maxV), xlab = "datetime", ylab = "Voltage",
          type = "n")
     lines(t, voltage, ylim = c(minV,maxV), xlab = "datetime", ylab = "Voltage")
     
     #(2,2) plot
     
     grp <- as.numeric(levels(
          graphData$Global_reactive_power))[graphData$Global_reactive_power]
     minGrp <- min(grp)
     maxGrp <- max(grp)
     plot(t, grp, ylim = c(minGrp,maxGrp), xlab = "datetime",
          ylab = "Global_reactive_power", type = "n")
     lines(t, grp, ylim = c(minGrp,maxGrp), xlab = "datetime",
           ylab = "Global_reactive_power")
     
     dev.off()
}