plot1 <- function(filePath) {
     data <- read.csv2(filePath)
     
     data$Date <- sapply(data$Date,function(x) as.Date(x,format="%d/%m/%Y"))
     graphData <- data[which(data$Date >= as.Date("2007-02-01") &
                                  data$Date <= as.Date("2007-02-02")),]
     
     x <- as.numeric(levels(
          graphData$Global_active_power))[graphData$Global_active_power]
     
     png(filename = "plot1.png", width = 480, height = 480)
     hist(x, xlab = "Global Active Power (kilowatts)", col="red",
          main = "Global Active Power")
     dev.off()
}