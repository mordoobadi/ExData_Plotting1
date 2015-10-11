plot4 <- function() {
  ## File handle
  fHandle <- file("household_power_consumption.txt")
  
  ## Read the Power consumption dataset only for the specific dates
  powerConsumption <- read.table(
    text = grep("^[1,2]/2/2007", readLines(fHandle), value = TRUE), 
    sep = ";", 
    header = TRUE, 
    col.names = c("Date", "Time", "Global_active_power", 
                  "Global_reactive_power", "Voltage", 
                  "Global_intensity", "Sub_metering_1", 
                  "Sub_metering_2", "Sub_metering_3"), 
    colClasses = c(rep("character", 2), rep("numeric", 7)), 
    na.strings = "?")
  
  ## Close the file
  close(fHandle)
  
  ## X-Axis vaues
  dateAndTime <- strptime(
    paste(powerConsumption$Date, 
          powerConsumption$Time), 
    "%d/%m/%Y %H:%M:%S")
  
  ## 2 by 2 graph
  par(mfrow = c(2,2), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0.5, 0, 0))
  
  ## Plot the following four graphs
  ##   1 - Global active power vs time
  ##   2 - Voltage vs time
  ##   3 - Energy Sub-metering vs time
  ##   4 - Global reactive power vs time
  with(powerConsumption, {
    plot(dateAndTime, 
         Global_active_power, 
         type = "l", 
         ylab = "Global Active Power", 
         xlab = "")
    plot(dateAndTime, 
         Voltage, 
         type = "l", 
         ylab = "Voltage", 
         xlab = "datetime")
    plot( dateAndTime, 
          Sub_metering_1, 
          type = "l",
          col = "Black",
          xlab = "",
          ylab = "Energy sub metering")
    lines( dateAndTime, 
           Sub_metering_2,
           col = "Red")
    lines( dateAndTime, 
           Sub_metering_3,
           col = "Blue")
    legend("topright", 
           col = c("Black", "Red", "Blue"), 
           lty = 1, 
           lwd = 2, 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           cex = 0.7,
           bty = "n")
    plot(dateAndTime, 
         Global_reactive_power, 
         type = "l", 
         ylab = "Global_rective_power", 
         xlab = "datetime")
  })
  
  
  
  ## Create the PNG file with size 480 by 480 pixels
  dev.copy(png, file = "plot4.png", width=480, height=480)
  
  ## close the device
  dev.off()
  
  invisible()
}