plot3 <- function() {
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
  
  ## X axis values
  dateAndTime <- strptime(
                          paste(powerConsumption$Date, 
                                powerConsumption$Time), 
                          "%d/%m/%Y %H:%M:%S")
  
  ## Only one graph
  par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0.5, 0, 0))
  
  ## Plot Energy Sub-metering vs time 
  with(powerConsumption, 
       {
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
                 cex = 0.8)
    }
  )
  

  

  
  ## Create the PNG file with size 480 by 480 pixels
  dev.copy(png, file = "plot3.png", width=480, height=480)
  
  ## close the device
  dev.off()
  
  invisible()
}