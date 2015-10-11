plot2 <- function() {
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
  
  ## Only one graph
  par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0.1, 0.1, 0.1, 0.1))

  ## Plot the Global Active Power vs Time
  plot(strptime(
                paste(powerConsumption$Date, 
                      powerConsumption$Time), 
                "%d/%m/%Y %H:%M:%S"), 
       powerConsumption$Global_active_power, 
       type = "l", 
       xlab = "",
       ylab = "Global Active Power (kilowatts)")
  
  ## Create the PNG file with size 480 by 480 pixels
  dev.copy(png, file = "plot2.png", width=480, height=480)
  
  ## close the device
  dev.off()
  
  invisible()
}