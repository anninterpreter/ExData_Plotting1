# convert date column into Date which is a factor column for now
load_and_preprocess_data <- function(){
  "
  Load and Transforms the data into required format and return the objective DataFrame for 
  further proceedings
  return: DataFrame in required format where range of Date is in [2007-02-01, 2007-02-02]
  "
  df <- read.csv2("./data/household_power_consumption.txt", stringsAsFactors  = FALSE)
  # Converting variables into the numeric from 'Global_active_power' to 'Sub_metering_3'
  df <- tbl_df(df)
  df[, 3:9] <- sapply(df[, 3:9], as.numeric)
  
  df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
  x <- paste(df$Date, df$Time)
  df$Time <- strptime(x, "%Y-%m-%d %H:%M:%S")
  filtered_df <- subset(df, Date>= as.Date("2007-02-01") & Date<= as.Date("2007-02-02"))
  return(filtered_df)
}

create_plot4 <- function(df){
  
  png(filename = "plot4.png", width = 480, height = 480, unit = "px")
  par(mfrow = c(2,2))
  # row 1 col 1
  with(df, plot(Time, Global_active_power, type = "n", xlab = "", 
                ylab = "Global Active Power (kilowatts)"))
  with(df, lines(Time, Global_active_power))
  # row 1 col 2
  with(df, plot(Time, Voltage, type = "n", xlab = "datetime", ylab = "Voltage"))
  with(df, lines(Time, Voltage))
  # row 2 col 1
  with(df, plot(Time, Sub_metering_1, type = "l", col = "black", xlab = NA, 
                ylab = "Energy sub metering"))
  with(df, lines(Time, Sub_metering_2, type = "l", col = "red"))
  with(df, lines(Time, Sub_metering_3, type = "l", col = "blue"))
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
         col = c("black", "red", "blue"), lty = rep(1,3))
  # row 2 col 2
  with(df, plot(Time, Global_reactive_power, type = "n", xlab = "datetime"))
  with(df, lines(Time, Global_reactive_power))
  
  dev.off()
}


df <- load_and_preprocess_data()
create_plot4(df)