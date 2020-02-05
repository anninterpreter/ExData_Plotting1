

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

create_plot1 <- function(df){
  png(filename = "plot1.png", width = 480, height = 480, unit = "px")
  hist(df$Global_active_power, xlab = "Global Active Power (kilowatts)", 
       main = "Global Active Power", col = 'red')
  dev.off()
}

df <- load_and_preprocess_data()
create_plot1(df)