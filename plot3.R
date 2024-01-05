library(data.table)

# Function to set the working directory
set_working_directory <- function(path) {
    setwd(path)
}

# Function to load data
load_data <- function(file_path) {
    data <- fread(file_path, 
                colClasses = c("character", "character", rep("numeric", 7)),
                na.strings = "?")
    return(data)
}

# Function to preprocess data
preprocess_data <- function(data) {
    data[, Date := as.Date(Date, format="%d/%m/%Y")]
    data[, Time := strptime(Time, format="%H:%M:%S")]

    ##############################################################
    subset_data <- data[Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02")]
    subset_data[,weekday := weekdays(Date)]

    return(subset_data)
}

# Plots Functions ##############################################################
generate_png <- function(plot_func, data, png_filename) {
    # Start the PNG device
    png(png_filename, width = 480, height = 480)

    # Call the provided plotting function with the data
    plot_func(data)

    # End the PNG device
    dev.off()
}

weekdays_labeling <- function(subset_data) {
    # Find the start index for Thursday and Friday
    thursday_start <- which(subset_data$weekday == "Thursday")[1]
    friday_start <- which(subset_data$weekday == "Friday")[1]
    saturday_start <- length(subset_data$Global_active_power)


    # Define the positions for the x-axis ticks
    xaxt <- c(thursday_start, friday_start, saturday_start)

    # Define the labels for the x-axis ticks
    xaxt_labels <- c("Thu", "Fri", "Sat")

    return(list(xaxt, xaxt_labels))
}



# Function to plot the time series with 3 lines
plot_time_series_3_line <- function(subset_data) {
    # Ensure the data is ordered by time if it's not already
    xaxt_info <- weekdays_labeling(subset_data)
    xaxt <- xaxt_info[[1]]
    xaxt_labels <- xaxt_info[[2]]

    # Plot the data for sub-metering 1, 2, and 3
    plot(subset_data$Sub_metering_1, type = "l", col = "black", xaxt = "n", xlab = "", ylab = "Energy sub metering", 
        xlim = c(1, length(subset_data$Global_active_power)), ylim = range(c(subset_data$Sub_metering_1, subset_data$Sub_metering_2, subset_data$Sub_metering_3), na.rm = TRUE))
    lines(subset_data$Sub_metering_2, type = "l", col = "red")
    lines(subset_data$Sub_metering_3, type = "l", col = "blue")
    
    # Add the custom x-axis
    axis(1, at = xaxt, labels = xaxt_labels)
    
    # Add a legend
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
        col = c("black", "red", "blue"), lty = 1)
}


# Usage of functions
file_path <- "household_power_consumption.txt"
data <- load_data(file_path)
subset_data <- preprocess_data(data)

generate_png(plot_time_series_3_line, subset_data, "plot3.png")


