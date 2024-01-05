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

    subset_data <- data[Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02")]
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

# Function to plot the histogram
plot_histogram <- function(subset_data) {
    hist(subset_data$Global_active_power, col = "red", main = "Global Active Power",
        xlab = "Frequency", ylab = "Frequency", xlim = c(0, max(subset_data$Global_active_power, na.rm = TRUE)))
}


file_path <- "household_power_consumption.txt"
data <- load_data(file_path)
subset_data <- preprocess_data(data)
generate_png(plot_histogram, subset_data, "plot1.png")