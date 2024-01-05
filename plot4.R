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
# generate_png_custom_columns <- function(plot_func, data, png_filename, y_column_name_list) {
    
#     # generate a multiple plot on a single page

#     # Start the PNG device
#     png(png_filename, width = 480, height = 480)

#     # Call the provided plotting function with the data
#     par(mfrow=c(2,2))

#     for (i in 1:length(y_column_name_list)) {
#         plot_func(data, y_column_name_list[i])
#     }

#     # add the plot_time_series_3_line to the plot as the 3rd plot
#     plot_time_series_3_line(data)


#     # End the PNG device
#     dev.off()
# }

# Function to multiple plot on a single page with custom columns and functions
generate_png_custom_columns <- function(plot_func, data, png_filename, y_column_name_list) {
    # Start the PNG device
    png(png_filename, width = 480, height = 480)

    # Set up a 2x2 plot layout
    par(mfrow=c(2,2))

    # Plot the first two columns from y_column_name_list
    for (i in 1:2) {
        plot_func(data, y_column_name_list[i])
    }

    # Now plot the third plot with plot_time_series_3_line
    plot_time_series_3_line(data)

    # If there's a fourth y-column, plot it; otherwise, leave the space blank
    if (length(y_column_name_list) > 2) {
        plot_func(data, y_column_name_list[3])
    }
    
    # End the PNG device
    dev.off()
}

# Function to plot the histogram with custom x-axis labels
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

# Function to convert a string to title case
str_to_title <- function(str) {
    # Split the string into words
    words <- strsplit(str, " ")[[1]]

    # Capitalize the first letter of each word
    words <- sapply(words, function(word) {
        paste(toupper(substring(word, 1, 1)), substring(word, 2), sep = "", collapse = NULL)
    })

    # Collapse the words back into a single string
    title <- paste(words, collapse = " ")

    return(title)
}

###############################################################################

# Function to plot the time series
plot_time_series <- function(subset_data, y_column_name) {
    # Check if the specified y_column_name exists in subset_data
    if (!y_column_name %in% names(subset_data)) {
        stop("The specified column does not exist in the data.")
    }

    # Get the x-axis ticks and labels
    xaxt_info <- weekdays_labeling(subset_data)
    xaxt <- xaxt_info[[1]]
    xaxt_labels <- xaxt_info[[2]]

    # remove the _ from the y_column_name
    ylab <- gsub("_", " ", y_column_name)

    # Capitalize the first letter of each word
    ylab <- str_to_title(ylab)

    # Plot the data using the specified y_column_name
    plot(subset_data[[y_column_name]], type = "l", xaxt = "n", xlab = "Datetime", 
        ylab = ylab, xlim = c(1, length(subset_data[[y_column_name]])), 
        ylim = range(subset_data[[y_column_name]], na.rm = TRUE))

    # Add the custom x-axis
    axis(1, at = xaxt, labels = xaxt_labels)
}


# Function to plot the time series with 3 lines
plot_time_series_3_line <- function(subset_data) {
    # Ensure the data is ordered by time if it's not already
    xaxt <- weekdays_labeling(subset_data)[[1]]
    xaxt_labels <- weekdays_labeling(subset_data)[[2]]

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

column_list <- c("Global_active_power", "Voltage", "Global_reactive_power")
generate_png_custom_columns(plot_time_series, subset_data, "plot4.png", column_list)













