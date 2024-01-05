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
    subset_data[,weekday := weekdays(Date)]

    return(subset_data)
}

###############################################################################
generate_png_custom_columns <- function(plot_func, data, png_filename, y_column_name) {
    # Start the PNG device
    png(png_filename, width = 480, height = 480)

    # Call the provided plotting function with the data
    plot_func(data, y_column_name)

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
    plot(subset_data[[y_column_name]], type = "l", xaxt = "n", xlab = "Time", 
        ylab = ylab, xlim = c(1, length(subset_data[[y_column_name]])), 
        ylim = range(subset_data[[y_column_name]], na.rm = TRUE))

    # Add the custom x-axis
    axis(1, at = xaxt, labels = xaxt_labels)
}


# Usage of functions
file_path <- "household_power_consumption.txt"
data <- load_data(file_path)
subset_data <- preprocess_data(data)

generate_png_custom_columns(plot_time_series, subset_data, "plot2.png", "Global_active_power")











