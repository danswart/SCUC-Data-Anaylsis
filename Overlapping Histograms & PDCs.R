# EXPLORING HISTOGRAMS & PROBABILITY DENSITY CURVES
# SIMULATION DATA FOR PLANNING IMPROVEMENTS TO THE
# DISTRIBUTION OF 3RD GRADE MATH STAAR SCORES


#####  CREATE A SYNTHETIC HISTOGRAM FROM RNORM() #####
#####      BINS ARE NOT SYMETRICAL.  IT IS AN    #####
#####   APPROXIMATION OF A NORMAL DISTRIBUTION   #####


# Load the necessary library
library(ggplot2)
library(patchwork)
library(scales)


# Step 1: Generate a sample of data from a normal distribution
set.seed(123)  # Set a seed for reproducibility
n <- 500       # Number of samples
mu <- 50       # Mean of the normal distribution
sigma <- 16    # Standard deviation of the normal distribution

# Generate the data
normal_data <- rnorm(n, mean = mu, sd = sigma)

# Step 2: Create a data frame
df <- data.frame(value = normal_data)

# Step 3: Plot a histogram using ggplot2
p1 <- ggplot(df, aes(x = value)) + # creates blank canvas for plot
  geom_histogram(bins = 30, fill = "blue", color = "white", alpha = 0.7) + # specifies the type of plot along with formatting
  geom_vline(aes(xintercept = mean(value)), color = "red", size = 1, linetype = "dashed") + # adds a verticle line along with formatting
  labs(title = "Histogram of a Normal Distribution with Mean",
       x = "Value",
       y = "Frequency") + # specifies labels
  theme_minimal()

# Compute the counts and positions for the text labels
p1 + geom_text(stat = "bin", aes(label = after_stat(count)), vjust = -0.5, size = 3) # displays plot and adds text to plot



#######################
#####  CODE FOR PRODUCING A DENSITY CURVE OVERLAY 
#####  USES IRIS DATA FRAME
#####  FROM https://www.youtube.com/watch?v=VJoP0_TONrc&t=411s
#######################

# Load the necessary library
library(ggplot2)
library(patchwork)
library(scales)


# Create the plot
p2 <- ggplot2::ggplot(iris, aes(x = Sepal.Length)) +  # Initialize blank convas for ggplot, using Sepal.Length from the iris dataset

  # Add histogram layer
  ggplot2::geom_histogram(
    aes(
      y = ggplot2::after_stat(count/sum(count))  # Scale y-axis to show relative frequency (count/sum(count))
    ),
    binwidth = 1,  # Set the width of each bin in the histogram
    fill = "blue",  # Set the fill color of the bars
    color = "white",  # Set the color of the bar borders
    alpha = 0.7  # Set the transparency of the bars
  ) +

  # Add density curve layer
  ggplot2::geom_density() +  # Add a density curve (smooth estimate) of the distribution

  # Add text labels inside each bar of the histogram
  ggplot2::stat_bin(
    aes(
      y = after_stat(count / sum(count)),  # Ensure y-axis is scaled as relative frequency
      label = percent(after_stat(count / sum(count)))  # Format the count as a percentage label
    ),
    position = position_stack(vjust = 0.5),  # Position the text vertically in the center of each bar
    binwidth = 1,  # Set the bin width (same as for the histogram)
    geom = "text",  # Specify that we want to add text labels
    color = "white"  # Set the color of the text labels to white for contrast
  ) +

  # Format the y-axis to show percentages instead of raw counts
  ggplot2::scale_y_continuous(labels = percent) +  # Use scales::percent to display y-axis as percentages

  # Add a vertical dashed line at the mean of Sepal.Length
  geom_vline(
    aes(xintercept = mean(Sepal.Length)),  # Calculate the mean of Sepal.Length and plot a vertical line at this value
    color = "red",  # Set the line color to red
    size = 1,  # Set the line width to 1
    linetype = "dashed"  # Make the line dashed
  ) +

  # Add labels and title to the plot
  labs(
    title = "Histogram and Density Curve of a Normal Distribution with Mean Indicated",  # Title of the plot
    x = "Value",  # Label for the x-axis
    y = "Frequency"  # Label for the y-axis
  ) +

  # Use a minimal theme for cleaner visual appearance
  theme_minimal()  # Apply minimalistic theme to the plot

# Display the plot
p2





# Check the normality of the distribution
shapiro.test(df$value)

# Extract the data used to create the plot
plot_data <- ggplot_build(p2)$data[[1]]

# Print the counts (frequency) in each bin
plot_data$count





#############################
#####  PERFECTLY SYMETRIC NORMAL HISTOGRAM WITH   #####
#####  DENSITY CURVE, BIN VALUES DERIVED BY CODE  #####
############################


# Load the necessary library
library(ggplot2)
library(patchwork)
library(scales)


# Parameters
total_values <- 500      # Total number of data points
bins <- 20               # Number of bins
range_min <- 0           # Minimum value
range_max <- 100         # Maximum value
median <- 50             # Median of the normal distribution
sd <- 15                 # Standard deviation
bin_width <- (range_max - range_min) / bins  # Width of each bin

# Generate the bin edges (from 0 to 100, with 20 bins)
bin_edges <- seq(range_min, range_max, by = bin_width)

# Generate the data (normal distribution) using a mean of 50 and standard deviation of 15
x_values <- seq(range_min, range_max, length.out = 1000)
normal_density <- dnorm(x_values, mean = median, sd = sd)

# Calculate the frequency for each bin (scaled to total_values)
bin_centers <- bin_edges[-length(bin_edges)] + bin_width / 2  # Centers of bins
bin_frequencies <- diff(pnorm(bin_edges, mean = median, sd = sd)) * total_values  # Scaled frequencies

# Create a data frame for plotting the histogram
df <- data.frame(
  bin = factor(1:bins),
  bin_center = bin_centers,
  frequency = round(bin_frequencies)  # Round the frequencies to the nearest integer
)

# Calculate the scaling factor to match the normal curve to the histogram
max_frequency <- max(df$frequency)  # Maximum frequency in the histogram
normal_density_scaled <- dnorm(bin_centers, mean = median, sd = sd) * total_values * bin_width  # Scale the normal density to match the histogram frequencies

# Plot the histogram with the normal distribution curve overlay
p3 <- ggplot(df, aes(x = bin_center, y = frequency)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", width = bin_width * 0.9) +  # Plot the histogram
  geom_line(aes(x = bin_centers, y = normal_density_scaled), color = "red", size = 1) +  # Overlay the scaled normal curve
  geom_text(aes(label = frequency), vjust = -0.5, size = 3, color = "black") +  # Display frequencies on the bars
  labs(title = "Symmetrical Histogram of Normal Distribution with Overlayed Normal Curve",
       x = "Value", y = "Frequency") +
  theme_minimal()

p3




########################
#####  SYMETRIC NORMAL HISTOGRAM WITH DENSITY CURVE  #####
#####          BIN VALUES SPECIFIED BY USER         #####
########################


# Load the necessary library
library(ggplot2)
library(patchwork)
library(scales)


# Parameters
total_values <- 500      # Total number of data points
bins <- 20               # Number of bins
range_min <- 0           # Minimum value
range_max <- 100         # Maximum value
mean <- 50               # Mean of the normal distribution
sd <- 15                 # Standard deviation
bin_width <- (range_max - range_min) / bins  # Width of each bin

# Generate the bin edges (from 0 to 100, with 20 bins)
bin_edges <- seq(range_min, range_max, by = bin_width)

# Generate the bin centers (midpoints of each bin)
bin_centers <- bin_edges[-length(bin_edges)] + bin_width / 2

# Create an empty data frame for manually specifying the frequencies
# Initialize it with zeros (you can modify the frequencies later)
df <- data.frame(
  bin = factor(1:bins),
  bin_center = bin_centers, # As derived above
  frequency = rep(0, bins)  # Initialize with 0, you can modify these values
)

# Now, you can manually input the frequencies for each bin. Example:
df$frequency <- c(0, 1, 3, 6, 13, 22, 34, 47, 58, 65, 65, 58, 47, 34, 22, 13, 6, 3, 1, 0)

# Calculate the scaling factor to match the normal curve to the histogram
max_frequency <- max(df$frequency)  # Maximum frequency in the histogram
normal_density_scaled <- dnorm(bin_centers, mean = mean, sd = sd) * total_values * bin_width  # Scale the normal density

# Plot the histogram with the manually specified frequencies and overlay the normal distribution curve
p4 <- ggplot(df, aes(x = bin_center, y = frequency)) + # Initialize plot with blank canvas
  geom_bar(stat = "identity", fill = "lightgreen", alpha = 0.50, color = "black", width = bin_width * 0.9) +  # Add the histogram layer
  geom_line(aes(x = bin_centers, y = normal_density_scaled), color = "red", size = 1) +  # Add the line layer to overlay the scaled normal curve
  geom_text(aes(label = frequency), vjust = -0.5, size = 3, color = "black") +  # Display frequencies in the bars
  geom_vline(xintercept = mean, color = "blue", linetype = "dashed", size = 2) +  # Red dashed vertical line at mean for normal curve
  labs(title = "Symmetrical Histogram of Normal Distribution with Overlayed Normal Curve",
       x = "Value", y = "Frequency") + # Add layer of labels
  theme_minimal()

p4




#############################
#####  Right-Skewed Log-Normal Distribution        #####
#####  with Frequency on Y-Axis and Vertical       #####
#####  Line at Mean, with Area under curve filled  #####
#############################


# Load the necessary library
library(ggplot2)
library(patchwork)
library(scales)


# Parameters
total_values <- 500      # Total number of data points (n)
range_min <- 0           # Minimum value
range_max <- 100         # Maximum value
meanlog <- 4.0           # Mean of the log-transformed data (log-scale)
sdlog <- 0.25           # Standard deviation of the log-transformed data
bins <- 20               # Number of bins (not used directly here but for context)
bin_width <- (range_max - range_min) / bins  # Width of each bin

# Generate x values for the curve (from 0 to 100)
x_values <- seq(range_min, range_max, length.out = 1000)

# Calculate the log-normal density for each x value
log_normal_density <- dlnorm(x_values, meanlog = meanlog, sdlog = sdlog)

# Scale the density to match the frequency on the y-axis
scaled_density <- log_normal_density * total_values * bin_width

# Create a data frame for plotting
df <- data.frame(x = x_values, y = scaled_density)

# Calculate the mean of the log-normal distribution on the original scale
mean_value <- exp(meanlog + (sdlog^2) / 2)

# Plot the scaled log-normal density curve with color fill under the curve
p5 <- ggplot(df, aes(x = x, y = y)) +
  geom_area(fill = "lightgreen", alpha = 0.5) +  # Fill the area under the curve
  geom_line(color = "blue", size = 1) +    # Border line for the density curve
  geom_vline(xintercept = mean_value, color = "red", linetype = "dashed", size = 1) +  # Red dashed vertical line at mean for log-normal curve
  labs(title = "Right-Skewed Log-Normal Distribution with Frequency on Y-Axis",
       x = "Value", y = "Frequency") +
  theme_minimal()

p5

library(patchwork)
p4 + p5




#################
#####  Normal and Right-Skewed (Log-Normal) Curves
#####  On Same Plot
#################


# Load the necessary library
library(ggplot2)
library(patchwork)
library(scales)


# Parameters for normal curve AND skewed curve
total_values <- 500      # Total number of data points (n)
range_min <- 0           # Minimum value
range_max <- 100         # Maximum value
mean_normal <- 50        # Mean for the normal distribution
sd_normal <- 10          # Standard deviation for the normal distribution
meanlog <- 4.0           # Mean of the log-transformed data (log-scale) for skewed curve
sdlog <- 0.12             # Standard deviation of the log-transformed data for skewed curve
bins <- 30               # Number of bins (not used directly here but for context)
bin_width <- (range_max - range_min) / bins  # Width of each bin

# Generate x values for the curves (from 0 to 100)
x_values <- seq(range_min, range_max, length.out = 1000)

# Calculate the normal distribution density for each x value
normal_density <- dnorm(x_values, mean = mean_normal, sd = sd_normal)

# Calculate the right-skewed (log-normal) density for each x value
log_normal_density <- dlnorm(x_values, meanlog = meanlog, sdlog = sdlog)

# Create a data frame for plotting
df <- data.frame(x = x_values, normal = normal_density, log_normal = log_normal_density)

# Plot the normal and log-normal curves with color fills
p6 <- ggplot(df) + # Initialize plot as blank layer 
  # Fill area under the normal curve (Symmetrical Normal Curve)
  geom_area(aes(x = x, y = normal), fill = "lightgreen", color = "darkgreen", alpha = 0.5) +
  geom_vline(aes(xintercept = mean_normal), color = "blue", size = 1, linetype = "dashed") + # adds a vertical line along with formatting at mean
  # Fill area under the log-normal curve (Right-Skewed Curve)
  geom_area(aes(x = x, y = log_normal), fill = "lightcoral", color = "red", alpha = 0.5) +
  # Line for the normal curve
  geom_line(aes(x = x, y = normal), color = "darkgreen", linewidth = 1) +
  # Line for the right-skewed curve
  geom_line(aes(x = x, y = log_normal), color = "red", linewidth = 1) +
  geom_vline(xintercept = mean_value, color = "blue", linetype = "dashed", size = 1) +  # Red dashed vertical line at mean
  labs(title = "Normal and Right-Skewed (Log-Normal) Curves",
       x = "Value", y = "Density") +
  theme_minimal()

p6

