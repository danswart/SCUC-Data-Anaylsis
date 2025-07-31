#####  CONTROL CHARTS IN R   #####
#####  FOR PRODUCING IMAGES  #####
#####  FOR RMARKDOWN PAPERS  #####

#####  USING SYNTHETIC DATA  #####

#####            USING ggQC EXTENSION TO GGPLOT2 PACKAGE         #####
#####       See http://rcontrolcharts.com/ for documentation     #####
#####          I PREFER THIS PACKAGE FOR CREATING BASIC          #####
#####  CONTROL CHARTS FOR INCLUSION IN RMARKDOWN OR QUARTO DOCS  #####


# Load libraries
library(tidyverse)
library(ggQC)
library(gridExtra)

# Set the seed for reproducibility
set.seed(123)

# Number of data points
num_points <- 15

# Generate 15 random positive integers
values <- sample.int(10, 
                     num_points, 
                     replace = TRUE
                     )

# Create a data frame
df <- data.frame(Date = seq(as.Date("2024-01-01"), 
                            by = "day", 
                            length.out = num_points), 
                 Value = values
                 )

# Create the Individuals chart using ggQC
ggplot2::ggplot(df, 
                aes(x = Date, 
                    y = Value
                    )
                ) +
  ggplot2::geom_point() + #add the points
  ggplot2::geom_line() + #add the lines
  ggQC::stat_QC(
    mapping = NULL,
    data = NULL,
    geom = "hline",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    n = NULL,
    method = "XmR",
    color.qc_limits = "red",
    color.qc_center = "blue",
    color.point = "black",
    color.line = "black",
    physical.limits = c(0, NA), # Set lower limit to 0
    auto.label = T,
    limit.txt.label = c("LCL", "UCL"),
    label.digits = 2,   #Use two digit in the label
    show.1n2.sigma = F  #Show 1 and two sigma lines
  ) + 
  ggplot2::scale_x_date(date_breaks = "1 day", 
                        date_labels = "%Y-%m-%d"
                        ) +  # Set date breaks and labels
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                     hjust = 1
                                                     )
                 ) +  # Rotate x-axis labels
  ggplot2::ggtitle("Individuals Chart created with ggQC") +
  ggplot2::labs(subtitle = "This is a subtitle",
                caption = "This is a caption"
                ) +
  ggplot2::ylab("New Y-axis Label") +  # Change y-axis label
  ggplot2::xlab("New X-axis Label")    # Change x-axis label





#####  A RUN chart in R using qicharts2 alone #####
###################################################


# Load necessary libraries
library(qicharts2)

# Set the seed for reproducibility
set.seed(123)

# Number of data points
num_points <- 25

# Generate 25 random positive integers
values <- sample.int(10, num_points, replace = TRUE)

# Create a sequence of 25 dates
dates <- seq(as.Date("2024-01-01"), by = "day", length.out = num_points)

# Create a data frame
df <- data.frame(Date = dates, Value = values)


# Create basic run chart with qicharts2

qicharts2::qic(
  x = dates,
  y = values,
  n = NULL, 
  data = df,
  # facets = NULL,
  # notes = NULL,
  chart = "run",
  agg.fun = "median",
  method = "anhoej",
  # multiply = 1,
  # freeze = NULL,
  # part = NULL,
  # exclude = NULL,
  # target = NA * 1,
  # cl = NA * 1,
  # nrow = NULL,
  # ncol = NULL,
  # scales = "fixed",
  title = "Run Chart created with qicharts2",
  ylab = "Measurement",
  xlab = "",
  subtitle = "Subtitle goes here",
  caption = "Caption goes here",
  # part.labels = NULL,
  show.labels = TRUE,
  # show.95 = FALSE,
  decimals = 1,
  point.size = 2.0,
  # x.period = NULL,
  # x.format = NULL,
  x.angle = 45,
  x.pad = 1,
  y.expand = NULL,
  y.neg = FALSE,
  # y.percent = FALSE,
  # y.percent.accuracy = NULL,
  # show.grid = TRUE,
  # flip = FALSE,
  # strip.horizontal = FALSE,
  # print.summary = TRUE,
  # return.data = TRUE
  # 
)




#####  A basic XmR chart in R with qicharts2 #####
##################################################


qicharts2::qic(
  x = dates,
  y = values,
  n = NULL, 
  data = df,
  # facets = NULL,
  # notes = NULL,
  chart = "i",
  agg.fun = "mean",
  method = "anhoej",
  # multiply = 1,
  # freeze = NULL,
  # part = NULL,
  # exclude = NULL,
  # target = NA * 1,
  # cl = NA * 1,
  # nrow = NULL,
  # ncol = NULL,
  # scales = "fixed",
  title = "Individuals Chart created with qicharts2",
  ylab = "Measurement",
  xlab = "",
  subtitle = "Subtitle goes here",
  caption = "Caption goes here",
  # part.labels = NULL,
  show.labels = TRUE,
  # show.95 = FALSE,
  decimals = 1,
  point.size = 2.0,
  # x.period = NULL,
  # x.format = NULL,
  x.angle = 45,
  x.pad = 1,
  y.expand = NULL,
  y.neg = FALSE,
  # y.percent = FALSE,
  # y.percent.accuracy = NULL,
  # show.grid = TRUE,
  # flip = FALSE,
  # strip.horizontal = FALSE,
  # print.summary = TRUE,
  # return.data = TRUE
  # 
)


######################################
#####     EXPERIMENTAL DATA      #####

# Load libraries
library(tidyverse)
library(ggQC)
library(gridExtra)


# Set the seed for reproducibility
set.seed(123)

# Number of data points
num_points <- 25

# Generate 25 random positive integers
values <- sample.int(10, num_points, replace = TRUE)

# Generate a sequence of years from 1999 to 2024, and convert them to integers
years <- as.integer(seq(1999, 2024, length.out = num_points))

# Create a data frame
df2 <- data.frame(Year = years, Value = values)



qicharts2::qic(
  x = years, # use the original name of variable, not name in df
  y = values, # use the original name of variable, not name in df
  n = NULL, 
  data = df2,
  # facets = NULL,
  # notes = NULL,
  chart = "i",
  agg.fun = "mean",
  method = "anhoej",
  # multiply = 1,
  # freeze = NULL,
  part = 10, # use this to show where new limits begin
  exclude = 10,
  # target = NA * 1,
  # cl = NA * 1,
  # nrow = NULL,
  # ncol = NULL,
  # scales = "fixed",
  title = "Individuals Chart created with qicharts2",
  ylab = "Measurement",
  xlab = "",
  subtitle = "Subtitle goes here",
  caption = "Caption goes here",
  # part.labels = NULL,
  show.labels = TRUE,
  # show.95 = FALSE,
  decimals = 1,
  point.size = 2.0,
  # x.period = NULL,
  # x.format = NULL,
  x.angle = 45,
  x.pad = 1,
  y.expand = NULL,
  y.neg = FALSE,
  # y.percent = FALSE,
  # y.percent.accuracy = NULL,
  # show.grid = TRUE,
  # flip = FALSE,
  # strip.horizontal = FALSE,
  # print.summary = TRUE,
  # return.data = TRUE
  # 
)

################################


# Specify the year to begin control chart calculations (e.g., 2012)
start_year <- 2012

# Subset the data to start from the specified year
df_subset <- df2[df2$Year >= start_year, ]

# Create a vector of x-axis labels (first and last data points)
x_labels <- c(min(df_subset$Year), max(df_subset$Year))

# Now use qic with the subsetted data
chart <- qicharts2::qic(
  x = df_subset$Year,  # Use the subsetted data starting from 2012
  y = df_subset$Value,  # Use the corresponding values from the subsetted data
  # n = NULL, 
  data = df_subset,
  chart = "i",
  agg.fun = "mean",  # Aggregation function
  method = "anhoej",  # Control chart method (e.g., Anhoej method)
  # part = 10,  # Start new limits at the specified part (can adjust based on your needs)
  # exclude = 10,  # Optional exclusion of points
  title = "Individuals Chart created with qicharts2",
  ylab = "Measurement",
  xlab = "Year",
  subtitle = "Subtitle goes here",
  caption = "Caption goes here",
  show.labels = TRUE,
  decimals = 1,
  point.size = 2.0,
  x.angle = 45,
  x.pad = 1,
  y.expand = NULL,
  y.neg = FALSE
)

# Modify the x-axis to include every year from 2012 to 2024
chart + 
  ggplot2::scale_x_continuous(
    breaks = seq(2012, 2024, by = 1),  # Every year from 2012 to 2024
    labels = seq(2012, 2024, by = 1)   # Labels for every year
  )

