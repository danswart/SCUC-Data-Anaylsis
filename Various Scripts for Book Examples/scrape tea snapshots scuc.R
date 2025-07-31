library(rvest)
library(dplyr)
library(tidyr)


# Specify the URL for the first webpage
url2018 <- "https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=perfrept.perfmast.sas&_debug=0&ccyy=2018&lev=D&id=094902&prgopt=reports%2Fsnapshot%2Fsnapshot.sas"

# url2019 <- https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=perfrept.perfmast.sas&_debug=0&ccyy=2019&lev=D&id=094902&prgopt=reports%2Fsnapshot%2Fsnapshot.sas


# url2020 <- https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=perfrept.perfmast.sas&_debug=0&ccyy=2020&lev=D&id=094902&prgopt=reports%2Fsnapshot%2Fsnapshot.sas

# url2021 <- https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=perfrept.perfmast.sas&_debug=0&ccyy=2021&lev=D&id=094902&prgopt=reports%2Fsnapshot%2Fsnapshot.sas

# url2022 <- https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=perfrept.perfmast.sas&_debug=0&ccyy=2022&lev=D&id=094902&prgopt=reports%2Fsnapshot%2Fsnapshot.sas




# Read the HTML content of the webpage
webpage <- read_html(url2018)

# Extract the data you need from the HTML using CSS selectors
data <- webpage %>%
  html_table(fill = TRUE)


# Convert all columns to character type to avoid data type mismatch
data_cleaned <- lapply(data, function(tibble) {
  tibble %>%
    mutate(across(everything(), as.character))  # Convert all columns to character
})

# Flatten the list of tibbles into a single tibble (data frame)
data_combined <- bind_rows(data_cleaned)

# Ensure consistent column names (if needed)
colnames(data_combined) <- make.names(colnames(data_combined))

# Write the combined tibble to a text file
write.table(data_combined, file = "TEST_combined_snapshot_data.txt", sep = "\t", quote = FALSE, row.names = FALSE)



##### DUMP INTO A DATAFRAME FOR PLOTTING #####
##### PLOT IS NONSENSICLE BUT NO TIME SPENT ON CREATING A PLOT #####


# Load the required libraries
library(ggplot2)

# Step 1: Read the .txt file into a data frame
data_from_txt <- read.table("TEST_combined_snapshot_data.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)

# Step 2: Inspect the first few rows to understand the structure of the data
head(data_from_txt)

# Step 3: If necessary, clean the data (e.g., converting columns to appropriate types, handling NAs, etc.)
# For example, let's assume you need to convert a column to numeric
# data_from_txt$some_column <- as.numeric(data_from_txt$some_column)

# Step 4: Plot the data using ggplot2
# Here's a simple example assuming you have x and y columns in the data
ggplot(data_from_txt, aes(x = X1, y = X2)) +  # Replace X1 and X2 with actual column names
  geom_point() +
  theme_minimal() +
  labs(title = "Plot from .txt data", x = "X Axis", y = "Y Axis")

