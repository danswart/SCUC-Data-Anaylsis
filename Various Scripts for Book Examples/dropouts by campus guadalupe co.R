library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(ggiraph)


# download excel file
url <- "https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/campus-data-download-drop-2122.xlsx"

download.file(url, "2122-dropouts-by-campus.xlsx")


# read-in excel file
dropouts_2122_by_campus <- read_excel("2122-dropouts-by-campus.xlsx", sheet = "Campus_Dropout_2122")


# add school year column as first column in dataset
dropouts_2122_by_campus <- dropouts_2122_by_campus %>%
  mutate(YEAR = "21-22") %>%
  select(YEAR, everything())


# retrieve column names for converting columns from character to numeric
column_names <- colnames(dropouts_2122_by_campus)


# keep column names to be converted to numeric, exclude columns that should remain as character
filtered_column_names <- column_names[10:length(column_names)]


# convert appropriate columns to numeric
dropouts_2122_by_campus <- dropouts_2122_by_campus %>%
  mutate_at(vars(filtered_column_names), as.numeric)


# select only Guadalupe County schools
dropouts_2122_by_campus_guadalupe <- subset(dropouts_2122_by_campus, COUNTY == "094")


# select those rows where numbers have been calculated with state accountability criterion
dropouts_2122_by_campus_guadalupe <- subset(dropouts_2122_by_campus_guadalupe, CALC_FOR_STATE_ACCT != "No")


# select grade levels 9 - 12 as cohort
dropouts_2122_by_campus_guadalupe <- subset(dropouts_2122_by_campus_guadalupe, GRADESPAN == "912")


# select only those schools with dropout ratio between zero and 10
dropouts_2122_by_campus_guadalupe <- dropouts_2122_by_campus_guadalupe %>%
  filter(CAMP_ALLR > 0.0, CAMP_ALLR < 10)


# reorder to descending order for display on axis
dropouts_2122_by_campus_guadalupe$CAMPNAME <- reorder(dropouts_2122_by_campus_guadalupe$CAMPNAME, -dropouts_2122_by_campus_guadalupe$CAMP_ALLR)



# create a tooltip column for ggiraph to use
dropouts_2122_by_campus_guadalupe <- dropouts_2122_by_campus_guadalupe %>%
  mutate(
    tooltip_text = paste0(toupper(CAMPNAME), "\n",
                          CAMP_ALLR, "%")
  )




# create and view the plot

p <- ggplot(data = dropouts_2122_by_campus_guadalupe,
       aes(x = CAMP_ALLR,
           y = CAMPNAME,
           tooltip = tooltip_text, data_id = CAMPNAME,
       )
) +
  geom_col_interactive(color = "black", aes(fill = ifelse(DISTRICT == "094902", "green", "#0072B2")), linewidth = 0.5) +
  geom_text(aes(label = paste0("(", CAMP_ALLR, ")")), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  scale_fill_manual(values = c("green" = "green", "#0072B2" = "#0072B2")) +
  theme(axis.text = element_text(size = 10)) +
  labs(x = "Campus Overall Dropout Rate (%)", y = "Campus Name") +
  ggtitle("Dropout Rates by Campus in Guadalupe\nCounty for Grade Span 9-12") +
  ylab("") +
  guides(fill = "none")


# view the static plot
p




# activate ggiraph optons if you want interactivity with the cursor
pg <- girafe(ggobj = p)
pg <- girafe_options(x = pg,
                     opts_tooltip(offx = 50)  # Set to a positive value to move the tooltip to the right of the cursor
)


# view the interactive plot
pg




