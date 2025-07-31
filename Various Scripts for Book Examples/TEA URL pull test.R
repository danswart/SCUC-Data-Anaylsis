library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)


# download excel file
url <- "https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/district-data-download-drop-2122.xlsx"

download.file(url, "2122-dropouts-by-district.xlsx")


# read-in excel file
dropouts_2122_by_district <- read_excel("2122-dropouts-by-district.xlsx", sheet = "District_Dropout_2122")


# add school year column as first column in dataset
dropouts_2122_by_district <- dropouts_2122_by_district %>%
  mutate(YEAR = "21-22") %>%
  select(YEAR, everything())


# retrieve column names for converting columns from character to numeric
column_names <- colnames(dropouts_2122_by_district)



# keep column names to be converted to numeric, exclude columns that should remain as character
filtered_column_names <- column_names[10:length(column_names)]


# convert appropriate columns to numeric
dropouts_2122_by_district <- dropouts_2122_by_district %>%
  mutate_at(vars(filtered_column_names), as.numeric)


# select only Region 20 schools
dropouts_2122_region20 <- subset(dropouts_2122_by_district, REGION == 20)


# select those rows where numbers have been calculated with state accountability criterion
dropouts_2122_region20 <- subset(dropouts_2122_region20, CALC_FOR_STATE_ACCT != "No")


# select grade levels 9 - 12 as cohort
dropouts_2122_region20 <- subset(dropouts_2122_region20, GRADESPAN == "912")


# select only those schools with dropout ratio above zero
dropouts_2122_region20 <- dropouts_2122_region20 %>%
  filter(DIST_ALLR > 0.0)


# reorder to descending order for display on axis
dropouts_2122_region20$DISTNAME <- reorder(dropouts_2122_region20$DISTNAME, -dropouts_2122_region20$DIST_ALLR)



# create the plot
ggplot(data = dropouts_2122_region20,
       aes(x = DIST_ALLR,
           y = DISTNAME,
           fill = DIST_ALLR <= 1.0
       )
) +
  geom_col(color = "black", fill = "#0072B2", linewidth = 0.5) +
  geom_text(aes(label = paste0("(", DIST_ALLR, ")")), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = percent_format(scale = 1)) +  # Format x-axis as percentages
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "darkgrey")) +
  labs(x = "District Overall Dropout Rate (%)", y = "District Name") +
  ggtitle("Dropouts by District in Region 20 for Grade Span 9-12") +
  ylab("") +
  guides(fill = "none")






##### WORK ON THIS LATER ##########

# CREATE A TOOLTIP COLUMN IN R

# bar_graph_data_recent <- bar_graph_data_recent %>%
#   mutate(
#     tooltip_text = paste0(toupper(State), "\n",
#                           PctFullyVaccinated, "%")
#   )


# Make the bar chart interactive with ggiraph

# latest_vax_graph <- ggplot(bar_graph_data_recent,
#                            aes(x = reorder(State, PctFullyVaccinated),
#                                y = PctFullyVaccinated,
#                                tooltip = tooltip_text, data_id = State #<<
#                            )) +
#   geom_col_interactive(color = "black", fill="#0072B2", size = 0.5) +  #<<
#   theme_minimal() +
#   theme(axis.text=element_text(size = 6)) +  #<<
#   labs(title = "Percent Fully Vaccinated July 2021",
#        subtitle = "Data from Our World in Data GitHub repo"
#   ) +
#   ylab("") +
#   xlab("") +
#   coord_flip()
#
# girafe(ggobj = latest_vax_graph, width_svg = 5, height_svg = 4)


