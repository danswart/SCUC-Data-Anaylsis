library(readxl) # read_excel
library(dplyr) # %>% mutate select everything mutate_at vars filter
library(ggplot2) # vars ggplot aes geom_text scale_x_continuous scale_fill_manual theme element_text labs ggtitle ylab guides
library(scales) # percent_format
library(plotly) # %>% mutate select filter
library(ggiraph) # geom_col_interactive girafe girafe_options opts_tooltip


# download excel file
url <- "https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/district-data-download-drop-2122.xlsx"

download.file(url, "2122-dropouts-by-district.xlsx")


# read-in excel file
dropouts_2122_by_district <- readxl::read_excel("2122-dropouts-by-district.xlsx", sheet = "District_Dropout_2122")


# add school year column as first column in dataset
dropouts_2122_by_district <- dropouts_2122_by_district %>%
  dplyr::mutate(YEAR = "21-22") %>%
  dplyr::select(YEAR,
                dplyr::everything())


# retrieve column names for converting columns from character to numeric
column_names <- colnames(dropouts_2122_by_district)



# keep column names to be converted to numeric, exclude columns that should remain as character
filtered_column_names <- column_names[10:length(column_names)]


# convert appropriate columns to numeric
dropouts_2122_by_district <- dropouts_2122_by_district %>%
  deplyr::mutate_at(
    deplyr::vars(filtered_column_names),
    as.numeric)


# select only Region 20 schools
dropouts_2122_region20 <- subset(dropouts_2122_by_district, REGION == 20)


# select those rows where numbers have been calculated with state accountability criterion
dropouts_2122_region20 <- subset(dropouts_2122_region20, CALC_FOR_STATE_ACCT != "No")


# select grade levels 9 - 12 as cohort
dropouts_2122_region20 <- subset(dropouts_2122_region20, GRADESPAN == "912")


# select only those schools with dropout ratio between zero and 10
dropouts_2122_region20 <- dropouts_2122_region20 %>%
  dplyr::filter(DIST_ALLR > 0.0, DIST_ALLR < 10)



# reorder to descending order for display on axis
dropouts_2122_region20$DISTNAME <- reorder(dropouts_2122_region20$DISTNAME, -dropouts_2122_region20$DIST_ALLR)


# create a tooltip column for ggiraph to use
dropouts_2122_region20 <- dropouts_2122_region20 %>%
  deplyr::mutate(
    tooltip_text = paste0(toupper(DISTNAME), "\n",
                          DIST_ALLR, "%")
  )



# Make the bar chart with ggiraph geoms and tooltip 

p <- ggplot2::ggplot(data = dropouts_2122_region20,
            ggplot2::aes(x = DIST_ALLR,
                y = DISTNAME,
                tooltip = tooltip_text,
                data_id = DISTNAME
                )
) +
  
  ggiraph::geom_col_interactive(color = "black",
                                gglot2::aes(fill = ifelse(DISTRICT == "094902",
                                                          "green",
                                                          "#0072B2")),
                                linewidth = 0.5) +
  
  ggplot2::geom_text(
    ggplot2::aes(label = paste0("(", DIST_ALLR, ")")),
    hjust = -0.1,
    size = 2) +
  
  ggplot2::scale_x_continuous(labels = percent_format(scale = 1)) +  # Format x-axis as percentages
  
  ggplot2::scale_fill_manual(values = c("green" = "green",
                                        "#0072B2" = "#0072B2")
                             ) +
  
  ggplot2::theme(axis.text = element_text(size = 6)
                 ) +
  
  ggplot2::labs(x = "District Overall Dropout Rate (%)",
                y = "District Name") +
  
  ggplot2::ggtitle("Dropout Rates by District in Region 20\nfor Grade Span 9-12") +
  
  ggplot2::ylab("") +
  
  ggplot2::guides(fill = "none")


# view static plot
p



  # activate ggiraph optons if you want interactivity with the cursor
pg <- ggiraph::girafe(ggobj = p)
pg <- ggiraph::ggirafe_options(x = pg,
                       ggiraph::opts_tooltip(offx = 50)  # Set to a positive value to move the tooltip to the right of the cursor
                     )


# view the interactive plot
pg

