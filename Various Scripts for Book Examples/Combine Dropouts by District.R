# TX DROPOUT DATA BY DISTRICT 2002-2003 TO 2021-2022


#####  See https://www.cararthompson.com/posts/2021-09-02-alignment-cheat-sheet/alignment-cheat-sheet#so-what-does-what #####
#####  For Alignment Cheat Sheet for v/h adjust and v/h align  #####


# load libraries

library(purrr) # %>% map_df 
library(readxl) # read_excel 
library(dplyr) # %>% filter 
library(ggplot2) # ggplot aes geom_text scale_x_continuous scale_fill_manual theme element_text labs ggtitle ylab guides
library(scales) # percent_format 
library(plotly) # %>% filter 
library(ggiraph) # geom_col_interactive girafe girafe_options opts_tooltip 
library(writexl)


# download all excel files


https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/district-data-download-drop-2122.xlsx


https://tea.texas.gov/sites/default/files/district-data-download-drop-2021.xlsx


https://tea.texas.gov/sites/default/files/district-data-download-drop-1920.xlsx


https://tea.texas.gov/sites/default/files/District_Data_Download_Drop_1819_v2.xlsx


https://tea.texas.gov/sites/default/files/District_Data_Download_Drop_1718.xlsx


https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/districtdatadownloaddrop1617.xlsx


https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/districtdatadownloaddrop2016.xlsx


https://tea.texas.gov/acctres/drop_annual/1415/District_Data_Download_Drop


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_1314_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_1213_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_1112_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_1011_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0910_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0809_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0708_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0607_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0506_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0405_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0304_distdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0203_distdown_csv.sas









url2122 <- "2122-dropouts-by-district.xlsx"

# download.file(url2122, "2122-dropouts-by-district.xlsx")

url2021 <- "2021-dropouts-by-district.xlsx"

# download.file(url2021, "2021-dropouts-by-district.xlsx")

url1920 <- "1920-dropouts-by-district.xlsx"

# download.file(url1920, "1920-dropouts-by-district.xlsx")

# read in xlsx files and assign object names
dropouts_2122_by_district <- readxl::read_xlsx(url2122, sheet = 3)
dropouts_2021_by_district <- readxl::read_xlsx("dropouts_2021_by_district-rev1.xlsx", sheet = 1)
dropouts_1920_by_district <- readxl::read_xlsx(url1920, sheet = 3)

# etc etc etc for all available school years

##### examine structures for consistency and remove unwanted rows or columns

# To reorder the columns in dataset2 to match the order of columns in dataset1 in R, you can use the following approach:  This code snippet reorders the columns in dataset2 to match the order of columns in dataset1. It uses the column names of dataset1 to subset dataset2, resulting in dataset2 having the same column order as dataset11.  This method ensures that the columns in both datasets are aligned in the same order, facilitating easier comparison and analysis.


dropouts_2021_by_district_test <- dropouts_2021_by_district[, colnames(dropouts_2122_by_district)]



# to check if two datasets have the exact same column names in R, you can use the following approaches:

# Using the all.equal() function:  This will return TRUE if the column names of both data frames are exactly the same, and FALSE otherwise.

all.equal(names(dropouts_2122_by_district), names(dropouts_2021_by_district))


# Using the identical() function:  This will also return TRUE if the column names of both data frames are exactly the same, and FALSE otherwise.

identical(names(dropouts_2122_by_district), names(dropouts_2021_by_district))



# Since the df's have unequal columns

# Assuming dropouts_2122_by_district is the reference dataframe
# and dropouts_2021_by_district is the dataframe you want to add columns to

missing_columns <- setdiff(names(dropouts_2122_by_district), names(dropouts_2021_by_district))

for (col_name in missing_columns) {
  dropouts_2021_by_district[[col_name]] <- NA  # Populate with "NA" for missing data
}


# write back df for further work in excel

write_xlsx(dropouts_2021_by_district, "dropouts_2021_by_district-rev1.xlsx")


##########
# ADD A NEW 'YEAR' COLUMN 1 IN EACH EXCEL WORKBOOK BEFORE READING THEM INTO THE NEW DATASET.  THE DATA IS OF TYPE 'CHARACTER'
##########


# Create a list of file paths for the 10 Excel files. Assuming the files are named "Year1.xlsx," "Year2.xlsx," ..., "Year10.xlsx" and are located in the same directory as your R script or notebook:


# file_paths <- list.files(pattern = "Year[1-9]|Year10\\.xlsx")

file_paths <- list.files("2122-dropouts-by-district.xlsx", "2021-dropouts-by-district.xlsx", "1920-dropouts-by-district.xlsx")

# Read and combine the data from all 10 Excel files into one dataframe.
# In this step, we use map_df from the purrr package to read each Excel file and bind them into a single dataframe. The .id argument specifies a column to indicate the year of each observation.

combined_data_yrs_2013_to_2022 <- file_paths %>%
  purrr::map_df(~ readxl::read_excel(.x,
                                     sheet = 3),
                .id = "YEAR")


# Now you have a combined dataset with information for all 10 years. You can filter, manipulate, and plot the data as needed. For example, to plot Measurement A, B, or C for a specific school district (let's assume "School District 1") over the 10-year period:


# Filter the data for "School District 1"
filtered_data <- combined_data %>%
  dplyr::filter(`School District` == "School District 1")


# Plot Measurement A
p1 <- ggplot2::ggplot(data = filtered_data,
                ggplot2::aes(x = YEAR,
                             y = `Measurement A`),
                tooltip = tooltip_text,
                data_id = DISTNAME
                ) +
  
  ggiraph::geom_col_interactive(color = "black",
                                ggplot2::aes(fill = ifelse(DISTRICT == "094902",
                                                           "green",
                                                           "#0072B2")
                                             ),
                                linewidth = 0.5) +
  
  ggplot2::geom_text(
    ggplot2::aes(label = paste0("(", DIST_ALLR, ")")),
    hjust = -0.1,
    size = 2) +
  
  ggplot2::scale_x_continuous(labels = scales::percent_format(scale = 1)) +  
  
  ggplot2::scale_fill_manual(values = c("green" = "green",
                                        "#0072B2" = "#0072B2")
                             ) +
  
  ggplot2::theme(axis.text = element_text(size = 6)) +
  
  ggplot2::labs(x = "District Measurement A",
                y = "District Name") +
  
  ggplot2::ggtitle("District Measurement A by District in Region 20\nfor Grade Span 9-12") +
  
  ggplot2::ylab("") +
  
  ggplot2::guides(fill = "none")


# view static plot
p1


# # activate ggiraph optons if you want interactivity with the cursor
# pg1 <- girafe(ggobj = p1)
# pg1 <- girafe_options(x = pg1,
#                      opts_tooltip(offx = 50)  # Set to a positive value to move the tooltip to the right of the cursor
# )
# 
# 
# # view the interactive plot
# pg1



# Plot Measurement B
p2 <- ggplot2::ggplot(data = filtered_data,
                      ggplot2::aes(x = YEAR,
                                   y = `Measurement B`),
                      tooltip = tooltip_text,
                      data_id = DISTNAME
) +
  
  ggiraph::geom_col_interactive(color = "black",
                                ggplot2::aes(fill = ifelse(DISTRICT == "094902",
                                                           "green",
                                                           "#0072B2")
                                ),
                                linewidth = 0.5) +
  
  ggplot2::geom_text(
    ggplot2::aes(label = paste0("(", DIST_ALLR, ")")),
    hjust = -0.1,
    size = 2) +
  
  ggplot2::scale_x_continuous(labels = scales::percent_format(scale = 1)) +  
  
  ggplot2::scale_fill_manual(values = c("green" = "green",
                                        "#0072B2" = "#0072B2")
  ) +
  
  ggplot2::theme(axis.text = element_text(size = 6)) +
  
  ggplot2::labs(x = "District Measurement B",
                y = "District Name") +
  
  ggplot2::ggtitle("District Measurement B by District in Region 20\nfor Grade Span 9-12") +
  
  ggplot2::ylab("") +
  
  ggplot2::guides(fill = "none")


# view static plot
p2


# # activate ggiraph optons if you want interactivity with the cursor
# pg2 <- girafe(ggobj = p2)
# pg2 <- girafe_options(x = pg2,
#                      opts_tooltip(offx = 50)  # Set to a positive value to move the tooltip to the right of the cursor
# )
# 
# 
# # view the interactive plot
# pg2



# etc etc etc








