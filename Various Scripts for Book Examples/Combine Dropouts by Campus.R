# TX DROPOUT DATA BY CAMPUS 2002-2003 TO 2021-2022


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


# download all excel files

https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/campus-data-download-drop-2122.xlsx


https://tea.texas.gov/sites/default/files/campus-data-download-drop-2021.xlsx


https://tea.texas.gov/sites/default/files/campus-data-download-drop-1920.xlsx


https://tea.texas.gov/sites/default/files/Campus_Data_Download_Drop_1819_v2.xlsx


https://tea.texas.gov/sites/default/files/Campus_Data_Download_Drop_1718.xlsx


https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/campusdatadownloaddrop1617.xlsx


https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/campusdatadownloaddrop2016.xlsx


https://tea.texas.gov/acctres/drop_annual/1415/Campus_Data_Download_Drop


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_1314_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_1213_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_1112_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_1011_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0910_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0809_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0708_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0607_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0506_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0405_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0304_campdown_csv.sas


https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=acctres.drp_0203_campdown_csv.sas












url1 <- "https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/campus-data-download-drop-2122.xlsx"

download.file(url1, "2122-dropouts-by-district.xlsx")

url2 <- "https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/district-data-download-drop-2021.xlsx"

download.file(url2, "2021-dropouts-by-district.xlsx")

url3 <- "https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout/district-data-download-drop-1920.xlsx"

download.file(url3, "1920-dropouts-by-district.xlsx")






# etc etc etc for all available school years


##########
# ADD A NEW 'YEAR' COLUMN 1 IN EACH EXCEL WORKBOOK BEFORE READING THEM INTO THE NEW DATASET.  THE DATA IS OF TYPE 'CHARACTER'
##########


# Create a list of file paths for the 10 Excel files. Assuming the files are named "Year1.xlsx," "Year2.xlsx," ..., "Year10.xlsx" and are located in the same directory as your R script or notebook:


file_paths <- list.files(pattern = "Year[1-9]|Year10\\.xlsx")



# Read and combine the data from all 10 Excel files into one dataframe.
# In this step, we use map_df from the purrr package to read each Excel file and bind them into a single dataframe. The .id argument specifies a column to indicate the year of each observation.

combined_data_yrs_2013_to_2022 <- file_paths %>%
  purrr::map_df(~ readxl::read_excel(.x,
                                     sheet = 1),
                .id = "Year")


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








