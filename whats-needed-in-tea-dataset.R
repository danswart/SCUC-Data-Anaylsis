##########  EXAMPLE OF DATASET NEEDED FOR SCUC-ISD IMPROVEMENT  ##########

# load libraries

library(readxl)
library(writexl)
library(ggplot2)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(forcats)
library(lubridate)
library(janitor)
library(scales)
library(ggtext)
library(paletteer)
library(viridis)
library(RColorBrewer)
library(wesanderson)
library(dutchmasters)
library(gghighlight)
library(monochromeR)
library(ggforce)


###########  DS STAAR THEMING FUNCTION  ##########
##################################################

ds_staar_theme <- function(base_size = 16,
                           dark_text = "#1A242F") {
  mid_text <-
    monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <-
    monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(
        color = dark_text,
        family = "Trebuchet MS",
        lineheight = 1.3
      ),
      plot.title = ggtext::element_textbox_simple(
        family = "Trebuchet MS",
        size = rel(1.5),
        lineheight = 1.3,
        margin = margin(0, 0, .5, 0, "lines"),
        face = "bold",
        halign = 0,
        color = dark_text
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        family = "Trebuchet MS",
        size = rel(0.9),
        lineheight = 1.1,
        margin = margin(0, 0, 0, 0, "lines"),
        face = "bold",
        halign = 0,
        color = dark_text
      ),
      strip.text = element_text(
        family = "Trebuchet MS",
        face = "bold",
        hjust = 0.03,
        size = rel(1.1),
        margin = margin(0, 0, 0, 0, "lines"),
        color = dark_text
      ),
      axis.text.x = element_markdown(
        color = dark_text,
        size = rel(1.4),
        margin = margin(-1, 0, 0, 0, "lines")
      ),
      axis.text.y = element_markdown(color = dark_text, size = rel(1.0)),
      axis.title.x = element_markdown(color = dark_text, size = rel(1.0)),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      legend.justification = 1,
      panel.grid = element_blank(),
      plot.caption = element_text(
        size = rel(1.0),
        margin = margin(8, 0, 0, 0),
        face = "bold"
      ),
      plot.margin = margin(1.00, 0.25, 1.00, 0.25, "cm")  # this controls the margins of the overall plot, not the space between the parts
    )
}



###############  DATA WRANGLING  ###############

# read in Excel file pre-made for pivoting longer

df <-
  read_excel(
    "data/staar-wide-disagg-2016-2022.xlsx",
    col_types = c(
      "numeric",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  )


# Pivot the data into long format

staar_disagg_wide_to_long <- df %>%
  pivot_longer(cols = c(3:9),
               names_to = "rating",
               values_to = "value")


########  BEGIN EXPERIMENTAL AREA  ##########

# Add 2 new columns

staar_disagg_long_with_grade_and_race <- staar_disagg_wide_to_long %>%
  dplyr::mutate(
    grade_level = factor(rep("all", n())),
    race = factor(rep("all", n()))
  )

# Reorder the columns

staar_disagg_long_with_grade_and_race <- staar_disagg_long_with_grade_and_race %>%
  dplyr::relocate(grade_level, .after = 1) %>% 
  dplyr::relocate(race, .after = 2)

# Save new df as an Excel file

writexl::write_xlsx(staar_disagg_long_with_grade_and_race, "data/staar_disagg_long_with_grade_and_race.xlsx")

# Slice the first 18 rows and create a new, smaller df
first_18_rows_df <- staar_disagg_long_with_grade_and_race %>%
  slice(1:18)

# Create a gt table object from the new df
gt_table <- gt::gt(first_18_rows_df)

# View the table

gt_table

# Save the first 18 rows of the gt_table as a PNG image

gt::gtsave(gt_table,
           "img/snippet-of-what-i-need-in-a-dataframe.png",
           expand = 20
           )


##########  END EXPERIMENTAL AREA  ##########
