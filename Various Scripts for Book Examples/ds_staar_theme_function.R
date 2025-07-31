#########################################################
###  CREATE A THEMING FUNCTION FOR STAAR SCORE PLOTS  ###
#########################################################


#####  See https://www.cararthompson.com/posts/2021-09-02-alignment-cheat-sheet/alignment-cheat-sheet#so-what-does-what #####
#####  For Alignment Cheat Sheet for v/h adjust and v/h align  #####



# load libraries

library(readxl) # read_excel
library(writexl) # write_xlsx
library(ggplot2) # ggplot aes geom_col position_dodge2 labs scale_color_manual scale_y_continuous theme element_blank element_text coord_cartesian ggtitle geom_hline geom_line
library(tibble) # %>% glimpse
library(tidyr) # %>% pivot_wider pivot_longer
library(readr) # No used functions found
library(purrr) # %>%
library(dplyr) # %>% filter mutate glimpse group_by summarise bind_rows arrange
library(stringr) # %>%
library(forcats) # %>%
library(lubridate) # No used functions found
library(janitor) # %>% clean_names
library(scales) # percent_format
library(ggtext) # geom_richtext
library(paletteer) # scale_fill_paletteer_d
library(viridis) # No used functions found
library(RColorBrewer) # No used functions found
library(wesanderson) # No used functions found
library(dutchmasters) # No used functions found
library(gghighlight)
library(monochromeR)


###########  DS STAAR THEMING FUNCTION  ##########

ds_staar_theme <- function(base_size = 18,
                           dark_text = "#1A242F") {
  mid_text <-
    monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <-
    monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(
        color = dark_text,
        family = "Roboto",
        lineheight = 1.3
      ),
      plot.title = ggtext::element_textbox_simple(
        family = "Roboto",
        size = rel(1.5),
        lineheight = 1.3,
        margin = margin(0, 0, .5, 0, "lines"),
        face = "bold",
        halign = 0,
        color = dark_text
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        family = "Roboto",
        size = rel(0.9),
        lineheight = 1.1,
        margin = margin(0, 0, 0, 0, "lines"),
        face = "bold",
        halign = 0,
        color = dark_text
      ),
      strip.text = element_text(
        family = "Roboto",
        face = "bold",
        hjust = 0.03,
        size = rel(1.1),
        margin = margin(0, 0, 0, 0, "lines"),
        color = dark_text
      ),
      axis.text.x = element_markdown(color = dark_text, size = rel(1.4), margin = margin(-1, 0, 0, 0, "lines")),
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

