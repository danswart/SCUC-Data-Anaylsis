############  4 DISAGG LEVELS, ONE-SUBJECT, BY YEAR PLOT  ############
############   WITH HORIZONTAL BAR LABELS FOR YEAR-ENDS   ############
######################################################################

#####  STORY:  BAR CHARTS IN TIME SEREIES CANNOT DISTINGUISH 'SPECIAL #####
####          CAUSE' VARIATION FROM 'COMMON CAUSE' VARIATION          #####

#####  See https://www.cararthompson.com/posts/2021-09-02-alignment-cheat-sheet/alignment-cheat-sheet#so-what-does-what #####
#####  For Alignment Cheat Sheet for v/h adjust and v/h align  #####


#####  USE gghighlight TO HIGHLIGHT ONE, OR MORE, ITEMS OF INTEREST  #####
#####   See below in plot code:  gghighlight(subject == "all_subj")  #####
#####                   Use 'calculate_per_facet' and                #####
#####                 'label_key = category for labels'              #####
##########################################################################


#####  Consider using the geomtextpath() package for text you want   #####
#####   to follow a curved path, or even just a straight path.  It   #####
#####    permits labels and text that can be treated as richtext.    #####
#####        see https://allancameron.github.io/geomtextpath/        #####
##########################################################################

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
    "../data/staar-wide-disagg-2016-2022.xlsx",
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



# Convert RATING column to a factor for proper ordering on the x axis

staar_disagg_wide_to_long$rating <-
  factor(staar_disagg_wide_to_long$rating,
         levels = unique(staar_disagg_wide_to_long$rating))


# Filter out rows with NA values in the 'value' column

staar_disagg_wide_to_long <- staar_disagg_wide_to_long %>%
  filter(!is.na(value))


# Establish range of text colors

dark_text <- "#1A242F"
mid_text <-
  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
light_text <-
  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]


# Setup custom labels for TEA RATINGS levels and assign them colors matching the fill color

custom_labels <-
  c(
    "**Approaches Standard**",
    "**Meets Standard**",
    "**Masters Standard**",
    "**Failing**"
  )

label_colors <- c(
  "Approaches Standard" = "#94B6D2FF",
  "Meets Standard" = "#DD8047FF",
  "Masters Standard" = "#A5AB81FF",
  "Failing" = "#D8B25CFF"
)

# Setup data frame for placing the chosen RATING levels above their respective bar grouping

labels_data <- data.frame(
  # Adjust the x positions of labels as needed
  x = 1:4,
  # Adjust the y positions as needed
  y = rep(40, 4),
  # Select the first 4 labels
  label = custom_labels[1:4],
  # Map colors to labels
  label_color = label_colors[c('Approaches Standard', 'Meets Standard', 'Masters Standard', 'Failing')] 
)


# Setup data frame for placing 4 ellipses around 4 RATING level names above their respective bar grouping

e <- data.frame(
  x = c(1, 2, 3, 4),  # define x axis values
  y = c(40, 40, 40, 40), # define y axis values
  a = c(0.3, 0.3, 0.3, 0.3),  # define ellipses width
  b = c(3, 3, 3, 3),  # define ellipses height
  angle = c(0, 0, 0, 0)  # always set to zero
)



# Possible alternative palette

ds_palette_scuc <- c("#9F248FFF",
                     "#FFCE4EFF",
                     "#017A4AFF",
                     "#F9791EFF",
                     "#244579FF",
                     "#C6242DFF")


# Possible alternative palette, desaturated

ds_palette_scuc_desat <- c("#7F589CFF",
                           "#D9994DFF",
                           "#00524BFF",
                           "#C54C0EFF",
                           "#192E46FF",
                           "#972019FF")


# Possible alternative palette, lightened

ds_palette_scuc_light <- c("#C858D6FF",
                           "#D9994DFF",
                           "#33A3E6FF",
                           "#FFB481FF",
                           "#6194C5FF",
                           "#E5454CFF")



# These functions format all geoms that use text, label, and label_repel to use the Trebuchet MS font. Those layers are *not* influenced by whatever you include in the base_family argument in something like theme_minimal(), so ordinarily you'd need to specify the font in each individual annotate(geom = "text") layer or geom_label() layer, and that's tedious! This removes that tediousness.


# update_geom_defaults("label_repel", list(family = "Trebuchet MS"))

update_geom_defaults("text", list(family = "Trebuchet MS"))
update_geom_defaults("label", list(family = "Trebuchet MS"))



# Build the plot pipeline

staar_2016_2022_disagg_math_all_ratings <-
  staar_disagg_wide_to_long %>%
  
  # Choose RATING levels and subjects in plot
  
  filter(rating %in% c('approaches_only',
                       'meets_only',
                       'masters_only',
                       'failing') &
           subject %in% c('math')) %>%
  
  
#####  WATCH OUT FOR THIS.  R WILL ALWAYS PUT  #####
#####  COLUMNS (VARIABLES) IN ALPHA ORDER.  IF #####
#####   YOU WANT A DIFFERENT ORDER YOU MUST    #####
#####          SPECIFY IT IN YOUR CODE         #####
#####  ALWAYS VERIFY AND MATCH DATA TO LABELS! #####

# Set order of the rating column for desired presentation order in plot

mutate(rating = factor(rating, levels = c('approaches_only', 'meets_only', 'masters_only', 'failing'))) %>% 
  
  
  # add year_end to aes call to create bar for each year
  
  ggplot(aes(
    x = rating,
    y = value * 100,
    fill = rating,
    group = year_end
  )) + 
  
  geom_col(position = position_dodge2(0.9)) +
  
  # USE gghighlight TO HIGHLIGHT ONE, OR MORE, RATINGS
  
  gghighlight(rating %in% c('masters_only', 'failing')) +
  
  
  # Set gridlines for debugging chart; will remove later
  
  theme(panel.grid.major = element_line(
    color = dark_text,
    linetype = 3,
    linewidth = 0.5
  )) +
  
  
  # Add y axis title, remove x axis title and legend title
  
  labs(x = "", y = "Proportion Reaching This Level", fill = "") +
  
  # Use user-defined colors
  
  scale_color_manual(values = label_colors) +
  
  # Set x axis to discrete
  
  scale_x_discrete() +
  
  # Format y axis as percentage
  
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    breaks = seq(0, 100, by = 5),
    minor_breaks = NULL
  ) +
  
  # Theme-out text, titles, and ticks on both axis
  
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(hjust = 1, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  
  # Remove legend
  
  theme(legend.position = "none") +
  
  # set y axis range
  
  coord_cartesian(ylim = c(0, 50)) +
  
  # Add plot titles
  
  ggtitle(
    "STAAR Achievement Levels by Year for Subject = 'Math'"
  ) +
  
  labs(subtitle = "<span style = 'color: firebrick'>Bar Charts in Time Series Cannot Distinguish 'Special Cause' Variation from 'Common Cause' Variation</span>") +
  
  labs(caption = "Missing Year = No STAAR Score Available") +
  
  
  # Set colored horizontal line for bars to sit on
  
  geom_hline(yintercept = 0,
             color = "orange",
             linewidth = 0.75) +
  
  # Specify color palette
  
  paletteer::scale_fill_paletteer_d("ggthemes::excel_Median") +
  
  # Put the RATING labels over the groupings of bars.  Use the labels_data data frame and geom_richtext().  Text color can be matched to fill color, but is not in this template
  
  geom_richtext(
    aes(
      x = x,
      y = y,
      label = label,
      label.size = 16,
      size = 20,
      label.color = "white",
      fontface = "bold"
    ),
    data = labels_data,  # Use the labels_data data frame to establish 'label' variable
    inherit.aes = FALSE,
    hjust = 0.5
  ) +
  
  # use this to allow for specific color coding below
  
  scale_color_identity() +
  
  #####    IT IS ABSOLUTELY CRITICAL TO SET THESE CONDITIONAL   #####
#####   FORMATTING PARAMETERS TO THE PROPER LENGTHS FOR THE   #####
#####  BARS TO GET THE SCORE VALUES AND YEAR LABELS IN THEIR  #####
#####   PROPER PLACES INSIDE/OUTSIDE AND ON TOP OF THE BARS   #####

# Place STAAR score at the top/end of each bar

geom_text(
  aes(
    # Format values as percentages
    label = scales::percent(value, scale = 100),
    
    # If bars are vertical, set score value inside bar unless bar value < 10%, then place on outside
    vjust = case_when(value < .10 ~ 0,
                      TRUE ~ 2),
    
    # valign = case_when(value < .30 ~ 0,
    #                   TRUE ~ .5),
    
    # If bars are horizontal, set score value inside bar unless bar value < 10%, then place on outside
    hjust = case_when(value < .10 ~ 0,
                      TRUE ~ .5),
    
    
    # halign = case_when(value < .30 ~ 0,
    #                   TRUE ~ .5),
    
    # If label is set outside bar set text color to black, else white
    color = case_when(value > .10 ~ "black",
                      TRUE ~ "white")
  ), # end of aes call
  position = position_dodge2(0.9),
  fontface = "bold",
  family = "Trebuchet MS",
  # color = dark_text, # this argument will override conditional coloring above
  size = 4  # Adjust the font size as needed
) +
  
  
  # Place year labels (2016-2022) over the bars with adjusted size
  
  geom_text(
    aes(label = year_end),
    position = position_dodge2(0.9),
    vjust = -0.5,
    fontface = "bold",
    family = "Trebuchet MS",
    color = dark_text,
    size = 3  # Adjust the font size as needed
  ) +
  
  
  # Place explanatory text on plot
  
  annotate(
    "text",
    # Specify the value/category on the x-axis
    x = "masters_only",
    
    # Specify the value on the y-axis
    y = 45,
    
    label = "(Visually, These Changes Still Look Unsettling)",
    size = 5,
    fontface = "bold",
    hjust = 0.0,
    color = "firebrick"
  ) +
  
  
  # Add an ellipse with a specific color, size, and angle over x axis categories
  
  geom_ellipse(
    aes(
      x0 = x,
      y0 = y,
      a = a,
      b = b,
      angle = angle,
      label = NULL,
      # Remove the label mapping for this layer
      fill = NULL,
      # Remove the fill mapping for this layer
      y = NULL,
      # Remove the y mapping for this layer
      x = NULL,
      # Remove the x mapping for this layer
      year_end = NULL  # Remove the 'year_end' mapping
    ),
    inherit.aes = FALSE,
    data = e,
    color = "blue",
    fill = "lightblue",
    alpha = 0.0
  ) 

# view the plot

staar_2016_2022_disagg_math_all_ratings +
  ds_staar_theme() +
  theme(axis.text.x = element_blank())


# view the palette

paletteer::paletteer_d("ggthemes::excel_Median")


# Generally best to save plot as image from the Plots tab in RStudio.
# However, if needed, you can save the plot as an image file to get
# larger dimensions with ggsave call.

# ggsave("img/staar-2016-2022-disagg-math-all-ratings-annot.png",
#        plot = staar_2016_2022_disagg_math_all_ratings,
#        width = 12,
#        height = 12,
#        units = "in",
#        dpi = 300,
#        device = "png",
#        bg = "transparent")









#####  THE FOLLOWING CODE IS FROM A PRESENTATION BY CARA THOMPSON #####
#####  AND HAS NOTHING TO DO WITH SCUC DATA ANALYSIS.  IT IS A TEMPLATE   #####


# add a labelled smooth line for each suppliment

# geomtextpath::geom_textline(
#   stat = "smooth",
#   aes(label = supplement),
#   hjust = 0.1,
#   vjust = 0.3,
#   fontface = "bold",
#   family = "Cabin"
# ) +
#   
#   # color the lines by specified palette
#   
  # scale_colour_manual(values = vit_c_palette) +
  # 
  # gghighlight::gghighlight(supp == "OJ") +
#   
  # add text boxes with name and data inside for max and min color-coded for each suppliment

  # ggtext::geom_textbox(
  #   data = filter(min_max_gps,
  #                 dose == 2),
  #   aes(
  #     x = case_when(dose < 1.5 ~ dose + 0.05,
  #                   TRUE ~ dose - 0.05),
  #     y = case_when(min_or_max  == "max" ~ len * 1.1,
  #                   TRUE ~ len * 0.9),
  #     label = paste0(
  #       "**<span style='font-family:Enriqueta'>",
  #       guinea_pig_name,
  #       "</span>** - ",
  #       len,
  #       " mm"
  #     ),
  #     hjust = case_when(dose < 1.5 ~ 0,
  #                       TRUE ~ 1),
  #     halign = case_when(dose < 1.5 ~ 0,
  #                        TRUE ~ 1)
  #   ),
  #   family = "Cabin",
  #   size = 4,
  #   fill = NA,
  #   box.colour = NA
  # ) +
  # 
  # # add the arrows from text boxes to data point
  # 
  # ggplot2::geom_curve(
  #   data = filter(min_max_gps,
  #                 dose == 2),
  #   aes(
  #     x = case_when(dose < 1.5 ~ dose + 0.05,
  #                   TRUE ~ dose - 0.05),
  #     y = case_when(min_or_max  == "max" ~ len * 1.1,
  #                   TRUE ~ len * 0.9),
  #     xend = case_when(dose < 1.5 ~ dose + 0.02,
  #                      TRUE ~ dose - 0.02),
  #     yend = case_when(min_or_max  == "max" ~ len + 0.5,
  #                      TRUE ~ len - 0.5)
  #   ),
  #   curvature = 0,
  #   arrow = arrow(length = unit(0.1, "cm")),
  #   alpha = 0.5
  # )
