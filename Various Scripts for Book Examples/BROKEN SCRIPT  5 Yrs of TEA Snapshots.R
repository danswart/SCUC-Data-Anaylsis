##### SCUC EXPLORATORY ANALYSIS ON SEVERAL VARIABLES FOUND IN THE 5 YEARS OF TEA SNAPSHOP REPORT DATA #####

#####  THIS SCRIPT IS BROKEN!!!  #####


#####  See https://www.cararthompson.com/posts/2021-09-02-alignment-cheat-sheet/alignment-cheat-sheet#so-what-does-what #####
#####  For Alignment Cheat Sheet for v/h adjust and v/h align  #####




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



# list of 130 column names in TEA downloaded Excel file I named "scuc-tea-5yr-snapshot-df.xlsx"
#
# year
# fund_balance
# tot_exp
# op_exp
# op_exp_per_pupil
# instruct_prop
# central_admin_prop
# school_leadership_prop
# plant_svs_prop
# other_op_prop
# instruct_exp
# instruct_exp_per_pupil
# basic_ed_svs_prop
# sped_prop
# state_compensatory_ed_prop
# bi_esl_ed_prop
# cte_prop
# g_t_ed_prop
# athletics_related_activities_prop
# high_school_allotment_prop
# prekinder_prop
# un_allocated_prop
# taxable_value_per_pupil_prior_tax_year
# local_adopted_tax_rate_prior_tax_year
# op_and_other_rev
# op_and_other_rev_per_pupil
# op_rev
# state_rev_prop
# local_and_other_rev_prop
# fed_rev_prop
# other_rev
# teachers_5_or_fewer_yrs_of_exp_prop
# teachers_avg_yrs_of_exp
# teachers_with_adv_degrees_prop
# teacher_turnover_prop
# teachers_african_amer_prop
# teachers_hispanic_prop
# teachers_white_prop
# teachers_amer_indian_prop
# teachers_asian_prop
# teachers_pac_isl_prop
# teachers_two_or_more_races_prop
# teachers_reg_ed_prop
# teachers_sped_prop
# teachers_compensatory_ed_prop
# teachers_bi_esl_ed_prop
# teachers_cte_prop
# teachers_other_ed_includes_g_t_prop
# teacher_cnt
# central_admin_sal_prop
# campus_admin_sal_prop
# prof_support_staff_sal_prop
# teachers_sal_prop
# educ_aides_sal_prop
# aux_sal_prop
# avg_central_admin_sal
# avg_campus_admin_sal
# avg_prof_support_staff_sal
# avg_teacher_sal
# minority_sal_prop
# students_per_staff
# students_per_teacher
# staff_cnt
# sat_act_tested
# sat_act_at_or_above_criterion
# sat_avg_score
# act_avg_score
# attend_rate
# annual_dropout_rate_gr_9_12
# x4_year_long_grad_rate_class_of_2017
# x5_year_long_grad_rate_class_of_2016
# x6_year_long_grad_rate_class_of_2015
# annual_grad_cnt
# annual_rhsp_dap_fhsp_e_fhsp_dla_grad_cnt
# ethnicity_prop_african_amer
# ethnicity_prop_hispanic
# ethnicity_prop_white
# ethnicity_prop_amer_indian
# ethnicity_prop_asian
# ethnicity_prop_pac_isl
# ethnicity_prop_two_or_more_races
# enrollment_cnt
# students_econ_disadvantaged
# students_eng_learners_el
# students_sped
# students_bi_esl_educ
# students_cte
# students_g_t_educ
# approach_african_amer
# approach_hispanic
# approach_white
# x92
# approach_asian
# approach_pac_isl
# approach_two_or_more_races
# approach_econ_disadv
# mts_african_amer
# mts_hispanic
# mts_white
# mts_amer_indian
# mts_asian
# mts_pac_isl
# mts_two_or_more_races
# mts_econ_disadv
# masters_african_amer
# masters_hispanic
# masters_white
# masters_amer_indian
# masters_asian
# masters_pac_isl
# masters_two_or_more_races
# masters_econ_disadv
# approach_all_subjects
# approach_ela_reading
# approach_writing
# approach_math
# approach_science
# approach_soc_stud
# mts_all_subjects
# mts_ela_reading
# mts_writing
# mts_math
# mts_science
# mts_soc_stud
# masters_all_subjects
# masters_ela_reading
# masters_writing
# masters_math
# masters_science
# masters_soc_stud




# read in Excel file
df <- readxl::read_xlsx("scuc-tea-5yr-snapshot-df.xlsx", 
                        sheet = 1,
                        col_names = TRUE)


# Clean up the column names
df <- df %>% janitor::clean_names()


# List 130 column names with baseR function
column_names <- colnames(df)


# Write column names to a text file using baseR
writeLines(column_names, "column_names.txt")


# Example: Find columns with "rate" or "prop" in their names

# rate_prop_columns <- grep("(rate|prop)", names(df), value = TRUE)


# Convert those columns to numeric percentages

# df <- df %>%
#   dplyr::mutate_at(vars(rate_prop_columns), ~as.numeric(gsub("%", "", .)))



# Define a vector of keywords to identify columns to analyze

keywords <- c("op_rev", "other_rev", "approach", "mts", "masters")


# Identify columns that contain any of the keywords in their names

selected_columns <- grep(paste0(keywords, collapse = "|"), names(df), value = TRUE)


# Convert selected columns to numeric

df <- df %>%
  dplyr::mutate_at(vars(selected_columns), ~as.numeric(gsub("%", "", .)))



# Rename the column 'x92' to 'approach_amer_indian'
# df <- df %>%
#   rename(approach_amer_indian = x92)


# Convert the 'approach_amer_indian' column to numeric percentages

df <- df %>%
  dplyr::mutate(approach_amer_indian = as.numeric(gsub("%", "", approach_amer_indian)))


# Convert all columns to character type
df[2:130] <- lapply(df[2:130], as.character)

# Pivot the data into long format with values rounded to 2 decimal places

df_long <- df %>%
  tidyr::pivot_longer(cols = c(2:130),
                      names_to = "variable",
                      values_to = "value") %>%
  dplyr::mutate(value = round(as.numeric(value), 2))  # Convert back to numeric for rounding



# Replace all NA values with zeros in the entire dataset
# THIS IS USUALLY NOT A GOOD IDEA
#
# df <- df %>%
#   mutate_all(~ifelse(is.na(.), 0, .))


# Filter the DataFrame to include rows with 'variable' matching the desired keywords
# Create a vector of keywords

keywords <- c("avg_teacher_sal")

# "act_avg_score", "sat_avg_score", "avg_teacher_sal"

filtered_df <- df %>%
  dplyr::filter(variable %in% keywords)



# create a bar chart

snapshots <- ggplot2::ggplot(filtered_df,
       aes(x = year,
           y = value
           )
       ) +
  ggplot2::geom_col(position = position_dodge2(0.9)) +
  ggplot2::coord_cartesian(ylim = c(40000, 75000))


snapshots

# build a line plot

snapshots <- ggplot2::ggplot(filtered_df,
                    aes(x = year,
                        y = value)) +
  ggplot2::geom_line()

snapshots

