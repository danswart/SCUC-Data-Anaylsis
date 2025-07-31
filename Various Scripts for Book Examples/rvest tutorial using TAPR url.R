# Welbers rvest tutorial using the 2022-2023 TAPR webpage

# load libraries

library(tidyverse)
library(rvest)
library(tibble)
library(janitor)
library(DT)


tapr = 'https://rptsvr1.tea.texas.gov/cgi/sas/broker?_service=marykay&_program=perfrept.perfmast.sas&_debug=0&ccyy=2022&lev=D&id=094902&prgopt=reports/tapr/performance.sas' %>% read_html()

## find any <table> element
tapr %>% html_element('table')            ## first element 
tapr %>% html_elements('table')           ## all elements

## find any <td> element
tapr %>% html_element('td')            ## first element 
tapr %>% html_elements('td')           ## all elements


## find any element with class="data"
tapr %>% html_element('.data')       ## first element with class='data'
tapr %>% html_elements('.data')      ## all elements with class='data'

## find any element with id="094902" 
## (id can be anything the developer chooses)
tapr %>% html_element('#rh2b1c227')           ## NO id's used 
tapr %>% html_elements('#rh2b1c227')          ## NO id's used 

## find any <span> element with class="aod"
tapr %>% html_element('span.aod')     ## first <span> element with class='aod'
tapr %>% html_elements('span.aod')    ## all <span> elements with class='aod'

## find any element with class="dropdown-content"
tapr %>% html_element('.dropdown-content')  ## first element with class="dropdown-content"
tapr %>% html_elements('.dropdown-content') ## all elements with class="dropdown-content"


tapr_tables = tapr %>% html_element('tr #rh2b1c227')
html_table(tapr_tables)



########## END OF EXERCISE ##########



##### BEGIN CREATING USABLE TABLE

# Read the webpage

# Extract the first table
tapr_table1 <- html_table(tapr)[[1]]  # Assuming it's the first table on the page

# Extract the second table
tapr_table2 <- html_table(tapr)[[2]] 



# cleanup column names
tapr_table2 <- tapr_table2 %>% clean_names()


# remove all 2022 rows
tapr_table2_2023 <- tapr_table2 %>% 
  filter(!grepl("2022", school_year))


library(DT)

library(DT)

# Assume 'my_data' is the data frame
datatable(my_data, options = list(
  pageLength = 10,  # Set the number of rows per page
  lengthMenu = c(10, 20, 30),  # Define the length menu
  displayStart = 1  # Set the initial row to display
))



# include a button in the DT table to export to Excel
datatable(tapr_table2_2023, extensions = 'Buttons', options = list(
  dom = 'Bfrtip',
  buttons = c('excel')
))





# Select specific rows
specific_rows <- table[c(3, 4), ]  # Selecting the 3rd and 4th rows



