library(vroom)
library(janitor)
library(dplyr)
library(ggplot2)

df <- vroom("data/parental_leave.csv", 
            col_select = 1:6, 
            col_types = cols("c", "c", "i", "i", "i", "i")) %>% 
  clean_names() %>% 
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>% 
  mutate(
    total_maternity_leave = paid_maternity_leave + unpaid_maternity_leave,
    total_paternity_leave = paid_paternity_leave + unpaid_paternity_leave
  )

# Companies with more and less parental leave (total, separated by gender)
# Maybe a distribution (histogram) chart

# Cards with best and worst companies depending on filters

# Check if is there some relation between industries