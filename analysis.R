library(vroom)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)

### DEVAULT VALUES
app_colours <- list(
  title = "#616161",
  axis = "#9e9e9e",
  main = "#1976d2",
  no_emphasis = "#757575",
  divergent = "#f57c00",
  line_main = "#42a5f5",
  line_complementary = "#78909c"
)

theme_minimalistic <- function() {
  theme_classic() +
    theme(plot.title = element_text(hjust = 0, colour = app_colours$title),
          plot.title.position = "plot",
          axis.line = element_line(colour = app_colours$axis),
          axis.ticks = element_line(colour = app_colours$axis),
          axis.text = element_text(colour = app_colours$axis),
          axis.title = element_text(colour = app_colours$axis),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)
    )
}

theme_set(theme_minimalistic())

### CLEANING THE DATA
df <- vroom("data/parental_leave.csv", 
            col_select = 1:6, 
            col_types = cols("c", "c", "i", "i", "i", "i")) %>% 
  clean_names() %>% 
  mutate_if(is.integer, ~replace_na(., 0)) %>%
  mutate(
    total_maternity_leave = paid_maternity_leave + unpaid_maternity_leave,
    total_paternity_leave = paid_paternity_leave + unpaid_paternity_leave,
    total_leave = total_maternity_leave + total_paternity_leave
  )

### ANALYSIS AND CHARTS
# Companies with more and less parental leave (total, separated by gender)
# Maybe a distribution (histogram) chart
# Create data to be used by the geom_density
df_test <- df %>% 
  group_by(total_leave) %>% 
  summarise(qty = n(),
            women_time = mean(total_maternity_leave)) %>% 
  mutate(men_time = total_leave - women_time,
         pct_women = women_time / ifelse(total_leave == 0, 1, total_leave),
         pct_men = men_time / ifelse(total_leave == 0, 1, total_leave),
         share_women = pct_women * qty,
         share_men = pct_men * qty) %>% 
  pivot_longer(c(share_women, share_men)) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  select(total_leave, name, value)

generate_values <- function(i, tb) {
  tb_temp <- tibble(
    total_leave = df_test[i,]$total_leave,
    type_leave = rep(df_test[i,]$name, ceiling(df_test[i,]$value))
  )
  
  rbind(tb, tb_temp)
}

tb3 <- tibble()
tb3 <- purrr::map_df(1:nrow(df_test), generate_values, tb = tb3)

tb3 %>% 
  ggplot(aes(x = total_leave)) + 
  geom_density(aes(fill = type_leave), position = "stack")
  

# Cards with best and worst companies depending on filters

# Check if is there some relation between industries