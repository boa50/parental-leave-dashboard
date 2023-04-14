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
df_density <- df %>% 
  group_by(total_leave) %>% 
  summarise(qty = n(),
            women_time = mean(total_maternity_leave)) %>% 
  mutate(men_time = total_leave - women_time,
         share_women = women_time * qty / ifelse(total_leave == 0, 1, total_leave),
         share_men = men_time * qty / ifelse(total_leave == 0, 1, total_leave)) %>% 
  pivot_longer(c(share_women, share_men), 
               names_to = "leave_type", 
               values_to = "percentage") %>%
  select(total_leave, leave_type, percentage)

generate_values <- function(i, tb, source_df) {
  tb_temp <- tibble(
    total_leave = source_df[i,]$total_leave,
    leave_type = rep(source_df[i,]$leave_type, ceiling(source_df[i,]$percentage))
  )
  
  rbind(tb, tb_temp)
}

tb_density <- tibble()
tb_density <- purrr::map_df(1:nrow(df_density), generate_values, 
                            tb = tb_density,
                            source_df = df_density)

tb_density %>% 
  ggplot(aes(x = total_leave)) + 
  geom_density(aes(fill = leave_type), 
               colour = "transparent", 
               position = "stack") +
  scale_fill_manual(values = c("#9AC0CD", "#CD8C95"))
  

# Cards with best and worst companies depending on filters

# Check if is there some relation between industries