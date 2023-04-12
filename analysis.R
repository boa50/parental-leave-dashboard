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

df %>% 
  group_by(total_leave) %>% 
  summarise(qty = n(),
            women_time = mean(total_maternity_leave)) %>% 
  mutate(men_time = total_leave - women_time,
         pct_women = women_time / total_leave,
         pct_men = men_time / total_leave,
         share_women = pct_women * qty,
         share_men = pct_men * qty) %>% 
  pivot_longer(c(share_women, share_men)) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(x = total_leave)) + 
  geom_density(aes(fill = name), position = "stack")
  # geom_area(aes(fill = name), position = "stack")

df %>% 
  ggplot(aes(x = total_leave, y = value)) + 
  # geom_histogram() +
  geom_density()

df %>% 
  filter(total_paternity_leave > 0) %>% 
  pivot_longer(c(total_maternity_leave, total_paternity_leave)) %>% 
  group_by(name, value) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x = value, y = total)) + 
  # geom_histogram() +
  geom_area(aes(fill = name), position = "stack")

df %>% 
  pivot_longer(c(total_maternity_leave, total_paternity_leave)) %>% 
  group_by(company, name, value) %>% 
  summarise(total_per_company = sum(value))
  

# Cards with best and worst companies depending on filters

# Check if is there some relation between industries