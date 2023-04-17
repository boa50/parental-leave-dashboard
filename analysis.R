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
  line_complementary = "#78909c",
  women = "#ff84d1",
  men = "#5a84b6"
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
  ggplot(aes(x = total_leave, after_stat(count))) + 
  geom_density(aes(fill = leave_type), 
               adjust = 1.5,
               colour = "transparent", 
               position = "stack") +
  scale_fill_manual(values = c(app_colours$men, app_colours$women)) +
  labs(x = "Weeks leave") +
  scale_x_continuous(breaks = c(0, 50, 100, 150),
                     limits = c(0, 160),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

df_density %>% 
  pivot_wider(names_from = leave_type, values_from = percentage) %>% 
  mutate(wm_ratio = share_women / share_men) %>% 
  filter(wm_ratio >= 0 & wm_ratio <= 9)
  
# Bar plot showing the median differences between men and women
df_women_leaves <- df %>% 
  filter(total_maternity_leave > 0)

df_men_leaves <- df %>% 
  filter(total_paternity_leave > 0)

tb_medians <- tibble(
  gender = c("women", "men"),
  weeks_leave = c(median(df_women_leaves$total_maternity_leave),
                  median(df_men_leaves$total_paternity_leave))
)
  
tb_medians %>%
  ggplot(aes(x = weeks_leave, y = gender)) +
  geom_col(aes(fill = gender)) +
  scale_fill_manual(values = c(app_colours$men, app_colours$women)) +
  labs(x = "Median Weeks Leave") +
  scale_x_continuous(breaks = c(0, 5, 10, 15),
                     limits = c(0, 15),
                     expand = expansion(mult = 0)) +
  scale_y_discrete(expand = expansion(mult = 0.5)) +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

print(tb_medians)

# What is the proportion of weeks that they are payed
leave_types <- c("unpaid_women", "paid_women", "unpaid_men", "paid_men")

tb_percentage_payment <- tibble(
  gender = factor(c(rep("women", 2), rep("men", 2)), levels = c("women", "men")),
  percentage = c(
    sum(df_women_leaves$unpaid_maternity_leave) / sum(df_women_leaves$total_maternity_leave),
    sum(df_women_leaves$paid_maternity_leave) / sum(df_women_leaves$total_maternity_leave),
    sum(df_men_leaves$unpaid_paternity_leave) / sum(df_men_leaves$total_paternity_leave),
    sum(df_men_leaves$paid_paternity_leave) / sum(df_men_leaves$total_paternity_leave)
  ),
  type = factor(leave_types, levels = leave_types)
)

tb_percentage_payment %>% 
  ggplot(aes(x = gender, y = percentage)) +
  geom_col(aes(fill = type)) +
  scale_fill_manual(values = c("#d9d9d9", 
                               app_colours$women, 
                               "#d9d9d9", 
                               app_colours$men)) +
  labs(y = "Payment Percentage") +
  scale_y_continuous(breaks = c(0, .25, .50, .75, 1),
                     limits = c(0, 1),
                     expand = expansion(mult = 0)) +
  scale_x_discrete(expand = expansion(mult = 0.5)) +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

print(tb_percentage_payment)

# Check if is there some relation between industries