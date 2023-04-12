library(vroom)

df <- vroom("data/parental_leave.csv", 
            col_select = 1:6, 
            col_types = cols("c", "c", "i", "i", "i", "i"),
            na = 0)

df[is.na(df)] <- 0

# Companies with more and less parental leave (total, separated by gender)
# Maybe a distribution (histogram) chart

# Cards with best and worst companies depending on filters

# Check if is there some relation between industries