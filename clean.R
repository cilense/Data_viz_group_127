library(tidyverse)
library(dplyr)

# read all datasets
all_files = list.files(path=".",pattern = ".csv",all.files = TRUE,full.names = FALSE)

all_data = data.frame()


# combine and label with stock name
for (filename in all_files) {
  sm_data = read_csv(filename)
  sm_data$stock = substr(filename, 1, nchar(filename)-4)
  all_data = rbind(all_data, sm_data)
}

# convert and filter date
all_data$Date = as.Date(all_data$Date)

recent_data = all_data %>% 
  filter(Date > as.Date("2009-12-31") & Date < as.Date("2021-01-01"))

# drop dividends and stock splits columns b/c most of them are zeros
recent_data <- subset(recent_data, select = -c(7,8))

# get months and years
recent_data$Month = months(recent_data$Date)
recent_data$Year = format(recent_data$Date, format="%Y")

# get the mean of close prices
recent_data = recent_data %>% 
  mutate(monthly_average_close = recent_data$Close)
aggregate(monthly_average_close ~ Month + Year, recent_data, mean)

# keeps only the last date of the month
average_data = recent_data %>% 
  group_by(stock, Month, Year) %>% 
  filter(Date == max(Date))

# reorder the columns
col_order <- c("stock", "Year", "Month", "monthly_average_close")
average_data <- average_data[, col_order]

# calculate montly growth rate
average_data = average_data %>% 
  group_by(stock) %>% 
  mutate(Growth = (monthly_average_close - lag(monthly_average_close))/lag(monthly_average_close))

# write csv
write_csv(average_data, "average_data.csv")

# filter out the 100 largest American companies by market capitalization
comp_cap <- read_csv("companies_cap.csv")
comp_cap = comp_cap %>%
  slice(1:100)

# combine two dataframe
largest_comp_growth = right_join(average_data, comp_cap, by = c("stock"="Symbol"))

# drop and reorder the columns
cap_col_order <- c("Rank", "Name", "stock", "marketcap", 
                   "Year", "Month", "monthly_average_close", "Growth")
largest_comp_growth <- subset(largest_comp_growth, select = cap_col_order)
largest_comp_growth <- largest_comp_growth[, cap_col_order]

# sort the rows by cap rank
largest_comp_growth = largest_comp_growth %>%
  group_by(Rank) %>%
  arrange(Rank)

# write csv
write_csv(largest_comp_growth, "largest_comp_growth.csv")  
  
  
  
  
  
  














