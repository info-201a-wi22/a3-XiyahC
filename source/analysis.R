# library

library(tidyverse)
library(dplyr)
library(ggplot2)
library("plotly", warn.conflicts = FALSE)

#install.packages('usdata')
library(usdata, warn.conflicts = FALSE)

#--------------------

# data loading - incarceration_trends.csv 
file <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
data <- read.csv(file, stringsAsFactors = FALSE)

#--------------------

# data cleaning
# 1. want to focus on things related to aapi
# Asian American / Pacific Islander comparing to white
# 2. want to focus on the state - CA - California

# Note:
#   jail_pop: average daily population held in the jail
#   prison_pop: total number of population in the prison on Dec. 31 of the current year

df <- data %>% 
  filter(state == "CA") %>%
  select("yfips","year","county_name",
         "total_pop_15to64","aapi_pop_15to64","white_pop_15to64",
         "total_jail_pop",  "aapi_jail_pop",  "white_jail_pop",
         "total_prison_pop","aapi_prison_pop","white_prison_pop",
         "aapi_female_prison_pop", "aapi_male_prison_pop",
         "white_female_prison_pop","white_male_prison_pop",
         "total_jail_pop_rate",  "aapi_jail_pop_rate",  "white_jail_pop_rate",
         "total_prison_pop_rate","aapi_prison_pop_rate","white_prison_pop_rate")

brief_sum1 <- summary(df)

# Use the brief_sum1, can view the basic missing values for each variable
# since the population rates exist as variables,
# we will eliminate our selected variables.

df2 <- df %>%
  select("yfips","year","county_name", "aapi_pop_15to64",
         "total_jail_pop_rate",  "aapi_jail_pop_rate",   "white_jail_pop_rate",
         "total_prison_pop_rate","aapi_prison_pop_rate", "white_prison_pop_rate",
         "aapi_prison_pop", "aapi_female_prison_pop", "aapi_male_prison_pop",
         "white_prison_pop","white_female_prison_pop","white_male_prison_pop") %>%
  mutate("aapi_female_prison_pop_rate"=round(aapi_female_prison_pop/aapi_prison_pop, 2),
         "aapi_male_prison_pop_Rate"  =round(aapi_male_prison_pop  /aapi_prison_pop, 2),
         "white_female_prison_pop_rate"=round(white_female_prison_pop/white_prison_pop,2),
         "white_male_prison_pop_rate"  =round(white_male_prison_pop  /white_prison_pop,2))

#-------------------------------

# Summary Info
# Table stores year + each year's total_prison_pop_rate_avg
avg_prison_pop_rate_by_year <-df2 %>% 
  group_by(year) %>% 
  summarize(avg_prison_pop_rate = mean(total_prison_pop_rate, na.rm = TRUE))

summary_info <- list()
  # 1. store data year range
summary_info$year_range <- avg_prison_pop_rate_by_year %>%
  summarize(year = unique(year))
  # 2. how many years
summary_info$year_length<- max(summary_info$year_range)-min(summary_info$year_range)
  # 3. store the year having max avg prison population rate
summary_info$max_year   <- avg_prison_pop_rate_by_year %>%
  filter(avg_prison_pop_rate == max(avg_prison_pop_rate, na.rm=TRUE)) %>%
  select(year)
  # 4. store the value of that year
summary_info$max_rate   <- avg_prison_pop_rate_by_year %>%
  filter(avg_prison_pop_rate == max(avg_prison_pop_rate, na.rm=TRUE)) %>%
  select(avg_prison_pop_rate)
  # 5. 2006 white_prison_pop_rate
summary_info$white_06 <- df2 %>% 
  filter(year == '2006') %>%
  summarize(avg_white_prison_pop_rate_06 = mean(white_prison_pop_rate,na.rm=TRUE))
  # 6. 2006 aapi_prison_pop_rate
summary_info$aapi_06  <- df2 %>% 
  filter(year == '2006') %>%
  summarize(avg_aapi_prison_pop_rate_06 = mean(aapi_prison_pop_rate,na.rm=TRUE))
  # 7. differences
summary_info$diff     <- abs(summary_info$white_06 - summary_info$aapi_06)

#-------------------------------

# Chart Overtime.

# Average White Prison Population Rate by Year in Cali
year_white_avg_prison_pop_rate <- df2 %>%
  group_by(year) %>%
  summarize(year, avg_white_prison_pop_rate = mean(white_prison_pop_rate, na.rm=TRUE), .groups='drop')%>%
  distinct()

  # change NaN to zeros
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
year_white_avg_prison_pop_rate[is.nan(year_white_avg_prison_pop_rate)] <- 0

#ggplot(year_white_avg_prison_pop_rate) +
#  geom_col(mapping = aes(x=year, y=avg_white_prison_pop_rate, fill=year)) +
#  labs(title = "Average White Prison Population Rate by Year in California",
#       subtitle = "From 1970-2018.",
#       x = "Year", y = "Average White Prison Population Rate")

# Average AAPI Prison Population Rate by Year in Cali
year_aapi_avg_prison_pop_rate <- df2 %>%
  group_by(year) %>%
  summarize(year, avg_aapi_prison_pop_rate = mean(aapi_prison_pop_rate, na.rm=TRUE), .groups='drop')%>%
  distinct()

# change NaN to zeros
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
year_aapi_avg_prison_pop_rate[is.nan(year_aapi_avg_prison_pop_rate)] <- 0

#ggplot(year_aapi_avg_prison_pop_rate) +
#  geom_col(mapping = aes(x=year, y=avg_aapi_prison_pop_rate, fill=year)) +
#  labs(title = "Average AAPI Prison Population Rate by Year in California",
#       subtitle = "From 1970-2018.",
#       x = "Year", y = "Average AAPI Prison Population Rate")

#-------------------------------

# Comparison Chart (Compare two things in one chart)
# use Dodge bar chart to show gender groups within each of the two populations

# 1. need to select sum white women pop and men pop, store in white_pop dataframe
white_women <- df2 %>%
  summarize(white_women = sum(white_female_prison_pop, na.rm=TRUE))
white_men   <- df2 %>%
  summarize(white_men = sum(white_male_prison_pop, na.rm=TRUE))

# 1. need to select sum aapi women pop and men pop, store in white_pop dataframe
aapi_women  <- df2 %>%
  summarize(aapi_women  = sum(aapi_female_prison_pop, na.rm=TRUE))
aapi_men    <- df2 %>%
  summarize(aapi_men    = sum(aapi_male_prison_pop, na.rm=TRUE))

white_women  #82,311
white_men   #927,290
aapi_women    #2,575
aapi_men     #30,663

pop_white <- data.frame(gender=c("Women","Men"), race="White", population=c(82311, 927290))
pop_aapi  <- data.frame(gender=c("Women","Men"), race="aapi", population=c(2575, 30663))

pop_gender <- pop_white %>% merge(pop_aapi, all=TRUE)

#ggplot(pop_gender) +
#  geom_col(mapping = aes(x = race, y = population, fill = gender), position="dodge")+
#      labs(title = "Total Prison Population between AAPI and White",
#         subtitle = "From 1970-2018",
#         x = "Race", y = "Population")

#-------------------------------

# Map

df3 <- data %>%
  group_by(state) %>%
  summarize(state,
            total_aapi_pop = sum(aapi_prison_pop, na.rm=TRUE), 
            total_white_pop = sum(white_prison_pop, na.rm=TRUE),.groups='drop') %>%
  distinct() %>%
  mutate(state = abbr2state(state)) %>%
  mutate(state = tolower(state))


state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(df3, by="state")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )


