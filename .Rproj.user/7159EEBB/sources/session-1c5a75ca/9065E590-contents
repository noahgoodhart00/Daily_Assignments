#Noah Goodhart
#ESS 330
#Daily Assignment #8

#First I loaded up tidyverse and dplyr, then I read the url and created a data frame for it titled "covid".
library(tidyverse)
library(dplyr)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read.csv(url)
View(covid)

#I created a new data frame that has state regions, names and abbreviations, from base R.
state.df = data.frame(region = state.region,
                      state = state.name,
                      state_abbreviation = state.abb)
View(state.df)

#I made a new data frame, joined the data together, using a full_join, with the shared column of "state". 
#I also filtered out the NA values that didn't have a region, like Puerto Rico and the US Virgin Islands.
#I also made the date format readable for ggplot, since I know I am going to need that later.
joined_data <- full_join(state.df,covid, by = "state")
joined_data <- joined_data %>% 
  filter(!is.na(region))
joined_data <- joined_data %>% 
  mutate(date = as.Date(date))

View(joined_data)

#I created a new data frame for just the cases, and grouped the data by region and date.
daily_cummulative_cases <- joined_data %>% 
  group_by(region, date) %>% 
  summarise(daily_cases = sum(cases,na.rm = TRUE)) %>% 
  arrange(region, date) 
View(daily_cummulative_cases)

#I created another data frame just for deaths, and grouped the data by region and date.
daily_cummulative_deaths <- joined_data %>% 
  group_by(region, date) %>% 
  summarise(daily_deaths = sum(deaths, na.rm = TRUE)) %>% 
  arrange(region, date)
View(daily_cummulative_deaths)

#I then joined the previous data frames, using a left_join, so that my cases data and death data were all in the same data frame.
final_data <- daily_cummulative_cases %>%
  left_join(daily_cummulative_deaths, by = c("region", "date"))
view(final_data)

#I also mutated the date using as.Date AGAIN, because I am paranoid.
final_data <- final_data %>% 
  mutate(date = as.Date(date))

#After having my final data set, I reformatted it from wide data to long data.
#I also changed all the names to "metric" and all the values to "value", so it would be easier to plot.
long_final_data <- final_data %>%
  pivot_longer(cols = c(daily_cases, daily_deaths), 
               names_to = "metric", 
               values_to = "value")

#Here is my code for my ggplot. 
ggplot(long_final_data, aes(x = date, y = value, color = metric)) +
  geom_line(linewidth = 1) +  
  facet_wrap(~ region + metric, scales = "free_y", ncol = 2, # I made sure that the y axis could be unique for each graph.
             labeller = labeller(metric = function(x) "")) + #This replaced the "metric" labels with empty strings, so they don't show up on my plot.
  theme_minimal() +
  labs(title = "Daily Cases and Deaths by Region for the COVID-19 Pandemic",
       x = "Date",
       y = "Count",
       color = "Metric Type") +
  scale_color_manual(values = c("daily_cases" = "blue", "daily_deaths" = "red"),
                     labels = c("Daily Cases", "Daily Deaths")) + #This makes the legend look more clean, with concise labels.
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #This adjusts the angle of the x axis labels, making them easier to read.

#Finally, I saved my plot to my img directory.
ggsave("img/covid_plot_daily_8.pdf", width = 10, height = 6, dpi = 300)

