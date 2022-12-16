library(tidyverse)


#Loading and cleaning column names of data)
fcc <- janitor::clean_names(read_csv('fcc_complaints_CA-2021.csv'))

#Filtering out n/a zip & method values)
fcc_clean <- fcc %>%
  drop_na(method, zip) %>%
  filter(zip != '00000')

#Creating new column that takes area code from caller_id)
fcc_clean <- fcc_clean %>%
  extract(
    col = caller_id_number,
    into = 'area_code',
    regex = "(\\d\\d\\d)",
    remove = F
  )

#Creating separate columns for month and year of entry )
fcc_clean$date_of_issue <- lubridate::ymd(fcc_clean$date_of_issue)
fcc_clean <- fcc_clean %>%
  mutate(
    month_of_issue = lubridate::month(date_of_issue, label = T),
    year_of_issue = lubridate::year(date_of_issue)
  )
#Formatting and standardizing time values in column)
fcc_clean$time_of_issue <-
  str_replace_all(fcc_clean$time_of_issue, "\\.", '')
fcc_clean <- fcc_clean %>%
  mutate(time_of_issue_clean = parse_time(fcc_clean$time_of_issue))

#Creating visualization of distribution of phone complaints )
call_distribution <- fcc_clean %>%
  filter(form == "Phone") %>%
  ggplot(aes(time_of_issue_clean, fill = method)) + ## edit the fill
  geom_histogram(alpha = 0.8) +
  theme_minimal() +
  scale_x_time(
    breaks = scales::date_breaks("3 hour"),
    labels = scales::time_format("%I:%M %p")
  ) + ## edit the time_format
  theme(legend.position = "bottom") + ## edit this line
  labs(
    x = "",
    y = "Number of FCC Complaints",
    fill = "",
    title = "Distribution of Phone Complaints to the FCC",
    subtitle = "From January 2021 - July 2022"
  ) +
  ggthemes::scale_fill_fivethirtyeight()

#Reading data)
acs <- read_csv('acs_data.csv')

#Creating new column of zip code values)
acs <- acs %>%
  extract(
    col = name,
    into = 'zip',
    regex = "(\\d\\d\\d\\d\\d)",
    remove = F
  )

#Left joining with fcc_clean by zip code and filtering year to 2021 and pop != 0)
fcc_joined <- fcc_clean %>%
  merge(acs, by = 'zip') %>%
  filter(year_of_issue == 2021,
         total_pop != 0)

#Creating columns to distinguish income and age status)
fcc_joined <- fcc_joined %>%
  mutate(
    low_income_zip = ifelse(
      median_income < mean(median_income, na.rm = T),
      'Below Average Median Income',
      'Above Average Median Income'
    ),
    high_age_zip = ifelse(
      median_age > mean(median_age, na.rm = T),
      'Above Average Median Age',
      'Below Average Median Age'
    )
  )

#Creating tibbles of values that only include phone complaints and spam complaints

fcc_joined_spam <- fcc_joined %>%
  filter(form == 'Phone' & issue == 'Unwanted Calls')

fcc_joined_phone <- fcc_joined %>%
  filter(form == 'Phone')


#Summarizing tibble to count number of demographic similarities 
aggregate <- fcc_joined %>%
  group_by(median_age, median_income, total_pop, zip, low_income_zip) %>%
  summarise(n_complaints = n())

#Creating measure of complaints per-1000 to adjust for differences in population between zip codes)
aggregate <- aggregate %>%
  mutate(complaints_per_1000 = n_complaints/total_pop*1000)


#Finding average median income of spam call complaints)
mean(fcc_joined_spam, median_income, na.rm = T)

#Adding average median income and average median age to spam call dataset 
fcc_joined_spam <- fcc_joined_spam %>%
  mutate(
    low_income_zip =  mean(median_income, na.rm = T),
    high_age_zip =mean(median_age, na.rm = T)
  )

#Counting number of complaints by zip codes
dominant_city <- count(fcc_joined_spam, zip, sort = T)

#Counting number of complaints by month
dominant_time_of_year <- count(fcc_joined_spam, month_of_issue, sort = T) 

#Creating table of number of complaints to the FCC by income level
dominant_income <- count(fcc_joined_spam, low_income_zip,sort = T) %>%
  drop_na()
colnames(dominant_income) <- c('Income Level', 'Number of Complaints to the FCC')

#Creating tiable of number of complaints to the FCC by age level
dominant_age <- count(fcc_joined_spam, high_age_zip, sort = T) %>%
  drop_na()
colnames(dominant_age) <- c('Age Level', 'Number of Complaints to the FCC')

#Finding average median income
average_income <- mean(fcc_joined_spam$median_income, na.rm = T)

#Visualizing number of complaints to FCC by zip-codes
dominant_cities <-head(dominant_city, 10) %>%
  ggplot(aes(x = reorder(zip, -n), y = n)) +
  geom_bar(stat = 'identity', fill="lightblue") +
  labs(x = 'Zip code', y = 'Number of complaints to the FCC', title = "Zip Codes With the Most Complaints to the FCC ",subtitle = '(Only In 2021)') +
  scale_fill_manual(values=c("lightblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 

#Visualizing number of complaints to FCC by year
time_of_year_calls <- dominant_time_of_year %>%
  ggplot(aes(x = month_of_issue, y = n, group = 1)) +
  labs(x = 'Month of the year', y = 'Number of complaints to the FCC', title = 'Number of Complaints to the FCC by Month',subtitle = '(Only In 2021)')+
  geom_line() +
  geom_point()+
  theme_minimal()

#Finding average median income by zip-codes
income_by_zip <- fcc_joined_spam %>%
  group_by(zip) %>%
  summarise(average_median_income = round(mean(median_income, na.rm = T)))

#Merging data set of zip-codes with most complaints and data set of average median income by zip-codes
dominant_city <- merge(dominant_city, income_by_zip, by = 'zip')
dominant_city <- dominant_city[order(dominant_city$n, decreasing = TRUE),]
  
#Visualizing number of complaints to FCC by zip-codes and overlaying the respective zip-codes' average median income
dominant_cities_income_graph <-head(dominant_city, 10) %>%
  ggplot(aes(x = reorder(zip, -n), y = n)) +
  geom_bar(stat = 'identity', fill="lightblue") +
  labs(x = 'Zip code', y = 'Number of complaints to the FCC', title = "Zip Codes With the Most Complaints to the FCC",subtitle = '(Only In 2021)') +
  scale_fill_manual(values=c("lightblue")) +
  theme_minimal() +
  geom_text(aes(label=average_median_income, vjust=-0.5, colour = 'average median income '))+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 

#Finding percentage of complaints about a phone that were about spam calls
spam_percentage <- nrow(fcc_joined_spam)/nrow(fcc_joined_phone)

#Counting the number of complaints to FCC by zip-code
zip_density <- fcc_joined_spam %>%
  count(zip)
colnames(zip_density) = c('region', 'value')

#Loading in library for choropleth
library(choroplethrZip)

#Creating choropleth of complaints to FCC by zip-codes in California
zip_choropleth(zip_density, title = "Density Map of FCC Complaints by Zip Codes", legend = "Number of Complaints", num_colors = 6,
               state_zoom = 'california')

