# FCC and ACS Pre-processing and EDA

The following document analyzes complaints filed to the Federal Communications Commission (FCC) from January 2021 to July 2022. This data set was pulled from the FCC’s website. To better understand the demographics of the individuals who filed complaints, the FCC data set was merged with a subset of data from the American Community Survey (ACS) which comes from the United States Census Bureau. The attached analysis report summarizes the key findings and insights from the EDA. _Analyzing complaints about spam phone calls was an area of interest._

__Data Sources:__

fcc_complaints_CA-2021.csv
acs_data.csv

__Below is a list of variables that were of importance:__

FCC Data:

zip - the zip code where the complaint originated from.
caller_id_number - the phone number from which the spam or robocall came from.
date_of_issue - the date the incident occurred.
time_of_issue - the time of day the incident occurred.
method - how the spam or robocall connected with the individual.

ACS Data:

name - contains the ZIP code areas.
median_income - contains the median income in each ZIP code.
median_age - contains the median age in each ZIP code.
total_pop - contains the total population within each ZIP code.

__Link to analysis webpage:__

https://omkarsagar.github.io/fcc_acs_eda/
