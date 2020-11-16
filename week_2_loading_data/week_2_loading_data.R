library(package = "haven")
library(package = "forcats")
library(package = "stringr")
library(package = "dplyr")
#loading the packages we would need for the rest of script

icu <- read_sas(data_file = "icu.sas7bdat") %>% 
  select(ID, AGE, GENDER) %>% 
  rename(id = ID,
         age = AGE, 
         gender = GENDER) %>% 
  mutate(gender = as_factor(x = gender),
         gender = fct_recode(.f = gender, 
                             Male = "0",
                             Female = "1"),
         id = str_c(id)) %>% 
  arrange(age) %>% 
  slice(1:10)

icu

ebola_liberia <- read_csv(file = "country_timeseries.csv") %>% 
  select(Date, Cases_Liberia, Deaths_Liberia) %>%  
  rename(date = Date, 
         cases = Cases_Liberia, 
         deaths = Deaths_Liberia) %>% 
  mutate(ratio = deaths / cases) %>% 
  filter(!is.na(x = cases))

head(x = ebola_liberia)

## Create an object with just the top five observations in terms of death counts
first_five <- arrange(.data = ebola_liberia, 
                      desc(deaths)) # First, rearrange the rows by deaths
first_five <- slice(.data = first_five, 
                    1:5) # Limit the dataframe to the first five rows
