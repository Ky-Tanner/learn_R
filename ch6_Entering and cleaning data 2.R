library(tidyverse)
library(readr)
library(titanic)
library(lubridate)
library(dplyr)
data("titanic_train")

#------------------------------------------------------
country_timeseries <- read_csv("data/country_timeseries.csv")
max_deaths <- read_csv("data/mexico_deaths.csv")
max_exp <- read_csv("data/mexico_exposure.csv")

mexico <- full_join(max_deaths,max_exp, by = "day") %>% 
  mutate(day = mdy(day))

ggplot(mexico, aes(day, deaths)) +
  geom_point(alpha = 0.5) + 
  labs(x = "Date in 2008", y = "# of deaths")
#------------------------------------------------------

ebola <- read_csv("data/country_timeseries.csv")
ebola <- ebola %>% 
  pivot_longer(cols = Cases_Guinea:Deaths_Mali,
               names_to = "Variable",
               values_to = "Count") %>% 
  mutate(Date = mdy(Date)) 

ebola <- ebola %>% 
  separate(col = Variable, into = c("Variable", "Country")) %>% 
  pivot_wider(names_from = "Variable", values_from = "Count") %>% 
  filter(!is.na(Cases) & !is.na(Deaths))

cases_plot <- ebola %>% 
  ggplot(aes(Date,Cases))+
  geom_line()+
  facet_wrap(~ Country, ncol = 4, scales = "free_y")
deaths_plot <- ebola %>% 
  ggplot(aes(Date,Deaths))+
  geom_line()+
  facet_wrap(~ Country, ncol = 4, scales = "free_y")

#------------------------------------------------------

data("VADeaths")
VA_deaths <- VADeaths %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "age") %>% 
  pivot_longer(cols = 2:5, names_to = "loc_gen", values_to = "death_rate") %>% 
  separate(col = loc_gen, into = c("location", "gender"), sep = " ")

VA_deaths %>% 
  ggplot(aes(age, death_rate)) +
  geom_point(aes(color = gender)) +
  facet_wrap(gender ~ location) +
  theme_minimal() +
  labs(x = "Age Category", y = "Death Rate (per 1,000)")

#------------------------------------------------------

library(babynames)
library(stringr)
library(scales)

top_letters <- babynames %>% 
  mutate(first_letter = str_sub(name, 1, 1))
  
top_letters <- top_letters %>% 
  group_by(first_letter) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(prop))

top_letters %>% 
  mutate(first_letter = fct_reorder(first_letter, prop)) %>% 
  ggplot(aes(x = first_letter)) + 
  geom_bar(aes(weight = prop)) + 
  coord_flip() + 
  scale_y_continuous(labels = percent) + 
  labs(x = "", y = "Percent of names that start with ...")


student_list <- data_frame(name = c("Conaway", "Shabana", "Natalie", "Sarah",
                                    "Audry", "Robert", "Mackenzie", "Peter",
                                    "Linda", "Ikaia", "Elizabeth", "Elizabeth",
                                    "Kailee", "James", "Kellin", "Ky","Ariel",
                                    "Breanna", "Becky", "Valeria", "Caleb",
                                    "Jixindong", "Sophia", "Sara", "Tyler"))

student_list <- student_list %>% 
  mutate(first_letter = str_sub(name, 1, 1))

student_list <- student_list %>% 
  group_by(first_letter) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(prop))

student_list %>% 
  mutate(first_letter = fct_reorder(first_letter, prop)) %>% 
  ggplot(aes(x = first_letter)) + 
  geom_bar(aes(weight = prop)) + 
  coord_flip() + 
  scale_y_continuous(labels = percent) + 
  labs(x = "", y = "Percent of names that start with ...")


