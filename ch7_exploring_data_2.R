library(babynames)
library(stringr)
library(scales)
library(broom)
library(tidyverse)
library(lubridate)
library(purrr)

top_letters <- babynames %>% 
  mutate(first_letter = str_sub(name, 1, 1))

top_letters <- top_letters %>% 
  group_by(first_letter) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(prop))

top_letters %>% 
  mutate(first_letter = fct_reorder(first_letter, desc(prop))) %>% 
  ggplot(aes(x = first_letter)) + 
  geom_bar(aes(weight = prop)) + 
  scale_y_continuous(labels = percent) + 
  labs(x = "", y = "Percent of names that start with ...")

#-------------------------------------------------------------------------------

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
  mutate(first_letter = fct_reorder(first_letter, desc(prop))) %>% 
  ggplot(aes(x = first_letter)) + 
  geom_bar(aes(weight = prop)) + 
  scale_y_continuous(labels = percent) + 
  labs(x = "", y = "Percent of names that start with ... in our class")

#-------------------------------------------------------------------------------

student_list <- data_frame(name = c("Conaway", "Shabana", "Natalie", "Sarah",
                                    "Audry", "Robert", "Mackenzie", "Peter",
                                    "Linda", "Ikaia", "Elizabeth", "Elizabeth",
                                    "Kailee", "James", "Kellin", "Ky","Ariel",
                                    "Breanna", "Becky", "Valeria", "Caleb",
                                    "Jixindong", "Sophia", "Sara", "Tyler"))

cs_df_class <- mutate(student_list, starts_with = str_sub(name, 1, 1),
                 c_or_s = starts_with %in% c("C", "S")) %>% 
  group_by(c_or_s) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(total = sum(n), 
         prop = (n / total)*100)

cs_df_total <- mutate(babynames, starts_with = str_sub(name, 1, 1),
                      c_or_s = starts_with %in% c("C", "S")) %>% 
  group_by(c_or_s) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(total = sum(n), 
         prop = (n / total)*100)

cs_df_class %>% 
  select(c_or_s, n) %>%
  as.matrix() %>% 
  prop.test()

cs_df_total %>% 
  select(c_or_s, n) %>%
  as.matrix() %>% 
  prop.test()

#videos 9 -> 11#-----------------------------------------------------------------
library(nycflights13)
data(flights)

flights <- flights %>% 
  select(dep_delay, carrier, hour, origin) %>% 
  filter(origin == "LGA") %>% 
  filter(!is.na(dep_delay)) %>% 
  mutate(late_dep = dep_delay >= 15)

plot1 <- flights %>% 
  group_by(hour) %>% 
  summarise(prop_late = mean(late_dep)) %>% 
  ggplot(aes(x = hour, y = prop_late)) +
  geom_line()
plot1

glm(late_dep ~ hour, data = flights, family = binomial(link = "logit")) %>% 
  tidy() %>% 
  slice(2) %>% 
  mutate(odds_ratio = exp(estimate))

nested_flights <- flights %>% 
  group_by(carrier) %>% 
  nest() %>% 
  mutate(glm_ex = map(data, glm(late_dep ~ hour, data = .x, family = binomial(link = "logit"))))

#videos 12-14 functions#--------------------------------------------------------
word <- "ling"
word_list <-  read_csv("../Homework/ERHS_535_homework/Data/words.txt", col_names = c("word"))
other_word_list <- read_csv("https://raw.githubusercontent.com/first20hours/google-10000-english/master/google-10000-english-no-swears.txt",
                            col_names = "word")

move_letter <- function(word){
  first <- str_sub(word, start = 1, end = -2)
  last <- str_sub(word, start = -1, end = -1)
  new <- paste0(last, first)
  return(new)
}

tibble(oldword = c("hey", "whatsup", "okay")) %>% 
  mutate(newword = move_letter(oldword))

is_word <- function(words_to_check, real_word_list = word_list$word){
  check <- words_to_check %in% real_word_list
  return(check)
}

is_word(c("ling", "scat", "soil"))

filter(other_word_list, word == "ling")

#Guest Lecture 2#---------------------------------------------------------------





