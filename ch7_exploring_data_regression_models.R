library(ggplot2)
library(broom)
library(ggfortify)
library(lubridate)

library(dlnm)
data("chicagoNMMAPS")
chic <- chicagoNMMAPS

mod_1 <- lm(data = chic, dptp ~ temp)
mod_2 <- glm(data = chic, dptp ~ temp)

mod_1_tidy <- tidy(mod_1) #To get the lm model in tidy (df) format
mod_2_tidy <- tidy(mod_2)

mod_1 %>% 
  autoplot() #To get statistic charts on model fit

chic %>% 
  ggplot(aes(x = temp, y = dptp)) +
  geom_point() +
  geom_smooth(method = lm) #This is all you need to plot the regression line

mod_3 <- lm(data = chic, pm10 ~ dow) #Y value needs to be on left

anova(mod_3)

summer <- chic %>% 
  mutate(month = month(date, label = TRUE)) %>% 
  filter(month %in% c("Jun", "Jul", "Aug"))

