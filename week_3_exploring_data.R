library("readr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("faraway")
library("tibble")

beijing_pm_raw <- read_csv(file = "data/Beijing_2017_HourlyPM25.csv",
                           skip = 3)

beijing_pm <- beijing_pm_raw %>% 
  rename(sample_time = 'Date (LST)',
         value = Value,
         qc = 'QC Name') %>% 
  select(sample_time, value, qc) %>% 
  mutate(aqi = cut(value, 
                   breaks = c(0, 50, 100, 150, 200, 300, 500, Inf),
                   labels = c("Good", "Moderate",
                              "Unhealthy for some groups", 
                              "Unhealthy", "Very unhealthy", 
                              "Hazardous", "Beyond index")),
         sample_time = mdy_hm(sample_time)) %>% 
  filter(value >0)



beijing_pm %>% 
  mutate(sample_time = mdy_hm(sample_time))

mean(beijing_pm$value)

beijing_pm %>% 
  pull(value) %>% 
  mean()

beijing_pm %>% 
  summarize(min_pm = min(value),
            mean_pm = mean(value),
            max_pm = max(value))

beijing_pm %>% 
  group_by(aqi) %>% 
  count()
  
beijing_pm %>% 
  group_by(aqi) %>% 
  summarize(mean_pm = mean(value),
            min_pm = min(value),
            count = n())

beijing_pm %>% 
  mutate(beyone_index = value>500)

beijing_pm <- beijing_pm %>% 
  mutate(heating = sample_time< ymd("2017-03-13"))

ggplot(beijing_pm)+
  geom_point(aes(sample_time,value),
       color = "blue")
  
ggplot(data = beijing_pm) + 
  geom_histogram(aes(value))
  


data("worldcup")
ggplot(worldcup, mapping = aes(x = Time,y = Passes, color = Position))+
  geom_point()+
  geom_rug()

worldcup %>% 
  rownames_to_column(var = "Name") %>% 
  filter(Team %in% c("Spain", "Netherlands", "Germany", "Uruguay")) %>%
  ggplot(worldcup, mapping = aes(x = Time,y = Passes, color = Position, shape = Team),
         color =)+
  geom_point()+
  geom_rug()
  
worldcup %>% 
  rownames_to_column(var = "Name") %>% 
  ggplot(worldcup, mapping = aes(x = Time,y = Passes))+
  geom_point(color = "blue", alpha = 0.5)+
  ggtitle("World Cup Time vs Passes", subtitle = "2010")+
  labs(x ="Time (minutes)",y="Number of Passes")



  

  

  