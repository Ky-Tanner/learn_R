library("dplyr")
library("ggplot2")
library("gridExtra")
library("ggthemes")
library("lubridate")
library("faraway")
library("dlnm")

data("worldcup")
data("chicagoNMMAPS")

chic <- chicagoNMMAPS

chic_july <- chic %>% 
  filter(year == 1995 & month == 7)

ggplot(data = chic_july, aes(x = date, y= death)) +
  geom_point(color = "red") +
  geom_text(data = hottest_day,label = "Max",
            nudge_x = 1, nudge_y = 5)

hottest_day <- chic_july %>% 
  filter(temp == max(temp))


worldcup %>% 
  ggplot(aes(Passes,Shots)) +
  geom_point() +
  geom_smooth(method = lm) + 
  theme_few()


ggplot()+
  geom_text(data = hottest_day, aes(x=date,y=death,label = temp))


worldcup %>% 
  filter(Team %in% c("Spain", "Netherlands", "Germany", "Uruguay")) %>% 
  ggplot(aes(x=Time,fill=Team)) +
  geom_histogram()

worldcup %>% 
  ggplot(aes(x=Time,y=Shots,color=Position)) +
  geom_point(alpha=.3, size = 5) +
  geom_smooth(method="lm")
  

worldcup <- worldcup %>%
  tibble::rownames_to_column(var = "Player")

worldcup <- worldcup %>% 
  mutate(top_four = Team %in% c("Netherlands", "Uruguay", "Spain", "Germany"),
         Position = factor(Position, levels = c("Goalkeeper", "Defender","Midfielder", "Forward")))

#new lecture
top_shooter <- worldcup %>% 
  filter(Shots == max(Shots)) %>% 
  mutate(Player_team = paste(Player, Team, sep = ", "))

plot1 <- ggplot(worldcup, aes(Time, Shots)) +
  geom_point(aes(color = top_four), alpha = .5, size = 2)+
  theme_few() +
  labs(x = "Time played in World Cup (minutes)",
       y = "Shots", color = "Team's final \n ranking") +
  geom_vline(aes(xintercept = 270), linetype = 2, color = "gray") + 
  geom_text(data = top_shooter, aes(label = Player_team),hjust = 1.1) +
  facet_wrap(~ Position, ncol = 4)

plot1 +
  geom_hline(aes(yintercept = 10))
