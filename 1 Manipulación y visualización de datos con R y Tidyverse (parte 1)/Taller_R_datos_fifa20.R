library(tidyverse)
library(ggrepel)
library(skimr)



datos <- read_csv("players_20.csv")

head(datos)
glimpse(datos)
skim(datos)

names(datos)





datos %>% 
  filter(nationality == "Chile") %>% 
  ggplot(aes(age, overall, color = international_reputation > 2)) +
  geom_jitter(alpha=.5, size=2.5)



datos %>% 
  filter(nationality == "Chile") %>% 
  ggplot(aes(age, overall, color = international_reputation > 1)) +
  geom_jitter(alpha=.5, size=2.5) +
  geom_label(data = datos %>% 
               filter(international_reputation > 2, nationality == "Chile"), 
             aes(label=short_name),
             size=3) +
  theme_minimal() +
  theme(legend.position = "none")



datos %>% 
  filter(nationality == "Chile") %>% 
  ggplot(aes(potential, overall, color = age <= 21)) +
  geom_jitter(alpha=.5, size=2.5) +
  geom_label(data = datos %>% 
                   filter(age <= 21, nationality == "Chile", potential >=80), 
                   aes(label=short_name),
                   size=3) +
  theme_minimal() +
  theme(legend.position = "none")




library(ggrepel)

datos %>% 
  filter(nationality == "France") %>% 
  ggplot(aes(potential, overall, color= age <= 21)) +
  geom_jitter(alpha=.5, size=2.5) +
  geom_label_repel(data = datos %>% 
                      filter(age <= 21, potential >= 80, nationality == "France"), 
                   aes(label=paste(short_name, "\n", club)),
                   size=2) +
  theme_minimal()




datos %>% 
  filter(overall>85) %>% 
ggplot(aes(power_shot_power, attacking_finishing, label = short_name, color = preferred_foot))+
  geom_text_repel(size=2)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_jitter(alpha = 0.3, size = 2.5, width = 0.3, height = 0.3)+
  geom_smooth(method = "lm", color = "gray40", lty = 2, se = FALSE, size = 0.6)+
  scale_color_manual(values = c("orangered","steelblue"))
