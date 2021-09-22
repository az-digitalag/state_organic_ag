practices <- readr::read_csv('raw_data/practices.csv') %>% 
  select(-X4)


practices <- practices %>% 
  mutate(year = case_when(year == 2007 ~ 2008,
                          year == 2012 ~ 2014, 
                          year == 2017 ~ 2019 ),
         percent = case_when(year == 2008 ~ number / 10903,
                             year == 2014 ~ number / 12595,
                             year == 2019 ~ number / 16476)
  )
z <- practices %>% mutate(factor = gsub("\\ \\(farms\\)", "", factor)) 

library(ggplot2)
library(ggbump)
library(wesanderson)
library(ggrepel)
library(cowplot)
library(dplyr)
ggplot(data = z %>% mutate(factor = gsub("\\ \\(farms\\)", "", factor)), 
       aes(year, percent, color = factor)) +
  geom_point() + 
  geom_bump() +   
  geom_text_repel(data = z %>% filter(year == min(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
                            aes(x = year, hjust = 0, label = factor), 
            size = 3, direction = 'y') +
#  geom_text(data = z %>% filter(year == max(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
#                  aes(x = year + 0.1, label = factor), 
#                  size = 3) +
  
  theme_minimal_grid(font_size = 10, line_size = 1) + 
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  scale_x_continuous(limits = c(2008, 2019),
                     breaks = c(2008, 2014, 2019)) +
  scale_y_continuous(position = 'right', name = "") +
  scale_color_manual(values = wes_palette(n = 11, name = "FantasticFox1", type = "continuous")) +
  ggtitle("percent of farms reporting having adopted the practice")
  
ggplot(data = z %>% mutate(factor = gsub("\\ \\(farms\\)", "", factor)), 
       aes(year, number, color = factor)) +
  geom_point() + 
  geom_bump() +   
  geom_text_repel(data = z %>% filter(year == min(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
                  aes(x = year, hjust = 0, label = factor), 
                  size = 3, direction = 'y') +
  #  geom_text(data = z %>% filter(year == max(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
  #                  aes(x = year + 0.1, label = factor), 
  #                  size = 3) +
  
  theme_minimal_grid(font_size = 10, line_size = 1) + 
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  scale_x_continuous(limits = c(2008, 2019),
                     breaks = c(2008, 2014, 2019)) +
  scale_y_continuous(position = 'right', name = "") +
  scale_color_manual(values = wes_palette(n = 11, name = "FantasticFox1", type = "continuous")) +
  ggtitle("number of farms reporting having adopted the practice")


broom::tidy(aov(percent ~ factor * year, data = practices))
broom::tidy(aov(number ~ factor * year, data = practices))
a <- lm(percent ~ factor * year, data = practices)
broom::tidy(a) %>% knitr::kable()
broom::tidy(summary(a))
broom::augment(a)

library(lme4)
almer <- lmer(number ~ factor * year + (1|year), data = practices)

library(nlme)
alme <- lme(number ~ factor * year, random = ~ 1|year, data = practices)
knitr::kable(tidy(alme))


challenges <- readr::read_csv('raw_data/challenges.csv') %>% 
  select(-X4:-X5) %>% 
  mutate(year = case_when(year == 2007 ~ 2008,
                          year == 2012 ~ 2014, 
                          year == 2017 ~ 2019 ),
         percent = case_when(year == 2008 ~ number / 10903,
                             year == 2014 ~ number / 12595,
                             year == 2019 ~ number / 16476))

b <- lm(number ~ factor * year, data = challenges)

broom::tidy(b)
