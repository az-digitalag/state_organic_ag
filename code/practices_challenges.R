practices <- readr::read_csv('raw_data/practices.csv') %>% 
  select(-X4)

z <- practices %>% mutate(factor = gsub("\\ \\(farms\\)", "", factor))

library(ggplot2)
library(ggbump)
library(wesanderson)
library(ggrepel)
library(cowplot)
library(dplyr)
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
  scale_x_continuous(limits = c(2007, 2017),
                     breaks = c(2007, 2012, 2017)) +
  scale_y_continuous(position = 'right', name = "") +
  scale_color_manual(values = wes_palette(n = 11, name = "FantasticFox1", type = "continuous")) +
  ggtitle("Number of farms reporting having adopted the practice")
  


a <- aov(number ~ factor * year, data = practices)
a <- lm(number ~ factor * year, data = practices)
broom::tidy(a)
broom::tidy(summary(a))
broom::augment(a)

library(lme4)
almer <- lmer(number ~ factor * year + (1|year), data = practices)

library(nlme)
alme <- lme(number ~ factor * year, random = ~ 1|year, data = practices)
knitr::kable(tidy(alme))


challenges <- readr::read_csv('raw_data/challenges.csv') %>% 
  select(-X4:-X5)

b <- lm(number ~ factor * year, data = challenges)

broom::tidy(b)
