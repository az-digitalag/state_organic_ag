## Figures for practices and challenges
library(ggplot2)
library(ggbump)
library(wesanderson)
library(ggrepel)
library(cowplot)
library(dplyr)


### Proportional and raw trends in practices
practices <- readr::read_csv('raw_data/practices.csv') %>% 
  select(-4) %>% 
  mutate(year = case_when(year == 2007 ~ 2008,
                          year == 2012 ~ 2014, 
                          year == 2017 ~ 2019 ),
         percent = case_when(year == 2008 ~ number / 10903 * 100,
                             year == 2014 ~ number / 12595 * 100,
                             year == 2019 ~ number / 16476 * 100),
         practice = case_when(factor == "Maintained beneficial insect/vertebrate habitat (farms)" ~ "Beneficial habitat",
                              factor == "Released beneficial organisms (farms)" ~ "Beneficial organisms",
                              factor == "Used no-till or minimum till (farms)" ~ "No/minimum till",
                              factor == "Used water management practices (farms)" ~ "Water management",
                              factor == "Selected planting locations to avoid pests (farms)" ~ "Location (avoid pests)",
                              factor == "Chose pest resistant varieties (farms)" ~ "Variety (resist pests)",
                              factor == "Planned plantings to avoid cross- contamination (farms)" ~ "Planning (avoid contaminants)",
                              factor == "Maintained buffer strips (farms)" ~ "Buffer strips",
                              factor == "Produced or used organic mulch/compost (farms)" ~ "Organic mulch/compost",
                              factor == "Used green or animal manures (farms)" ~ "Green/animal manures",
                              factor == "Practiced rotational grazing (farms)" ~ "Rotational grazing")
  )

fig4a <- ggplot(data = practices, 
                aes(year, number, color = practice)) +
  geom_point() + 
  geom_bump() +   
  geom_text_repel(data = practices %>% filter(year == min(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
                  aes(x = year, hjust = 0, label = practice), 
                  size = 3.5, direction = 'y') +
  #  geom_text(data = practices %>% filter(year == max(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
  #                  aes(x = year + 0.1, label = factor), 
  #                  size = 3) +
  theme_minimal_grid(font_size = 12, line_size = 1) + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  scale_x_continuous(limits = c(2008, 2019),
                     breaks = c(2008, 2014, 2019)) +
  scale_y_continuous(position = 'right', name = "", 
                     breaks = seq(0, 15000, 5000),
                     labels = seq(0, 15000, 5000)) +
  scale_color_manual(values = wes_palette(n = 11, name = "FantasticFox1", type = "continuous")) +
  ggtitle("# of farms adopted practice")

fig4b <- ggplot(data = practices, 
       aes(year, percent, color = practice)) +
  geom_point() + 
  geom_bump() +   
  geom_text_repel(data = practices %>% filter(year == min(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
                            aes(x = year, hjust = 0, label = practice), 
            size = 3.5, direction = 'y') +
#  geom_text(data = practices %>% filter(year == max(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
#                  aes(x = year + 0.1, label = factor), 
#                  size = 3) +
  theme_minimal_grid(font_size = 12, line_size = 1) + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  scale_x_continuous(limits = c(2008, 2019),
                     breaks = c(2008, 2014, 2019)) +
  scale_y_continuous(position = 'right', name = "", 
                     breaks = seq(0, 100, 25),
                     labels = seq(0, 100, 25)) +
  scale_color_manual(values = wes_palette(n = 11, name = "FantasticFox1", type = "continuous")) +
  ggtitle("% of farms adopted practice")
  


fig4 <- plot_grid(fig4a, fig4b, ncol = 2)

ggsave(filename = "figures/Fig4_practices.png",
       plot = fig4,
       device = "png",
       height = 6,
       width = 6,
       units = "in",
       dpi = 600)

# broom::tidy(aov(percent ~ factor * year, data = practices))
# broom::tidy(aov(number ~ factor * year, data = practices))
# a <- lm(percent ~ factor * year, data = practices)
# broom::tidy(a) %>% knitr::kable()
# broom::tidy(summary(a))
# broom::augment(a)
# 
# library(lme4)
# almer <- lmer(number ~ factor * year + (1|year), data = practices)
# 
# library(nlme)
# alme <- lme(number ~ factor * year, random = ~ 1|year, data = practices)
# knitr::kable(tidy(alme))

### Proportional and raw trends in challenges
challenges <- readr::read_csv('raw_data/challenges.csv') %>% 
  select(-4:-5) %>% 
  mutate(year = case_when(year == 2007 ~ 2008,
                          year == 2012 ~ 2014, 
                          year == 2017 ~ 2019 ),
         percent = case_when(year == 2008 ~ number / 10903 * 100,
                             year == 2014 ~ number / 12595 * 100,
                             year == 2019 ~ number / 16476 * 100),
         )

# b <- lm(number ~ factor * year, data = challenges)
# 
# broom::tidy(b)

fig5a <- ggplot(data = challenges, 
                aes(year, number, color = factor)) +
  geom_point() + 
  geom_bump() +   
  geom_text_repel(data = challenges %>% filter(year == median(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
                  aes(x = year, hjust = 0.5, label = factor), 
                  size = 4, direction = 'y') +
  #  geom_text(data = challenges %>% filter(year == max(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
  #                  aes(x = year + 0.1, label = factor), 
  #                  size = 3) +
  theme_minimal_grid(font_size = 12, line_size = 1) + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  scale_x_continuous(limits = c(2008, 2019),
                     breaks = c(2008, 2014, 2019)) +
  scale_y_continuous(position = 'right', name = "", 
                     breaks = seq(0, 8000, 2000),
                     labels = seq(0, 8000, 2000)) +
  scale_color_manual(values = wes_palette(n = 11, name = "FantasticFox1", type = "continuous")) +
  ggtitle("# of farms faced challenge")

fig5b <- ggplot(data = challenges, 
                aes(year, percent, color = factor)) +
  geom_point() + 
  geom_bump() +   
  geom_text_repel(data = challenges %>% filter(year == median(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
                  aes(x = year, hjust = 0.5, label = factor), 
                  size = 4, direction = 'y') +
  #  geom_text(data = challenges %>% filter(year == max(year)) %>% mutate(rank = rank(-number), pos = rank %% 2),
  #                  aes(x = year + 0.1, label = factor), 
  #                  size = 3) +
  
  theme_minimal_grid(font_size = 12, line_size = 1) + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray75", size = 0.2)) +
  scale_x_continuous(limits = c(2008, 2019),
                     breaks = c(2008, 2014, 2019)) +
  scale_y_continuous(position = 'right', name = "", 
                     breaks = seq(0, 50, 10),
                     labels = seq(0, 50, 10)) +
  scale_color_manual(values = wes_palette(n = 11, name = "FantasticFox1", type = "continuous")) +
  ggtitle("% of farms faced challenge")



fig5 <- plot_grid(fig5a, fig5b, ncol = 2)

ggsave(filename = "figures/Fig5_challenges.png",
       plot = fig5,
       device = "png",
       height = 6,
       width = 6,
       units = "in",
       dpi = 600)
