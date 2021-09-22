library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

all <- read_csv('derived_data/all_transformed.csv',
                col_types = cols(
                  state = col_character(),
                  year = col_double(),
                  farm_knumber = col_double(),
                  farm_kha = col_double(),
                  farm_msales = col_double()
                ))


all_long <- all %>% 
  tidyr::pivot_longer(cols = farm_knumber:farm_msales, 
                      names_to = 'metric', 
                      values_to = 'value')

# ggplot(data = all_long %>% filter(!is.na(value))) +
#   geom_histogram(bins = 100, aes(value)) +
#   facet_wrap(~metric, scales = 'free_x', ncol = 1) +
#   scale_x_log10()
# 
# ggplot(data = all_long %>% filter(metric == 'farm_knumber'), 
#        aes(year, value)) +
#   geom_point() +
#   geom_line()+
#   facet_wrap(~state) +
#   ylab('farm k number') +
#   scale_y_log10()
# 
# ggplot(data = all_long %>% filter(metric == 'farm_kha'), 
#        aes(year, value)) +
#   geom_point() +
#   geom_line()+
#   facet_wrap(~state) +
#   ylab('farm kha') +
#   scale_y_log10()
# 
ggplot(data = all_long %>% filter(metric == 'farm_msales'),
       aes(year, value)) +
  geom_point() +
  geom_line()+
  facet_wrap(~state) +
  ylab('farm sales (m$)') +
  scale_y_log10()

totals <- all_long %>% group_by(year, metric) %>%
  summarize(us = sum(value, na.rm = TRUE),
            us_50 = median(value, na.rm = TRUE),
            us_75 = quantile(value, 0.75, na.rm = TRUE),
            us_25 = quantile(value, 0.25, na.rm = TRUE))
# 
ggplot(data = totals, aes(year, us)) +
  geom_point() +
  geom_line()+
  facet_wrap(~metric, scales = 'free_y', ncol = 1)+
  scale_x_log10()

ggplot(data = totals, aes(year, us_50)) +
  geom_point() +
  geom_line() +
  geom_line(aes(y = us_25), alpha = 0.5)+
  geom_line(aes(y = us_75), alpha = 0.5)+
  facet_wrap(~metric, scales = 'free_y', ncol = 1)


### Modeling

## LM multivariate
mod_lm <- lm(cbind(log(farm_knumber), log(farm_kha), log(farm_msales)) ~ state * year, data = all)
summary(mod_lm)


library(statebins)
all_long <- all %>% pivot_longer(cols = c('farm_knumber', 'farm_msales', 'farm_kha'))

ggplot(data = all_long %>% filter(year == 2019 & name == 'farm_knumber')) + 
  geom_statebins(aes(state = state, fill = value * 1000)) +
  scale_fill_binned(name = "Farm\nNumber", trans = 'log', breaks = c(1, 50, 100, 500, 1000), low = 'white', high = 'black') +
#  facet_wrap(~name, scales = 'free') +
  theme_nothing() +
  theme(legend.position = 'right') +
  labs(fill = 'Number of Farms') 
  

ggplot(data = all_long %>% filter(name == 'farm_knumber')) + 
  geom_statebins(aes(state = state, fill = value * 1000)) +
  scale_fill_binned(name = "Farm\nNumber", trans = 'log', breaks = c(10, 50, 100, 500, 1000), low = 'white', high = 'black') +
  #  facet_wrap(~name, scales = 'free') +
  theme_nothing() +
  theme(legend.position = 'right') +
  labs(fill = 'Number of Farms') +
  facet_wrap(~year)


# library 
library(usmap)


us_states <- map_data("state")
plot_usmap(data = all %>% filter(year == 2019) %>% mutate(number = farm_knumber * 1000), 
           values = 'number', color = 'white') + 
  scale_fill_binned(name = "Farm\nNumber", trans = 'log', breaks = c(10, 50, 100, 500, 1000), low = 'white', high = 'black') +
  theme(legend.position = 'right')

library(geofacet)
s <- cbind(abb = state.abb, state = state.name)
all <- all %>% left_join(s, by = 'state', copy = TRUE)

ggplot(all %>% mutate(year = lubridate::ymd(year, truncated = 2L), 
                      number = farm_knumber * 1000), 
       aes(year, number)) +
  geom_line() +
  geom_point(aes(color = number)) +
  facet_geo(~ abb, grid = "us_state_grid1") +
  scale_x_date(guide = guide_axis(angle = 90),
               labels = NULL) + 
#               breaks = lubridate::ymd(c(2008, 2011, 2014, 2015, 2016, 2019), truncated = 2L)) +
  scale_y_log10(breaks = c(10, 100, 1000)) + 
  theme_minimal() +
  theme(legend.position = 'right') +
  scale_color_binned(name = "Farm\nNumber", trans = 'log', breaks = c(10, 50, 100, 500, 1000), low = 'white', high = 'black') +
  xlab("") + ylab("") +
  theme(axis.line = element_blank(),
        plot.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color='black', fill = NULL)#element_blank()
        )


## Bump Chart - ranks
## https://github.com/davidsjoberg/ggbump
## devtools::install_github("davidsjoberg/ggbump")


library(ggbump)
library(wesanderson)
ggplot(data = all) + 
  geom_bump(aes(year, farm_kha, color = state))

df <- all_long %>% 
  group_by(year, name) %>% 
  mutate(rank = rank(1/value, ties.method = "random"), pos = rank %% 3 - 1) %>% 
  ungroup() %>% 
  group_by(name, state) %>% 
  mutate(minrank = min(rank)) %>%
  ungroup() %>% 
  filter(minrank <= 10 & state != 'Alaska')


ggplot(data = df , aes(year, rank, color = state)) + 
  geom_point() + 
  geom_bump() + 
  geom_text(data = df %>% filter(year == min(year)),
            aes(x = year - 1, label = abb), size = 3, hjust = 1) +
  geom_text(data = df %>% filter(year == max(year)),
            aes(x = year + 1, label = abb), size = 3, hjust = 0) +
  theme_minimal_grid(font_size = 10, line_size = 0) + 
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  scale_x_continuous(limits = c(2005, 2022),
                     breaks = c(2008, 2011, 2014, 2015, 2016, 2019)) +
  labs(y = "RANK",
       x = NULL) +
  scale_y_reverse() +
  scale_color_manual(values = wes_palette(n = 50, name = "BottleRocket2", type = "continuous")) +
  facet_wrap(~name)


ggplot(data = df , aes(year, value, color = state)) + 
  geom_point() + 
  geom_bump() + 
  geom_text(data = df %>% filter(year == min(year)),
            aes(x = year - 1 - pos, label = abb), size = 3, hjust = 1) +
  geom_text(data = df %>% filter(year == max(year)),
            aes(x = year + 1 + pos/2, label = abb), size = 3, hjust = 0) +
  theme_minimal_grid(font_size = 10, line_size = 0) + 
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  scale_x_continuous(limits = c(2005, 2022),
                     breaks = c(2008, 2011, 2014, 2015, 2016, 2019)) +
  labs(y = NULL,
       x = NULL) +
  scale_y_log10() +
  scale_color_manual(values = wes_palette(n = 50, name = "BottleRocket2", type = "continuous")) +
  facet_wrap(~name, scales = 'free_y')
