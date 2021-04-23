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
# 
mod_lm <- lm(cbind(log(farm_knumber), log(farm_kha), log(farm_msales)) ~ state * year, data = all)
summary(mod_lm)
