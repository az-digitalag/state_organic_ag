library(forecast)
library(tidyr)

x <- readr::read_csv('../USA organic agriculture state data in farms_landarea_sales value.csv', 
                     col_types = cols(
                       year = col_integer(),
                       state = col_character(),
                       farm_number = col_number(),
                       farm_ha = col_number(),
                       sales = col_number()
                     )) 
  

library(ggplot2)

y <- x %>% 
  mutate(sales_k = sales/1000,
         farm_kha = farm_ha/1000,
         farm_msales = sales/1000000) %>% 
  tidyr::pivot_longer(cols = farm_number:sales, names_to = 'metric', values_to = 'value')

ggplot(data = y, aes(year, value, color = state)) + 
  geom_point() +
  geom_line()+
  facet_wrap(~metric, scales = 'free_y', ncol = 1) +
  scale_y_log10()


totals <- y %>% group_by(year, metric) %>% 
  summarize(us = sum(value, na.rm = TRUE),
            us_50 = median(value, na.rm = TRUE),
            us_75 = quantile(value, 0.75, na.rm = TRUE),
            us_25 = quantile(value, 0.25, na.rm = TRUE))

ggplot(data = totals, aes(year, us)) + 
  geom_point() +
  geom_line()+
  facet_wrap(~metric, scales = 'free_y', ncol = 1)

ggplot(data = totals, aes(year, us_50)) + 
  geom_point() +
  geom_line() + 
  geom_line(aes(y = us_25), alpha = 0.5)+
  geom_line(aes(y = us_75), alpha = 0.5)+
  facet_wrap(~metric, scales = 'free_y', ncol = 1)
