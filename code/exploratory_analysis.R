library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

x <- readr::read_csv('raw_data/USA organic agriculture state data in farms_landarea_sales value.csv', 
                     col_types = cols(
                       year = col_integer(),
                       state = col_character(),
                       farm_number = col_number(),
                       farm_ha = col_number(),
                       sales = col_number()
                     )) %>% 
  drop_na()


# drop states w/ < 2 years

x2 <- x %>% group_by(state) %>% 
  summarise(n = n()) %>% 
  filter(n < 3) 

x3 <- x %>% filter(!state %in% x2$state) %>% 
  mutate(farm_knumber = farm_number/1000,
         farm_kha = farm_ha/1000,
         farm_msales = sales/1000000) %>% 
  select(!farm_number:sales) 

x4 <- x3 %>% 
  tidyr::pivot_longer(cols = farm_knumber:farm_msales, names_to = 'metric', values_to = 'value')

ggplot(data = x3) +
  geom_histogram(breaks = 20) +
  facet_wrap(~metric, scales = 'free_x', ncol = 1) 

ggplot(data = x4, aes(year, value, color = state)) + 
  geom_point() +
  geom_line()+
  facet_wrap(~metric, scales = 'free_y', ncol = 1) +
  scale_y_log10()


totals <- x4 %>% group_by(year, metric) %>% 
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


### Modeling

## LM multivariate
# 
# mod_lm <- lm(cbind(farm_knumber, farm_kha, farm_msales) ~ state + year, data = x3)

## LMER multivariate
# library(lme4)
# mod_lmermv <- lmer(cbind(farm_knumber, farm_kha, farm_msales) ~ year * state + (1 + year | state),
#          data = x3)

# https://m-clark.github.io/mixed-models-with-R/bayesian.html

## BRMS
## full multivariate using brms
# library(brms)
# mod_mvbrm <- brm(mvbind(farm_knumber, farm_kha, farm_msales) ~ 
#            year * state + (1 + year | state),
#          data = x3,
#          chains = 4,
#          family = lognormal(link = "identity")
#          )


library(rstanarm)
## is this the appropriate way to model the y 
## values that all seem log-normally distributed?
system.time(
mod_stanmvmer<- stan_mvmer(
  formula = list(farm_knumber ~ year * state + (1 + year | state),
                 farm_kha ~ year * state + (1 + year | state),
                 farm_msales ~ year * state + (1 + year | state)),
  data = x3,
  family = gaussian(link = "log"),
  # short test:
  # chains = 1, cores = 1, seed = 12345, iter = 1000)
  chains = 4, cores = 4, seed = 12345, iter = 2000)
)
summary(mod_stanmvmer)



####)

## univariate
#mod_stanlmer <- stan_lmer(farm_kha ~ year * state + (1 + year | state),
#               data = x3)

broom.mixed::tidyMCMC(mod_stanmvmer)
#plot(broom.mixed::tidyMCMC(mod_stanlmer))

newdata <- data.frame(state = unique(x3$state), 
  year = rep(c(2007, 2012, 2017, 2022, 2027, 2032),
             each= length(unique(x3$state))))

pp_farm_knumber <- cbind(newdata, t(posterior_predict(mod_stanmvmer, m = 1, newdata = newdata))) %>% 
  tidyr::pivot_longer(names_to = 'sample', values_to = 'farm_knumber', cols = 3:4002)
pp_farm_kha <- cbind(newdata, t(posterior_predict(mod_stanmvmer, m = 2, newdata = newdata))) %>% 
  tidyr::pivot_longer(names_to = 'sample', values_to = 'farm_kha', cols = 3:4002)
pp_farm_msales <- cbind(newdata, t(posterior_predict(mod_stanmvmer, m = 3, newdata = newdata))) %>% 
  tidyr::pivot_longer(names_to = 'sample', values_to = 'farm_msales', cols = 3:4002)

pp <- pp_farm_knumber %>% left_join(pp_farm_kha) %>% 
  left_join(pp_farm_msales) %>% 
  mutate(date = lubridate::ymd(paste0(year, '-01-01'))) %>% 
  pivot_longer(cols = farm_knumber:farm_msales, 
               names_to = 'name', 
               values_to = 'value')
# estimate pp for US (sum across states)
pp_us <- pp %>% 
  group_by(year, date, sample, name) %>%
  summarise(value = sum(value))
  
  
x4 <- x3 %>% mutate(date = lubridate::ymd(paste0(year, '-01-01'))) %>% 
  pivot_longer(cols = farm_knumber:farm_msales, 
               names_to = 'name', 
               values_to = 'value')

x4_us <- x4 %>%
  group_by(year, date, name) %>% 
  summarise(value = sum(value))

ggplot() + 
  geom_boxplot(data = pp_us %>% filter(year > 2020), aes(date, value, group = year), color = 'blue', outlier.shape = NA, width = 200) + 
  geom_quantile(data = pp_us, aes(date, value)) + 
  facet_wrap(~name, scales = 'free_y', ncol = 1) +
  geom_point(data = x4_us, aes(date, value)) +
  geom_line(data = x4_us, aes(date, value)) +
  #scale_y_log10() + 
  theme_bw() + xlab('year') + ylab('')

pp_summary <- pp_us %>% filter(year > 2020) %>% 
  group_by(year, name) %>% 
  summarise(mean = mean(value), median = median(value), sd = sd(value), 
            lcl_90 = quantile(value, 0.05),
            ucl_90 = quantile(value, 0.95))
write_csv(pp_summary, 'derived_data/pp_summary.csv')  
