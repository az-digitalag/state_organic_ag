
library(dplyr)
library(tidyr)
library(ggplot2)
preds <- readr::read_csv('code/BUGS/mod3/predicted_3a.csv') %>% 
  mutate(value = case_when(is.na(obs) ~ pred.mean,
                           !is.na(obs)~ obs),
         year = as.factor(Year))

wide_preds <- preds %>% 
  arrange(year, state) %>% 
  select(state, year, type, obs) %>%
  pivot_wider(names_from = type, values_from = obs) %>% 
  select(state, year, farm_kha, farm_knumber, farm_msales)

ggplot(data = preds, aes(obs, pred.mean)) +
  geom_point(aes(color = year)) +
  geom_abline(slope = 1)+
  facet_wrap(~type, scales = 'free')

ggplot(data = preds, aes(obs, pred.median)) +
  geom_point(aes(color = year)) +
  geom_abline(slope = 1)+
  facet_wrap(~type, scales = 'free')

ggplot(data = preds, aes(pred.mean, pred.median)) +
  geom_point(aes(color = year)) +
  geom_abline(slope = 1)+
  facet_wrap(~type, scales = 'free')

pred_wide <- wide_preds  %>% 
  pivot_wider(names_from = year, values_from = starts_with('farm'))

imputed <- preds %>% 
  arrange(year, state) %>% 
  select(state, year, type, value) %>%
  pivot_wider(names_from = type, values_from = value) %>% 
  select(state, year, farm_kha, farm_knumber, farm_msales) 
imputed_wide <- imputed %>% 
  pivot_wider(names_from = year, values_from = starts_with('farm'))
readr::write_csv(imputed_wide, 'derived_data/imputed_wide.csv')

params <- readr::read_csv('code/BUGS/mod3/params_3a.csv')
slopes <- params %>% filter(Parameter == 'slope') %>% 
  select(state = State, variable = Variable, slope = mean, significant) %>% 
  pivot_wider(names_from = variable, values_from = c(slope, significant)) 

readr::write_csv(slopes, 'derived_data/slopes.csv')
