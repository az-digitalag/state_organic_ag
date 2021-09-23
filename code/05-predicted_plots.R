# National totals calculated with imputed data
library(dplyr)
library(tidyr)
library(ggplot2)
library(usmap)

# Input (raw) data
# summarize to national totals
raw_sum <- readr::read_csv('derived_data/all_transformed.csv') %>% 
  group_by(year) %>%
  summarize(tot_kha = sum(farm_kha, na.rm = TRUE),
            kha_n = sum(!is.na(farm_kha)),
            tot_knumber = sum(farm_knumber, na.rm = TRUE),
            knumber_n = sum(!is.na(farm_knumber)),
            tot_msales = sum(farm_msales, na.rm = TRUE),
            msales_n = sum(!is.na(farm_msales)),)

# long
vars <- data.frame(metric = c('area', 'sales', 'number'), label = factor(c("area~(kha~yr^-1)", "number~(k~yr^-1)", "sales~(MM~yr^-1)")))

raw_long <- raw_sum %>% 
  mutate(year = lubridate::ymd(year, truncated = 2L)) %>% 
  mutate(area = tot_kha, number = tot_knumber * 1000, sales = tot_msales) %>%
  select(-tot_kha:-msales_n) %>% 
  tidyr::pivot_longer(cols = area:sales, 
                      names_to = 'metric', 
                      values_to = 'value') %>% 
  group_by(year, metric) %>% 
  summarise(total = sum(value)) %>% 
  left_join(vars)

# Imputed data
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


imputed_long <- imputed %>% 
  tidyr::pivot_longer(cols = farm_kha:farm_msales, 
                      names_to = 'metric', 
                      values_to = 'value')

readr::write_csv(imputed_long, 'derived_data/imputed_long.csv')
## Table 1, Figure 1
vars <- data.frame(metric = c('area', 'sales', 'number'), label = factor(c("area~(kha~yr^-1)", "number~(k~yr^-1)", "sales~(MM~yr^-1)")))

imp_summary <- imputed %>% 
  mutate(year = lubridate::ymd(year, truncated = 2L)) %>% 
  mutate(area = farm_kha, number = farm_knumber * 1000, sales = farm_msales) %>%
  select(-farm_kha:-farm_msales) %>% 
  tidyr::pivot_longer(cols = area:sales, 
                      names_to = 'metric', 
                      values_to = 'value') %>% 
  group_by(year, metric) %>% 
  summarise(total = sum(value)) %>% 
  left_join(vars)


16476 %>% 
  pivot_wider(id_cols = metric, names_from = year, values_from = total )  %>% 
  knitr::kable()

plot_names <- as_labeller(c(area = "Area~(10^3~ha)", 
                            number = "Farm~Number", 
                            sales = "Sales~(million~'USD in 2020')"), 
                          default = label_parsed)

fig1 <- ggplot() + 
  geom_point(data = imp_summary, 
             aes(year, total, group = metric)) + 
  # geom_point(data = raw_long, 
  #            aes(year, total, group = metric, color = "raw")) + 
  geom_line(data = imp_summary, 
            aes(year, total, group = metric)) + 
  facet_wrap(~metric, scales = 'free_y', ncol = 3, labeller = plot_names) +
  theme_minimal() +
  scale_y_continuous("National total",
                     limits = c(0, NA), labels = scales::comma) + 
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        panel.grid.minor = element_blank())
ggsave(filename = "figures/Fig1_imputed_natl_sum.png",
       plot = fig1,
       device = "png",
       height = 2,
       width = 8,
       units = "in")

imputed_long %>% 
  group_by(year, metric) %>% 
  summarise(total = sum(value))

## Imputed data map, total 

imp_summary

us_states <- map_data("state")
fig0 <- plot_usmap(data = imputed %>% filter(year == 2019) %>% mutate(number = farm_knumber * 1000), 
           values = 'number', color = 'gray') + 
  scale_fill_binned(name = "Farm\nNumber", trans = 'log', breaks = c(10, 50, 100, 500, 1000), low = 'white', high = 'black') +
  ggtitle("2019 US total: 16,476") +
  theme(legend.position = 'right') 

ggsave(filename = "figures/Fig0_2019number_bystate.png",
       plot = fig0,
       device = "png",
       height = 4,
       width = 7,
       units = "in")
