library(dplyr)
library(tidyr)
library(readr)


if(grepl('state_organic_ag/code$', getwd())) setwd('../')
   
all <- readr::read_csv('derived_data/all_transformed.csv',
                col_types = cols(
                  state = col_character(),
                  year = col_double(),
                  farm_knumber = col_double(),
                  farm_kha = col_double(),
                  farm_msales = col_double()
                ))



## LMER multivariate


all_complete <-  all[complete.cases(all),]
# m1 <- rstanarm::stan_glmer(farm_knumber ~ year + (1 + year | state),
#                            data = all_complete,
#                            family = gaussian(link = "identity")
# )
# mod_lm <- lm(cbind(farm_knumber, farm_kha, farm_msales) ~ year * state, data = all_complete)
# library(lme4)
# mod_lmermv <- lme4::lmer(cbind(farm_knumber, farm_kha, farm_msales) ~ year + (1 + year | state),
#                           data = all_complete
# )

# https://m-clark.github.io/mixed-models-with-R/bayesian.html

## BRMS
## full multivariate using brms
library(brms)
# handle missing data using `|mi()` ?
# farm_kha: 2 NA's; farm_msales: 9 NA's
# https://m-clark.github.io/easy-bayes/brms-mo-models.html

brms_formula <-  
  bf(farm_knumber  ~ 1 + year) + 
  bf(farm_kha|mi()  ~ 1 + year) +
  bf(farm_msales|mi()  ~ 1 + year) +
  set_rescor(TRUE)

brms_formula_re_state <-  
  bf(farm_knumber  ~ 1 + year + (1 | state)) + 
  bf(farm_kha|mi()  ~ 1 + year + (1 | state)) +
  bf(farm_msales|mi()  ~ 1 + year + (1 | state)) +
  set_rescor(TRUE)

brms_formula_re_yr_state <-  
  bf(farm_knumber  ~ 1 + year + (1 + year | state)) + 
  bf(farm_kha|mi()  ~ 1 + year + (1 + year | state)) +
  bf(farm_msales|mi()  ~ 1 + year + (1 + year | state)) +
  set_rescor(TRUE)

priors <- 
  prior(normal(0, 10), class = Intercept)
  prior(normal(0, 1),  class = b, coef = year) +
  prior(normal(0, 4),  class = )
  prior(inv_gamma(2, 1), class = sigma) 


## Univariate
mod_rys_no <- brm(farm_knumber  ~ 1 + year + (1 + year | state),
                     inits = "0",
                     #prior = priors,
                     data = all,
                     iter = 5000,
                     chains = 4,
                     cores = 4,
                     family = gaussian(link = "log"),
                     file = 'derived_data/mod_rys_no') 
mod_rys_sales <- brm(farm_msales|mi() ~ 1 + year + (1 + year | state),
                     inits = "0",
                     #prior = priors,
                     data = all,
                     iter = 5000,
                     chains = 4,
                     cores = 4,
                     family = gaussian(link = "log"),
                     file = 'derived_data/mod_rys_sales') 
mod_rys_area <- brm(farm_kha|mi()  ~ 1 + year + (1 + year | state),
                     inits = "0",
                     #prior = priors,
                     data = all,
                     iter = 5000,
                     chains = 4,
                     cores = 4,
                     family = gaussian(link = "log"),
                     file = 'derived_data/mod_rys_area') 

## Multivariate w/ Complete Data  
mmm <-  
  bf(farm_knumber  ~ 1 + year + (1 + year | state)) + 
  bf(farm_kha  ~ 1 + year + (1 + year | state)) +
  bf(farm_msales  ~ 1 + year + (1 + year | state)) +
  set_rescor(TRUE)

# priors <- 
#   prior(normal(0, 2),    class = Intercept) +
#   prior(normal(0, 4),    class = b) +
#   prior(inv_gamma(2, 1), class = sigma, resp = 'farmkha') +
#   prior(inv_gamma(2, 1), class = sigma, resp = 'farmknumber') +
#   prior(inv_gamma(2, 1), class = sigma, resp = 'farmmsales') 
mod_mvbrm_rys_complete <- brm(mmm,
                              inits = "0",
                              #prior = priors,
                              data = all_complete,
                              iter = 5000,
                              chains = 4,
                              cores = 4,
                              family = gaussian(link = "log"),
                              file = 'derived_data/mod_mvbrm_re_yrs_complete')

## Multivariate w/ Missing Data  
mod_mvbrm <- brm(brms_formula,
                 #prior = priors,
                 data = all,
                 iter = 5000,
                 chains = 4,
                 cores = 4,
                 family = gaussian(link = "log"),
                 file = 'derived_data/mod_mvbrm')

mod_mvbrm_rs <- brm(brms_formula_re_state,
                    #prior = priors,
                    data = all,
                    iter = 5000,
                    chains = 4,
                    cores = 4,
                    family = gaussian(link = "log"),
                    file = 'derived_data/mod_mvbrm_re_state')

mod_mvbrm_rys <- brm(brms_formula_re_yr_state,inits = "0",
                     #prior = priors,
                     data = all,
                     iter = 5000,
                     chains = 4,
                     cores = 4,
                     family = gaussian(link = "log"),
                     file = 'derived_data/mod_mvbrm_re_yr_state')

## Model analysis

a <- all_rescaled %>% mutate(date = lubridate::ymd(paste0(year, '-01-01'))) %>%
  pivot_longer(cols = farm_knumber:farm_msales,
               names_to = 'name',
               values_to = 'value')
ggplot(data = a, aes(date, value)) + 
  geom_point() + 
  geom_line(aes(group = state)) +
  facet_wrap(~name) 

#brms::loo(mod_mvbrm, mod_mvbrm_rs)

shinystan::launch_shinystan(mod_mvbrm_rys, rstudio = TRUE)
library(ggplot2)
library(tidybayes)
mod_mvbrm_rs %>% 
  spread_draws(b_farmknumber_year) %>%
  median_qi()

mod_mvbrm_rs %>% 
  spread_draws(b_farmkha_year) %>%
  median_qi()

mod_mvbrm_rs %>% 
  spread_draws(b_farmmsales_year) %>%
  median_qi()

ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeye()

#library(rstanarm)
## is this the appropriate way to model the y
## values that all seem log-normally distributed?
#mod_stanmvmer<- stan_mvmer(
#  formula = list(farm_knumber ~ year * state + (1 + year | state),
#                 farm_kha ~ year * state + (1 + year | state),
#                 farm_msales ~ year * state + (1 + year | state)),
#  data = x3 %>% drop_na(),
#  family = gaussian(link = "log"),
#  # short test:
#  # chains = 1, cores = 1, seed = 12345, iter = 1000)
#  chains = 4, cores = 4, seed = 12345, iter = 2000)
#)
#summary(mod_stanmvmer)



####)

## univariate
#mod_stanlmer <- stan_lmer(farm_kha ~ year * state + (1 + year | state),
#               data = x3)

#broom.mixed::tidyMCMC(mod_mvbrm)
#plot(broom.mixed::tidyMCMC(mod_stanlmer))

# newdata <- data.frame(state = unique(x3$state),
#                       year = rep(c(2007, 2012, 2017, 2022, 2027, 2032),
#                                  each= length(unique(x3$state))))
#
# ## for rstanarm
# # pp_farm_knumber <- cbind(newdata, t(posterior_predict(mod_stanmvmer, m = 1, newdata = newdata))) %>%
# #   tidyr::pivot_longer(names_to = 'sample', values_to = 'farm_knumber', cols = 3:4002)
# # pp_farm_kha <- cbind(newdata, t(posterior_predict(mod_stanmvmer, m = 2, newdata = newdata))) %>%
# #   tidyr::pivot_longer(names_to = 'sample', values_to = 'farm_kha', cols = 3:4002)
# # pp_farm_msales <- cbind(newdata, t(posterior_predict(mod_stanmvmer, m = 3, newdata = newdata))) %>%
# #   tidyr::pivot_longer(names_to = 'sample', values_to = 'farm_msales', cols = 3:4002)
#
# system.time(pp_array <- posterior_predict(mod_mvbrm, newdata = newdata, cores = 4))
#
# pp_farm_knumber <- cbind(newdata, t(pp_array[1:4000,,1])) %>%
#   tidyr::pivot_longer(names_to = 'sample', values_to = 'farm_knumber', cols = 3:4002)
#
# pp_farm_kha <- cbind(newdata, t(pp_array[1:4000,,2])) %>%
#   tidyr::pivot_longer(names_to = 'sample', values_to = 'farm_kha', cols = 3:4002)
#
# pp_farm_msales <- cbind(newdata, t(pp_array[1:4000,,3])) %>%
#   tidyr::pivot_longer(names_to = 'sample', values_to = 'farm_msales', cols = 3:4002)
#
#
#
# pp <- pp_farm_knumber %>% left_join(pp_farm_kha) %>%
#   left_join(pp_farm_msales) %>%
#   mutate(date = lubridate::ymd(paste0(year, '-01-01'))) %>%
#   pivot_longer(cols = farm_knumber:farm_msales,
#                names_to = 'name',
#                values_to = 'value')
# # estimate pp for US (sum across states)
# pp_us <- pp %>%
#   group_by(year, date, sample, name) %>%
#   summarise(value = sum(value))
#
#
# x4 <- x3 %>% mutate(date = lubridate::ymd(paste0(year, '-01-01'))) %>%
#   pivot_longer(cols = farm_knumber:farm_msales,
#                names_to = 'name',
#                values_to = 'value')
#
x4_us <- x4 %>%
  group_by(year, date, name) %>%
  summarise(value = sum(value, na.rm = TRUE))

# https://stackoverflow.com/a/57825639/199217
# otherwise y axis is range of posterior, which is much larger than quantiles
calc_stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(stats)
}


ggplot() +
  stat_summary(data = pp_us %>% filter(year > 2020),
               aes(date, value, group = year),
               color = 'blue',
               fun.data = calc_stat, geom = 'boxplot',
               width = 200) +
  geom_quantile(data = pp_us, aes(date, value), method = "rqss") +
  geom_point(data = x4_us, aes(date, value)) +
  geom_line(data = x4_us, aes(date, value)) +
  facet_wrap(~name, scales = 'free_y', ncol = 1) +
  #scale_y_log10() +
  theme_bw() + xlab('year') + ylab('')

pp_summary <- pp_us %>% filter(year > 2020) %>%
  group_by(year, name) %>%
  summarise(mean = mean(value), median = median(value), sd = sd(value),
            lcl_90 = quantile(value, 0.05),
            ucl_90 = quantile(value, 0.95))
write_csv(pp_summary, 'derived_data/pp_summary.csv')
