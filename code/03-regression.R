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

all_rescaled <- all %>% 
  mutate(year = year - 2019)

## LMER multivariate


all_complete <-  all_rescaled[complete.cases(all_rescaled),]
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
                     data = all_rescaled,
                     iter = 5000,
                     chains = 4,
                     cores = 4,
                     family = gaussian(link = "log"),
                     file = 'derived_data/mod_rys_no') 
mod_rys_sales <- brm(farm_msales|mi() ~ 1 + year + (1 + year | state),
                     inits = "0",
                     #prior = priors,
                     data = all_rescaled,
                     iter = 5000,
                     chains = 4,
                     cores = 4,
                     family = gaussian(link = "log"),
                     file = 'derived_data/mod_rys_sales') 
mod_rys_area <- brm(farm_kha|mi()  ~ 1 + year + (1 + year | state),
                     inits = "0",
                     #prior = priors,
                     data = all_rescaled,
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
                 data = all_rescaled,
                 iter = 5000,
                 chains = 4,
                 cores = 4,
                 family = gaussian(link = "log"),
                 file = 'derived_data/mod_mvbrm')

mod_mvbrm_rs <- brm(brms_formula_re_state,
                    #prior = priors,
                    data = all_rescaled,
                    iter = 5000,
                    chains = 4,
                    cores = 4,
                    family = gaussian(link = "log"),
                    file = 'derived_data/mod_mvbrm_re_state')

mod_mvbrm_rys <- brm(brms_formula_re_yr_state,inits = "0",
                     #prior = priors,
                     data = all_rescaled,
                     iter = 5000,
                     chains = 4,
                     cores = 4,
                     family = gaussian(link = "log"),
                     file = 'derived_data/mod_mvbrm_re_yr_state')
