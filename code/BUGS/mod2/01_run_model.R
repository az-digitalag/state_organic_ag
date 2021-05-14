# Controls script for OpenBUGS model
# of state-level organic farm survey
# multivariate log likelihood of farm number, acerage, and sales
# predictor of year
# random effect of state
# using "mod_2a.R", which is identical to "mod_1b.R"
# but now using inflation-corrected data from 4 time points

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(udunits2)
library(ggplot2)
library(R2OpenBUGS)
library(postjags)
library(mcmcplots)
library(cowplot)

# Read in data
x <- readr::read_csv('../../../derived_data/all_transformed.csv')

dat <- x %>% 
  mutate(year = year - 2000,
         stateID = as.numeric(as.factor(state))) # "center" by using years since 2000

#plot
ggplot(pivot_longer(dat, 3:5, names_to = "type", values_to = "value"),
       aes(x = year, y = log(value), group = state)) +
  geom_line() +
  geom_point() +
  facet_wrap(~type, scales = "free_y")

datlist <- list(farm = as.matrix(log(dat[,3:5])),
                year = dat$year,
                st = dat$stateID,
                N = nrow(dat),
                Nst = length(unique(dat$stateID)),
                R = diag(x = 1, 3, 3),
                Ab = 10,
                ts = 3)

# Parameters to monitor
params <- c("deviance", "Dsum", 
            "B", "Estar",
            "tau.Eps", "Sig", "Rho", "omega")

# Function to initialize precision matrix, use in initials funcion
# farm_mat <- as.matrix(log(dat[,3:5]))
# omega.gen<-function(x){
#   noise = rnorm(n = nrow(farm_mat)*ncol(farm_mat), mean = 0, sd = 10)
#   nois.mat = matrix(noise,ncol=ncol(farm_mat))
#   return(solve(cov(farm_mat+nois.mat, use="complete.obs")))
# }

# Initials function, use if no prior initials are available
# inits <- function(){
#   list(B = matrix(rnorm(6, 0, 10), ncol = 3), # indexing order [r,c] is opposite in BUGS
#        tau.Eps = rgamma(3, 0.1, 0.1),
#        omega = round(omega.gen(), 4)
#   )
# }
# initslist <- list(inits(), inits(), inits())
load("inits/inits_2a.Rdata")

# Compile and adapt BUGS model
start <- proc.time()
model <- bugs(data = datlist, 
              inits = saved.state[[2]],
              parameters.to.save = params, 
              n.iter = 5000, n.chains = 3, n.burnin = 1000, n.thin = 20,
              model.file="mod_2a.R", 
              codaPkg = TRUE, debug = FALSE)
end <- proc.time()
print((end - start)/60)

#change to coda object
coda_out <- read.bugs(model)

save(coda_out, file = "coda/coda_out_2a.Rdata")
