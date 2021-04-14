# Controls script for OpenBUGS model
# of state-level organic farm survey
# multivariate response of number, acerage, and sales
# predictor of year
# random effect of state

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(R2OpenBUGS)
library(postjags)
library(mcmcplots)

x <- readr::read_csv('../../../raw_data/USA organic agriculture state data in farms_landarea_sales value.csv', 
                     col_types = cols(
                       year = col_integer(),
                       state = col_character(),
                       farm_number = col_number(),
                       farm_ha = col_number(),
                       sales = col_number()
                     ))
dat <- x %>% 
  mutate(farm_knumber = farm_number/1000,
         farm_kha = farm_ha/1000,
         farm_msales = sales/1000000,
         year = year - 2000,
         stateID = as.numeric(as.factor(state))) %>% # "center" by using years since 2000
  select(!farm_number:sales)

datlist <- list(farm = as.matrix(dat[,3:5]),
                year = dat$year,
                st = dat$stateID,
                N = nrow(dat),
                Nst = length(unique(dat$stateID)),
                R = diag(x = 1, 3, 3),
                Ab = 10)


# Parameters to monitor
params <- c("deviance", "Dsum", 
          "B", "Estar",
          "tau.Eps", "Sig", "Rho", "omega")

# Function to initialize precision matrix
farm_mat <- as.matrix(dat[,3:5])
omega.gen<-function(x){
  noise = rnorm(n = nrow(farm_mat)*ncol(farm_mat), mean = 0, sd = 10)
  nois.mat = matrix(noise,ncol=ncol(farm_mat))
  return(solve(cov(farm_mat+nois.mat, use="complete.obs")))
}

# Initials function
inits <- function(){
  list(B = matrix(rnorm(6, 0, 10), ncol = 3), # indexing order [r,c] is opposite in BUGS
       tau.Eps = rgamma(3, 0.1, 0.1),
       omega = round(omega.gen(), 4)
  )
}
initslist <- list(inits(), inits(), inits())

# Compile and adapt BUGS model
start <- proc.time()
model <- bugs(data = datlist, 
            inits = saved.state[[2]],
            parameters.to.save = params, 
            n.iter = 10000, n.chains = 3, n.burnin = 5000, n.thin = 10,
            model.file="mod_1a.R", 
            codaPkg = TRUE, debug = FALSE)
end <- proc.time()
print((end - start)/60)

#change to coda object if codaPkg=T
coda_out <- read.bugs(model)

#extract final iteration, save initials for reuse
newinits <- initfind(coda_out, OpenBUGS = TRUE)
saved.state <- removevars(initsin = newinits, 
                          variables=c(2:5))
#dir.create("inits")
save(saved.state, file = "inits_1a.Rdata")

round(apply(coda_out[[1]][,7:9],2,mean), 3)
round(apply(coda_out[[2]][,7:9],2,mean), 3)
round(apply(coda_out[[3]][,7:9],2,mean), 3)

#check convergence
gel <- gelman.diag(coda_out, multivariate = F)
str(gel)
gel$psrf[match("Dsum[1]", row.names(gel$psrf)):match("Dsum[3]", row.names(gel$psrf)),]
Bs<-matrix(NA, ncol = 3, nrow = 2)
for(i in 1:2){
  for(j in 1:3){
    Bs[i,j]<-paste0("B[", i, ",", j, "]")
  }
}
gel$psrf[match(Bs[,1], row.names(gel$psrf)),]
gel$psrf[match(Bs[,2], row.names(gel$psrf)),]
gel$psrf[match(Bs[,3], row.names(gel$psrf)),]

Estars<-matrix(NA, ncol = 3, nrow = 50)
for(i in 1:50){
  for(j in 1:3){
    Estars[i,j]<-paste0("Estar[", i, ",", j, "]")
  }
}
gel$psrf[match(Estars[,1], row.names(gel$psrf)),]
gel$psrf[match(Estars[,2], row.names(gel$psrf)),]
gel$psrf[match(Estars[,3], row.names(gel$psrf)),]
#Sig or sd of each response
gel$psrf[match("Sig[1]", row.names(gel$psrf)):match("Sig[3]", row.names(gel$psrf)),]
#Rho, or correlation coefficient between each pair of traits
gel$psrf[match("Rho[3,1]", row.names(gel$psrf)):match("Rho[3,2]", row.names(gel$psrf)),]
gel$psrf[match("Rho[2,1]", row.names(gel$psrf)),]
#Omega (precision matrix)
gel$psrf[match("omega[1,1]", row.names(gel$psrf)):match("omega[3,3]", row.names(gel$psrf)),]

#plotting random effects
caterplot(coda_out, regex=c("Estar\\[\\d{1,2},1\\]", perl=T), reorder=F)
caterplot(coda_out, regex=c("Estar\\[\\d{1,2},2\\]", perl=T), reorder=F)
caterplot(coda_out, regex=c("Estar\\[\\d{1,2},3\\]", perl=T), reorder=F)

#view chains
mcmcplot(coda_out, parms = c("Dsum", "B"))
mcmcplot(coda_out, parms = c("Estar"))

