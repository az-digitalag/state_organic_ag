# Prediction script for OpenBUGS model
# of state-level organic farm survey
# multivariate response of number, acerage, and sales
# predictor of year
# random effect of state
# Predict for next 3 sampling dates: 2022 2027, 2032

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(R2OpenBUGS)
library(postjags)
library(mcmcplots)
library(cowplot)

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
         farm_5ha = farm_ha/100000,
         farm_msales = sales/1000000,
         year = year - 2000,
         stateID = as.numeric(as.factor(state))) %>% # "center" by using years since 2000
  select(!farm_number:sales)

predrange <- data.frame(year = rep(c(22, 27, 32), each = 50),
                        state = rep(unique(dat$state), 3),
                        farm_knumber = NA,
                        farm_5ha = NA,
                        farm_msales = NA,
                        stateID = rep(1:50, 3))

dat2 <- rbind.data.frame(dat, predrange)

datlist <- list(farm = as.matrix(dat2[,3:5]),
                year = dat2$year,
                st = dat2$stateID,
                N = nrow(dat2),
                Nst = length(unique(dat2$stateID)),
                R = diag(x = 1, 3, 3),
                Ab = 10)

# Load saved initials
load("inits/inits_1a.Rdata")

# Prediction of replicated data
# Compile and adapt BUGS model
start <- proc.time()
model_rep <- bugs(data = datlist, 
                  inits = saved.state[[2]],
                  parameters.to.save = c("Dsum", "B", "farm.rep"), 
                  n.iter = 10000, n.chains = 3, n.burnin = 5000, n.thin = 10,
                  model.file="mod_1a.R", 
                  codaPkg = TRUE, debug = FALSE)
end <- proc.time()
print((end - start)/60)

#change to coda object if codaPkg=T
coda_rep <- read.bugs(model_rep)
save(coda_rep, file = "coda/pred_coda_1a.Rdata")

#view chains
mcmcplot(coda_rep, parms = c("Dsum", "B"))
round(apply(coda_rep[[1]][, grep("Dsum", colnames(coda_rep[[1]]))], 2, mean), 3)
round(apply(coda_rep[[2]][, grep("Dsum", colnames(coda_rep[[2]]))], 2, mean), 3)
round(apply(coda_rep[[3]][, grep("Dsum", colnames(coda_rep[[3]]))], 2, mean), 3)

#check convergence
gel <- gelman.diag(coda_rep, multivariate = F)
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

#summarizing chains, reshape, append to data
sum_rep <- coda.fast(chains = 3, burn.in = 0, thin = 1, coda = coda_rep)
labs <- c("Number (1e3)", "Area (1e5 acres)", "Sales (1e6 $)")
pred_df <- data.frame(pivot_longer(dat2,3:5, names_to = "type", values_to = "obs"),
                      sum_rep[grep("farm.rep", row.names(sum_rep)),]) %>%
  mutate(Type = case_when(type == "farm_5ha" ~ "Area (1e5 acres)",
                          type == "farm_knumber" ~ "Number (1e3)",
                          type == "farm_msales" ~ "Sales (1e6 $)"),
         Type = factor(Type, levels = labs),
         coverage = ifelse(obs <= pc97.5 & obs >= pc2.5, 1, 0),
         Year = factor(year + 2000))

# Group states by USDA farm production regions 
usda <- data.frame(state = unique(dat2$state)) %>%
  mutate(Region = case_when(state == "Alaska" ~ "Alaska",
                            state == "Hawaii" ~ "Hawaii",
                            state %in% c("Washington", 
                                         "Oregon",
                                         "California") ~ "Pacific",
                            state %in% c("Montana", 
                                         "Idaho",
                                         "Wyoming",
                                         "Nevada", "Utah", "Colorado",
                                         "Arizona", "New Mexico") ~ "Mountain",
                            state %in% c("North Dakota", 
                                         "South Dakota",
                                         "Nebraska", "Kansas") ~ "Northern Plains",
                            state %in% c("Oklahoma", 
                                         "Texas") ~ "Southern Plains",
                            state %in% c("Minnesota", 
                                         "Wisconsin",
                                         "Michigan") ~ "Lake",
                            state %in% c("Iowa", "Illinois",
                                         "Indiana", "Ohio",
                                         "Missouri") ~ "Corn Belt",
                            state %in% c("Arkansas", 
                                         "Louisiana",
                                         "Mississippi") ~ "Delta",
                            state %in% c("Kentucky","Tennessee",
                                         "West Virginia", "Virginia",
                                         "North Carolina") ~ "Appalachia",
                            state %in% c("South Carolina", 
                                         "Georgia", "Florida",
                                         "Alabama") ~ "Southeast"),
         Region = ifelse(is.na(Region), "Northeast", Region))

## Create plotting df
pred_all <- left_join(pred_df, usda, by = "state")
obs <- pred_all %>%
  filter(year < 20)
preds <- pred_all %>%
  filter(year > 20)

rgs <- unique(usda$Region)
for(r in rgs){
  sub_obs <- subset(obs, Region == r)
  sub_preds <- subset(preds, Region == r)
  
  fig_pred <- ggplot() +
    geom_point(data = sub_obs, aes(x = Year, y = obs, color = state, fill = state),
               position = position_jitterdodge(jitter.width = 0.1),
               size = 2, shape = 15) +
    geom_pointrange(data = sub_preds, aes(x = Year, y = mean,
                                      ymin = pc2.5, ymax = pc97.5,
                                      color = state),
                    position = position_jitterdodge(jitter.width = 0.1)) +
    scale_x_discrete("Year") +
    scale_y_continuous("Value") +
    facet_wrap(~Type, scales = "free_y") +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank()) 

  jpeg(filename = paste0("figs/preds/fig_pred_", r, ".jpg"), 
       height = 3, width = 10, units = "in",
       res = 600)
  print(fig_pred)
  dev.off()
}

                  