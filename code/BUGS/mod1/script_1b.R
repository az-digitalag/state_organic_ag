# Controls script for OpenBUGS model
# of state-level organic farm survey
# multivariate log likelihood of farm number, acerage, and sales
# predictor of year
# random effect of state

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
  rename(farm_sales = sales) %>%
  mutate(year = year - 2000,
         stateID = as.numeric(as.factor(state))) # "center" by using years since 2000


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
          "tau.Eps", "Sig", "Rho", "omega", "natl")

# Function to initialize precision matrix
farm_mat <- as.matrix(log(dat[,3:5]))
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
# load("inits/inits_1b.Rdata")

# Compile and adapt BUGS model
start <- proc.time()
model <- bugs(data = datlist, 
            inits = saved.state[[2]],
            parameters.to.save = params, 
            n.iter = 5000, n.chains = 3, n.burnin = 1000, n.thin = 20,
            model.file="mod_1b.R", 
            codaPkg = TRUE, debug = FALSE)
end <- proc.time()
print((end - start)/60)

#change to coda object if codaPkg=T
coda_out <- read.bugs(model)

save(coda_out, file = "coda/coda_out_1b.Rdata")

#view chains
mcmcplot(coda_out, parms = c("deviance", "Dsum", "B", "natl"))
mcmcplot(coda_out, parms = c("Estar"))

#extract final iteration, save initials for reuse
newinits <- initfind(coda_out, OpenBUGS = TRUE)
saved.state <- removevars(initsin = newinits, 
                          variables=c(2:6))
#dir.create("inits")
save(saved.state, file = "inits/inits_1b.Rdata")

#check convergence
gel <- gelman.diag(coda_out, multivariate = F)
str(gel)
gel$psrf[match("deviance", row.names(gel$psrf)),]
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

#summarizing chains
sum_out<-coda.fast(chains=3, burn.in=0, thin=1, coda=coda_out)
sum_out$var <- row.names(sum_out)
sum_out$sig<-ifelse(sum_out$pc2.5*sum_out$pc97.5 > 0, TRUE, FALSE)

## Plots
labs <- c("Number", "Area (acres)", "Sales ($)")

### Intercepts
dat_B1 <- sum_out[grep("B\\[1", row.names(sum_out)),]
sig_B1 <- subset(dat_B1, sig == TRUE)
fig_intercept<-ggplot() +
  geom_pointrange(data = dat_B1, aes(x = var, y = exp(mean), 
                                     ymin = exp(pc2.5), ymax = exp(pc97.5)), 
                  size = 0.5) +
  geom_hline(yintercept=0, col = "red") +
  scale_y_continuous("Value in 2000")+ 
  scale_x_discrete(labels = labs) +
  theme_bw(base_size = 12)+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_intercept)

### Slopes
dat_B2 <- sum_out[grep("B\\[2", row.names(sum_out)),]
sig_B2 <- subset(dat_B2, sig == TRUE)
fig_slope<-ggplot() +
  geom_pointrange(data = dat_B2, aes(x = var, y = mean, 
                                     ymin = pc2.5, ymax = pc97.5), 
                  size = 0.5) +
  # geom_point(data = sig_B2, aes(x = var, y = (sig*(max(sub1[,4])+1))), 
  # col = "red", shape = 8, size = 2)+
  geom_hline(yintercept=0, col = "red") +
  scale_y_continuous(expression(paste("Log rate of change (", yr^-1, ")")))+ 
  scale_x_discrete(labels = labs) +
  theme_bw(base_size = 12)+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_slope)

jpeg(filename = "figs_1b/fig_B.jpg", height = 3, width = 8, units = "in", 
     res = 600)
plot_grid(fig_intercept, fig_slope, ncol = 2)
dev.off()

## Random effects
dat_RE1 <- sum_out[grep("Estar\\[[0-9]{1,2}\\,1", row.names(sum_out)),]
dat_RE1$state <- unique(dat$state)
fig_RE1<-ggplot() +
  geom_pointrange(data = dat_RE1, aes(x = state, y = exp(mean), 
                                     ymin = exp(pc2.5), ymax = exp(pc97.5)), 
                  size = 0.25) +
  geom_hline(yintercept = 0, col = "red") +
  scale_y_continuous("RE (number)")+ 
  scale_x_discrete() +
  theme_bw(base_size = 12)+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_RE1)

dat_RE2 <- sum_out[grep("Estar\\[[0-9]{1,2}\\,2", row.names(sum_out)),]
dat_RE2$state <- unique(dat$state)
fig_RE2<-ggplot() +
  geom_pointrange(data = dat_RE2, aes(x = state, y = exp(mean), 
                                      ymin = exp(pc2.5), ymax = exp(pc97.5)), 
                  size = 0.25) +
  geom_hline(yintercept = 0, col = "red") +
  scale_y_continuous("RE (area, acres)")+ 
  scale_x_discrete() +
  theme_bw(base_size = 12)+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_RE2)

dat_RE3 <- sum_out[grep("Estar\\[[0-9]{1,2}\\,3", row.names(sum_out)),]
dat_RE3$state <- unique(dat$state)
fig_RE3<-ggplot() +
  geom_pointrange(data = dat_RE3, aes(x = state, y = exp(mean), 
                                      ymin = exp(pc2.5), ymax = exp(pc97.5)), 
                  size = 0.25) +
  geom_hline(yintercept = 0, col = "red") +
  scale_y_continuous("RE (sales, $)")+ 
  scale_x_discrete() +
  theme_bw(base_size = 12)+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_RE3)

jpeg(filename = "figs_1b/fig_RE.jpg", height = 6, width = 8, units = "in", 
     res = 600)
plot_grid(fig_RE1, fig_RE2, fig_RE3, ncol = 3)
dev.off()

### Standard deviation
dat_sigs <- sum_out[grep("Sig\\[", row.names(sum_out)),]
fig_sig <- ggplot(dat_sigs, aes(x = var)) +
  geom_pointrange(aes(y = mean, ymin = pc2.5, ymax = pc97.5), size = 0.5) +
  scale_y_continuous(expression(paste(sigma))) + 
  scale_x_discrete(labels = labs) +
  theme_bw(base_size=12) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_sig)

#Rho, or correlation coefficient between each pair of traits
dat_rho <- sum_out[grep("Rho\\[", row.names(sum_out)),]
dat_rho$labs <- c("number - area", "number - sales", "area - sales")
fig_rho <- ggplot(dat_rho, aes(x = labs)) +
  geom_pointrange(aes(y = mean, ymin = pc2.5, ymax = pc97.5), size = 0.5) +
  scale_y_continuous(expression(paste(rho))) + 
  scale_x_discrete() +
  theme_bw(base_size=12) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_rho)

jpeg(filename = "figs_1b/fig_sig_rho.jpg", height = 3, width = 8, units = "in", 
     res = 600)
plot_grid(fig_sig, fig_rho, ncol = 2)
dev.off()

# Replicated data
# Compile and adapt BUGS model
start <- proc.time()
model_rep <- bugs(data = datlist, 
              inits = saved.state[[2]],
              parameters.to.save = c("farm.rep", "natl"),
              n.iter = 5000, n.chains = 3, n.burnin = 1000, n.thin = 20,
              model.file="mod_1b.R", 
              codaPkg = TRUE, debug = FALSE)
end <- proc.time()
print((end - start)/60)

#change to coda object if codaPkg=T
coda_rep <- read.bugs(model_rep)
save(coda_rep, file = "coda/coda_rep_1b.Rdata")

#summarizing chains, reshape, append to data
sum_rep <- coda.fast(chains = 3, burn.in = 0, thin = 1, coda = coda_rep)

pred_df <- data.frame(pivot_longer(dat,3:5, names_to = "type", values_to = "obs"),
                      sum_rep[grep("farm.rep", row.names(sum_rep)),]) %>%
  mutate(mean = exp(mean),
         median = exp(median),
         pc2.5 = exp(pc2.5),
         pc97.5 = exp(pc97.5),
         Type = case_when(type == "farm_ha" ~ "Area (acres)",
                          type == "farm_number" ~ "Number",
                          type == "farm_sales" ~ "Sales ($)"),
         Type = factor(Type, levels = labs),
         coverage = ifelse(obs <= pc97.5 & obs >= pc2.5, 1, 0))
tapply(pred_df$coverage, pred_df$Type, mean,  na.rm = T)

fitByType <- by(
  data    = pred_df,
  INDICES = pred_df$Type,
  FUN     = function(subset)
    summary(lm(mean ~ obs, data = subset)))

bestfit <- data.frame(Type = labs,
                      R2 = c(fitByType[[1]]$adj.r.squared, 
                             fitByType[[2]]$adj.r.squared,
                             fitByType[[3]]$adj.r.squared),
                      slope = c(fitByType[[1]]$coef[2,1],
                                fitByType[[2]]$coef[2,1],
                                fitByType[[3]]$coef[2,1]),
                      int = c(fitByType[[1]]$coef[1,1],
                              fitByType[[2]]$coef[1,1],
                              fitByType[[3]]$coef[1,1]),
                      x = rep(0, 3),
                      y = tapply(pred_df$pc97.5, pred_df$Type, max, na.rm = T)) %>%
  mutate(r2 = paste0("R^2==", round(R2, 3)),
         Type = factor(Type, levels = labs))

fig_fit <- ggplot() +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  geom_abline(data = bestfit, aes(slope = slope, intercept = int), col = "gray", lty = 2) +
  geom_errorbar(data = pred_df, aes(x = obs, ymin = pc2.5, ymax = pc97.5), color = "gray", width = 0, alpha = 0.5) +
  geom_point(data = pred_df, aes(x = obs, y = mean), alpha = 0.75) +
  geom_text(data = bestfit, aes(x = x, y = y, label = r2), parse = TRUE,
            hjust = 0, vjust = 1) +
  scale_x_continuous("Observed") +
  scale_y_continuous("Predicted") +
  facet_wrap(~Type, scales = "free") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank())

jpeg(filename = "figs_1b/fig_fit.jpg", height = 3, width = 8, units = "in",
     res = 600)
print(fig_fit)
dev.off()
                  