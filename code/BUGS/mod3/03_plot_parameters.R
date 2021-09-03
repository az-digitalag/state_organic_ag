# Once converged, summarize chains and plot parameters

# Load packages
library(coda)
library(postjags)
library(ggplot2)
library(tidyverse)
library(mcmcplots)
library(cowplot)

x <- readr::read_csv('../../../derived_data/all_transformed.csv')

dat <- x %>% 
  mutate(year = year - 2014,
         stateID = as.numeric(as.factor(state))) # "center" by using years since 2014


# Load coda
load("coda/coda_out_3a.Rdata")
load("coda/coda_rep_3a.Rdata")

# Summarize chains
sum_out<-coda.fast(chains=3, burn.in=0, thin=1, coda=coda_out)
sum_out$var <- row.names(sum_out)
sum_out$sig<-ifelse(sum_out$pc2.5*sum_out$pc97.5 > 0, TRUE, FALSE)

sum_rep<-coda.fast(chains=3, burn.in=0, thin=1, coda=coda_rep)
sum_rep$var <- row.names(sum_rep)
sum_rep$year <- rep(dat$year + 2014, each = 3)
sum_rep$Var <- rep(c("number~(k)", "area~(kha)", "sales~(MM)"), 300)
sum_rep$state <- rep(dat$state, each = 3)

## Table
sum1 <- sum_out %>%
  mutate(Parameter = NA,
         Variable = NA,
         State = NA,
         mean = round(mean, 3),
         pc2.5 = round(pc2.5, 3),
         pc97.5 = round(pc97.5, 3),
         significant = sig) %>%
  select(var, Parameter, Variable, mean, pc2.5, pc97.5, significant)

inds <- grep("Astar|B|R|S|mu.natl|tau.natl", row.names(sum_out))
sum_df <- sum1[inds,]
sum_df$Parameter <- c(rep("intercept", 3),
                      rep("slope", 150),
                      rep("correlation", 3),
                      rep("stdev", 3),
                      rep("mu.slope", 3),
                      rep("tau.slope", 3))
sum_df$Variable <- c(c("farm_knumber", "farm_kha", "farm_msales"),
                     rep(c("farm_knumber", "farm_kha", "farm_msales"), each = 50),
                     c("kha-knumber", "msales-knumber", "msales-kha"),
                     c("farm_knumber", "farm_kha", "farm_msales"),
                     c("farm_knumber", "farm_kha", "farm_msales"),
                     c("farm_knumber", "farm_kha", "farm_msales"))
sum_df$State[sum_df$Parameter == "slope"] <- rep(unique(dat$state), 3)
sum_df$significant[grep("R|S|tau.natl", sum_df$var)] <- NA

write.csv(sum_df, file = "params_3a.csv", row.names = FALSE)

## Plots
labs <- c("Number (k)", "Area (kha)", "Sales (MM)")
states <- unique(dat$state)

## rates
dat_rep <- sum_rep %>%
  filter(year == 2019) # Include predicted value for the year 2019
dat_B <- sum_out[grep("B\\[", row.names(sum_out)),]
dat_B$state <- rep(unique(dat$state), 3)
dat_B$state <- factor(dat_B$state, levels = unique(dat$state[rev(order(dat_B$mean[101:150]))]))
dat_B$var <- rep(c("number~(k~yr^-1)", "area~(kha~yr^-1)", "sales~(MM~yr^-1)"), each = 50)
dat_B$var <- factor(dat_B$var, c("number~(k~yr^-1)", "area~(kha~yr^-1)", "sales~(MM~yr^-1)"))
dat_B$Var <- rep(c("number~(k)", "area~(kha)", "sales~(MM)"), each = 50)
dat_B$Var <- factor(dat_B$Var, c("number~(k)", "area~(kha)", "sales~(MM)"))
dat_B <- dat_B %>%
  left_join(dat_rep, by = c("state", "Var"), suffix = c("", ".y"))

dat_mu <- sum_out[grep("mu.natl", row.names(sum_out)),]
dat_mu$var <- c("number~(k~yr^-1)", "area~(kha~yr^-1)", "sales~(MM~yr^-1)")
dat_mu$var <- factor(dat_mu$var, c("number~(k~yr^-1)", "area~(kha~yr^-1)", "sales~(MM~yr^-1)"))
dat_mu$Var <- c("number~(k)", "area~(kha)", "sales~(MM)")
dat_mu$Var <- factor(dat_mu$Var, c("number~(k)", "area~(kha)", "sales~(MM)"))

fig_rates <- ggplot() +
  geom_rect(data = dat_mu, aes(xmin = -Inf, xmax = Inf,
                               ymin = pc2.5*100, ymax = pc97.5*100),
            alpha = 0.1, col = "gray80") +
  geom_hline(yintercept = 0,
             size = 1, lty = 2, col = "red") +
  geom_hline(data = dat_mu, aes(yintercept = mean*100),
             size = 1, lty = 2, col = "gray50") +
  geom_errorbar(data = dat_B, aes(x = fct_rev(state), 
                                    ymin = pc2.5*100, ymax = pc97.5*100),
                width = 0) +
  geom_point(data = dat_B, aes(x = fct_rev(state), y = mean*100), size = 2.5) +
  scale_y_continuous(expression(paste("Annual percent change")))+ 
  scale_x_discrete(labels = rev(unique(dat$state[rev(order(dat_B$mean[101:150]))]))) + 
  # scale_size(range = c(1, 3)) +
  theme_bw(base_size = 12)+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  facet_wrap(~Var, scales = "free_x", labeller = label_parsed) +
  coord_flip() +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank())

jpeg(filename = "figs_3a/fig_B.jpg", height = 10, width = 8, units = "in", 
     res = 600)
print(fig_rates)
dev.off()

### intercepts
dat_A <- sum_out[grep("Astar", row.names(sum_out)),]
fig_int <- ggplot() +
  geom_pointrange(data = dat_A, aes(x = var, y = mean, 
                                     ymin = pc2.5, ymax = pc97.5), 
                  size = 0.5) +
  scale_y_continuous(expression(paste("2014 value")))+ 
  scale_x_discrete(labels = labs) +
  theme_bw(base_size = 12)+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_int)


## Random effects
dat_RE1 <- sum_out[grep("Estar\\[1", row.names(sum_out)),]
sum(dat_RE1$mean)
dat_RE1$state <- unique(dat$state)[1:50]
fig_RE1<-ggplot() +
  geom_pointrange(data = dat_RE1, aes(x = fct_rev(state), y = mean, 
                                      ymin = pc2.5, ymax = pc97.5), 
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

dat_RE2 <- sum_out[grep("Estar\\[2", row.names(sum_out)),]
sum(dat_RE2$mean)
dat_RE2$state <- unique(dat$state)[1:50]
fig_RE2<-ggplot() +
  geom_pointrange(data = dat_RE2, aes(x = fct_rev(state), y = mean, 
                                      ymin = pc2.5, ymax = pc97.5), 
                  size = 0.25) +
  geom_hline(yintercept = 0, col = "red") +
  scale_y_continuous("RE (area)")+ 
  scale_x_discrete() +
  theme_bw(base_size = 12)+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_RE2)

dat_RE3 <- sum_out[grep("Estar\\[3", row.names(sum_out)),]
sum(dat_RE3$mean)
dat_RE3$state <- unique(dat$state)[1:50]
fig_RE3<-ggplot() +
  geom_pointrange(data = dat_RE3, aes(x = fct_rev(state), y = mean, 
                                      ymin = pc2.5, ymax = pc97.5), 
                  size = 0.25) +
  geom_hline(yintercept = 0, col = "red") +
  scale_y_continuous("RE (sales)")+ 
  scale_x_discrete() +
  theme_bw(base_size = 12)+
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_RE3)

jpeg(filename = "figs_3a/fig_RE.jpg", height = 6, width = 8, units = "in", 
     res = 600)
plot_grid(fig_RE1, fig_RE2, fig_RE3, ncol = 3)
dev.off()



### Standard deviations
# among random effects- slopes
dat_signatl <- sum_out[grep("sig.natl\\[", row.names(sum_out)),]
fig_signatl <- ggplot(dat_signatl, aes(x = var)) +
  geom_pointrange(aes(y = mean, ymin = pc2.5, ymax = pc97.5), size = 0.5) +
  scale_y_continuous(expression(paste(sigma[natl]))) + 
  scale_x_discrete(labels = labs) +
  theme_bw(base_size=12) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_signatl)

dat_sigeps <- sum_out[grep("sig.eps\\[", row.names(sum_out)),]
fig_sigeps <- ggplot(dat_sigeps, aes(x = var)) +
  geom_pointrange(aes(y = mean, ymin = pc2.5, ymax = pc97.5), size = 0.5) +
  scale_y_continuous(expression(paste(sigma[RE]))) + 
  scale_x_discrete(labels = labs) +
  theme_bw(base_size=12) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.y = element_blank()) +
  coord_flip()
print(fig_sigeps)

jpeg(filename = "figs_3a/fig_sigRE.jpg", height = 3, width = 8, units = "in", 
     res = 600)
plot_grid(fig_signatl, fig_sigeps, ncol = 2)
dev.off()


# population st dev
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

jpeg(filename = "figs_3a/fig_sig_rho.jpg", height = 3, width = 8, units = "in", 
     res = 600)
plot_grid(fig_sig, fig_rho, ncol = 2)
dev.off()
