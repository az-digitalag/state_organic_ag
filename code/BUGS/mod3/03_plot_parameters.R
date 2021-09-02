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
  mutate(year = year - 2000,
         stateID = as.numeric(as.factor(state))) # "center" by using years since 2000


# Load coda
load("coda/coda_out_2a.Rdata")

# Summarize chains
sum_out<-coda.fast(chains=3, burn.in=0, thin=1, coda=coda_out)
sum_out$var <- row.names(sum_out)
sum_out$sig<-ifelse(sum_out$pc2.5*sum_out$pc97.5 > 0, TRUE, FALSE)

## Table
sum1 <- sum_out %>%
  mutate(Parameter = NA,
         Variable = NA,
         mean = round(mean, 3),
         pc2.5 = round(pc2.5, 3),
         pc97.5 = round(pc97.5, 3),
         significant = sig) %>%
  select(var, Parameter, Variable, mean, pc2.5, pc97.5, significant)

grep("B|R|S", row.names(sum_out))
sum_df <- sum1[grep("B|R|S", row.names(sum_out)),]
sum_df$Parameter <- c(rep("intercept", 3),
                      rep("slope", 3),
                      rep("correlation", 3),
                      rep("standard deviation", 3))
sum_df$Variable <- c(rep(c("farm_knumber", "farm_kha", "farm_msales"), 2),
                      c("kha-knumber", "msales-knumber", "msales-kha"),
                      c("farm_knumber", "farm_kha", "farm_msales"))
sum_df$significant[grep("R|S", sum_df$var)] <- NA
write.csv(sum_df, file = "params_2a.csv", row.names = FALSE)

## Plots
labs <- c("Number (k)", "Area (k ha)", "Sales (MM)")

### Intercepts
dat_B1 <- sum_out[grep("B\\[1", row.names(sum_out)),]
sig_B1 <- subset(dat_B1, sig == TRUE)
fig_intercept<-ggplot() +
  geom_pointrange(data = dat_B1, aes(x = var, y = exp(mean), 
                                     ymin = exp(pc2.5), ymax = exp(pc97.5)), 
                  size = 0.5) +
  geom_point(data = sig_B1, aes(x = var, y = exp(max(pc97.5)) + 1), 
             pch = 8, color = "blue") +
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
  geom_point(data = sig_B2, aes(x = var, y = max(pc97.5) + 0.01), 
             pch = 8, color = "blue") +
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

jpeg(filename = "figs_2a/fig_B.jpg", height = 3, width = 8, units = "in", 
     res = 600)
plot_grid(fig_intercept, fig_slope, ncol = 2)
dev.off()

## Random effects
dat_RE1 <- sum_out[grep("Estar\\[[0-9]{1,2}\\,1", row.names(sum_out)),]
dat_RE1$state <- unique(dat$state)[1:50]
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
dat_RE2$state <- unique(dat$state)[1:50]
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
dat_RE3$state <- unique(dat$state)[1:50]
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

jpeg(filename = "figs_2a/fig_RE.jpg", height = 6, width = 8, units = "in", 
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

jpeg(filename = "figs_2a/fig_sig_rho.jpg", height = 3, width = 8, units = "in", 
     res = 600)
plot_grid(fig_sig, fig_rho, ncol = 2)
dev.off()
