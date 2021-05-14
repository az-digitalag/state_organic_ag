# Explore and prepare data for analysis
library(ggplot2)
library(dplyr)
dat <- read.csv("../../../derived_data/all_transformed.csv") %>%
  tidyr::pivot_longer(!c(year, state), names_to = "variable")

# Plot on linear scale
ggplot(dat, aes(x = year)) +
  geom_point(aes(y = value, color = state)) +
  facet_wrap(~variable, scales = "free_y") +
  scale_y_continuous(limits = c(0, 100)) +
  guides(color = F)

# Plot on log scale
ggplot(dat, aes(x = year)) +
  geom_point(aes(y = log(value), color = state)) +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = F)

tapply(dat$value, dat$variable, FUN = min, na.rm = TRUE)

# States clearly have different slopes when on linear scale, 
# but differences disappear on log scale