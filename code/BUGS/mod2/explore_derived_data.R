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
  guides(color = FALSE)

# Plot on log scale
ggplot(dat, aes(x = year)) +
  geom_point(aes(y = log(value), color = state)) +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = FALSE)

tapply(dat$value, dat$variable, FUN = min, na.rm = TRUE)

# States clearly have different slopes when on linear scale, 
# but differences disappear on log scale

# Plot just farm knumber on both scales
farm <- dat %>%
  filter(variable == "farm_knumber") %>%
  mutate(lvalue = log(value))

ggplot(farm, aes(x = year, color = state, group = state)) +
  geom_point(aes(y = lvalue)) +
  geom_line(aes(y = lvalue)) +
  scale_y_continuous("log farm number") +
  scale_x_continuous(breaks = unique(farm$year)) +
  theme_bw(base_size = 12) +
  guides(color = FALSE)

ggplot(farm, aes(x = year, color = state, group = state)) +
  geom_point(aes(y = value)) +
  geom_line(aes(y = value)) +
  scale_y_continuous("farm number", limits = c(0, 0.5)) +
  scale_x_continuous(breaks = unique(farm$year)) +
  theme_bw(base_size = 12) +
  guides(color = FALSE)


# Plot just farm number on both scales
farm2 <- dat %>%
  filter(variable == "farm_knumber") %>%
  mutate(value = value*1000,
         lvalue = log(value))

ggplot(farm2, aes(x = year, color = state, group = state)) +
  geom_point(aes(y = lvalue)) +
  geom_line(aes(y = lvalue)) +
  scale_y_continuous("log farm number") +
  scale_x_continuous(breaks = unique(farm$year)) +
  theme_bw(base_size = 12) +
  guides(color = FALSE)

ggplot(farm2, aes(x = year, color = state, group = state)) +
  geom_point(aes(y = value)) +
  geom_line(aes(y = value)) +
  scale_y_continuous("farm number") +
  scale_x_continuous(breaks = unique(farm$year)) +
  theme_bw(base_size = 12) +
  guides(color = FALSE)
