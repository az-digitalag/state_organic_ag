library(readr)
library(dplyr)

census_2007_2012_2017 <- readr::read_csv('raw_data/2007_2012_2017_number_area_sales.csv', 
                                         col_types = cols(
                                           state = col_character(),
                                           year = col_integer(),
                                           farm_number = col_number(),
                                           farm_ha = col_number(),
                                           sales = col_number()
                                         ))

x_2011 <- readr::read_csv('raw_data/2011_number_area_sales.csv', 
                          col_types = cols(
                            state = col_character(),
                            year = col_integer(),
                            farm_number = col_number(),
                            farm_ha = col_number(),
                            sales = col_number()
                          ))
x_2016 <- readr::read_csv('raw_data/2016_number_area_sales.csv', 
                          col_types = cols(
                            state = col_character(),
                            year = col_integer(),
                            farm_number = col_number(),
                            farm_ha = col_number(),
                            sales = col_number(),
                            farm_number_cv = col_number(),
                            farm_ha_cv = col_number(),
                            sales_cv = col_number()
                          )) %>% select(!ends_with('cv'))

x_2019 <- readr::read_csv('raw_data/2019_number_area_sales.csv', 
                          col_types = cols(
                            state = col_character(),
                            year = col_integer(),
                            farm_number = col_number(),
                            farm_ha = col_number(),
                            sales = col_number(),
                            farm_number_cv = col_number(),
                            farm_ha_cv = col_number(),
                            sales_cv = col_number()
                          )) %>% select(!ends_with('cv')) 

area_1997 <- readr::read_csv('raw_data/1997_area.csv',
                             col_types = cols(
                               state = col_character(),
                               year = col_double(),
                               farm_ha = col_double()
                             )) %>% 
  mutate(farm_number = NA, sales = NA)


all <- dplyr::bind_rows(census_2007_2012_2017, 
                        x_2011, x_2016, x_2019,
                        area_1997)

all_transformed <- all %>%  
  mutate(farm_knumber = farm_number/1000,
         farm_kha = farm_ha/1000,
         farm_msales = sales/1000000) %>% 
  select(!farm_number:sales) 

readr::write_csv(all_transformed, 'derived_data/all_transformed.csv')

missing_years <- c(1998:2020)[!c(1998:2020) %in% unique(all$year)] 
missing <- expand.grid(year = missing_years,
                       state = unique(all$state),
                       farm_knumber = NA,
                       farm_kha = NA,
                       farm_msales = NA)
all_wide <- all %>% 
  rbind(missing) %>% 
  arrange(year, state) %>% 
  pivot_wider(names_from = year, values_from = starts_with('farm'))

readr::write_csv(all_wide, 'derived_data/all_wide.csv')
