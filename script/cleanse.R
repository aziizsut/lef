# Library and Initialization ----------------------------------------------

library(tidyverse) # Load Dplyr and GGplot2


# Current Tariff Data -----------------------------------------------------
# Based on Stedin Document

f_calc_bill <- function(dset){
  
  dset %>% mutate(Datum = lubridate::dmy(Datum)) %>% 
  mutate(bulan = lubridate::month(Datum)) %>% group_by(bulan) %>%  #Since the tariff structure require monthly max to calculate
  summarise(cijfers = max(abs(Meetcijfers)) * 4,
            Load = max(Import) * 4,
            Export = min(Export) * 4) -> set_pld_tariff

mset <- max(set_pld_tariff$cijfers)
vastrecht <- case_when(
  mset <= 50 ~ 1.5,
  mset >50 & mset <= 1500 ~ 36.75,
  TRUE ~ 230
)

cset <- mset / 0.7 # cset = Contracted capacity. assume the company contracted capacity is 30% more than their maximal

contracto <- case_when(
  cset <= 50 ~ 0.6667 * cset,
  cset >50 & cset <= 150 ~ 1.6792 * cset,
  cset >150 & cset <= 1500 ~ 0.933 * cset,
  TRUE ~ cset * 0.9763
)

set_pld_tariff %>% mutate(kwmax = case_when(
  Load <= 50 ~ Load * 0,
  Load >50 & Load <= 150 ~ 1.42 * Load,
  Load >150 & Load <= 1500 ~ 1.42 * Load,
  TRUE ~ Load * 0.8649 * 4
)) %>% mutate(cprice = contracto,
              fprice = vastrecht) %>% mutate(totalkw = kwmax + cprice + fprice) -> pld_kw

normaal <- if_else(cset > 50, 0.0325, 0.0085)
laag <- if_else(cset > 50, 0.02, 0.0085)


rset_lemkes_2019 %>% mutate(Datum = lubridate::dmy(Datum)) %>% 
  mutate(day_week = lubridate::wday(Datum),
         jam = lubridate::hour(Tijd)) %>% 
  mutate(kwh_tariff = 
           if_else(day_week %in% c(2:5) & jam %in% c(7:23), 
         normaal * Import, 0)) -> pld_kwh

pld_kwh %>% mutate(bulan = lubridate::month(Datum)) %>% 
  group_by(bulan) %>% summarise(kwh_tariff = sum(kwh_tariff),
                                Import = sum(Import * 4),
                                TExport = sum(Export * 4)) -> pld_kwh2

calc_bills <- left_join(pld_kw, pld_kwh2)

return(calc_bills)
  
}


lemkes2019 <- f_calc_bill(rset_lemkes_2019)

lemkes2019 %>% mutate(price_kw = totalkw / Import) %>% 
  select(bulan, price_kw) -> harga2019

rset_lemkes_2019 %>% mutate(Datum = lubridate::dmy(Datum), usage = Import * 4) %>% select(Datum, usage) %>% 
  mutate(bulan = lubridate::month(Datum)) %>% left_join(harga2019) -> Lemkes_sens_2019

Lemkes_sens_2019 %>% ggplot(aes(x = price_kw, y = usage)) + geom_smooth() + geom_point()

# Data Wrangling ----------------------------------------------------------
#' Data Wrangling standardization
#' All raw data sored in the data folder
#' All cleansed data stored in the script folder with .RData 
#' If necessary the .csv copy of the cleansed data will be stored at the data folder
#' For any initial data imported from raw or other than cleanse data will use rset_* prefix
#' For final dataset after cleansing process will have dset_* prefix
#' IF the dataset bound to a single year or time, user need to put specific time signal as suffix
#' 

rset_lemkes_2019 <- read_csv("./data/lemkes19.csv")
rset_lemkes_2019 %>% mutate(Datum = lubridate::dmy(Datum),
                power = (Meetcijfers) * 4, # simple conversion from kWh usage to kW (Assume no spikes) 15 minutes is a quarter of hour
                jam = lubridate::hour(Tijd)) %>% 
  group_by(Tijd, Datum) %>% summarise(energy = sum(Meetcijfers, na.rm = TRUE),
                                     power = max(power, na.rm = TRUE)) %>% ungroup() %>% 
  group_by(Tijd) %>% summarise(power = mean(power)) -> dset_lemkes_2019 # Average daily usage of Royal Lemkes



# Graph to show how the Royal Lemkes data looks like  
ggplot(dset_lemkes_2019,aes(x = Tijd, y = power)) + 
  geom_step() + geom_smooth() +
  geom_hline(yintercept = 0, size = 1.5, color = "red") + theme_bw() +
  labs(y = "P Usage (in kW)",
       x = "Daily Time Stamp",
       title = "Average Daily Capacity Usage of Royal Lemkes",
       subtitle = "based on 2019 data") + scale_y_continuous(breaks = c(seq(-80,90,10))) 

dset_lemkes_2019
