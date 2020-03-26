rset_haluco_2019 # Haluco P is in kW no need to convert 

rset_haluco_2019 %>% mutate(usage = usage / rnorm(1, 2.63, 1),
                            daya = daya / rnorm(1, 2.63, 1)) -> rset_bhal_2019

rset_haluco_2019 %>% mutate(usage = usage / rnorm(1, 6.235, 0.65),
                            daya = daya / rnorm(1, 6.235, 0.68)) -> rset_chal_2019


f_calc_annual <- function(dset){
  
  dset %>% mutate(Datum = lubridate::ymd(tanggal)) %>% 
    mutate(bulan = lubridate::month(Datum)) %>% group_by(bulan) %>%  #Since the tariff structure require monthly max to calculate
    summarise(cijfers = mean(abs(daya)),
              min_cif = min(abs(daya)),
              max_cif = max(abs(daya))) -> set_pld_tariff
  
  mset <- max(set_pld_tariff$max_cif)
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
    max_cif <= 50 ~ max_cif * 0,
    max_cif >50 & max_cif <= 150 ~ 1.42 * max_cif,
    max_cif >150 & max_cif <= 1500 ~ 1.42 * max_cif,
    TRUE ~ max_cif * 0.8649 * 4
  )) %>% mutate(cprice = contracto,
                fprice = vastrecht) %>% mutate(totalkw = kwmax + cprice + fprice) -> pld_kw
  
  normaal <- if_else(cset > 50, 0.0325, 0.0085)
  laag <- if_else(cset > 50, 0.02, 0.0085)
  
  dset %>% mutate(Datum = lubridate::ymd(tanggal)) %>% 
    mutate(day_week = lubridate::wday(Datum)) %>%
    mutate(jam = lubridate::hm(waktu)) %>% mutate(jam = lubridate::hour(jam)) %>% 
    mutate(kwh_tariff = 
             if_else(day_week %in% c(2:5) & jam %in% c(7:23), 
                     normaal * usage, 0)) -> pld_kwh
  
  pld_kwh %>% mutate(bulan = lubridate::month(Datum)) %>% 
    group_by(bulan) %>% summarise(kwh_tariff = sum(kwh_tariff),
                                  Daya = sum(daya)) -> pld_kwh2
  
  calc_bills <- left_join(pld_kw, pld_kwh2)
  
  return(calc_bills)
}


f_calc_annual(rset_haluco_2019) -> haluco_hasil_2019
f_calc_annual(rset_bhal_2019) -> b_hasil_2019
f_calc_annual(rset_chal_2019) -> c_hasil_2019


haluco_hasil_2019$totalkw %>% sum()  / haluco_hasil_2019$Daya %>% sum()
b_hasil_2019$totalkw %>% sum()  / b_hasil_2019$Daya %>% sum()
c_hasil_2019$totalkw %>% sum()  / c_hasil_2019$Daya %>% sum()

b_hasil_2019 %>%  mutate(price_kw = totalkw / Daya) %>% 
  select(bulan, price_kw) -> price_b
price_b

c_hasil_2019 %>%  mutate(price_kw = totalkw / Daya) %>% 
  select(bulan, price_kw) -> price_c

calc_bills %>%  mutate(price_kw = totalkw / Daya) %>% 
  select(bulan, price_kw) -> price_haluco

rset_haluco_2019 %>% mutate(bulan = lubridate::month(tanggal)) %>% group_by(bulan) %>% 
  summarise(daya = sum(daya))


haluco2019_bills <- calc_bills$totalkw %>% sum() + calc_bills$kwh_tariff %>% sum()







# graph haluco ------------------------------------------------------------



set_pld_tariff %>% pivot_longer(cols = cijfers:max_cif, names_to = "status", values_to = "power") %>% 
  ggplot(aes(x = bulan, y = power, group = bulan)) + geom_boxplot(fill = "orange") +
  theme_bw() + theme(axis.text = element_text(family = font_rc_light, size = 12),
                     axis.title = element_text(family = font_rc, size = 13, face = "bold"),
                     title = element_text(family = font_rc, size = 14, face = "bold")) +
  scale_x_continuous(breaks = seq(1:12)) + labs(x = "Months", y = "Power in kW", title = "Haluco Monthly Load Profile") +
  scale_y_continuous(breaks = seq(0,450,50)) + geom_point(data = set_pld_tariff, aes(y = max_cif, x  = bulan), size = 2, color = "red")

set_pld_tariff$max_cif %>% mean()                     
