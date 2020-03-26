rset_stedin %>% select(waktu, stamps, vermogen) %>% separate(waktu, into = c("tanggal", "waktu"), sep = " ") %>% 
  select(tanggal, stamps, vermogen) %>% mutate(tanggal = lubridate::mdy(tanggal)) %>% 
  filter(tanggal >= lubridate::dmy("01-01-2018")) %>% mutate(congestion = vermogen / 4) %>% 
  mutate(bulan = lubridate::month(tanggal)) %>% select(bulan, stamps, congestion) -> trafo_kon

price_b %>% rename(price_b = price_kw) -> price_b
price_c %>% rename(price_c = price_kw) -> price_c
harga2019 %>% rename(price_l = price_kw) -> price_l
price_haluco %>% rename(price_h = price_kw) -> price_h

price_col <- left_join(price_b, price_c) %>% left_join(price_l) %>% left_join(price_h)
price_col %>% pivot_longer(cols = price_b:price_h, names_to = "company", values_to = "harga") -> price_col

trafo2 <- left_join(trafo_kon, price_col)
trafo2 %>% ggplot(aes(x = congestion, y = harga*100)) + geom_point() + 
  scale_y_continuous(limits = c(0, 3.8), breaks = seq(0, 4, 0.5)) + 
  labs(x = "Grid Utilization",
       y = "Price per kW in Cents Euro",
       title = "Current price per kW and Grid Utilization") +
  scale_x_percent() + theme_bw() +
  theme(axis.text = element_text(family = font_rc_light, size = 12),
        axis.title = element_text(family = font_rc, size = 13, face = "bold"),
        title = element_text(family = font_rc, size = 14, face = "bold"))
  


seq(1, 10, 1)
price_for <- crossing(congestion= seq(0.01, 1, 0.001),
                      demand = seq(1, 400, 1))
price_for %>% mutate(price = case_when(
  congestion <= 0.2 & demand <= 150 ~ rnorm(1, 0.112, 0.05) + 0.01 * demand ,
  congestion <= 0.2 & demand >= 150  & demand <= 300 ~ rnorm(1, 0.675, 0.15) + 0.01 * demand,
  congestion <= 0.2 & demand >= 300 ~ rnorm(1, 1.18, 0.15) + 0.2 * demand,
  # Congestion 30 - 50% 
  congestion >= 0.3 & congestion <= 0.5 & demand <= 150 ~ rnorm(1, 0.453, 0.05) + 0.01 * demand ,
  congestion >= 0.3 & congestion <= 0.5 & demand >= 150  & demand <= 300 ~ rnorm(1, 1.475, 0.15) + 0.06 * demand,
  congestion >= 0.3 & congestion <= 0.5 & demand >= 300 ~ rnorm(1, 2.13, 0.15) + 0.2 * demand,
  # Congestion 50 - 80% 
  congestion >= 0.5 & congestion <= 0.8 & demand <= 150 ~ rnorm(1, 0.753, 0.05) + 0.01 * demand ,
  congestion >= 0.5 & congestion <= 0.8 & demand >= 150  & demand <= 300 ~ rnorm(1, 2.75, 0.15) + 0.06 * demand,
  congestion >= 0.5 & congestion <= 0.8 & demand >= 300 ~ rnorm(1, 3.13, 0.15) + 0.2 * demand,
  TRUE ~ rnorm(1, 30.213, 0.4) + 0.06 * demand
)) -> price_fin

price_fin %>% ggplot(aes(x = demand, y = price, z = congestion)) + geom_line()

price_for2 <- crossing(congestion= seq(0.01, 1, 0.001),
                       price = seq(0.1, 400, 1))

data.loess <- loess(congestion ~ demand * price, data = price_fin) 
data.loess

price_fin
mtrx3d %>% as_tibble()

# Create a sequence of incrementally increasing (by 0.3 units) values for both wt and hp
xgrid <-  seq(min(price_fin$congestion), max(price_fin$congestion), 0.01)
ygrid <-  seq(min(price_fin$price), max(price_fin$price), 0.001)
# Generate a dataframe with every possible combination of wt and hp
data.fit <-  expand.grid(congestion = xgrid, price = ygrid)
mtrx3d <-  predict(data.loess, newdata = data.fit)


xgrid <-  seq(min(price_fin$demand), max(mtcars$wt), 0.3)
ygrid <-  seq(min(mtcars$hp), max(mtcars$hp), 0.3)
# Generate a dataframe with every possible combination of wt and hp
data.fit <-  expand.grid(wt = xgrid, hp = ygrid)
# Feed the dataframe into the loess model and receive a matrix output with estimates of
# acceleration for each combination of wt and hp
mtrx3d <-  predict(data.loess, newdata = data.fit)
# Abbreviated display of final matrix
mtrx3d[1:4, 1:4]