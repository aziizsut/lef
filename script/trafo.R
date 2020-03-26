library(ggthemes)

dset_lemkes_2019 %>% mutate(tstamp = row_number()) %>% rename(daya_lemkes = power) %>% select(tstamp, daya_lemkes) -> lemkes_1
ceko %>% mutate(tstamp = row_number()) %>% rename(daya_haluco = daya) %>%  select(tstamp, daya_haluco) -> haluco_1


# Check Condition of the Main Transformer ---------------------------------

kondisi <- left_join(lemkes_1, haluco_1) # Join the condition of the companies load
kondisi <- kondisi %>% 
  mutate(trafo = abs(daya_lemkes) + abs(daya_haluco)) %>% 
  pivot_longer(cols = daya_lemkes:trafo, names_to = "lokasi", values_to = "daya")



rset_stedin <- read_csv("./data/stedin_data.csv")
rset_stedin %>% select(waktu, stamps, vermogen) %>% separate(waktu, into = c("tanggal", "waktu"), sep = " ") %>% 
  select(tanggal, stamps, vermogen) %>% mutate(tanggal = lubridate::mdy(tanggal)) %>% 
  filter(tanggal >= lubridate::dmy("01-01-2018")) %>% 
  group_by(stamps) %>% summarise(avg_vermogen = 1000 * mean(vermogen, na.rm = TRUE),
                                 max_vermogen = 1000 * max(vermogen, na.rm = TRUE),
                                 min_vermogen = 1000 * min(vermogen, na.rm = TRUE)) -> test_stedin

test_stedin %>% mutate(jam = lubridate::hour(stamps)) %>% select(jam, avg_vermogen,max_vermogen, min_vermogen ) -> kon_stedin

pld_jam <- tibble(
  jam = rep(0:23, 4)
)

pld_jam %>% arrange(jam) -> pld_jam

t_lemkes <- bind_cols(dset_lemkes_2019, pld_jam) %>% rename(d_lemkes = power) %>% mutate(waktu = as.character(Tijd)) %>% 
  select(jam, d_lemkes)
t_haluco <- bind_cols(ceko, pld_jam) %>% rename(d_haluco = daya) %>% select(jam, d_haluco)

kondisi_2 <- left_join(t_lemkes, t_haluco)
kondisi_3 <- left_join(kondisi_2, kon_stedin).

kondisi_4 <- kondisi_3 %>% mutate(d_besar = d_lemkes + d_haluco) %>% 
  mutate(surv = vermogen - d_besar)

t_haluco %>% mutate(z = (d_haluco - min(d_haluco)) / (max(d_haluco) - min(d_haluco))) -> z_haluco
t_haluco$d_haluco %>% mean()

kon_stedin %>% pivot_longer(cols = avg_vermogen:min_vermogen, names_to = "vermo", values_to = "power") %>% 
  ggplot(aes(x = jam, y = power, group = vermo)) + geom_line(aes(color = vermo), size = 1.5, alpha = 0.6) +
  theme_bw() + theme(axis.text = element_text(family = font_rc_light, size = 12),
                     axis.title = element_text(family = font_rc, size = 13, face = "bold"),
                     title = element_text(family = font_rc, size = 14, face = "bold"),
                     legend.position = "none") +
  scale_y_continuous(breaks = seq(-700, 2500, 200)) +
  labs(x = "Hour", y = "Main Transformer Usage", title = "Average Daily Profile") + scale_x_continuous(breaks = seq(0, 24,1))
