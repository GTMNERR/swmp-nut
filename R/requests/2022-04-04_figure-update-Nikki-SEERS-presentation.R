a <- NUT_monthly %>% 
  filter(STATION_CODE %in% c("gtmpcnut", "gtmfmnut")) %>% 
  ggplot(aes(x = DATE, y = CHLA_avg, color = STATION_CODE)) +
  geom_point(size = 2) +
  geom_line() +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black")) +
  labs(x = "",
       y = expression(paste("Chlorophyll ", italic(" a "), mu*"g/L")),
       color = "")
  
ggsave(a, filename = here('output', 'figures', '2022_seers-figure-dix.png'),
       dpi = 300, height = 4, width = 13.33, units = "in")