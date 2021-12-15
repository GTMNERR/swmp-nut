sd <- NUT %>% 
  select(STATION_CODE, !contains('F_')) %>% 
  group_by(STATION_CODE) %>% 
  summarise_all(sd, na.rm = TRUE) %>% 
  select(-DATE_TIME_STAMP, -MONITORING_PROGRAM, -REP, -X16) %>% 
  tidyr::pivot_longer(cols = c(2:39),
                      names_to = "Parameter",
                      values_to = "SD")

sal <- tribble(
  ~STATION_CODE, ~salinity,
  'gtmpinut', 28.03,
  'gtmssnut', 33.71,
  'gtmfmnut', 38.8,
  'gtmpcnut', 15.71
)

all <- NUT %>% 
  mutate(YEAR = lubridate::year(DATE_TIME_STAMP)) %>% 
  select(STATION_CODE, YEAR, !contains('F_'), -DATE_TIME_STAMP, -MONITORING_PROGRAM, -REP) %>%
  tidyr::pivot_longer(cols = c(3:41),
                      names_to = "Parameter",
                      values_to = "VALUE") %>% 
  filter(!is.na(VALUE)) %>% 
  group_by(STATION_CODE, Parameter) %>% 
  summarise(min = min(YEAR, na.rm = T), 
            max = max(YEAR, na.rm = T),
            range = max-min) %>% 
  left_join(sd, by = c("STATION_CODE", "Parameter")) %>% 
  left_join(sal, by = "STATION_CODE")

write_csv(all, here::here('output', 'reports', '2021-RCP-Priorities.csv'))
