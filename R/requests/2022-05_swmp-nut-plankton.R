# prepping NUT for plankton data
nut_plank <- NUT %>% 
  filter(STATION_CODE %in% c("gtmpcnut", "gtmfmnut") & MONITORING_PROGRAM == 1 & REP == 1) %>% 
  mutate(DATE = as.Date(DATE_TIME_STAMP),
         MONTH = month(DATE, label = T),
         YEAR = year(DATE)) %>% 
  filter(DATE > "2017-01-01" & DATE < "2020-12-31")

nut_plank <- sapply(nut_plank, as.character)
nut_plank[is.na(nut_plank)] <- " "
nut_plank <- as.data.frame(nut_plank)


write.xlsx(nut_plank, here::here('output', 'data', 'swmp-nut_plank.xlsx'))
