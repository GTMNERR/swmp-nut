# using methods based on Cloern et al. 2020
# Nitrogen partitioning
# By SWMP Station

# 01 pull out and QAQC each parameter, plus conversions to micro Moles ----
# keeping only data collected at DEP lab (2018-present)
NH4 <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        select(STATION_CODE, DATE_TIME_STAMP, NH4F, F_NH4F) %>% 
        filter(!grepl("CUS", F_NH4F)) %>% 
        mutate(NH4uM = NH4F * (1000/14.01))

NO23 <- NUT %>% 
          filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
          select(STATION_CODE, DATE_TIME_STAMP, NO23F, F_NO23F) %>% 
          filter(!grepl("CUS", F_NO23F)) %>% 
          mutate(NO23uM = NO23F * (1000/14.01))

TKN <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        select(STATION_CODE, DATE_TIME_STAMP, TKN, F_TKN) %>% 
        filter(!grepl("CUS|GQS", F_TKN)) %>% 
        mutate(TKNuM = TKN * (1000/14.01))

TKNF <- NUT %>% 
          filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
          select(STATION_CODE, DATE_TIME_STAMP, TKNF, F_TKNF) %>% 
          filter(!grepl("CUS", F_TKNF)) %>% 
          mutate(TKNFuM = TKNF * (1000/14.01))

CHLA <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        select(STATION_CODE, DATE_TIME_STAMP, CHLA_N, F_CHLA_N) %>% 
        filter(!grepl("CUS", F_CHLA_N))

TSS <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        select(STATION_CODE, DATE_TIME_STAMP, TSS, F_TSS) %>% 
        filter(!grepl("CUS|CHB", F_TSS))

DIP <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        select(STATION_CODE, DATE_TIME_STAMP, PO4F, F_PO4F) %>% 
        filter(!grepl("CUS|CHB", F_PO4F)) %>% 
        rename(DIP = PO4F) %>% 
        mutate(DIPuM = DIP * (1000/30.97))

TP <- NUT %>% 
        filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
        select(STATION_CODE, DATE_TIME_STAMP, TP, F_TP) %>% 
        filter(!grepl("CUS", F_TP)) %>% 
        mutate(TPuM = TP * (1000/30.97))

SALT <- NUT %>% 
  filter(MONITORING_PROGRAM == 1 & DATE_TIME_STAMP > "2018-01-01") %>% 
  select(STATION_CODE, DATE_TIME_STAMP, SALT_N, F_SALT_N) 

# 02 Merge all parameters back into one dataframe called "nitro" ----

nitro <- NH4 %>% left_join(NO23, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(TKN, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(TKNF, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(TSS, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(CHLA, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(DIP, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(TP, by = c("STATION_CODE", "DATE_TIME_STAMP")) %>% 
  left_join(SALT, by = c("STATION_CODE", "DATE_TIME_STAMP"))

# clean up environment
rm(NH4, NO23, TKN, TKNF, TSS, CHLA, TP, DIP, SALT)

# 03 calculate DIN, TN, DON, and PN
# calculations of DIN, TN, DON, and PN

nitro1 <- nitro %>% 
            mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
            group_by(STATION_CODE, DATE) %>% 
            summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
            ungroup() %>% 
            mutate(DIN = NO23F + NH4F,
                   TN = TKN + NO23F,
                   DON = TKNF - NH4F,
                   PN = TN - (DIN + DON),
                   DINuM = NO23uM + NH4uM,
                   TNuM = TKNuM + NO23uM,
                   DONuM = TKNFuM - NH4uM,
                   PNuM = TNuM - (DINuM + DONuM)) 

# separate out sites to calculate multiple linear regression of PN as functions of TSS and CHLA

            
# pine island
pi <- nitro1 %>% filter(STATION_CODE == "gtmpinut")

pi_fit <- lm(PNuM ~ TSS + CHLA_N, data = pi)
summary(pi_fit)
broom::tidy(pi_fit)

pi <- pi %>% 
  mutate(N_sed = 0.236*TSS,
         N_phyto = 0.503*CHLA_N)

# san sebastian
ss <- nitro1 %>% filter(STATION_CODE == "gtmssnut")

ss_fit <- lm(PNuM ~ TSS + CHLA_N, data = ss)
summary(ss_fit)
broom::tidy(ss_fit)

ss <- ss %>% 
  mutate(N_sed = 0.022*TSS,
         N_phyto = 0.302*CHLA_N)

# fort matanzas
fm <- nitro1 %>% filter(STATION_CODE == "gtmfmnut")

fm_fit <- lm(PNuM ~ TSS + CHLA_N, data = fm)
summary(fm_fit)
broom::tidy(fm_fit)

fm <- fm %>% 
  mutate(N_sed = 0.002*TSS,
         N_phyto = 0.673*CHLA_N)
         

# pellicer creek
pc <- nitro1 %>% filter(STATION_CODE == "gtmpcnut")

pc_fit <- lm(PNuM ~ TSS + CHLA_N, data = pc)
summary(pc_fit)
broom::tidy(pc_fit)

pc <- pc %>% 
  mutate(N_sed = 0.329*TSS,
         N_phyto = 0.417*CHLA_N)


# merge all sites back into one dataframe
sites <- bind_rows(pi, ss, fm, pc) %>% 
          mutate(N_limit = 100*(DINuM/(DINuM + 1.6)),
                 P_limit = 100*(DIPuM/(DIPuM +0.24)),
                 TN_TPuM = TNuM/TPuM,
                 DIN_DIPuM = DINuM/DIPuM)

# clean-up environment
rm(pi, pi_fit, ss, ss_fit, fm, fm_fit, pc, pc_fit)

# calculate min, max, and mean of each parameter
sites_calc <- sites %>%
  group_by(STATION_CODE) %>% 
  summarise(across(where(is.numeric), list(min = min, max = max, mean = mean), na.rm = TRUE))

count <- sites %>% 
  group_by(STATION_CODE) %>% 
  summarise(across(everything(), ~ n()))

# replace columns in "count" with a _N to identify them as a count
colnames(count) <- paste(colnames(count), sep = "_", "N")
count <- count %>% rename(STATION_CODE = STATION_CODE_N) %>% select(-DATE_N)
all <- sites_calc %>% 
  left_join(count, by = "STATION_CODE") %>% 
  mutate(Nitrogen_eff = CHLA_N_mean/DIN_mean,
         Nitrogen_effuM = CHLA_N_mean/DINuM_mean,
         Phosphorus_eff = CHLA_N_mean/DIP_mean,
         Phosphorus_effuM = CHLA_N_mean/DIPuM_mean)


write.xlsx(all, here::here("output", "data", "N_and_P_statistics.xlsx"))
