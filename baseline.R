library(tidyverse)
library(tidylog)
library(tableone)
library(MatchIt)
library(survival)
library(lubridate)
library(ggpubr)
library(SCCS)

dx_ASCVD <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/dx_ASCVD.rds")
df <- read_rds("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/full_5.cohort.matched.bll.evippc.RDS") %>% as_tibble() %>% 
  select(-starts_with("hx"), -starts_with("dx"), -starts_with("attn"), -starts_with("ip"), -starts_with("outcome"), -starts_with("lab"))
colnames(df) <- colnames(df) %>% str_replace_all(" ", "\\.") %>% str_to_lower()

# load("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/DX.RData")
# cohort <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/4.cohort_full.RDS")
# cohort <- cohort %>% mutate(id = openssl::sha256(paste0(PseudoID , "hKyou$haRma-cYCARe|oroj@")))
# df <- df %>% mutate(patient_pssn = cohort$patient_pssn[match(id, cohort$id)])
# dx_all <- rbind(dx_latest, dx_clean) %>% as_tibble() %>%
#   mutate(PseudoID  = cohort$PseudoID [match(patient_pssn, cohort$patient_pssn)]) %>%
#   mutate(id = openssl::sha256(paste0(PseudoID , "hKyou$haRma-cYCARe|oroj@")))
# dx_others <- dx_all %>% select(id, everything()) %>% select(-patient_pssn, -PseudoID) %>%
#   filter(str_detect(code, "^250|^456.[012]|^571.[2456]|^572.[2348]|^58[2568]|^583.[012467]|^427.3|^40[1-5]|^437.2"))
# write_rds(dx_others, "CARE data_20220228/dx_others.rds")
dx_others <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/dx_others.rds")

df1 <- df %>% 
  mutate(date.of.vaccination.1st = as_date(date.of.vaccination.1st)) %>%
  mutate(date.of.vaccination.2nd = as_date(date.of.vaccination.2nd)) %>% 
  mutate(date.of.vaccination.3rd = as_date(date.of.vaccination.3rd)) %>% 
  mutate(nid = seq_along(id))

dx_pre <- dx_ASCVD %>% 
  # filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410")) %>%
  mutate(date = as_date(date)) %>% 
  filter(date < date("2021-02-23")|is.na(date)) %>% # have MI/IS before 2021-02-23
  # filter(date < date("2021-02-1")|is.na(date)) %>% # have MI/IS before 2021-02-1
  group_by(id) %>% 
  slice(1)

id_revascularization <- dx_ASCVD %>% filter(id %in% df2$id) %>% filter(str_detect(code, "^36\\.[0-1]")) %>% filter(date >= date("2021-02-23")) %>% .$id %>% unique()
id_MI <- dx_ASCVD %>% filter(id %in% df2$id) %>% filter(str_detect(code, "^410")) %>% filter(date >= date("2021-02-23")) %>% .$id %>% unique()
setdiff(id_revascularization, id_MI)
setdiff(id_MI, id_revascularization)
dx_ASCVD %>% filter(id %in% id_revascularization$id) %>% filter(date >= date("2021-02-23")) %>% arrange(id) %>% View()

dx_mace <- dx_ASCVD %>% 
  # MI or IS
  # filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410")) %>%
  filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410|^36\\.[0-1]")) %>%
  mutate(date = as_date(date)) %>% 
  filter(!is.na(date)) %>% 
  select(id, date) %>% 
  rbind(select(rename(filter(df1, str_detect(death_diag_cd, "^I")), date = death_date_ymd), id, date)) %>% # add cardiovascular death into the definition of MACE
  filter(date >= date("2021-02-23")) %>% 
  arrange(id, date) %>% 
  group_by(id) %>% # not recurrent event to be consider
  slice(1) %>% 
  ungroup()

df2 <- df1 %>% 
  filter(id %in% dx_pre$id) %>% # only have the MI/IS patients
  filter(id %in% dx_mace$id) %>%
  mutate(eventdate = dx_mace$date[match(id, dx_mace$id)]) %>% 
  # rbind(mutate(filter(df1, str_detect(death_diag_cd, "^I")), eventdate = as_date(death_date_ymd))) %>%
  # filter(is.na(death_date_ymd)) %>%
  # filter(!is.na(date.of.vaccination.1st)) %>%
  group_by(id) %>% 
  slice(1) %>% 
  ungroup()


df2$vaccine.brand.1st %>% table()
df2 %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% filter(!is.na(death_date_ymd)) %>% select(id, date.of.vaccination.1st, date.of.vaccination.2nd, death_date_ymd, death_diag_cd, eventdate)
df2 %>% filter(vaccine.brand.1st == "Sinovac") %>% filter(!is.na(death_date_ymd)) %>% select(id, date.of.vaccination.1st, date.of.vaccination.2nd, death_date_ymd, death_diag_cd, eventdate)

df2 %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% filter(!is.na(date.of.vaccination.2nd)) %>% dim()
df2 %>% filter(vaccine.brand.1st == "Sinovac") %>% filter(!is.na(date.of.vaccination.2nd)) %>% dim()


baseline <- df1 %>% 
  filter(id %in% dx_pre$id) %>% 
  filter(id %in% dx_mace$id) %>%
  select("id", "vaccine.brand.1st", "date.of.vaccination.1st", "date.of.vaccination.2nd", "sex", "age", "death_date_ymd", "death_diag_cd", "vaccinated", 
         "rx.lipid", "rx.apt", "rx.ras", "rx.bb", "rx.ccb", "rx.diuretic", "rx.oac") %>% 
  mutate(dx.MI = if_else(id %in% filter(dx_pre, str_detect(code, "^410"))$id, T, F)) %>%
  mutate(dx.stroke = if_else(id %in% filter(dx_pre, str_detect(code, "^433.[012389]1|^43[46]|^437.[01]"))$id, T, F)) %>%
  mutate(dx.CHD = if_else(id %in% filter(dx_pre, str_detect(code, "^41[0-4]"))$id, T, F)) %>%
  mutate(dx.CBD = if_else(id %in% filter(dx_pre, str_detect(code, "^43[0-8]"))$id, T, F)) %>%
  mutate(dx.PVD = if_else(id %in% filter(dx_pre, str_detect(code, "^44[0-3]"))$id, T, F)) %>%
  mutate(dx.surgery = if_else(id %in% filter(dx_pre, str_detect(code, "^36\\."))$id, T, F)) %>%
  mutate(dx.DM = if_else(id %in% filter(dx_others, str_detect(code, "^250"))$id, T, F)) %>%
  mutate(dx.liverdisease = if_else(id %in% filter(dx_others, str_detect(code, "^456.[012]|^571.[2456]|^572.[2348]"))$id, T, F)) %>%
  mutate(dx.renaldisease = if_else(id %in% filter(dx_others, str_detect(code, "^58[2568]|^583.[012467]"))$id, T, F)) %>%
  mutate(dx.HTN = if_else(id %in% filter(dx_others, str_detect(code, "^40[1-5]|^437.2"))$id, T, F)) %>%
  mutate(dx.AF = if_else(id %in% filter(dx_others, str_detect(code, "^427.3"))$id, T, F)) %>%
  mutate(rx.lipid = as.factor(rx.lipid)) %>% 
  mutate(rx.apt = as.factor(rx.apt)) %>% 
  mutate(rx.ras = as.factor(rx.ras)) %>% 
  mutate(rx.bb = as.factor(rx.bb)) %>% 
  mutate(rx.ccb = as.factor(rx.ccb)) %>% 
  mutate(rx.diuretic = as.factor(rx.diuretic)) %>% 
  mutate(rx.oac = as.factor(rx.oac)) %>% 
  mutate(death = if_else(is.na(death_date_ymd), F, T)) %>% 
  mutate(type = if_else(id %in% dx_mace$id, "unvaccinated", "without MACE")) %>% 
  mutate(type = if_else(vaccine.brand.1st == "BioNTech/Fosun" & (id %in% dx_mace$id), "BNT162b2", type, missing = type)) %>% 
  mutate(type = if_else(vaccine.brand.1st == "Sinovac"& (id %in% dx_mace$id), "CoronaVac", type, missing = type))

# Table 1
vars <- c("age", "sex", "death", colnames(select(baseline, starts_with("dx"), starts_with("rx"))))
tableone1 <- CreateTableOne(vars = vars, strata = c("type"), data = baseline)
print(tableone1)
print(tableone1, quote = F, noSpaces = F)
write.csv(print(tableone1, quote = F, noSpaces = F), file = "baseline.csv")


# all baseline stratified by mace
df_CVD <- df1 %>% as_tibble() %>% 
  filter(id %in% dx_pre$id) %>% 
  mutate(MACE = if_else(id %in% dx_mace$id, T, F))
df_CVD$vaccine.brand.1st %>% table()
df_CVD$vaccine.brand.2nd %>% table()
df_CVD %>% filter(is.na(vaccine.brand.1st)) %>% dim()
df_CVD %>% filter(MACE == T) %>% .$vaccine.brand.1st %>% table()
df_CVD %>% filter(MACE == T) %>% .$vaccine.brand.2nd %>% table()
df_CVD %>% filter(MACE == T) %>% filter(is.na(vaccine.brand.1st)) %>% dim()
df_CVD %>% filter(MACE == F) %>% .$vaccine.brand.1st %>% table()
df_CVD %>% filter(MACE == F) %>% .$vaccine.brand.2nd %>% table()
df_CVD %>% filter(MACE == F) %>% filter(is.na(vaccine.brand.1st)) %>% dim()

df_CVD2 <- df_CVD %>% 
  select("id", "vaccine.brand.1st", "date.of.vaccination.1st", "date.of.vaccination.2nd", "sex", "age", "death_date_ymd", "death_diag_cd", "vaccinated", 
         "rx.lipid", "rx.apt", "rx.ras", "rx.bb", "rx.ccb", "rx.diuretic", "rx.oac", "MACE") %>% 
  mutate(dx.MI = if_else(id %in% filter(dx_pre, str_detect(code, "^410"))$id, T, F)) %>%
  mutate(dx.stroke = if_else(id %in% filter(dx_pre, str_detect(code, "^433.[012389]1|^43[46]|^437.[01]"))$id, T, F)) %>%
  mutate(dx.CHD = if_else(id %in% filter(dx_pre, str_detect(code, "^41[0-4]"))$id, T, F)) %>%
  mutate(dx.CBD = if_else(id %in% filter(dx_pre, str_detect(code, "^43[0-8]"))$id, T, F)) %>%
  mutate(dx.PVD = if_else(id %in% filter(dx_pre, str_detect(code, "^44[0-3]"))$id, T, F)) %>%
  mutate(dx.surgery = if_else(id %in% filter(dx_pre, str_detect(code, "^36\\."))$id, T, F)) %>%
  mutate(dx.DM = if_else(id %in% filter(dx_others, str_detect(code, "^250"))$id, T, F)) %>%
  mutate(dx.liverdisease = if_else(id %in% filter(dx_others, str_detect(code, "^456.[012]|^571.[2456]|^572.[2348]"))$id, T, F)) %>%
  mutate(dx.renaldisease = if_else(id %in% filter(dx_others, str_detect(code, "^58[2568]|^583.[012467]"))$id, T, F)) %>%
  mutate(dx.HTN = if_else(id %in% filter(dx_others, str_detect(code, "^40[1-5]|^437.2"))$id, T, F)) %>%
  mutate(dx.AF = if_else(id %in% filter(dx_others, str_detect(code, "^427.3"))$id, T, F)) %>%
  mutate(rx.lipid = as.factor(rx.lipid)) %>% 
  mutate(rx.apt = as.factor(rx.apt)) %>% 
  mutate(rx.ras = as.factor(rx.ras)) %>% 
  mutate(rx.bb = as.factor(rx.bb)) %>% 
  mutate(rx.ccb = as.factor(rx.ccb)) %>% 
  mutate(rx.diuretic = as.factor(rx.diuretic)) %>% 
  mutate(rx.oac = as.factor(rx.oac)) %>% 
  mutate(death = if_else(is.na(death_date_ymd), F, T))

# Table 1
vars <- c("age", "sex", "death", colnames(select(df_CVD2, starts_with("dx"), starts_with("rx"))))
tableone_biontech <- CreateTableOne(vars = vars, strata = c("MACE"), data = filter(df_CVD2, vaccine.brand.1st == "BioNTech/Fosun"))
tableone_sinovac <- CreateTableOne(vars = vars, strata = c("MACE"), data = filter(df_CVD2, vaccine.brand.1st == "Sinovac"))
tableone_unvaccinated <- CreateTableOne(vars = vars, strata = c("MACE"), data = filter(df_CVD2, vaccinated == 0))
print(tableone_biontech, quote = F, noSpaces = F)
write.csv(print(tableone_biontech, quote = F, noSpaces = F), file = "tableone_biontech.csv")
write.csv(print(tableone_sinovac, quote = F, noSpaces = F), file = "tableone_sinovac.csv")
write.csv(print(tableone_unvaccinated, quote = F, noSpaces = F), file = "tableone_unvaccinated.csv")
