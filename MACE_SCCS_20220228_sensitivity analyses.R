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
  select(-starts_with("hx"), -starts_with("dx"), -starts_with("rx"), -starts_with("attn"), -starts_with("ip"), -starts_with("outcome"), -starts_with("lab"))
colnames(df) <- colnames(df) %>% str_replace_all(" ", "\\.") %>% str_to_lower()

df1 <- df %>% 
  mutate(date.of.vaccination.1st = as_date(date.of.vaccination.1st)) %>%
  mutate(date.of.vaccination.2nd = as_date(date.of.vaccination.2nd)) %>% 
  mutate(date.of.vaccination.3rd = as_date(date.of.vaccination.3rd)) %>% 
  mutate(nid = seq_along(id))

dx_pre <- dx_ASCVD %>% 
  # filter(str_detect(code, "^410")) %>% # MI
  # filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]")) %>% # STROKE
  # filter(str_detect(code, "^41[0-4]")) %>% # CHD
  # filter(str_detect(code, "^43[0-8]")) %>% # CBD
  # filter(!str_detect(code, "^41[0-4]")) %>% # non-CHD
  # filter(!str_detect(code, "^43[0-8]")) %>% # non-CBD
  mutate(date = as_date(date)) %>% 
  filter(date < date("2021-02-23")|is.na(date)) %>% # have MI/IS before 2021-02-23
  group_by(id) %>% 
  slice(1)

# # non-CHD/non-CBD
# dx_pre <- dx_ASCVD %>% filter(!id %in% dx_pre$id) %>% 
#   mutate(date = as_date(date)) %>% 
#   filter(date < date("2021-02-23")|is.na(date)) %>% # have MI/IS before 2021-02-23
#   group_by(id) %>% 
#   slice(1)

dx_myocarditis <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/dx_myocarditis.rds")
dx_thromboembolism <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/dx_thromboembolism.rds")
dx_fracture <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/dx_fracture.rds")
positive_COVID <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/positive_COVID.rds")

dx_mace <- dx_ASCVD %>% 
  rbind(dx_myocarditis) %>% 
  rbind(dx_thromboembolism) %>% 
  rbind(dx_fracture) %>% 
  # MI or IS
  # filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410")) %>% # sensitivity analysis 2, excluding revascularization
  filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410|^36\\.[0-1]")) %>% # main analysis
  # filter(str_detect(code, "^8[0-2][0-9]")) %>% # negative control
  # filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410|^36\\.[0-1l]|^415.1|^453|^44[345]|^43[3456]|^437.[01689]|^45[12]|^325|^286.6|^459.9|^557.[09]|^K89|^K9[0134]")) %>% # sensitivity analysis, including thromboembolism
  # filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410|^36\\.[0-1l]|^415.1|^453|^44[345]|^43[3456]|^437.[01689]|^45[12]|^325|^286.6|^459.9|^557.[09]")) %>% # sensitivity analysis, including thromboembolism
  # filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410|^36\\.[0-1]|^422|^429.0|^420.9|^423.9|^K84")) %>% # sensitivity analysis, including myocarditis
  # filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410|^36\\.[0-1]|^422|^429.0|^420.9|^423.9")) %>% # sensitivity analysis, including myocarditis
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
  filter(id %in% dx_mace$id) %>%
  filter(id %in% dx_pre$id) %>% # only have the MI/IS patients
  # filter(!id %in% positive_COVID$id) %>% # sensitivity analysis 7
  mutate(eventdate = dx_mace$date[match(id, dx_mace$id)]) %>% 
  # rbind(mutate(filter(df1, str_detect(death_diag_cd, "^I")), eventdate = as_date(death_date_ymd))) %>%
  # filter(is.na(death_date_ymd)) %>% # sensitivity analysis 1
  # filter(vaccinated == 1) %>% # sensitivity analysis 4
  # filter(!is.na(date.of.vaccination.1st)) %>%
  group_by(id) %>% 
  slice(1) %>% 
  ungroup()

df2 %>% filter(!is.na(death_date_ymd)) %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% .$death_diag_cd
df2 %>% filter(!is.na(death_date_ymd)) %>% filter(vaccine.brand.1st == "Sinovac") %>% .$death_diag_cd

df3 <- df2 %>% 
  # filter(sex == "M") %>%
  # filter(sex == "F") %>%
  # filter(age >= 65) %>%
  # filter(age < 65) %>%
  filter(is.na(date.of.vaccination.3rd)) %>% # sensitivity analysis 8
  mutate(nid = seq_along(id)) %>%
  filter(vaccine.brand.1st == "BioNTech/Fosun"|is.na(vaccine.brand.1st)) %>%
  # filter(vaccine.brand.1st == "Sinovac"|is.na(vaccine.brand.1st)) %>%
  # mutate(dob = as_date(paste0(dob_y, "-01-01"))) %>% 
  mutate(dob = as_date("2021-01-01")) %>% 
  mutate(eventdate = as.integer(as_date(eventdate) - dob)) %>% 
  mutate(obs_start = as.integer(as_date("2021-02-23")-dob)) %>% 
  mutate(obs_end = as.integer(pmin(as_date("2022-02-01"), as_date(death_date_ymd), na.rm = T)-dob)) %>% 
  mutate(vaccdate1 = as.integer(as_date(date.of.vaccination.1st)-dob)) %>% 
  mutate(vaccdate2 = as.integer(as_date(date.of.vaccination.2nd)-dob)) %>% 
  mutate(vaccdate3 = as.integer(as_date(date.of.vaccination.3rd)-dob)) %>% 
  mutate(vaccdate1_p14 = as.integer(vaccdate1 + 14)) %>% 
  mutate(vaccdate2_p14 = as.integer(vaccdate2 + 14)) %>% 
  mutate(vaccdate3_p14 = as.integer(vaccdate3 + 14)) %>% 
  select(-match, -age.1st, -sex.1st,  -dose.sequence.1st, -date.of.vaccination.1st, -site.of.vaccination.1st, -age.2nd,
         -sex.2nd, -vaccine.brand.2nd, -dose.sequence.2nd, -date.of.vaccination.2nd, -site.of.vaccination.2nd, -dob_y,
         -district_res, -death_diag_cd, -death_ext_cd, -match_order, -index.date, -stroke.postbells, -ramsay.postbells)

ageq <- cumsum(c(90, 61, 61, 61, 61))

# (eventde_result <- standardsccs(event ~ vaccdate1+age, # sensitivity analysis 3
#              indiv = nid,
#              astart = obs_start,
#              aend = obs_end,
#              aevent = eventdate,
#              adrug = cbind(vaccdate1, vaccdate1_p14, vaccdate2, vaccdate2_p14, vaccdate3, vaccdate3_p14),
#              aedrug = cbind(vaccdate1+13, vaccdate1_p14 +13, vaccdate2+13, vaccdate2_p14+13, vaccdate3+13, vaccdate3_p14+13),
#              sameexpopar = F,
#              agegrp=ageq,
#              dataformat = "multi",
#              data = df3))

(eventde_result <- eventdepenexp(indiv = nid,
                                 astart = obs_start,
                                 aend = obs_end,
                                 aevent = eventdate,
                                 adrug = cbind(vaccdate1, vaccdate1_p14, vaccdate2, vaccdate2_p14),
                                 aedrug = cbind(vaccdate1+13, vaccdate1_p14 +13, vaccdate2+13, vaccdate2_p14+13),
                                 sameexpopar = F,
                                 agegrp=ageq,
                                 dataformat = "multi",
                                 data = df3))
str_c(round(eventde_result$conf.int[seq(1,4),], 2)[,1], "(",
      round(eventde_result$conf.int[seq(1,4),], 2)[,3], "-",
      round(eventde_result$conf.int[seq(1,4),], 2)[,4], ")")

mace_data <- formatdata(indiv = nid,
                        astart = obs_start,
                        aend = obs_end,
                        aevent = eventdate,
                        adrug = cbind(vaccdate1, vaccdate1_p14, vaccdate2, vaccdate2_p14),
                        aedrug = cbind(vaccdate1+13, vaccdate1_p14 +13, vaccdate2+13, vaccdate2_p14+13),
                        sameexpopar = F,
                        agegrp=ageq,
                        dataformat = "multi",
                        data = filter(df3, vaccinated == 1))
tapply(mace_data$event, mace_data$vaccdate1, sum)
tapply(mace_data$interval, mace_data$vaccdate1, sum)/365

result <- rbind(cbind(eventde_result$conf.int, eventde_result$coefficients)[, c(1,3,4,9)][seq(1,4),]) %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate_all(format, nsmall = 2)
colnames(result) <- c("exp", "lower", "upper", "p")
result <- result %>%
  mutate(IRR = str_c(exp, " (", lower, "-", upper, ")", sep = "")) %>% 
  mutate(p = if_else(p == "0.00", "<.01", p)) %>% 
  select(IRR, p)
result <- rbind(rep("", 2), result[seq(1,4),])
result <- result %>% mutate(No.events = c(tapply(mace_data$event, mace_data$vaccdate1, sum))) %>% 
  mutate(follow.up = c(tapply(mace_data$interval, mace_data$vaccdate1, sum))) %>% 
  mutate(No.events = as.integer(No.events)) %>% 
  mutate(follow.up = as.integer(follow.up)) %>% 
  mutate(absolute.rate = format(round(No.events*1000/follow.up, digits = 1), nsmall = 1)) %>% 
  select(No.events, follow.up, absolute.rate, IRR, p)
result <- rbind(rep("", 5), result[1,], rep("", 5), result[seq(2,3),], rep("", 5), result[seq(4,5),])
# write_csv(result, "dataset/sen8_biontech.csv")
# write_csv(result, "dataset/sen8_sinovac.csv")

