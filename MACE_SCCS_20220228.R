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
  # filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]|^410")) %>%
  mutate(date = as_date(date)) %>% 
  filter(date < date("2021-02-23")|is.na(date)) %>% # have MI/IS before 2021-02-23
  # filter(date < date("2021-02-1")|is.na(date)) %>% # have MI/IS before 2021-02-1
  group_by(id) %>% 
  slice(1)

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

# flow chart numbers############################
df1 %>% filter(id %in% dx_pre$id) %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% 
  filter(!is.na(date.of.vaccination.1st)) %>% 
  filter(!is.na(date.of.vaccination.2nd)) %>% 
  filter(!is.na(date.of.vaccination.3rd)) %>% 
  .$vaccine.brand.3rd %>% table()
df1 %>% filter(id %in% dx_pre$id) %>% filter(vaccine.brand.1st == "Sinovac") %>% 
  filter(!is.na(date.of.vaccination.1st)) %>% 
  filter(!is.na(date.of.vaccination.2nd)) %>% 
  filter(!is.na(date.of.vaccination.3rd)) %>% 
  .$vaccine.brand.3rd %>% table()
df1 %>% filter(id %in% dx_pre$id) %>% filter(is.na(date.of.vaccination.1st))

df1 %>% filter(id %in% dx_pre$id) %>% filter(!id %in% dx_mace$id) %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% 
  filter(!is.na(date.of.vaccination.1st)) %>% 
  filter(!is.na(date.of.vaccination.2nd)) %>% 
  filter(!is.na(date.of.vaccination.3rd)) %>% 
  .$vaccine.brand.3rd %>% table()
df1 %>% filter(id %in% dx_pre$id) %>% filter(!id %in% dx_mace$id) %>% filter(vaccine.brand.1st == "Sinovac") %>% 
  filter(!is.na(date.of.vaccination.1st)) %>% 
  filter(!is.na(date.of.vaccination.2nd)) %>% 
  filter(!is.na(date.of.vaccination.3rd)) %>% 
  .$vaccine.brand.3rd %>% table()
df1 %>% filter(id %in% dx_pre$id) %>% filter(!id %in% dx_mace$id) %>% filter(is.na(date.of.vaccination.1st))

df2 %>% filter(vaccine.brand.1st == "BioNTech/Fosun") %>% 
  filter(!is.na(date.of.vaccination.1st)) %>% 
  filter(!is.na(date.of.vaccination.2nd)) %>% 
  filter(!is.na(date.of.vaccination.3rd)) %>% 
  .$vaccine.brand.3rd %>% table()
df2 %>% filter(vaccine.brand.1st == "Sinovac") %>% 
  filter(!is.na(date.of.vaccination.1st)) %>% 
  filter(!is.na(date.of.vaccination.2nd)) %>% 
  filter(!is.na(date.of.vaccination.3rd)) %>% 
  .$vaccine.brand.3rd %>% table()
df2 %>% filter(is.na(date.of.vaccination.1st))
##############################################

df3 <- df2 %>% 
  mutate(nid = seq_along(id)) %>%
  # filter(vaccine.brand.1st == "BioNTech/Fosun"|is.na(vaccine.brand.1st)) %>%
  filter(vaccine.brand.1st == "Sinovac"|is.na(vaccine.brand.1st)) %>%
  # mutate(dob = as_date(paste0(dob_y, "-01-01"))) %>% 
  mutate(dob = as_date("2021-01-01")) %>% 
  mutate(eventdate = as.integer(as_date(eventdate) - dob)) %>% 
  mutate(obs_start = as.integer(as_date("2021-02-23")-dob)) %>% 
  mutate(vaccdate1 = as.integer(as_date(date.of.vaccination.1st)-dob)) %>% 
  mutate(vaccdate2 = as.integer(as_date(date.of.vaccination.2nd)-dob)) %>% 
  mutate(vaccdate3 = as.integer(as_date(date.of.vaccination.3rd)-dob)) %>% 
  mutate(vaccdate1_p14 = as.integer(vaccdate1 + 14)) %>% 
  mutate(vaccdate2_p14 = as.integer(vaccdate2 + 14)) %>% 
  mutate(vaccdate3_p14 = as.integer(vaccdate3 + 14)) %>% 
  mutate(obs_end = as.integer(pmin(as_date("2022-02-01"), as_date(death_date_ymd), na.rm = T)-dob)) %>%
  # mutate(obs_end = pmin(as.integer(as_date("2022-02-01")), as.integer(as_date(death_date_ymd)), vaccdate3, na.rm = T)-as.integer(dob)) %>% # censor at third dose
  select(-match, -age.1st, -sex.1st,  -dose.sequence.1st, -date.of.vaccination.1st, -site.of.vaccination.1st, -age.2nd,
         -sex.2nd, -vaccine.brand.2nd, -dose.sequence.2nd, -date.of.vaccination.2nd, -site.of.vaccination.2nd, -dob_y,
         -district_res, -death_diag_cd, -death_ext_cd, -match_order, -index.date, -stroke.postbells, -ramsay.postbells)

# df3 <- df3 %>% mutate(eventdate = if_else(nid == 3174, 357, as.double(eventdate))) %>% mutate(eventdate = if_else(nid == 3174, 357, as.double(eventdate)))
# df3 %>% filter(nid %in% c(1295,1916,2916,6888))
# df3 %>% filter(!is.na(vaccdate3)) %>% filter(eventdate > 320)
# df3 %>% filter(nid %in% c(1334,7007,8430))
# df3 %>% filter(!is.na(vaccdate3)) %>% filter(eventdate > 320)

# ageq <- floor(quantile(df3$eventdate, seq(0.1,0.9,0.1),
#                        names=F, na.rm = T))

ageq <- cumsum(c(90, 61, 61, 61, 61))
# ageq <- cumsum(c(59,31,30,31,30,31,31,30,31,30,31))

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
                        data = df3)
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
# write_csv(result, "dataset/biontech.csv")
# write_csv(result, "dataset/sinovac.csv")


(sccs_result <- standardsccs(event ~ vaccdate1+age,
             indiv = nid,
             astart = obs_start,
             aend = obs_end,
             aevent = eventdate,
             adrug = cbind(vaccdate1, vaccdate1_p14, vaccdate2, vaccdate2_p14),
             aedrug = cbind(vaccdate1+13, vaccdate1_p14 +13, vaccdate2+13, vaccdate2_p14+13),
             sameexpopar = F,
             agegrp=ageq,
             dataformat = "multi",
             data = df3))
str_c(round(sccs_result$conf.int[seq(1,4),], 2)[,1], "(",
      round(sccs_result$conf.int[seq(1,4),], 2)[,3], "-",
      round(sccs_result$conf.int[seq(1,4),], 2)[,4], ")")

