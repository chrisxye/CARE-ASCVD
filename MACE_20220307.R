library(tidyverse)
library(tidylog)
library(tableone)
library(MatchIt)
library(survival)
library(lubridate)
library(ggpubr)

df <- read_rds("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/full_5.cohort.matched.bll.evippc.RDS") %>% as_tibble()
# dx_obesity <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20211022/dx_obesity.rds")
dx_ASCVD <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/CARE Part 3/CARE data_20220228/dx_ASCVD.rds")
dx_ASCVD <- dx_ASCVD %>% mutate(index.date = df$index.date[match(id, df$id)])
# px_cohort <- dx_latest %>% filter(str_detect(code, "^36\\.[0123]")) %>% 
#   mutate(date = as_date(date)) %>% filter(date < index.date|is.na(date))

# dx_mi_outcome <- dx_ASCVD %>% filter(str_detect(code, "^410")) %>% 
#   mutate(date = as_date(date)) %>% filter(date >= index.date) %>% 
#   group_by(id) %>% 
#   filter(date ==min(date))
# dx_is_outcome <- dx_ASCVD %>% filter(str_detect(code, "^433.[012389]1|^43[46]|^437.[01]")) %>% 
#   mutate(date = as_date(date)) %>% filter(date >= index.date) %>% 
#   group_by(id) %>% 
#   filter(date ==min(date))
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
  rbind(select(rename(filter(df, str_detect(death_diag_cd, "^I")), date = death_date_ymd), id, date)) %>% # add cardiovascular death into the definition of MACE
  mutate(index.date = df$index.date[match(id, df$id)]) %>% 
  filter(date >= index.date) %>% 
  arrange(id, date) %>% 
  group_by(id) %>% # not recurrent event to be consider
  slice(1) %>% 
  ungroup()

df <- df %>% 
  mutate(death_date_ymd = as_date(death_date_ymd)) %>% 
  # mutate(dx.obesity = if_else(id %in% dx_obesity$id, 1, 0)) %>% 
  mutate(outcome.date = dx_mace$date[match(id, dx_mace$id)])
  # mutate(outcome.mi.date = dx_mi_outcome$date[match(id, dx_mi_outcome$id)]) %>% 
  # mutate(outcome.is.date = dx_is_outcome$date[match(id, dx_is_outcome$id)]) %>% 
  # mutate(outcome.cardiovascular.death.date = if_else(str_detect(death_diag_cd, "^I[0-9][0-9]"), death_date_ymd, NA_Date_))
  
df1 <- df %>% 
  # filter(dx.vascdz == 1|dx.cbd == 1|dx.pvd == 1) %>%
  # filter(dx.mi == 1|dx.stroke_isch == 1) %>%
  filter(id %in% dx_pre$id) %>% 
  filter(death_date_ymd > index.date|is.na(death_date_ymd)) %>% 
  filter(index.date <= date("2022-01-03"))
# df1 <- df %>% filter(dx.mi == 1|dx.stroke_isch == 1|dx.tia == 1) %>% filter(index.date <= date("2021-07-03"))

df1 <- df1 %>% mutate(vaccine = ifelse(is.na(`Vaccine Brand.1st`), "not vaccinated", `Vaccine Brand.1st`))
df1$index.date <- as_date(df1$index.date)
df1$death_date_ymd <- as_date(df1$death_date_ymd)
df1$lab.lipid.chol <- as.numeric(df1$lab.lipid.chol)
df1$lab.lipid.hdl <- as.numeric(df1$lab.lipid.hdl)
df1$lab.lipid.ldl.calc <- as.numeric(df1$lab.lipid.ldl.calc)
# df1[seq(150, 183)] <- lapply(df1[seq(150, 183)], as_date)


# clean covariates
df1 <- df1 %>% 
  # mutate(dx.revascularization = if_else(id %in% px_cohort$id, 1, 0)) %>% 
  mutate(dx.liver = if_else(dx.liver_mild == 1|dx.liver_modsev == 1, 1, 0)) %>% 
  mutate(dx.infection = if_else(dx.infection_resp == 1|dx.infection_viral == 1, 1, 0)) %>% 
  select(-dx.liver_mild, -dx.liver_modsev, -dx.infection_resp, -dx.infection_viral) %>% 
  select(-dx.dm_com0, -dx.dm_com1, -dx.aids, -dx.cancer, -dx.cancer_mets, -dx.stroke_embo, -dx.asthma, -dx.ramsayhunt, -dx.t2dm, -dx.thrombo_arterial, -dx.thrombo_venous,
         -dx.mental, -dx.schizo, -dx.bipolar, -dx.depress, -dx.anxiety, -dx.respdz, -dx.cvd, -dx.lipid, -dx.suicide, -dx.tobacco, -dx.alcohol, -dx.substance, -dx.neutropenia, -dx.myocarditis, -dx.eps) %>% 
  select(-rx.bevacizumab, -rx.transamin, -rx.ivig, -rx.adreno, -rx.hormone_oc, -rx.hormone_hrt, -rx.hormone_cancer, -rx.antipsychotic, -rx.adhd, -rx.hypnotic, -rx.anxiolytic, -rx.epilepsy, 
         -rx.epilepsy_ctrl, -rx.gout, -rx.antiviral, -rx.antibiotics, -rx.immunsupp, -rx.hormone, -rx.antidepressant) %>% 
  select(-starts_with("hx")) %>% 
  select(starts_with("dx"), starts_with("rx"), everything())

df2 <- df1 %>% 
  # mutate(lab.lipid.nhdl = lab.lipid.chol - lab.lipid.hdl) %>%
  # filter(!is.na(lab.lipid.chol)) %>%
  # filter(!is.na(lab.lipid.ldl.calc)) %>%
  # filter(ip.28 == 0) %>%
  # just updated, add mace
  # mutate(outcome.mace.date = pmin(outcome.mi.date, outcome.is.date, outcome.cardiovascular.death.date, na.rm = T)) %>%
  mutate(outcome.mace.date = outcome.date) %>%
  mutate(censor.mace.date = pmin(outcome.mace.date, death_date_ymd, date("2022-01-31"), na.rm = T)) %>% 
  mutate(censor.mace.date = pmin(outcome.mace.date, death_date_ymd, date("2022-01-31"), na.rm = T)) %>% 
  mutate(outcome.mace = if_else(outcome.mace.date <= censor.mace.date, 1, 0, missing = 0)) %>%
  mutate(follow.up = as.numeric(censor.mace.date - index.date)) %>% 
  # just updated, add mace
  mutate(death = as.factor(if_else(!is.na(death_date_ymd), 1, 0))) %>% 
  mutate(vaccine = ifelse(is.na(`Vaccine Brand.1st`), "not vaccinated", `Vaccine Brand.1st`)) %>% 
  mutate(vaccinated.biontech = ifelse(vaccine == "BioNTech/Fosun", 1, 0)) %>% 
  mutate(vaccinated.sinovac = ifelse(vaccine == "Sinovac", 1, 0))
df2$vaccine <- factor(df2$vaccine, c("BioNTech/Fosun", "Sinovac", "not vaccinated"))

df2[,seq(1, 38)] <- lapply(df2[,seq(1, 38)], as.factor)


# PSM
formula <- str_c(c("vaccinated ~ Age+sex+attn.N.ae", colnames(select(df2, starts_with("dx"), starts_with("rx")))), collapse = "+")
# formula <- str_c(c("vaccinated ~ Age+sex+dx.htn+dx.chf+dx.dm+dx.smoking+dx.obesity+rx.lipid+rx.antipsychotic", colnames(select(df2, starts_with("dx"), starts_with("rx")))), collapse = "+")
PSM1 <- matchit(formula(formula), data = filter(df2, vaccinated.sinovac == 0), method = "nearest", ratio = 1, caliper = 0.2)
PSM2 <- matchit(formula(formula), data = filter(df2, vaccinated.biontech == 0), method = "nearest", ratio = 1, caliper = 0.2)

df2 %>% filter(vaccinated.sinovac == 1) %>% filter(!is.na(`Date of vaccination.1st`)) %>% filter(is.na(`Date of vaccination.2nd`))
match.data(PSM1) %>% filter(!is.na(`Date of vaccination.1st`)) %>% filter(is.na(`Date of vaccination.2nd`))
match.data(PSM2) %>% filter(!is.na(`Date of vaccination.1st`)) %>% filter(is.na(`Date of vaccination.2nd`))
  

model <- glm(formula(formula), data = filter(df2, vaccinated.sinovac == 0), family = "binomial")
plot1 <- filter(df2, vaccinated.sinovac == 0) %>%
  mutate(prop.score = predict(model, type = "response")) %>%
  mutate(vaccinated = as.factor(vaccinated)) %>%
  ggplot() +
  geom_density(aes(prop.score, color = vaccinated), size = 1) +
  labs(title = "BNT162b2 Before matching") +
  theme_classic() +
  scale_x_continuous(name = "propensity score", breaks = seq(0, 1, 0.1))

# plot(PSM1, type = "jitter")
plot2 <- match.data(PSM1, distance = "prop.score") %>%
  mutate(vaccinated = as.factor(vaccinated)) %>%
  ggplot() +
  geom_density(aes(prop.score, color = vaccinated), size = 1) +
  labs(title = "BNT162b2 After matching") +
  theme_classic() +
  scale_x_continuous(name = "propensity score", breaks = seq(0, 1, 0.1))

model <- glm(formula(formula), data = filter(df2, vaccinated.biontech == 0), family = "binomial")
plot3 <- filter(df2, vaccinated.biontech == 0) %>%
  mutate(prop.score = predict(model, type = "response")) %>%
  mutate(vaccinated = as.factor(vaccinated)) %>%
  ggplot() +
  geom_density(aes(prop.score, color = vaccinated), size = 1) +
  labs(title = "CoronaVac Before matching") +
  theme_classic() +
  scale_x_continuous(name = "propensity score", breaks = seq(0, 1, 0.1))

# plot(PSM1, type = "jitter")
plot4 <- match.data(PSM2, distance = "prop.score") %>%
  mutate(vaccinated = as.factor(vaccinated)) %>%
  ggplot() +
  geom_density(aes(prop.score, color = vaccinated), size = 1) +
  labs(title = "CoronaVac After matching") +
  theme_classic() +
  scale_x_continuous(name = "propensity score", breaks = seq(0, 1, 0.1))
# tiff("response-figure2.tif", width = 960, height = 960, res = 144)
# ggarrange(plot1, plot2, plot3, plot4, common.legend = T, legend="right", ncol = 2, nrow = 2)
# dev.off()

# crude result
cox_all <- coxph(Surv(follow.up, outcome.mace) ~ vaccinated.biontech + vaccinated.sinovac, df2)
summary(cox_all)
# incidence
dim(df_biontech <- df2 %>% filter(vaccinated.biontech == 1))[[1]]
dim(df_sinovac <- df2 %>% filter(vaccinated.sinovac == 1))[[1]]
dim(ref <- df2 %>% filter(vaccinated == 0))[[1]]
str_c(dim(filter(df_biontech, outcome.mace == 1))[1], "/", as.numeric(sum(df_biontech$follow.up)), "/", 
      format(round(dim(filter(df_biontech, outcome.mace == 1))[1]*10000/as.numeric(sum(df_biontech$follow.up)), 2), nsmall = 2), sep = "")
str_c(dim(filter(df_sinovac, outcome.mace == 1))[1], "/", as.numeric(sum(df_sinovac$follow.up)), "/", 
      format(round(dim(filter(df_sinovac, outcome.mace == 1))[1]*10000/as.numeric(sum(df_sinovac$follow.up)), 2), nsmall = 2), sep = "")
str_c(dim(filter(ref, outcome.mace == 1))[1], "/", as.numeric(sum(df_sinovac_ref$follow.up)), "/", 
      format(round(dim(filter(df_sinovac_ref, outcome.mace == 1))[1]*10000/as.numeric(sum(df_sinovac_ref$follow.up)), 2), nsmall = 2), sep = "")


# main analysis
cox_all <- coxph(Surv(follow.up, outcome.mace) ~ vaccinated.biontech + vaccinated.sinovac + strata(subclass), data = match.data(PSM1))
summary(cox_all)
cox_all <- coxph(Surv(follow.up, outcome.mace) ~ vaccinated.biontech + vaccinated.sinovac + strata(subclass), data = match.data(PSM2))
summary(cox_all)

# incidence
dim(df_biontech <- match.data(PSM1) %>% filter(vaccinated.biontech == 1))[[1]]
dim(df_biontech_ref <- match.data(PSM1) %>% filter(subclass %in% df_biontech$subclass) %>% filter(vaccinated == 0))[[1]]
dim(df_sinovac <- match.data(PSM2) %>% filter(vaccinated.sinovac == 1))[[1]]
dim(df_sinovac_ref <- match.data(PSM2) %>% filter(subclass %in% df_sinovac$subclass) %>% filter(vaccinated == 0))[[1]]
str_c(dim(filter(df_biontech, outcome.mace == 1))[1], "/", as.numeric(sum(df_biontech$follow.up)), "/", 
      format(round(dim(filter(df_biontech, outcome.mace == 1))[1]*10000/as.numeric(sum(df_biontech$follow.up)), 2), nsmall = 2), sep = "")
str_c(dim(filter(df_biontech_ref, outcome.mace == 1))[1], "/", as.numeric(sum(df_biontech_ref$follow.up)), "/", 
      format(round(dim(filter(df_biontech_ref, outcome.mace == 1))[1]*10000/as.numeric(sum(df_biontech_ref$follow.up)), 2), nsmall = 2), sep = "")
str_c(dim(filter(df_sinovac, outcome.mace == 1))[1], "/", as.numeric(sum(df_sinovac$follow.up)), "/", 
      format(round(dim(filter(df_sinovac, outcome.mace == 1))[1]*10000/as.numeric(sum(df_sinovac$follow.up)), 2), nsmall = 2), sep = "")
str_c(dim(filter(df_sinovac_ref, outcome.mace == 1))[1], "/", as.numeric(sum(df_sinovac_ref$follow.up)), "/", 
      format(round(dim(filter(df_sinovac_ref, outcome.mace == 1))[1]*10000/as.numeric(sum(df_sinovac_ref$follow.up)), 2), nsmall = 2), sep = "")



# Table 1
vars <- c("Age", "sex", "death", "attn.N.ae", colnames(select(df2, starts_with("dx"), starts_with("rx"))))
df.pre <- df2 %>% select(vars, "vaccine")
df.after1 <- match.data(PSM1) %>% select(vars, "vaccinated.biontech")
df.after2 <- match.data(PSM2) %>% select(vars, "vaccinated.sinovac")
tableone1 <- CreateTableOne(vars = vars, strata = c("vaccine"), data = df.pre, test = T)
tableone2 <- CreateTableOne(vars = vars, strata = c("vaccinated.biontech"), data = df.after1, test = T)
tableone3 <- CreateTableOne(vars = vars, strata = c("vaccinated.sinovac"), data = df.after2, test = T)
print(tableone1, smd = T, test = T)
print(tableone2, smd = T, test = T)
print(tableone3, smd = T, test = T)
tableone <- cbind(print(tableone1, printToggle = F, smd = T), print(tableone2, printToggle = F, smd = T), print(tableone3, printToggle = F, smd = T))
print(tableone, quote = F, noSpaces = F)
# write.csv(print(tableone, quote = F, noSpaces = F), file = "response Table 3.csv")
