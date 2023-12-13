library(riskCommunicator) # version 1.0.1
library(tidyverse)
library(tableone) # version 0.13.2
library(mice)
library(gtsummary) # version 1.7.2
library(nhanesA) # version 0.7.4
###################################################### 
#### Framingham #### 
###################################################### 

data("framingham") ### outcome in framingham but not in NHANES

# The Framingham data has been used to create models for cardiovascular risk.
# The variable selection and model below are designed to mimic the models used
# in the paper General Cardiovascular Risk Profile for Use in Primary Care 
# This paper is available (cvd_risk_profile.pdf) on Canvas.

framingham_df <- framingham %>% dplyr::select(c(CVD, TIMECVD, SEX, TOTCHOL, AGE,
                                      SYSBP, CURSMOKE, DIABETES, BPMEDS,
                                      HDLC, BMI))
framingham_df <- na.omit(framingham_df)

framingham_df$SEX <- factor(framingham_df$SEX,labels=c("Male","Female"))
framingham_df$CURSMOKE <- factor(framingham_df$CURSMOKE,labels=c("No","Yes"))
framingham_df$DIABETES <- factor(framingham_df$DIABETES,labels=c("No","Yes"))
framingham_df$BPMEDS <- factor(framingham_df$BPMEDS,labels=c("No","Yes"))

CreateTableOne(data=framingham_df, strata = c("SEX"))

# Get blood pressure based on whether or not on BPMEDS
framingham_df$SYSBP_UT <- ifelse(framingham_df$BPMEDS == 0, 
                                 framingham_df$SYSBP, 0)
framingham_df$SYSBP_T <- ifelse(framingham_df$BPMEDS == 1, 
                                framingham_df$SYSBP, 0)

# Looking at risk within 15 years - remove censored data
dim(framingham_df)
framingham_df <- framingham_df %>%
  filter(!(CVD == 0 & TIMECVD <= 365*15)) %>%
  select(-c(TIMECVD))

# for the summary table purpose, we do not include two variables created by us
framingham_df_table <- framingham_df %>% select(-SYSBP_UT,-SYSBP_T)

# Filter to each sex
framingham_df_men <- framingham_df %>% filter(SEX == 1)
framingham_df_women <- framingham_df %>% filter(SEX == 2)

# Fit models with log transforms for all continuous variables
mod_men <- glm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
                 log(SYSBP_T+1)+CURSMOKE+DIABETES, 
      data= framingham_df_men, family= "binomial")
mean((predict(mod_men,type = "response")-framingham_df_men$CVD)^2)

mod_women <- glm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
                   log(SYSBP_T+1)+CURSMOKE+DIABETES, 
               data= framingham_df_women, family= "binomial")

###################################################### 
#### NHANES #### 
###################################################### 
# The NHANES data here finds the same covariates among this national survey data
# blood pressure, demographic, bmi, smoking, and hypertension info
bpx_2017 <- nhanes("BPX_J") %>% 
  select(SEQN, BPXSY1 ) %>% 
  rename(SYSBP = BPXSY1)
demo_2017 <- nhanes("DEMO_J") %>% 
  select(SEQN, RIAGENDR, RIDAGEYR) %>% 
  rename(SEX = RIAGENDR, AGE = RIDAGEYR)
bmx_2017 <- nhanes("BMX_J") %>% 
  select(SEQN, BMXBMI) %>% 
  rename(BMI = BMXBMI)
smq_2017 <- nhanes("SMQ_J") %>%
  mutate(CURSMOKE = case_when(SMQ040 %in% c(1,2) ~ 1,
                              SMQ040 == 3 ~ 0, 
                              SMQ020 == 2 ~ 0)) %>%
  select(SEQN, CURSMOKE)
bpq_2017 <- nhanes("BPQ_J") %>% 
  mutate(BPMEDS = case_when(BPQ020 == 2 ~ 0,
                            BPQ040A ==2 ~ 0,
                            BPQ050A==1 ~1,
                            TRUE~NA)) %>%
  select(SEQN, BPMEDS) 
tchol_2017 <- nhanes("TCHOL_J") %>% 
  select(SEQN, LBXTC) %>% 
  rename(TOTCHOL = LBXTC)
hdl_2017 <- nhanes("HDL_J") %>% 
  select(SEQN, LBDHDD) %>% 
  rename(HDLC = LBDHDD)
diq_2017 <- nhanes("DIQ_J") %>% 
  mutate(DIABETES = case_when(DIQ010 == 1 ~ 1, 
                              DIQ010 %in% c(2,3) ~ 0, 
                              TRUE ~ NA)) %>%
  select(SEQN, DIABETES) 

# Join data from different tables
df_2017 <- bpx_2017 %>%
  full_join(demo_2017, by = "SEQN") %>%
  full_join(bmx_2017, by = "SEQN") %>%
  full_join(hdl_2017, by = "SEQN") %>%
  full_join(smq_2017, by = "SEQN") %>%
  full_join(bpq_2017, by = "SEQN") %>%
  full_join(tchol_2017, by = "SEQN") %>%
  full_join(diq_2017, by = "SEQN")

CreateTableOne(data = df_2017, strata = c("SEX"))
dim(df_2017)

# filter the data by age 
df_2017 <- df_2017 %>% filter(AGE>=30 & AGE<=74)
# checking missingness - acceptable missing after filter age
num_na <- sapply(df_2017,function(x) sum(is.na(x)))
pct_na <- round(num_na/dim(df_2017)[1]*100,2)
missing_stat <- data.frame(num=num_na,pct=pct_na)
missing_stat

# factorize
df_2017$SEX <- factor(df_2017$SEX,labels=c("Male","Female"))
# saveRDS(df_2017,"NHANES.RDS") # save for simulation
df_2017$CURSMOKE <- factor(df_2017$CURSMOKE,labels=c("No","Yes"))
df_2017$DIABETES <- factor(df_2017$DIABETES,labels=c("No","Yes"))
df_2017$BPMEDS <- factor(df_2017$BPMEDS,labels=c("No","Yes"))


###################################################### 
#### Train-test split and multiple imputation#### 
###################################################### 
# for source, D is the indicator for test
set.seed(2550) # Setting seed for reproducibility
# stratified by sex
splitIndex <- createDataPartition(framingham_df$SEX, p = .8, list = FALSE)
framingham_df$D <- 1-(1:nrow(framingham_df) %in% splitIndex)
# save the data
# saveRDS(framingham_df,"source.RDS")



# for target, split data into training and test sets
set.seed(2550) # Setting seed for reproducibility
# stratified by sex
splitIndex2 <- createDataPartition(df_2017$SEX, p = .8, list = FALSE)
target_sample <- as.logical(1-(1:nrow(df_2017) %in% splitIndex2))
# multiple imputation on target, ignore those in test
imp <- mice(df_2017 %>% select(-SEQN), m = 5, ignore = target_sample, print = FALSE, seed = 2550)

# save the data
# saveRDS(imp,"target.RDS")
# saveRDS(splitIndex2,"target_train_test.RDS")


###################################################### 
#### Summary Tables #### 
###################################################### 
###### Summary 1 ######
labels <- list(TOTCHOL ~"Total-C",
               SYSBP ~"Systolic BP",
               CURSMOKE ~ "Current Smoking Status",
               DIABETES ~ "Diabetes Status",
               BPMEDS ~ "BP Treatment",
               HDLC ~ "HDL-C")
framingham_df_table$S <- 1
df_2017_table <- df_2017
df_2017_table$S <- 0
composite_table <- bind_rows(framingham_df_table,df_2017_table)
composite_table$S <- factor(composite_table$S, levels=c(1,0),labels=c("Source","Target"))
composite_table <- composite_table %>% select(-SEQN)
tbl1 <- composite_table %>%
  mutate(S=paste(S)) %>%
  tbl_strata(strata = S, .tbl_fun = ~.x %>%
  tbl_summary(by=SEX,
              type = all_continuous() ~ "continuous2",
              statistic = list(
              all_continuous() ~ c(
                "{mean} ({sd})"),
              all_categorical() ~ "{n} / {N} ({p}%)"),
              digits = all_continuous() ~ 2,
              label = labels,
              missing_text = "(Missing)"
)) %>%
  bold_labels()
tbl1
saveRDS(tbl1,"tbl1new.RDS")
# saveRDS(tbl1,"tbl1.RDS")

###### Summary 2 ######
labels2 <- list(log.TOTCHOL ~"Log-transformed Total-C",
                log.SYSBP ~"Log-transformed Systolic BP",
               # CURSMOKE ~ "Current Smoking Status",
               # DIABETES ~ "Diabetes Status",
               # BPMEDS ~ "BP Treatment",
               log.HDLC ~ "Log-transformed HDL-C",
               log.AGE ~ "Log-transformed Age")
tbl2 <- composite_table %>%
  mutate(log.TOTCHOL=log(TOTCHOL),
         log.SYSBP=log(SYSBP),
         log.HDLC=log(HDLC),
         log.AGE=log(AGE)) %>%
  dplyr::select(log.TOTCHOL,log.AGE,log.SYSBP,log.HDLC,S,SEX) %>%
  mutate(S=paste(S)) %>%
  tbl_strata(strata = S, .tbl_fun = ~.x %>%
               tbl_summary(by=SEX,
                           type = all_continuous() ~ "continuous2",
                           statistic = list(
                             all_continuous() ~ c(
                               "{mean} ({sd})"),
                             all_categorical() ~ "{n} / {N} ({p}%)"),
                           digits = all_continuous() ~ 2,
                           label = labels2,
                           missing_text = "(Missing)"
               )) %>%
  bold_labels()
tbl2
# saveRDS(tbl2,"tbl2.RDS")

