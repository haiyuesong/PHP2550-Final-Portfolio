# load the library
library(tidyverse) # version 1.3.2
library(kableExtra) # version 1.3.4
library(mice) # version 3.15.0
library(psych) # version 2.2.9
library(ggpubr) # version 0.4.0
library(gtsummary) # version 1.7.2
library(caret) # version 6.0-93

# read the data
df <- read_csv("project2.csv", 
               col_types = cols(bw = col_number(), ga = col_number(), 
                                blength = col_number(), 
                                birth_hc = col_number(), 
                                weight_today.36 = col_number(), 
                                inspired_oxygen.36 = col_number(), 
                                p_delta.36 = col_number(), 
                                peep_cm_h2o_modified.36 = col_number(), 
                                med_ph.36 = col_double(), 
                                hosp_dc_ga = col_number()))


###### simple pre-processing steps
# fill the center
df$center[is.na(df$center)] <- 1

# delete duplicate data
# duplicated(df)
df <- unique(df)

# check the missing in Death
# View(df[is.na(df$Death),])
# id: 2000016, fill Death with "No"; id: 4000014, drop the record
# df$Death[df$record_id=="2000016"] <- "No"
# delete two records where Death is missing
df <- df %>% filter(!is.na(Death))

# combine the outcome
df$outcome <- case_when(df$Trach==1 | df$Death=="Yes" ~ 1,
                        .default=0)
# drop the id
df <- df %>% dplyr::select(-record_id)

# drop center 20 and 21
df <- df %>% filter(!center %in% c(20,21))

###### factor
# factorize the categorical data
df$center <- factor(df$center)
df$mat_race <- factor(df$mat_race)
df$mat_ethn <- factor(df$mat_ethn,levels = c("1","2"),
                      labels = c("Hispanic or Latino","Not Hispanic or Latino"))
df$del_method <- factor(df$del_method,levels = c("1","2"),
                        labels = c("Vaginal delivery","Cesarean section"))
df$prenat_ster <- factor(df$prenat_ster)
df$com_prenat_ster <- factor(df$com_prenat_ster)
df$mat_chorio <- factor(df$mat_chorio)
df$gender <- factor(df$gender)
df$sga <- factor(case_when(df$sga=="Not SGA"~"No",
                           df$sga=="SGA"~"Yes"))
df$any_surf <- factor(df$any_surf)
df$ventilation_support_level.36 <- 
  factor(df$ventilation_support_level.36,
         levels=c("0","1","2"),
         labels=c("No respiratory support or supplemental oxygen",
                  "Non-invasive positive pressure",
                  "Invasive positive pressure"))
df$ventilation_support_level_modified.44 <- factor(df$ventilation_support_level_modified.44)
df$med_ph.36 <- factor(df$med_ph.36,levels=c(0,1),labels = c("No","Yes"))
df$med_ph.44 <- factor(df$med_ph.44)

df$Trach <- factor(df$Trach,levels=c(0,1),labels = c("No","Yes"))
df$Death <- factor(df$Death)
df$outcome <- factor(df$outcome,levels=c(0,1),labels = c("No Trach or Death",
                                                         "Trach or Death"))

# checking missingness
num_na <- sapply(df,function(x) sum(is.na(x)))
pct_na <- round(num_na/dim(df)[1]*100,2)
missing_stat <- data.frame(num=num_na,pct=pct_na)
missing_stat[missing_stat$pct>=15,]

# delete columns measured at 44 weeks
# focusing on just use wk 36 data
df <- df %>%
  dplyr::select(-c(weight_today.44:med_ph.44))

# steriod: if prenat_ster is no, then com_prenat_ster is no
table(df$prenat_ster,df$com_prenat_ster,useNA = "ifany")
df$com_prenat_ster[df$prenat_ster=="No"] <- "No"

###### any_surf ######
# any_surf: what contributed to that column being NA
# table(df$any_surf,df$ventilation_support_level.36,useNA = "ifany")
# missing indicator of any_surf
# df$any_surf_missing_indicator <- as.factor(ifelse(is.na(df$any_surf), 1, 0))
# try logistic regression for missing indicator
# missingness_model <- glm(any_surf ~ .,data=df,family = "binomial")
# summary(missingness_model)
table(df$any_surf,df$outcome)
chisq.test(df$any_surf,df$outcome)
# drop any_surf
df <- df %>% dplyr::select(-any_surf)

df <- df %>% dplyr::select(-mat_race)

###### check the outliers
# there is a hospital discharge gestational age of 573.90 weeks and 310.60 weeks, 
# we identify this as outlier and drop this record
# boxplot(df$hosp_dc_ga, horizontal=TRUE, notch=TRUE)
# IQR_value <- IQR(df$hosp_dc_ga,na.rm = TRUE)
# Q1 <- quantile(df$hosp_dc_ga, 0.25,na.rm = TRUE)
# Q3 <- quantile(df$hosp_dc_ga, 0.75,na.rm = TRUE)
# outliers <- subset(df$hosp_dc_ga, df$hosp_dc_ga < (Q1 - 1.5 * IQR_value) | 
#                      df$hosp_dc_ga > (Q3 + 1.5 * IQR_value))

df %>% filter(hosp_dc_ga >=300) %>% View()
df <- df[-which(df$hosp_dc_ga>300),]
# check the difference between hosp_dc_ga and ga
df %>% filter(hosp_dc_ga - ga <= 0.2) %>% View()
df <- df[-which(df$hosp_dc_ga - df$ga<=0.2),]

###### Summary 1 (Not included in final report) ######
labels <- list(mat_ethn ~ "Maternal Ethnicity",
               bw ~ "Birth Weight (g)",
               ga ~ "Gestational Age",
               blength ~ "Birth Length (cm)",
               birth_hc ~ "Birth Head Circumference (cm)",
               del_method ~ "Delivery Method",
               prenat_ster ~ "Prenatal Corticosteroids",
               com_prenat_ster ~ "Complete Prenatal Steroids",
               mat_chorio ~ "Maternal Chorioamnionitis",
               gender ~ "Gender",
               sga ~ "Small for Gestational Age",
               # any_surf ~ "Surfactant in the first 72 hours",
               weight_today.36 ~"Weight at 36 weeks (g)",
               ventilation_support_level.36 ~ "Ventilation Support Level at 36 weeks",
               inspired_oxygen.36 ~ "Fraction of Inspired Oxygen at 36 weeks",
               p_delta.36 ~ "Peak Inspiratory Pressure (cmH2O) at 36 weeks",
               peep_cm_h2o_modified.36 ~ "Positive and exploratory pressure (cm H2O) at 36 weeks",
               med_ph.36 ~ "Medication for Pulmonary Hypertension at 36 weeks",
               # weight_today.44 ~ "Weight at 44 weeks",
               # ventilation_support_level_modified.44 ~ "Ventilation support level at 44 weeks",
               # inspired_oxygen.44 ~ "Fraction of Inspired Oxygen needed at 44 weeks",
               # p_delta.44 ~	"Peak Inspiratory Pressure (cmH2O) needed at 44 weeks",
               # peep_cm_h2o_modified.44 ~	"Positive end exploratory pressure (cm H2O) needed at 44 weeks",
               # med_ph.44	~ "Medication for Pulmonary Hypertension at 44 weeks",
               hosp_dc_ga ~ "Hospital Discharge Gestational Age",
               Trach ~ "Trachoestomy",
               Death ~ "Death")
tbl1 <- tbl_summary(df[,-c(1,22)],
                    type = all_continuous() ~ "continuous2",
                    statistic = list(
                      all_continuous() ~ c(
                        "{mean} ({sd}); {median} ({min}, {max})"
                      ),
                      all_categorical() ~ "{n} / {N} ({p}%)"),
                    digits = all_continuous() ~ 2,
                    label = labels,
                    missing_text = "(Missing)"
) %>%
  bold_labels() 
# tbl1
# saveRDS(tbl1,"Project/Project 2/tbl1.RDS")
df <- df %>% dplyr::select(-Death,-Trach)

###### Summary 2 (Included in final report as Table 1) ######
labels2 <- list(#center~ "Medical Center",
  mat_ethn ~ "Maternal Ethnicity",
  bw ~ "Birth Weight (g)",
  ga ~ "Gestational Age",
  blength ~ "Birth Length (cm)",
  birth_hc ~ "Birth Head Circumference (cm)",
  del_method ~ "Delivery Method",
  prenat_ster ~ "Prenatal Corticosteroids",
  com_prenat_ster ~ "Complete Prenatal Steroids",
  mat_chorio ~ "Maternal Chorioamnionitis",
  gender ~ "Gender",
  sga ~ "Small for Gestational Age",
  hosp_dc_ga ~ "Hospital Discharge Gestational Age"
)
tbl2 <- tbl_summary(df[,-c(1,13:18)], by="outcome",
                    type = all_continuous() ~ "continuous2",
                    statistic = list(
                      all_continuous() ~ c(
                        "{mean} ({sd})"
                      ),
                      all_categorical() ~ "{n} / {N} ({p}%)"),
                    digits = all_continuous() ~ 2,
                    label = labels2,
                    missing_text = "(Missing)") %>%
  add_p(all_categorical() ~ "chisq.test") %>%
  add_overall() %>%
  bold_labels() 
# tbl2
# saveRDS(tbl2,"Project/Project 2/tbl2.RDS")

###### Summary 3 (Included in final report as Table 2) ######
labels3 <- list(weight_today.36 ~"Weight at 36 weeks (g)",
                ventilation_support_level.36 ~ "Ventilation Support Level at 36 weeks",
                inspired_oxygen.36 ~ "Fraction of Inspired Oxygen at 36 weeks",
                p_delta.36 ~ "Peak Inspiratory Pressure (cmH2O) at 36 weeks",
                peep_cm_h2o_modified.36 ~ 
                  "Positive and exploratory pressure (cm H2O) at 36 weeks",
                med_ph.36 ~ "Medication for Pulmonary Hypertension at 36 weeks"
)

tbl3 <- tbl_summary(df[,c(13:18,20)], by="outcome",
                    type = all_continuous() ~ "continuous2",
                    statistic = list(
                      all_continuous() ~ c(
                        "{mean} ({sd})",
                        "{median} ({min}, {max})"
                      ),
                      all_categorical() ~ "{n} / {N} ({p}%)"),
                    digits = all_continuous() ~ 2,
                    label = labels3,
                    missing_text = "(Missing)") %>%
  add_p(all_categorical() ~ "chisq.test") %>%
  add_overall() %>%
  bold_labels() 
# tbl3
# saveRDS(tbl3,"Project/Project 2/tbl3.RDS")


# gtsummary by center
#### Table 3 in report
labels4 <- list(#center~ "Medical Center",
  mat_ethn ~ "Maternal Ethnicity",
  bw ~ "Birth Weight (g)",
  ga ~ "Gestational Age",
  blength ~ "Birth Length (cm)",
  birth_hc ~ "Birth Head Circumference (cm)",
  del_method ~ "Delivery Method",
  prenat_ster ~ "Prenatal Corticosteroids",
  com_prenat_ster ~ "Complete Prenatal Steroids",
  mat_chorio ~ "Maternal Chorioamnionitis",
  gender ~ "Gender",
  sga ~ "Small for Gestational Age",
  weight_today.36 ~"Weight at 36 weeks (g)",
  ventilation_support_level.36 ~ "Ventilation Support Level at 36 weeks",
  inspired_oxygen.36 ~ "Fraction of Inspired Oxygen at 36 weeks",
  p_delta.36 ~ "Peak Inspiratory Pressure (cmH2O) at 36 weeks",
  peep_cm_h2o_modified.36 ~ 
    "Positive and exploratory pressure (cm H2O) at 36 weeks",
  med_ph.36 ~ "Medication for Pulmonary Hypertension at 36 weeks",
  outcome ~ "Outcome"
)
tbl4 <- tbl_summary(df , by="center",
            type = all_continuous() ~ "continuous2",
            statistic = list(
              all_continuous() ~ c(
                "{mean} ({sd})"
              ),
              all_categorical() ~ "{n} / {N} ({p}%)"),
            digits = all_continuous() ~ 2,
            label = labels4,
            missing_text = "(Missing)") %>%
  add_p(all_categorical() ~ "chisq.test") %>%
  bold_labels()
# saveRDS(tbl4,"tbl4.RDS")

# percentage of missing by center

df %>%
  group_by(center) %>%
  summarise(across(everything(), 
                   list(missing_pct = ~sum(is.na(.)) / n() * 100)))  %>%
  View()
###### multivariate exploratory data analysis
# fit the ANOVA model or Non-parametric Kruskal-Wallis test
summary(aov(inspired_oxygen.36 ~ ventilation_support_level.36, data = df)) # significant
kruskal.test(inspired_oxygen.36 ~ ventilation_support_level.36, data = df) # significant
summary(aov(p_delta.36 ~ ventilation_support_level.36, data = df)) # significant
kruskal.test(p_delta.36 ~ ventilation_support_level.36, data = df) # significant
summary(aov(peep_cm_h2o_modified.36 ~ ventilation_support_level.36, data = df)) # significant
kruskal.test(peep_cm_h2o_modified.36 ~ ventilation_support_level.36, data = df) # significant
# or use tbl_summary directly
tbl_summary(df , by=ventilation_support_level.36,
            type = all_continuous() ~ "continuous2",
            statistic = list(
              all_continuous() ~ c(
                "{mean} ({sd})"
              ),
              all_categorical() ~ "{n} / {N} ({p}%)"),
            digits = all_continuous() ~ 2,
            missing_text = "(Missing)") %>%
  add_p(all_categorical() ~ "chisq.test") 

tbl_summary(df , by=med_ph.36,
            type = all_continuous() ~ "continuous2",
            statistic = list(
              all_continuous() ~ c(
                "{mean} ({sd})"
              ),
              all_categorical() ~ "{n} / {N} ({p}%)"),
            digits = all_continuous() ~ 2,
            missing_text = "(Missing)") %>%
  add_p(all_categorical() ~ "chisq.test") 

tbl_summary(df , by=sga,
            type = all_continuous() ~ "continuous2",
            statistic = list(
              all_continuous() ~ c(
                "{mean} ({sd})"
              ),
              all_categorical() ~ "{n} / {N} ({p}%)"),
            digits = all_continuous() ~ 2,
            missing_text = "(Missing)") %>%
  add_p(all_categorical() ~ "chisq.test") 

# correlation
cor(cbind(df$bw,df$ga,df$blength,df$birth_hc,df$inspired_oxygen.36),use = "complete")

###### missing percentage checking before multiple imputation
num_na <- sapply(df,function(x) sum(is.na(x)))
pct_na <- round(num_na/dim(df)[1]*100,2)
missing_stat <- data.frame(num=num_na,pct=pct_na)
missing_stat

###### save the pre-processed data
# saveRDS(df,"Project/Project 2/preprocessed.RDS")

###### UPDATED: remove hospital discharge gestational age before multiple imputation
df <- df %>% select(-hosp_dc_ga)

###### train-test split
# split your data into training and test sets
set.seed(2550) # Setting seed for reproducibility
# get strata, which is interaction of outcome and center
df$strata <- interaction(df$outcome, df$center) 
table(df$strata) # check the levels of combined stratification
splitIndex <- createDataPartition(df$strata, p = .8, list = FALSE)
df <- df %>% dplyr::select(-strata)
table(df$outcome[splitIndex],df$center[splitIndex])
table(df$outcome[-splitIndex],df$center[-splitIndex])

##### Multiple Imputation with train-test split setting
full_indices <- 1:nrow(df)
ignore <- !(full_indices %in% splitIndex) # TRUE for test
# multiple imputation
imp <- mice(df, m = 5, ignore = ignore, print = FALSE, seed = 2550)
plot(imp) # check if converged
# get the imputed test
imp.test <- filter(imp, ignore)
# get the imputed train
imp.train <- filter(imp, !ignore)
# saveRDS(imp.train,file="training_mice.RDS")
# saveRDS(imp.test,file="test_mice.RDS")
