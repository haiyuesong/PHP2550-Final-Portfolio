# load the library
library(tidyverse) # version 1.3.2
library(kableExtra) # version 1.3.4
library(mice) # version 3.15.0
library(naniar) # version 1.0.0
library(tableone) # version 0.13.2
library(psych) # version 2.2.9
library(GGally) # version 2.1.2
library(ggpubr) # version 0.4.0
library(gtsummary) # version 1.7.2
# read the data
new_df <- read.csv("project1.csv")

###### clean the data and modify the format
# income: there is a "250,000", change it to 250000
new_df$income[6] <- 250000
# mom_numcig: How many cigarettes per day do you usually smoke?
# change 2 black and miles a day to 2
new_df$mom_numcig[1] <- 2
# change 20-25 cigarettes per day as 23
new_df$mom_numcig[37] <- 23
# change none cigarette per day as 0
new_df$mom_numcig[47] <- 0
# fill all records blank with NA
new_df[new_df==""] <- NA
# set the continuous variables as numeric data
for(i in c(2,14,20,21,29:36,43:51,52,63,65,67,69:78)){
  new_df[,i] <- as.numeric(new_df[,i])
}
# factorize the categorical data
new_df <- new_df %>% 
  mutate_if(is.character,as.factor)
new_df <- new_df %>%
  mutate_if(is.integer,as.factor)
# change facter levels
new_df <- new_df %>%
  mutate(mom_smoke_16wk = factor(case_when(mom_smoke_16wk=="1=Yes"~1,
                                           mom_smoke_16wk=="2=No"~0)),
         mom_smoke_22wk = factor(case_when(mom_smoke_22wk=="1=Yes"~1,
                                           mom_smoke_22wk=="2=No"~0)),
         mom_smoke_32wk = factor(case_when(mom_smoke_32wk=="1=Yes"~1,
                                           mom_smoke_32wk=="2=No"~0)),
         mom_smoke_pp1= factor(case_when(mom_smoke_pp1=="1=Yes"~1,
                                         mom_smoke_pp1=="2=No"~0)),
         mom_smoke_pp2= factor(case_when(mom_smoke_pp2=="1=Yes"~1,
                                         mom_smoke_pp2=="2=No"~0)),
         mom_smoke_pp12wk =factor(case_when(mom_smoke_pp12wk=="1=Yes"~1,
                                            mom_smoke_pp12wk=="2=No"~0)),
         mom_smoke_pp6mo = factor(case_when(mom_smoke_pp6mo=="1=Yes"~1,
                                            mom_smoke_pp6mo=="2=No"~0)))

# fill 0 to intensity of substance use if they stated never
new_df <- new_df %>%
  mutate(num_cigs_30=case_when(cig_ever=="0"~0,
                               cig_ever=="1"~num_cigs_30),
         num_e_cigs_30=case_when(e_cig_ever=="0"~0,
                                 e_cig_ever=="1"~num_e_cigs_30),
         num_mj_30=case_when(mj_ever=="0"~0,
                             mj_ever=="1"~num_mj_30),
         num_alc_30=case_when(alc_ever=="0"~0,
                              alc_ever=="1"~num_alc_30))
### replace 0 by NA for two swans (problem for previous pre-processing)
# parent raw data
parent_df <- read.csv("Project 1 Pre-Processing/K01BB.csv") %>%
  filter(redcap_event_name == "parent_baseline_arm_2") %>%
  select(c(parent_id, page:chart23)) 
# get wrong IDs
wrong_IDs <- parent_df$parent_id[which(
  rowSums(is.na(parent_df %>%
                  select(swan_p_on_c_timestamp:swan_p_on_c_complete)))==18)]
# replace the 0s by NAs
new_df$swan_hyperactive[new_df$parent_id %in% wrong_IDs] <- NA
new_df$swan_inattentive[new_df$parent_id %in% wrong_IDs] <- NA

###### outliers
# 44989 cigarettes per day is an outlier
# new_df$mom_numcig[26]

##### Updated: Summary for Demographic data
new_df$tage <- as.numeric(new_df$tage)
demo.data <- new_df %>%
  mutate(
    prace =  case_when(
      pethnic == 1 ~ "Hispanic/Latino",
      paian == 1 ~ "American Indian/Alaska Native",
      pasian == 1 ~ "Asian",
      pnhpi == 1 ~ "Native Hawaiian/Pacific Islander",
      pblack  == 1 ~ "Black/African American",
      pwhite == 1 ~ "White",
      prace_other == 1 ~ "Other",
      .default = NA
    ),
    trace = case_when(
      tethnic == 1 ~ "Hispanic/Latino",
      taian == 1 ~ "American Indian/Alaska Native",
      tasian == 1 ~ "Asian",
      tnhpi == 1 ~ "Native Hawaiian/Pacific Islander",
      tblack  == 1 ~ "Black/African American",
      twhite == 1 ~ "White",
      trace_other == 1 ~ "Other",
      .default = NA     
    )
  ) %>%
  select(parent_id, page, psex, plang, prace, employ, pedu, income, tage, tsex, language, trace)

# parent
demo.parent <- demo.data %>% 
  dplyr::select(parent_id, page, psex, plang, prace) %>% 
  rename_with(~ sub("^p", "", .x), starts_with("p")) %>%
  mutate(participant = "Parent")
colnames(demo.parent)[c(1,4)] <- c("parent_id","language")


# child
demo.child <- demo.data %>% 
  select(parent_id, tage, tsex, language, trace) %>% 
  rename_with(~ sub("^t", "", .x), starts_with("t")) %>% 
  mutate(participant = "Child")

# Binding the two data frames
tbl1 <- tbl_summary(bind_rows(demo.parent, demo.child)[,-1],
            by = "participant",
            type = all_continuous() ~ "continuous2",
            statistic = list(
              all_continuous() ~ c(
                "{mean} ({sd}); {median} ({min}, {max})"
              ),
              all_categorical() ~ "{n} / {N} ({p}%)"),
            digits = all_continuous() ~ 2,
            missing_text = "(Missing)"
) %>%
  bold_labels() 
# saveRDS(tbl1,"tbl1.RDS")

###### Missing Data
# calculate the number and percentage of missing in each column (not displayed in report)
num_na <- sapply(new_df,function(x) sum(is.na(x)))
pct_na <- round(num_na/dim(new_df)[1]*100,2)
missing_stat <- data.frame(num=num_na,pct=pct_na)
# 5 variables with greatest percentage of missing (not displayed in report)
kable(missing_stat[order(missing_stat[,2],decreasing=T)[1:5],],
      caption = "Five variables with greatest percentage of missing",booktabs=T,
      col.names = c("Number of Missing","Percentage of Missing (%)"),align = "c") %>%
  kable_styling(full_width=T,latex_options = c('HOLD_position'))

### missing pattern of demographic characteristics (not displayed in report)
md.pattern(new_df %>% select(tage:trace_other))
md.pattern(new_df %>% select(page:income))

### missing pattern of SDP and ETS
# md.pattern(new_df %>% select(mom_smoke_16wk:mom_smoke_pp6mo),rotate.names = T)
gg_miss_upset(new_df %>% select(mom_smoke_16wk:mom_smoke_pp6mo),
              nsets = n_var_miss(new_df %>% select(mom_smoke_16wk:mom_smoke_pp6mo)))

# missing for self-reported smoke exposure
gg_miss_upset(new_df %>% select(smoke_exposure_6mo:smoke_exposure_5yr),
              nsets = n_var_miss(new_df %>% select(smoke_exposure_6mo:smoke_exposure_5yr)))
# missing for cotimean level
gg_miss_upset(new_df %>% select(cotimean_34wk,cotimean_pp6mo,cotimean_pp6mo_baby),
              nsets = n_var_miss(new_df %>%
                                   select(cotimean_34wk,cotimean_pp6mo,
                                          cotimean_pp6mo_baby)))
# missing for self regulation
gg_miss_upset(new_df %>% select(erq_cog,erq_exp,erq_cog_a,erq_exp_a),
              nsets = n_var_miss(new_df %>% select(erq_cog,erq_exp,erq_cog_a,erq_exp_a)))

# missing for externalizing
gg_miss_upset(new_df %>% 
                select(bpm_att,bpm_ext,
                       bpm_att_p,bpm_ext_p,
                       bpm_att_a,bpm_ext_a,
                       swan_hyperactive,swan_inattentive),
              nsets = n_var_miss(new_df %>% 
                                   select(bpm_att,bpm_ext,
                                          bpm_att_p,bpm_ext_p,
                                          bpm_att_a,bpm_ext_a,
                                          swan_hyperactive,swan_inattentive)))

# missing for substance use
gg_miss_upset(new_df %>% 
                select(cig_ever:num_alc_30, nidaalc:mom_numcig),
              nsets = n_var_miss(new_df %>% 
                                   select(cig_ever:num_alc_30, nidaalc:mom_numcig)))

# overall missingness (not displayed in report)
md.pattern(new_df)
gg_miss_var(new_df)

###### Univariate Analysis
# descriptive statistics for demographic variables
kableone(CreateTableOne(data=new_df,vars=names(new_df)[c(2:14,52:61)])) %>%
  kable_styling(full_width=T, font_size=7,latex_options = c('HOLD_position'))

# descriptive statistics for variables related to SDP/ETS
kableone(CreateTableOne(data=new_df,vars=names(new_df)[c(22:31,37:42)])) %>%
  kable_styling(full_width=T, font_size=7,latex_options = c('HOLD_position'))

# Descriptive statistics for variables related to 
# self-regulation, externalizing and substance use
kableone(CreateTableOne(data=new_df,vars=names(new_df)[c(16:21,32:36,43:51,62:74)])) %>%
  kable_styling(full_width=T, font_size=9,latex_options = c('HOLD_position'))

# summary of statistics for self-regulation
des1 <- describe(cbind(new_df$erq_cog,new_df$erq_exp,new_df$erq_cog_a,new_df$erq_exp_a))
des1$skew <- des1$ kurtosis <- 
  des1$trimmed <- des1$mad <- des1$se <- NULL
des1<-round(des1,2)
des1$vars <- c("erq_cog","erq_exp","erq_cog_a","erq_exp_a")

# Descriptive Plot for self-regulation variables (not displayed in report)
df_plot1 <- data.frame(variable=c(rep("erq_cog",49),rep("erq_exp",49),
                                  rep("erq_cog_a",49),rep("erq_exp_a",49)),
                       values=c(new_df$erq_cog,new_df$erq_exp,new_df$erq_cog_a,new_df$erq_exp_a))
ggplot(df_plot1, aes(x = variable, y = values, fill = variable)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # boxplot without showing the outliers
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) +  # add jitter
  labs(title = "Descriptive Plot for self-regulation variables", x = "Variable", y = "Value") +
  theme_minimal()

# summary of statistics for externalizing
des2 <- describe(cbind(new_df$bpm_att,new_df$bpm_ext,
                       new_df$bpm_att_a,new_df$bpm_ext_a,
                       new_df$bpm_att_p,new_df$bpm_ext_p,
                       new_df$swan_hyperactive,new_df$swan_inattentive))
des2$skew <- des2$ kurtosis <- 
  des2$trimmed <- des2$mad <- des2$se <- NULL
des2<-round(des2,2)
des2$vars <- c("bpm_att","bpm_ext","bpm_att_a","bpm_ext_a",
               "bpm_att_p","bpm_ext_p","swan_hyperactive","swan_inattentive")

# Descriptive Plot for externalizing (not displayed in report)
df_plot2 <- data.frame(variable=c(rep("bpm_att",49),rep("bpm_ext",49),
                                  rep("bpm_att_a",49),rep("bpm_ext_a",49),
                                  rep("bpm_att_p",49),rep("bpm_ext_p",49),
                                  rep("swan_hyperactive",49),rep("swan_inattentive",49)),
                       values=c(new_df$bpm_att,new_df$bpm_ext,
                                new_df$bpm_att_a,new_df$bpm_ext_a,
                                new_df$bpm_att_p,new_df$bpm_ext_p,
                                new_df$swan_hyperactive,new_df$swan_inattentive))
ggplot(df_plot2, aes(x = variable, y = values, fill = variable)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # boxplot without showing the outliers
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) +  # add jitter
  labs(title = "Descriptive Plot for externalizing variables", x = "Variable", y = "Value") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

###### Prenatal and Postnatal Smoke Exposure Intensity Calculation
# create variable for SDP and its intensity
new_df$SDP <- factor(ifelse(new_df$mom_smoke_16wk==1 | 
                              new_df$mom_smoke_22wk==1 | 
                              new_df$mom_smoke_32wk==1,1,0))
new_df <- new_df %>%
  mutate(SDP_intensity=as.numeric(as.character(mom_smoke_16wk))+
           as.numeric(as.character(mom_smoke_22wk))+
           as.numeric(as.character(mom_smoke_32wk)))
# cor(new_df$SDP_intensity,new_df$cotimean_34wk,use = "pairwise.complete.obs")

# revise the smoke exposure variable by leverage the other variable's information
new_df$smoke_exposure_6mo_new <- factor(ifelse(new_df$smoke_exposure_6mo==1|
                                                 new_df$mom_smoke_pp12wk==1|new_df$mom_smoke_pp6mo==1|
                                                 new_df$cotimean_pp6mo >10 | new_df$cotimean_pp6mo_baby>10,1,0))
# smoke exposure intensity for ETS
new_df <- new_df %>%
  mutate(smoke_exposure_intensity=as.numeric(as.character(new_df$smoke_exposure_6mo_new))+
           as.numeric(as.character(new_df$smoke_exposure_12mo))+
           as.numeric(as.character(new_df$smoke_exposure_2yr))+
           as.numeric(as.character(new_df$smoke_exposure_3yr))+
           as.numeric(as.character(new_df$smoke_exposure_4yr))+
           as.numeric(as.character(new_df$smoke_exposure_5yr)))

###### Intercorrelations
# Interrelationship between Prenatal and Postnatal Smoke Exposure
# pairs.panels(new_df %>% select(mom_smoke_16wk:mom_smoke_32wk,smoke_exposure_6mo_new,
# smoke_exposure_12mo:smoke_exposure_5yr),stars = T)
# cor(new_df$SDP_intensity,new_df$smoke_exposure_intensity,use = "pairwise.complete.obs")
# pairs.panels(new_df %>% select(SDP_intensity,smoke_exposure_intensity))
ggplot(new_df,aes(x=SDP_intensity,y=smoke_exposure_intensity))+
  geom_point()+
  geom_smooth(method = "lm", se = T) +
  theme_minimal()+
  labs(x="Prenatal Exposure Intensity",y="Postnatal Exposure Intensity",
       title = "Interrelationship between Prenatal and Postnatal Smoke Exposure")

# interrelationship in self-regulation variables (not displayed in report)
pairs.panels(new_df %>% select(erq_cog,erq_exp,erq_cog_a,erq_exp_a),stars = T)

# interrelationship in externalizing variables (not displayed in report)
pairs.panels(new_df %>% select(bpm_att,
                               bpm_ext,
                               bpm_att_p,
                               bpm_ext_p,
                               swan_hyperactive,swan_inattentive),stars = T)

# interrelationship in substance use variables (not displayed in report)
pairs.panels(new_df %>% select(cig_ever,e_cig_ever,mj_ever,alc_ever,num_cigs_30,
                               num_e_cigs_30,
                               num_mj_30,
                               num_alc_30)%>%
               na.omit(),stars = T)

###### bivariate Analysis
# SDP vs self-regulation (not displayed in report)
pairs.panels(new_df %>% select(mom_smoke_16wk:mom_smoke_32wk,
                               erq_cog,erq_exp)%>%
               na.omit(),stars=TRUE)

# postnatal exposure intensity vs self-regulation (not displayed in report)
ggpairs(new_df %>% select(SDP_intensity,smoke_exposure_intensity,
                          erq_cog,erq_exp)%>%na.omit(),
        lower = list(continuous = wrap("smooth", alpha = 0.7, size=0.5)))+
  theme_bw()
# postnatal exposure vs self-regulation (not displayed in report)
pairs.panels(new_df %>% select(smoke_exposure_6mo_new,
                               smoke_exposure_12mo:smoke_exposure_5yr,
                               erq_cog,erq_exp)%>%
               na.omit(),stars=TRUE)
# significant parts: 2yr-5yr exposure vs self-regulation (not displayed in report)
pairs.panels(new_df %>% select(smoke_exposure_2yr:smoke_exposure_5yr,
                               erq_cog,erq_exp)%>%
               na.omit(),stars=TRUE)
# smoke exposure in 2 - 5 year vs Expressive Suppression
gg1 <- ggplot(new_df %>% rename(smoke_exposure=smoke_exposure_2yr),
              aes(x=smoke_exposure,y=erq_exp,color=smoke_exposure))+
  geom_point()+
  labs(x="Smoke Exposure in 2nd Year (1=Yes, 0=No)",
       y="Expressive Suppression")+
  theme_minimal()+
  theme(legend.position = "bottom")
gg2 <- ggplot(new_df,aes(x=smoke_exposure_3yr,y=erq_exp,color=smoke_exposure_3yr))+
  geom_point()+
  labs(x="Smoke Exposure in 3rd Year (1=Yes, 0=No)",
       y="Expressive Suppression")+
  theme_minimal()+
  theme(legend.position = "bottom")
gg3 <- ggplot(new_df,aes(x=smoke_exposure_4yr,y=erq_exp,color=smoke_exposure_4yr))+
  geom_point()+
  labs(x="Smoke Exposure in 4th Year (1=Yes, 0=No)",
       y="Expressive Suppression")+
  theme_minimal()+
  theme(legend.position = "bottom")
gg4 <- ggplot(new_df,aes(x=smoke_exposure_5yr,y=erq_exp,color=smoke_exposure_5yr))+
  geom_point()+
  labs(x="Smoke Exposure in 5th Year (1=Yes, 0=No)",
       y="Expressive Suppression")+
  theme_minimal()+
  theme(legend.position = "bottom")
ggarrange(gg1,gg2,gg3,gg4,nrow=2,ncol = 2,
          common.legend = T,
          legend = "bottom")

# association between prenatal smoke exposure and externalizing problems on adolescent
#  pairs.panels is not displayed in report
pairs.panels(new_df %>% select(SDP, smoke_exposure_intensity,
                               bpm_att,
                               bpm_ext,
                               bpm_att_p,
                               bpm_ext_p,
                               swan_hyperactive,swan_inattentive)%>%
               na.omit(),star=T)
#  pairs.panels is not displayed in report
pairs.panels(new_df %>% select(mom_smoke_16wk:mom_smoke_32wk,
                               bpm_att,
                               bpm_ext,
                               bpm_att_p,
                               bpm_ext_p,
                               swan_hyperactive,swan_inattentive))
gg5 <- ggplot(new_df,aes(x=SDP,y=bpm_att_p,color=SDP))+
  geom_point()+
  labs(x="Prenatal Smoke Exposure",
       y="Subscale for attention problems from parent on child")+
  theme_minimal()+
  theme(legend.position = "bottom",axis.title.y = element_text(size = 10))
gg6 <- ggplot(new_df,aes(x=SDP,y=swan_hyperactive,color=SDP))+
  geom_point()+
  labs(x="Prenatal Smoke Exposure",
       y="SWAN rating for ADHD- Hyperactive/Impulsive type")+
  theme_minimal()+
  theme(legend.position = "bottom",axis.title.y = element_text(size = 10))
ggarrange(gg5,gg6,ncol = 2,
          common.legend = T,
          legend = "bottom")

# smoke exposure versus externalizing problems
#  pairs.panels is not displayed in report
pairs.panels(new_df %>% select(smoke_exposure_6mo_new,
                               smoke_exposure_12mo:smoke_exposure_5yr,
                               bpm_att,
                               bpm_ext,
                               bpm_att_p,
                               bpm_ext_p,
                               swan_hyperactive,swan_inattentive)%>%
               na.omit(),star=T)

# smoke exposure from 0 to 6 months versus externalizing problems
gg7 <- ggplot(new_df,aes(x=smoke_exposure_6mo_new,y=bpm_att,
                         color=smoke_exposure_6mo_new))+
  geom_point()+
  labs(x="Smoke Exposure from 0 to 6 months",
       y="Attention (child)")+
  theme_minimal()+
  theme(legend.position = "bottom",axis.title.y = element_text(size = 7))
gg8 <- ggplot(new_df,aes(x=smoke_exposure_6mo_new,y=bpm_ext,
                         color=smoke_exposure_6mo_new))+
  geom_point()+
  labs(x="Smoke Exposure from 0 to 6 months",
       y="Externalizing (child)")+
  theme_minimal()+
  theme(legend.position = "bottom",axis.title.y = element_text(size = 7))
gg9 <- ggplot(new_df,aes(x=smoke_exposure_6mo_new,y=bpm_att_p,
                         color=smoke_exposure_6mo_new))+
  geom_point()+
  labs(x="Smoke Exposure from 0 to 6 months",
       y="Attention (from parent)")+
  theme_minimal()+
  theme(legend.position = "bottom",axis.title.y = element_text(size = 7))
gg10 <- ggplot(new_df,aes(x=smoke_exposure_6mo_new,y=bpm_ext_p,
                          color=smoke_exposure_6mo_new))+
  geom_point()+
  labs(x="Smoke Exposure from 0 to 6 months",
       y="Externalizing (from parent)")+
  theme_minimal()+
  theme(legend.position = "bottom",axis.title.y = element_text(size = 7))
gg11 <- ggplot(new_df,aes(x=smoke_exposure_6mo_new,y=swan_hyperactive,
                          color=smoke_exposure_6mo_new))+
  geom_point()+
  labs(x="Smoke Exposure from 0 to 6 months",
       y="ADHD- Hyperactive/Impulsive type")+
  theme_minimal()+
  theme(legend.position = "bottom",axis.title.y = element_text(size = 7))
gg12 <- ggplot(new_df,aes(x=smoke_exposure_6mo_new,y=swan_inattentive,
                          color=smoke_exposure_6mo_new))+
  geom_point()+
  labs(x="Smoke Exposure from 0 to 6 months",
       y="ADHD- Inattentive type")+
  theme_minimal()+
  theme(legend.position = "bottom",axis.title.y = element_text(size = 7))
ggarrange(gg7,gg8,gg9,gg10,gg11,gg12,nrow=3,ncol=2,common.legend = T,
          legend = "bottom")

# Association between SDP/ETS and Substance Use (not displayed in report)
pairs.panels(new_df %>% select(SDP_intensity,smoke_exposure_intensity,
                               cig_ever,e_cig_ever,mj_ever,alc_ever)%>%
               na.omit())
pairs.panels(new_df %>% select(SDP_intensity,smoke_exposure_intensity,
                               num_cigs_30,
                               num_e_cigs_30,
                               num_mj_30,
                               num_alc_30)%>%
               na.omit())
pairs.panels(new_df %>% select(mom_smoke_16wk:mom_smoke_32wk,
                               cig_ever,e_cig_ever,mj_ever,alc_ever,
                               num_cigs_30,
                               num_e_cigs_30,
                               num_mj_30,
                               num_alc_30)%>%
               na.omit())
pairs.panels(new_df %>% select(smoke_exposure_6mo_new,
                               smoke_exposure_12mo:smoke_exposure_5yr,
                               cig_ever,e_cig_ever,mj_ever,alc_ever,
                               num_cigs_30,
                               num_e_cigs_30,
                               num_mj_30,
                               num_alc_30)%>%
               na.omit())

