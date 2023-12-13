# load the library
library(tidyverse) # version 1.3.2
library(ggpubr) # version 0.4.0
library(MASS) # version 7.3-58.2
library(fitdistrplus) # version1.1-8

# read the source population data
framingham_df <- readRDS("source.RDS")
summary(framingham_df)

### when individual data is not available from target population, but only the summary data
### simulation based on summary data
### note the correlation between variables

###################################################### 
#### EDA for simulation #### 
###################################################### 
#### prepare the data set for EDA ####
# log transformation
framingham_eda <- framingham_df %>%
  mutate(log.TOTCHOL=log(TOTCHOL),
         log.SYSBP=log(SYSBP),
         log.HDLC=log(HDLC),
         log.AGE=log(AGE)) %>%
  dplyr::select(-BMI,-SYSBP_UT,-TOTCHOL,-SYSBP_T,-SYSBP,-HDLC,-AGE)
framingham_eda$SEX <- factor(framingham_eda$SEX,labels=c("Male","Female"))
summary(framingham_eda)

# separate men and women
framingham_men <- framingham_eda %>%
  filter(SEX=="Male") %>%
  dplyr::select(-SEX)
framingham_women <- framingham_eda %>%
  filter(SEX=="Female") %>%
  dplyr::select(-SEX) 

# drop D
framingham_eda <- framingham_eda %>% dplyr::select(-D,-CVD)

###################################################### 
#### Univariate EDA#### 
###################################################### 
#### marginal distribution compared with normal distribution #### 
### Total-C
# histogram with density 
log.TOTCHOL.p<- ggplot(framingham_eda, aes(x = log.TOTCHOL)) +
  geom_histogram(aes(y = ..density..), fill = 'white', colour = 'black') +
  geom_density(colour = "red", size = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(framingham_eda$log.TOTCHOL, na.rm = TRUE), 
                            sd = sd(framingham_eda$log.TOTCHOL, na.rm = TRUE)), 
                colour = "blue", size = 1, linetype = "dashed") +
  facet_wrap(~SEX) +
  theme_minimal()+
  labs(title = "Histogram with Density Plot for Log-transformed Total-C",
       x="Log-transformed Total-C")

# For the Q-Q plot
ggplot(framingham_df, aes(sample = log(TOTCHOL))) +
  stat_qq() +
  stat_qq_line(colour = "red", size = 1) +
  labs(title = "Q-Q Plot for Log-transformed Total-C")

# shapiro test
shapiro.test(log(framingham_df$TOTCHOL))

### Age
# histogram with density 
log.AGE.p<- ggplot(framingham_eda, aes(x = log.AGE)) +
  geom_histogram(aes(y = ..density..), fill = 'white', colour = 'black') +
  geom_density(colour = "red", size = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(framingham_eda$log.AGE, na.rm = TRUE), 
                            sd = sd(framingham_eda$log.AGE, na.rm = TRUE)), 
                colour = "blue", size = 1, linetype = "dashed") +
  facet_wrap(~SEX) +
  theme_minimal()+
  labs(title = "Histogram with Density Plot for Log-transformed Age",
       x="Log-transformed Age")

# qqplot
qqnorm(log(framingham_df$AGE))
qqline(log(framingham_df$AGE), col = "red")

### HDLC
# histogram with density 
log.HDLC.p<- ggplot(framingham_eda, aes(x = log.HDLC)) +
  geom_histogram(aes(y = ..density..), fill = 'white', colour = 'black') +
  geom_density(colour = "red", size = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(framingham_eda$log.HDLC, na.rm = TRUE), 
                            sd = sd(framingham_eda$log.HDLC, na.rm = TRUE)), 
                colour = "blue", size = 1, linetype = "dashed") +
  facet_wrap(~SEX) +
  theme_minimal()+
  labs(title = "Histogram with Density Plot for Log-transformed HDL-C",
       x="Log-transformed HDL-C")
# qqplot
qqnorm(log(framingham_df$HDLC))
qqline(log(framingham_df$HDLC), col = "red")
shapiro.test(log(framingham_df$HDLC))

### SYSBP
# histogram with density 
log.SYSBP.p<- ggplot(framingham_eda, aes(x = log.SYSBP)) +
  geom_histogram(aes(y = ..density..), fill = 'white', colour = 'black') +
  geom_density(colour = "red", size = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(framingham_eda$log.SYSBP, na.rm = TRUE), 
                            sd = sd(framingham_eda$log.SYSBP, na.rm = TRUE)), 
                colour = "blue", size = 1, linetype = "dashed") +
  facet_wrap(~SEX) +
  theme_minimal()+
  labs(title = "Histogram with Density Plot for Log-transformed Systolic BP",
       x="Log-transformed Systolic BP")
# qqplot
qqnorm(log(framingham_df$SYSBP))
qqline(log(framingham_df$SYSBP), col = "red")
shapiro.test(log(framingham_df$SYSBP))

# save the plots for final report
# saveRDS(list(log.AGE.p,log.HDLC.p,log.TOTCHOL.p,log.SYSBP.p),"plot1.RDS")

###### Discrete Variables
# tables can also be obtained in summary tables
table(framingham_df$CURSMOKE)
table(framingham_df$DIABETES)
table(framingham_df$BPMEDS)
# three_way <- xtabs(~ CURSMOKE + DIABETES + BPMEDS, data=framingham_df) 
# three_way_ftable <- ftable(three_way)

###################################################### 
#### Multivariate EDA#### 
###################################################### 
#### correlation #### 
cor.women <- framingham_eda %>%
  filter(SEX=="Female") %>%
  dplyr::select(-SEX) %>%
  cor()
cor.men <- framingham_eda %>%
  filter(SEX=="Male") %>%
  dplyr::select(-SEX) %>%
  cor()


###################################################### 
#### EDA for DGM3 #### 
###################################################### 
# check the raw data's distribution
descdist(framingham_df$TOTCHOL, discrete = FALSE) # lognormal
descdist(framingham_df$AGE, discrete = FALSE) # unif
descdist(framingham_df$SYSBP, discrete = FALSE) # gamma
descdist(framingham_df$HDLC, discrete = FALSE) # lognormal

descdist(framingham_df$BPMEDS, discrete = TRUE)

fit<- fitdist(framingham_df$SYSBP, "gamma")
plot(fit)


###################################################### 
#### DGM1 #### 
######################################################
df_2017 <- readRDS("NHANES.RDS")
df_2017 <- df_2017 %>%
  mutate(log.TOTCHOL=log(TOTCHOL),
         log.SYSBP=log(SYSBP),
         log.HDLC=log(HDLC),
         log.AGE=log(AGE)) %>%
  dplyr::select(-SEQN,-BMI,-TOTCHOL,-SYSBP,-HDLC,-AGE) %>%
  dplyr::select(SEX,CURSMOKE,DIABETES,BPMEDS,log.TOTCHOL,log.SYSBP,log.HDLC,log.AGE)
#### mean estimation ####
mean_men <- unlist(lapply(df_2017[df_2017$SEX=="Male",2:8],function(i) mean(i,na.rm=T)))
mean_women <- unlist(lapply(df_2017[df_2017$SEX=="Female",2:8],function(i) mean(i,na.rm=T)))
#### covariance matrix estimation #### 
cov_men <- df_2017[df_2017$SEX=="Male",2:8] %>%
  cov(use = "complete.obs")
# cov_men
cov_women <- df_2017[df_2017$SEX=="Female",2:8] %>%
  cov(use = "complete.obs")
# cov_women
n_sim <- 1000
bs.framingham.model.men.sim1.vec <- numeric(n_sim)
bs.framingham.model.women.sim1.vec <- numeric(n_sim)
bs.tailored.model.men.sim1.vec <- numeric(n_sim)
bs.tailored.model.women.sim1.vec <- numeric(n_sim)
framingham_men$S <- 1
framingham_women$S <- 1
set.seed(2550)
for(i in 1:n_sim){
  print(i)
  # simulated NHANES data for men
  data.men <- data.frame(mvrnorm(n = 2000, mean_men, Sigma=cov_men))
  data.men <- data.men %>%
    filter(log.AGE >= log(30), log.AGE <= log(74)) %>%
    mutate(CURSMOKE=ifelse(CURSMOKE>quantile(data.men$CURSMOKE,(1961-474)/1961),1,0),
           DIABETES=ifelse(DIABETES>quantile(data.men$DIABETES,(1961-358)/1961),1,0),
           BPMEDS=ifelse(BPMEDS>quantile(data.men$BPMEDS,(1829-604)/1829),1,0))
  data.men$D <- sample(c(0,1),nrow(data.men),prob=c(0.8,0.2),replace = TRUE)
  data.men$S <- 0
  # combine Framingham and NHANES as composite data
  combined_men <- bind_rows(data.men,framingham_men)
  combined_men$CURSMOKE <- factor(combined_men$CURSMOKE)
  combined_men$DIABETES <- factor(combined_men$DIABETES)
  combined_men$BPMEDS <- factor(combined_men$BPMEDS)
  combined_men$S <-factor(combined_men$S)
  combined_men$CVD <- factor(combined_men$CVD)
  combined_men$log.SYSBP_UT <-ifelse(combined_men$BPMEDS == 0, 
                                     combined_men$log.SYSBP, 0)
  combined_men$log.SYSBP_T <- ifelse(combined_men$BPMEDS == 1, 
                                     combined_men$log.SYSBP, 0)
  ###### Evaluation for Framingham Model ######
  if(i==1){
    # fit the Framingham model once only
    framingham.model.men <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                                data = combined_men %>% filter(D==FALSE,S==1),family = binomial())
  }
  men.test1 <- combined_men %>% filter((D==1 | S==0))
  ps.men.test1 <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES,  
                      data = men.test1,family = binomial())
  weights.men.test1 <- exp(predict(ps.men.test1))^(-1)
  bs.men1 <- sum((as.numeric(men.test1$S)-1) * weights.men.test1 * (predict(framingham.model.men,men.test1,type = "response")-ifelse(is.na(as.numeric(men.test1$CVD)), 0, as.numeric(men.test1$CVD))+1)^2)/sum(2-as.numeric(men.test1$S))
  bs.framingham.model.men.sim1.vec[i] <- bs.men1
  ###### Fit Tailored model ######
  # men
  ps.men.train <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                      data = combined_men %>% filter(D==0),family = binomial())
  weights.men.train <- exp(predict(ps.men.train,combined_men %>% filter(D==FALSE,S==1)))^(-1)
  mod.men <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                 data = combined_men %>% filter(D==0,S==1),family = binomial(),
                 weights = weights.men.train)
  # evaluate on test
  men.test <- combined_men %>% filter(D==1)
  ps.men.test <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                     data = men.test,family = binomial())
  weights.men.test <- exp(predict(ps.men.test))^(-1)
  bs.men <- sum((as.numeric(men.test$S)-1) * weights.men.test * (predict(mod.men,men.test,type = "response")-ifelse(is.na(as.numeric(men.test$CVD)), 0, as.numeric(men.test$CVD))+1)^2)/sum(2-as.numeric(men.test$S))
  bs.tailored.model.men.sim1.vec[i] <- bs.men
  

  # the same process for women
  data.women <- data.frame(mvrnorm(n = 2000, mean_women, Sigma=cov_women))
  data.women <- data.women %>%
    filter(log.AGE >= log(30), log.AGE <= log(74)) %>%
    mutate(CURSMOKE=ifelse(CURSMOKE>quantile(data.women$CURSMOKE,(2099-317)/2099),1,0),
           DIABETES=ifelse(DIABETES>quantile(data.women$DIABETES,(2098-317)/2098),1,0),
           BPMEDS=ifelse(BPMEDS>quantile(data.women$BPMEDS,(1983-629)/1983),1,0))
  data.women$D <- sample(c(0,1),nrow(data.women),prob=c(0.8,0.2),replace = TRUE)
  data.women$S <- 0
  combined_women <- bind_rows(data.women,framingham_women)
  combined_women$CURSMOKE <- factor(combined_women$CURSMOKE)
  combined_women$DIABETES <- factor(combined_women$DIABETES)
  combined_women$BPMEDS <- factor(combined_women$BPMEDS)
  combined_women$S <-factor(combined_women$S)
  combined_women$CVD <- factor(combined_women$CVD)
  combined_women$log.SYSBP_UT <-ifelse(combined_women$BPMEDS == 0, 
                                     combined_women$log.SYSBP, 0)
  combined_women$log.SYSBP_T <- ifelse(combined_women$BPMEDS == 1, 
                                     combined_women$log.SYSBP, 0)
  ###### Evaluation for Framingham Model ######
  if(i==1){
    # fit the Framingham model once only
    framingham.model.women <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                                data = combined_women %>% filter(D==FALSE,S==1),family = binomial())
  }
  women.test1 <- combined_women %>% filter((D==1 | S==0))
  ps.women.test1 <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                        data = women.test1,family = binomial())
  weights.women.test1 <- exp(predict(ps.women.test1))^(-1)
  bs.women1 <- sum((as.numeric(women.test1$S)-1) * weights.women.test1 *(predict(framingham.model.women,women.test1,type = "response")-ifelse(is.na(as.numeric(women.test1$CVD)), 0, as.numeric(women.test1$CVD))+1)^2)/sum(2-as.numeric(women.test1$S))
  bs.framingham.model.women.sim1.vec[i] <- bs.women1
  ###### Fit Tailored model ######
  # women
  ps.women.train <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                      data = combined_women %>% filter(D==0),family = binomial())
  weights.women.train <- exp(predict(ps.women.train,combined_women %>% filter(D==FALSE,S==1)))^(-1)
  mod.women <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                 data = combined_women %>% filter(D==0,S==1),family = binomial(),
                 weights = weights.women.train)
  # evaluate on test
  women.test <- combined_women %>% filter(D==1)
  ps.women.test <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                     data = women.test,family = binomial())
  weights.women.test <- exp(predict(ps.women.test))^(-1)
  bs.women <- sum((as.numeric(women.test$S)-1) * weights.women.test * (predict(mod.women,women.test,type = "response")-ifelse(is.na(as.numeric(women.test$CVD)), 0, as.numeric(women.test$CVD))+1)^2)/sum(2-as.numeric(women.test$S))
  bs.tailored.model.women.sim1.vec[i] <- bs.women
}

###################################################### 
#### DGM2 #### 
######################################################
df_2017_2 <- readRDS("NHANES.RDS")
df_2017_2 <- df_2017_2 %>%
  dplyr::select(SEX,CURSMOKE,DIABETES,BPMEDS,TOTCHOL,SYSBP,HDLC,AGE)
#### mean estimation ####
mean_men_raw <- unlist(lapply(df_2017_2[df_2017_2$SEX=="Male",2:8],function(i) mean(i,na.rm=T)))
mean_women_raw <- unlist(lapply(df_2017_2[df_2017_2$SEX=="Female",2:8],function(i) mean(i,na.rm=T)))
#### var estimation ####
var_men_raw <- unlist(lapply(df_2017_2[df_2017_2$SEX=="Male",2:8],function(i) var(i,na.rm=T)))
var_women_raw <- unlist(lapply(df_2017_2[df_2017_2$SEX=="Female",2:8],function(i) var(i,na.rm=T)))

#### mean estimation after log-transformation ####
mean_men_log <- mean_men_raw
mean_men_log[4:7] <- log(mean_men_raw[4:7]^2/sqrt(var_men_raw[4:7]+mean_men_raw[4:7]^2))
names(mean_men_log) <- c("CURSMOKE","DIABETES","BPMEDS","log.TOTCHOL","log.SYSBP","log.HDLC","log.AGE")
mean_women_log <- mean_women_raw
mean_women_log[4:7] <- log(mean_women_raw[4:7]^2/sqrt(var_women_raw[4:7]+mean_women_raw[4:7]^2))
names(mean_women_log) <- c("CURSMOKE","DIABETES","BPMEDS","log.TOTCHOL","log.SYSBP","log.HDLC","log.AGE")
#### sd estimation after log-transformation ####
sd_men <- sqrt(var_men_raw)
sd_men[4:7] <- sqrt(log(var_men_raw[4:7]/mean_men_raw[4:7]^2 + 1))
names(sd_men) <- c("CURSMOKE","DIABETES","BPMEDS","log.TOTCHOL","log.SYSBP","log.HDLC","log.AGE")
sd_women <- sqrt(var_women_raw)
sd_women[4:7] <- sqrt(log(var_women_raw[4:7]/mean_women_raw[4:7]^2 + 1))
names(sd_women) <-c("CURSMOKE","DIABETES","BPMEDS","log.TOTCHOL","log.SYSBP","log.HDLC","log.AGE")

#### covariance matrix estimation after log-transformation #### 
cov_men_log <- cor.men
cov_women_log <- cor.women
for (i in 1:nrow(cor.men)) {
  for (j in 1:ncol(cor.men)) {
    cov_men_log[i, j] <- cor.men[i, j] * sd_men[i] * sd_men[j]
    cov_women_log[i, j] <- cor.women[i, j] * sd_women[i] * sd_women[j]
  }
}
# cov_men_log
# cov_women_log
bs.framingham.model.men.sim2.vec <- numeric(n_sim)
bs.framingham.model.women.sim2.vec <- numeric(n_sim)
bs.tailored.model.men.sim2.vec <- numeric(n_sim)
bs.tailored.model.women.sim2.vec <- numeric(n_sim)
set.seed(2550)
for(i in 1:n_sim){
  print(i)
  # simulated NHANES data for men
  data.men <- data.frame(mvrnorm(n = 2000, mean_men_log, Sigma=cov_men_log))
  data.men <- data.men %>%
    filter(log.AGE >= log(30), log.AGE <= log(74)) %>%
    mutate(CURSMOKE=ifelse(CURSMOKE>quantile(data.men$CURSMOKE,(1961-474)/1961),1,0),
           DIABETES=ifelse(DIABETES>quantile(data.men$DIABETES,(1961-358)/1961),1,0),
           BPMEDS=ifelse(BPMEDS>quantile(data.men$BPMEDS,(1829-604)/1829),1,0))
  data.men$D <- sample(c(0,1),nrow(data.men),prob=c(0.8,0.2),replace = TRUE)
  data.men$S <- 0
  # combine Framingham and NHANES as composite data
  combined_men <- bind_rows(data.men,framingham_men)
  combined_men$CURSMOKE <- factor(combined_men$CURSMOKE)
  combined_men$DIABETES <- factor(combined_men$DIABETES)
  combined_men$BPMEDS <- factor(combined_men$BPMEDS)
  combined_men$S <-factor(combined_men$S)
  combined_men$CVD <- factor(combined_men$CVD)
  combined_men$log.SYSBP_UT <-ifelse(combined_men$BPMEDS == 0, 
                                     combined_men$log.SYSBP, 0)
  combined_men$log.SYSBP_T <- ifelse(combined_men$BPMEDS == 1, 
                                     combined_men$log.SYSBP, 0)
  ###### Evaluation for Framingham Model ######
  if(i==1){
    # fit the Framingham model once only
    framingham.model.men <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                                data = combined_men %>% filter(D==FALSE,S==1),family = binomial())
  }
  men.test1 <- combined_men %>% filter((D==1 | S==0))
  ps.men.test1 <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES,  
                      data = men.test1,family = binomial())
  weights.men.test1 <- exp(predict(ps.men.test1))^(-1)
  bs.men1 <- sum((as.numeric(men.test1$S)-1) * weights.men.test1 * (predict(framingham.model.men,men.test1,type = "response")-ifelse(is.na(as.numeric(men.test1$CVD)), 0, as.numeric(men.test1$CVD))+1)^2)/sum(2-as.numeric(men.test1$S))
  bs.framingham.model.men.sim2.vec[i] <- bs.men1
  ###### Fit Tailored model ######
  # men
  ps.men.train <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                      data = combined_men %>% filter(D==0),family = binomial())
  weights.men.train <- exp(predict(ps.men.train,combined_men %>% filter(D==FALSE,S==1)))^(-1)
  mod.men <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                 data = combined_men %>% filter(D==0,S==1),family = binomial(),
                 weights = weights.men.train)
  # evaluate on test
  men.test <- combined_men %>% filter(D==1)
  ps.men.test <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                     data = men.test,family = binomial())
  weights.men.test <- exp(predict(ps.men.test))^(-1)
  bs.men <- sum((as.numeric(men.test$S)-1) * weights.men.test * (predict(mod.men,men.test,type = "response")-ifelse(is.na(as.numeric(men.test$CVD)), 0, as.numeric(men.test$CVD))+1)^2)/sum(2-as.numeric(men.test$S))
  bs.tailored.model.men.sim2.vec[i] <- bs.men
  
  
  # the same process for women
  data.women <- data.frame(mvrnorm(n = 2000, mean_women_log, Sigma=cov_women_log))
  data.women <- data.women %>%
    filter(log.AGE >= log(30), log.AGE <= log(74)) %>%
    mutate(CURSMOKE=ifelse(CURSMOKE>quantile(data.women$CURSMOKE,(2099-317)/2099),1,0),
           DIABETES=ifelse(DIABETES>quantile(data.women$DIABETES,(2098-317)/2098),1,0),
           BPMEDS=ifelse(BPMEDS>quantile(data.women$BPMEDS,(1983-629)/1983),1,0))
  data.women$D <- sample(c(0,1),nrow(data.women),prob=c(0.8,0.2),replace = TRUE)
  data.women$S <- 0
  combined_women <- bind_rows(data.women,framingham_women)
  combined_women$CURSMOKE <- factor(combined_women$CURSMOKE)
  combined_women$DIABETES <- factor(combined_women$DIABETES)
  combined_women$BPMEDS <- factor(combined_women$BPMEDS)
  combined_women$S <-factor(combined_women$S)
  combined_women$CVD <- factor(combined_women$CVD)
  combined_women$log.SYSBP_UT <-ifelse(combined_women$BPMEDS == 0, 
                                       combined_women$log.SYSBP, 0)
  combined_women$log.SYSBP_T <- ifelse(combined_women$BPMEDS == 1, 
                                       combined_women$log.SYSBP, 0)
  ###### Evaluation for Framingham Model ######
  if(i==1){
    # fit the Framingham model once only
    framingham.model.women <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                                  data = combined_women %>% filter(D==FALSE,S==1),family = binomial())
  }
  women.test1 <- combined_women %>% filter((D==1 | S==0))
  ps.women.test1 <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                        data = women.test1,family = binomial())
  weights.women.test1 <- exp(predict(ps.women.test1))^(-1)
  bs.women1 <- sum((as.numeric(women.test1$S)-1) * weights.women.test1 *(predict(framingham.model.women,women.test1,type = "response")-ifelse(is.na(as.numeric(women.test1$CVD)), 0, as.numeric(women.test1$CVD))+1)^2)/sum(2-as.numeric(women.test1$S))
  bs.framingham.model.women.sim2.vec[i] <- bs.women1
  ###### Fit Tailored model ######
  # women
  ps.women.train <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                        data = combined_women %>% filter(D==0),family = binomial())
  weights.women.train <- exp(predict(ps.women.train,combined_women %>% filter(D==FALSE,S==1)))^(-1)
  mod.women <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                   data = combined_women %>% filter(D==0,S==1),family = binomial(),
                   weights = weights.women.train)
  # evaluate on test
  women.test <- combined_women %>% filter(D==1)
  ps.women.test <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                       data = women.test,family = binomial())
  weights.women.test <- exp(predict(ps.women.test))^(-1)
  bs.women <- sum((as.numeric(women.test$S)-1) * weights.women.test * (predict(mod.women,women.test,type = "response")-ifelse(is.na(as.numeric(women.test$CVD)), 0, as.numeric(women.test$CVD))+1)^2)/sum(2-as.numeric(women.test$S))
  bs.tailored.model.women.sim2.vec[i] <- bs.women
}

###################################################### 
#### DGM3 #### 
######################################################
#### estimate covariance matrix
#### sd estimation ####
sd_men_3 <- sqrt(var_men_raw)
sd_men_3[4] <- sqrt(log(var_men_raw[4]/mean_men_raw[4]^2 + 1))
sd_men_3[6] <- sqrt(log(var_men_raw[6]/mean_men_raw[6]^2 + 1))
names(sd_men_3) <- c("CURSMOKE","DIABETES","BPMEDS","log.TOTCHOL","SYSBP","log.HDLC","AGE")
sd_women_3 <- sqrt(var_women_raw)
sd_women_3[4] <- sqrt(log(var_women_raw[4]/mean_women_raw[4]^2 + 1))
sd_women_3[6] <- sqrt(log(var_women_raw[6]/mean_women_raw[6]^2 + 1))
names(sd_women_3) <-c("CURSMOKE","DIABETES","BPMEDS","log.TOTCHOL","SYSBP","log.HDLC","AGE")
#### covariance matrix estimation #### 
cov_men_3 <- matrix(nrow=7,ncol=7)
cov_women_3 <- matrix(nrow=7,ncol=7)
for (i in 1:nrow(cor.men)) {
  for (j in 1:ncol(cor.men)) {
    cov_men_3[i, j] <- cor.men[i, j] * sd_men_3[i] * sd_men_3[j]
    cov_women_3[i, j] <- cor.women[i, j] * sd_women_3[i] * sd_women_3[j]
  }
}
# square root of covariance matrix
L.men <- t(chol(cov_men_3))
L.women <- t(chol(cov_women_3))

DGM3_men <- function(size=2000){
  #' function for generating the simulated NHANES data under DGM3 for men
  #' @param size, sample size, default 2000
  #' @return generated data
  data.men <- data.frame(CURSMOKE=rnorm(size),
                         DIABETES=rnorm(size),
                         BPMEDS=rnorm(size),
                         log.TOTCHOL = rnorm(size,mean=mean_men_log[4],sd = sd_men[4]),
                         SYSBP = rgamma(size,shape=(mean_men_raw[5])^2/var_men_raw[5],scale=mean_men_raw[5]/var_men_raw[5]),
                         log.HDLC = rnorm(size,mean=mean_men_log[6],sd = sd_men[6]),
                         AGE = runif(size,30,74))
  mean.data.men <- apply(data.men,2,mean)
  for(i in 1:size){
    data.men[i,] <- t(L.men %*% t(data.men[i,]-mean.data.men)) + mean.data.men
  }
  # rescale the age
  data.men$AGE <- 30 + (74-30) * punif(data.men$AGE,min=min(data.men$AGE),max=max(data.men$AGE))
  data.men <- data.men %>%
    filter(SYSBP>0) %>%
    mutate(CURSMOKE=ifelse(CURSMOKE>quantile(data.men$CURSMOKE,(1961-474)/1961),1,0),
           DIABETES=ifelse(DIABETES>quantile(data.men$DIABETES,(1961-358)/1961),1,0),
           BPMEDS=ifelse(BPMEDS>quantile(data.men$BPMEDS,(1829-604)/1829),1,0),
           log.TOTCHOL = log.TOTCHOL,
           log.SYSBP = log(SYSBP),
           log.HDLC = log.HDLC,
           log.AGE = log(AGE))
  return(data.men)
}

DGM3_women <- function(size=2000){
  #' function for generating the simulated NHANES data under DGM3 for women
  #' @param size, sample size, default 2000
  #' @return generated data
  data.women <- data.frame(CURSMOKE=rnorm(size),
                         DIABETES=rnorm(size),
                         BPMEDS=rnorm(size),
                         log.TOTCHOL = rnorm(size,mean=mean_women_log[4],sd = sd_women[4]),
                         SYSBP = rgamma(size,shape=(mean_women_raw[5])^2/var_women_raw[5],scale=mean_women_raw[5]/var_women_raw[5]),
                         log.HDLC = rnorm(size,mean=mean_women_log[6],sd = sd_women[6]),
                         AGE = runif(size,30,74))
  mean.data.women <- apply(data.women,2,mean)
  for(i in 1:size){
    data.women[i,] <- t(L.women %*% t(data.women[i,]-mean.data.women)) + mean.data.women
  }
  # rescale the age
  data.women$AGE <- 30 + (74-30) * punif(data.women$AGE,min=min(data.women$AGE),max=max(data.women$AGE))
  data.women <- data.women %>%
    filter(SYSBP>0) %>%
    mutate(CURSMOKE=ifelse(CURSMOKE>quantile(data.women$CURSMOKE,(1961-474)/1961),1,0),
           DIABETES=ifelse(DIABETES>quantile(data.women$DIABETES,(1961-358)/1961),1,0),
           BPMEDS=ifelse(BPMEDS>quantile(data.women$BPMEDS,(1829-604)/1829),1,0),
           log.TOTCHOL = log.TOTCHOL,
           log.SYSBP = log(SYSBP),
           log.HDLC = log.HDLC,
           log.AGE = log(AGE))
  return(data.women)
}

bs.framingham.model.men.sim3.vec <- numeric(n_sim)
bs.framingham.model.women.sim3.vec <- numeric(n_sim)
bs.tailored.model.men.sim3.vec <- numeric(n_sim)
bs.tailored.model.women.sim3.vec <- numeric(n_sim)
set.seed(2550)
for(i in 1:n_sim){
  print(i)
  # simulated NHANES data for men
  data.men <- DGM3_men()
  data.men$D <- sample(c(0,1),nrow(data.men),prob=c(0.8,0.2),replace = TRUE)
  data.men$S <- 0
  # combine Framingham and NHANES as composite data
  combined_men <- bind_rows(data.men,framingham_men)
  combined_men$CURSMOKE <- factor(combined_men$CURSMOKE)
  combined_men$DIABETES <- factor(combined_men$DIABETES)
  combined_men$BPMEDS <- factor(combined_men$BPMEDS)
  combined_men$S <-factor(combined_men$S)
  combined_men$CVD <- factor(combined_men$CVD)
  combined_men$log.SYSBP_UT <-ifelse(combined_men$BPMEDS == 0, 
                                     combined_men$log.SYSBP, 0)
  combined_men$log.SYSBP_T <- ifelse(combined_men$BPMEDS == 1, 
                                     combined_men$log.SYSBP, 0)
  ###### Evaluation for Framingham Model ######
  if(i==1){
    # fit the Framingham model once only
    framingham.model.men <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                                data = combined_men %>% filter(D==FALSE,S==1),family = binomial())
  }
  men.test1 <- combined_men %>% filter((D==1 | S==0))
  ps.men.test1 <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES,  
                      data = men.test1,family = binomial())
  weights.men.test1 <- exp(predict(ps.men.test1))^(-1)
  bs.men1 <- sum((as.numeric(men.test1$S)-1) * weights.men.test1 * (predict(framingham.model.men,men.test1,type = "response")-ifelse(is.na(as.numeric(men.test1$CVD)), 0, as.numeric(men.test1$CVD))+1)^2)/sum(2-as.numeric(men.test1$S))
  bs.framingham.model.men.sim3.vec[i] <- bs.men1
  ###### Fit Tailored model ######
  # men
  ps.men.train <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                      data = combined_men %>% filter(D==0),family = binomial())
  weights.men.train <- exp(predict(ps.men.train,combined_men %>% filter(D==FALSE,S==1)))^(-1)
  mod.men <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                 data = combined_men %>% filter(D==0,S==1),family = binomial(),
                 weights = weights.men.train)
  # evaluate on test
  men.test <- combined_men %>% filter(D==1)
  ps.men.test <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                     data = men.test,family = binomial())
  weights.men.test <- exp(predict(ps.men.test))^(-1)
  bs.men <- sum((as.numeric(men.test$S)-1) * weights.men.test * (predict(mod.men,men.test,type = "response")-ifelse(is.na(as.numeric(men.test$CVD)), 0, as.numeric(men.test$CVD))+1)^2)/sum(2-as.numeric(men.test$S))
  bs.tailored.model.men.sim3.vec[i] <- bs.men
  
  
  # the same process for women
  data.women <- DGM3_women()
  data.women$D <- sample(c(0,1),nrow(data.women),prob=c(0.8,0.2),replace = TRUE)
  data.women$S <- 0
  combined_women <- bind_rows(data.women,framingham_women)
  combined_women$CURSMOKE <- factor(combined_women$CURSMOKE)
  combined_women$DIABETES <- factor(combined_women$DIABETES)
  combined_women$BPMEDS <- factor(combined_women$BPMEDS)
  combined_women$S <-factor(combined_women$S)
  combined_women$CVD <- factor(combined_women$CVD)
  combined_women$log.SYSBP_UT <-ifelse(combined_women$BPMEDS == 0, 
                                       combined_women$log.SYSBP, 0)
  combined_women$log.SYSBP_T <- ifelse(combined_women$BPMEDS == 1, 
                                       combined_women$log.SYSBP, 0)
  ###### Evaluation for Framingham Model ######
  if(i==1){
    # fit the Framingham model once only
    framingham.model.women <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                                  data = combined_women %>% filter(D==FALSE,S==1),family = binomial())
  }
  women.test1 <- combined_women %>% filter((D==1 | S==0))
  ps.women.test1 <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                        data = women.test1,family = binomial())
  weights.women.test1 <- exp(predict(ps.women.test1))^(-1)
  bs.women1 <- sum((as.numeric(women.test1$S)-1) * weights.women.test1 *(predict(framingham.model.women,women.test1,type = "response")-ifelse(is.na(as.numeric(women.test1$CVD)), 0, as.numeric(women.test1$CVD))+1)^2)/sum(2-as.numeric(women.test1$S))
  bs.framingham.model.women.sim3.vec[i] <- bs.women1
  ###### Fit Tailored model ######
  # women
  ps.women.train <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                        data = combined_women %>% filter(D==0),family = binomial())
  weights.women.train <- exp(predict(ps.women.train,combined_women %>% filter(D==FALSE,S==1)))^(-1)
  mod.women <- glm(CVD ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                   data = combined_women %>% filter(D==0,S==1),family = binomial(),
                   weights = weights.women.train)
  # evaluate on test
  women.test <- combined_women %>% filter(D==1)
  ps.women.test <- glm(S ~ log.HDLC+log.TOTCHOL+log.AGE+log.SYSBP_UT+log.SYSBP_T+CURSMOKE+DIABETES, 
                       data = women.test,family = binomial())
  weights.women.test <- exp(predict(ps.women.test))^(-1)
  bs.women <- sum((as.numeric(women.test$S)-1) * weights.women.test * (predict(mod.women,women.test,type = "response")-ifelse(is.na(as.numeric(women.test$CVD)), 0, as.numeric(women.test$CVD))+1)^2)/sum(2-as.numeric(women.test$S))
  bs.tailored.model.women.sim3.vec[i] <- bs.women
}
simulation_res <- data.frame(f.men.1 = bs.framingham.model.men.sim1.vec,
                             f.women.1=bs.framingham.model.women.sim1.vec,
                             t.men.1=bs.tailored.model.men.sim1.vec,
                             t.women.1=bs.tailored.model.women.sim1.vec,
                             f.men.2 = bs.framingham.model.men.sim2.vec,
                             f.women.2=bs.framingham.model.women.sim2.vec,
                             t.men.2=bs.tailored.model.men.sim2.vec,
                             t.women.2=bs.tailored.model.women.sim2.vec,
                             f.men.3 = bs.framingham.model.men.sim3.vec,
                             f.women.3=bs.framingham.model.women.sim3.vec,
                             t.men.3=bs.tailored.model.men.sim3.vec,
                             t.women.3=bs.tailored.model.women.sim3.vec)
# saveRDS(simulation_res,"simulation_result.RDS")
