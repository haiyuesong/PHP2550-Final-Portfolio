# load the library
library(tidyverse) # version 1.3.2
library(caret) # version 6.0-93

# read the source population data
framingham_df <- readRDS("source.RDS")
framingham_df$SEX <- factor(framingham_df$SEX)
framingham_df$CURSMOKE <- factor(framingham_df$CURSMOKE)
framingham_df$DIABETES <- factor(framingham_df$DIABETES)
framingham_df$BPMEDS <- factor(framingham_df$BPMEDS)
# read the target imputed data
target_mice <- readRDS("target.RDS")

# read the indices for train-test split for the target imputed data
splitIndex2 <- readRDS("target_train_test.RDS")

# Store each imputed target data set
# and store each composite data set
target_imp <- vector("list",5)
composite_imp <- vector("list",5)
for (i in 1:5){
  target_imp[[i]] <- mice::complete(target_mice,i) 
  target_imp[[i]]$SYSBP_UT <- ifelse(target_imp[[i]]$BPMEDS == 0, 
                                     target_imp[[i]]$SYSBP, 0)
  target_imp[[i]]$SYSBP_T <- ifelse(target_imp[[i]]$BPMEDS == 1, 
                                    target_imp[[i]]$SYSBP, 0)
  target_imp[[i]]$D <- 1-(1:nrow(target_imp[[i]]) %in% splitIndex2)
  composite_imp[[i]]<- bind_rows(framingham_df,target_imp[[i]])
  composite_imp[[i]]$S <- 1-is.na(composite_imp[[i]]$CVD)
  composite_imp[[i]]$CVD <- factor(composite_imp[[i]]$CVD)
  composite_imp[[i]]$SEX <- factor(composite_imp[[i]]$SEX)
  composite_imp[[i]]$CURSMOKE <- factor(composite_imp[[i]]$CURSMOKE)
  composite_imp[[i]]$DIABETES <- factor(composite_imp[[i]]$DIABETES)
  composite_imp[[i]]$BPMEDS <- factor(composite_imp[[i]]$BPMEDS)
  composite_imp[[i]]$S<- factor(composite_imp[[i]]$S)
}
# double check the missingness
num_na <- sapply(composite_imp[[1]],function(x) sum(is.na(x)))
pct_na <- round(num_na/dim(composite_imp[[1]])[1]*100,2)
missing_stat <- data.frame(num=num_na,pct=pct_na)
missing_stat

### Framingham Model without Tailoring ###
framingham.model.men <- glm(CVD ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+log(SYSBP_T+1)+CURSMOKE+DIABETES,
                        data = framingham_df %>% filter(D==FALSE,SEX==1),family = binomial())
framingham.model.women <- glm(CVD ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+log(SYSBP_T+1)+CURSMOKE+DIABETES,
                            data = framingham_df %>% filter(D==FALSE,SEX==2),family = binomial())
# saveRDS(framingham.model.men,"framingham_model_men.RDS")
# saveRDS(framingham.model.women,"framingham_model_women.RDS")

# Brier score on Framingham test
test.framingham.men <- framingham_df %>% filter(D==TRUE,SEX==1)
bs.framingham.men <- mean((predict(framingham.model.men,test.framingham.men,type = "response") - test.framingham.men$CVD)^2)
test.framingham.women <- framingham_df %>% filter(D==TRUE,SEX==2)
bs.framingham.women <- mean((predict(framingham.model.women,test.framingham.women,type = "response") - test.framingham.women$CVD)^2)

# transport to NHANES
bs.framingham.model.men.vec <- numeric(5)
bs.framingham.model.women.vec <- numeric(5)
bs.tailored.model.men.vec <- numeric(5)
bs.tailored.model.women.vec <- numeric(5)

for(i in 1:5){
  print(i)
  ###### Fit Tailored model ######
  # get propensity score of S in train
  # men
  ps.men.train <- glm(S ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+log(SYSBP_T+1)+CURSMOKE+DIABETES, 
                      data = composite_imp[[i]] %>% filter(D==FALSE,SEX==1),family = binomial())
  weights.men.train <- exp(predict(ps.men.train,composite_imp[[i]] %>% filter(D==FALSE,S==1,SEX==1)))^(-1)
  mod.men <- glm(CVD ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+log(SYSBP_T+1)+CURSMOKE+DIABETES,
                 data = composite_imp[[i]] %>% filter(D==FALSE,S==1,SEX==1),family = binomial(),
                 weights = weights.men.train)
  # women
  ps.women.train <- glm(S ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+log(SYSBP_T+1)+CURSMOKE+DIABETES,
                      data = composite_imp[[i]] %>% filter(D==FALSE,SEX==2),family = binomial())
  weights.women.train <- exp(predict(ps.women.train,composite_imp[[i]] %>% filter(D==FALSE,S==1,SEX==2)))^(-1)
  mod.women <- glm(CVD ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+log(SYSBP_T+1)+CURSMOKE+DIABETES,
                 data = composite_imp[[i]] %>% filter(D==FALSE,S==1,SEX==2),family = binomial(),
                 weights = weights.women.train)
  
  ###### Evaluation for Framingham Model ######
  # men
  men.test1 <- composite_imp[[i]] %>% filter((D==TRUE | S==0),SEX==1)
  ps.men.test1 <- glm(S ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+log(SYSBP_T+1)+CURSMOKE+DIABETES, 
                     data = men.test1,family = binomial())
  weights.men.test1 <- exp(predict(ps.men.test1))^(-1)
  bs.men1 <- sum((as.numeric(men.test1$S)-1) * weights.men.test1 * (predict(framingham.model.men,men.test1,type = "response")-ifelse(is.na(as.numeric(men.test1$CVD)), 0, as.numeric(men.test1$CVD))+1)^2)/sum(2-as.numeric(men.test1$S))
  bs.framingham.model.men.vec[i] <- bs.men1
  
  # women
  women.test1 <- composite_imp[[i]] %>% filter((D==TRUE | S==0),SEX==2)
  ps.women.test1 <- glm(S ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+log(SYSBP_T+1)+CURSMOKE+DIABETES,
                       data = women.test1,family = binomial())
  weights.women.test1 <- exp(predict(ps.women.test1))^(-1)
  bs.women1 <- sum((as.numeric(women.test1$S)-1) * weights.women.test1 *(predict(framingham.model.women,women.test1,type = "response")-ifelse(is.na(as.numeric(women.test1$CVD)), 0, as.numeric(women.test1$CVD))+1)^2)/sum(2-as.numeric(women.test1$S))
  bs.framingham.model.women.vec[i] <- bs.women1
  
  
  ###### Evaluation for Tailored Model ######
  # men
  men.test <- composite_imp[[i]] %>% filter(D==TRUE,SEX==1)
  ps.men.test <- glm(S ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+log(SYSBP_T+1)+CURSMOKE+DIABETES, 
                     data = men.test,family = binomial())
  weights.men.test <- exp(predict(ps.men.test))^(-1)
  bs.men <- sum((as.numeric(men.test$S)-1) * weights.men.test * (predict(mod.men,men.test,type = "response")-ifelse(is.na(as.numeric(men.test$CVD)), 0, as.numeric(men.test$CVD))+1)^2)/sum(2-as.numeric(men.test$S))
  bs.tailored.model.men.vec[i] <- bs.men
  # women
  women.test <- composite_imp[[i]] %>% filter(D==TRUE,SEX==2)
  ps.women.test <- glm(S ~ log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+log(SYSBP_T+1)+CURSMOKE+DIABETES,
                     data = women.test,family = binomial())
  weights.women.test <- exp(predict(ps.women.test))^(-1)
  bs.women <- sum((as.numeric(women.test$S)-1) * weights.women.test *(predict(mod.women,women.test,type = "response")-ifelse(is.na(as.numeric(women.test$CVD)), 0, as.numeric(women.test$CVD))+1)^2)/sum(2-as.numeric(women.test$S))
  bs.tailored.model.women.vec[i] <- bs.women
  
}

# save the results
# saveRDS(bs.framingham.model.men.vec,"bs_framingham_model_men.RDS")
# saveRDS(bs.framingham.model.women.vec,"bs_framingham_model_women.RDS")
# saveRDS(bs.tailored.model.men.vec,"bs_tailored_model_men.RDS")
# saveRDS(bs.tailored.model.women.vec,"bs_tailored_model_women.RDS")