library(tidyverse) # version 1.3.2
library(L0Learn) # version 2.1.0
library(pROC) # version 1.18.0
library(mice) # version 3.15.0
source("evaluation_updated.R")

# read the training
training_mice <- readRDS("training_mice.RDS")
# Store each imputed data set
training_imp <- vector("list",5)    
for (i in 1:5){
  training_imp[[i]] <- mice::complete(training_mice,i) 
}
# training_long for threshold optimization
training_long <- mice::complete(training_mice,action="long") 
training_long <- training_long[,-c(1,2)]# drop the number of imputation set and id

# read the test
test_mice <- readRDS("test_mice.RDS")
test_long <- mice::complete(test_mice,action="long") 
test_long <- test_long[,-c(1,2)] # drop the number of imputation set and id
formula.bs <- formula(outcome~ .+ventilation_support_level.36*(ga+del_method+prenat_ster+gender+sga+
                                                                 weight_today.36+inspired_oxygen.36+
                                                                 p_delta.36+peep_cm_h2o_modified.36+med_ph.36))
best_subset <- function(df){
  #' use the function from L0Learn to get the coefficients under best subset of logistic regression
  #' @param df, data set
  #' @return coef, coefficients for minimum cv error
  x.ord <- model.matrix(formula.bs, data = df)
  y.ord <- df$outcome
  fit <- L0Learn.cvfit(x.ord[,-1], y.ord, penalty="L0",loss="Logistic", maxSuppSize=20,nFolds=10, seed=1)
  best_index <- which(unlist(fit$cvMeans)==min(unlist(fit$cvMeans)))
  coef.bs <- as.vector(coef(fit, lambda=print(fit)[best_index,]$lambda))
  names(coef.bs) <- colnames(x.ord)
  return(coef.bs)
}
predict.by.coef <- function(newdata,coef){
  #' get probability prediction for test data based on coefficients
  #' binary outcome
  #' @param newdata, test data
  #' @param coef, coefficients
  #' @return predictions 
  x_vars <- model.matrix(formula.bs, newdata)[,]
  return(as.vector(plogis(x_vars[,names(coef)] %*% coef)))
}

###################################################### 
#### Best Subset with center #### 
######################################################
bs_coef1 <- best_subset(training_imp[[1]]) 
bs_coef2 <- best_subset(training_imp[[2]]) 
bs_coef3 <- best_subset(training_imp[[3]]) 
bs_coef4 <- best_subset(training_imp[[4]]) 
bs_coef5 <- best_subset(training_imp[[5]]) 
bs_coef <- cbind(bs_coef1, bs_coef2, bs_coef3, 
                    bs_coef4, bs_coef5) 
bs_coef_num <- apply(bs_coef,1,function(x) sum(x!=0))

###### Method 1: Include all variables appearing in any model
rownames(bs_coef)[which(bs_coef_num>=1)] # get the variables
refit_bs_1 <- with(training_mice,exp=glm(outcome ~ center + mat_ethn + 
                                           bw + birth_hc + prenat_ster + com_prenat_ster + mat_chorio + 
                                           gender+ weight_today.36 + (gender+sga)*ventilation_support_level.36 +ventilation_support_level.36*(inspired_oxygen.36+med_ph.36),
                                         family = "binomial"))
refit_bs_1.pool <- pool(refit_bs_1)
bs_1_coef_refit <- refit_bs_1.pool$pooled[,3]
names(bs_1_coef_refit) <- refit_bs_1.pool$pooled[,1]
# evaluation on test
test_pred_bs_1_refit <- predict.by.coef(test_long,bs_1_coef_refit) # prediction
# data frame for roc
roc_bs_1_refit <- roc(test_long$outcome,test_pred_bs_1_refit)
roc_curve_bs_1_refit <- data.frame(FPR = 1-roc_bs_1_refit$specificities, 
                                      TPR = roc_bs_1_refit$sensitivities)

# UPDATED: get best threshold
train_pred_bs_1_refit <- predict.by.coef(training_long,bs_1_coef_refit)
thres_bs_1_refit<- best_threshold(roc(training_long$outcome,train_pred_bs_1_refit))

# evaluation metrics
evaluation_bs_1_refit <- evaluation(test_pred_bs_1_refit,test_long$outcome, threshold = thres_bs_1_refit)
evaluation_bs_1_refit



###### Method 2: include the variables has non-zero coefficients within at least a half of imputed data (3)
rownames(bs_coef)[which(bs_coef_num>=3)]
refit_bs_3 <- with(training_mice,exp=glm(outcome~center+mat_ethn+prenat_ster+weight_today.36+
                                           gender * ventilation_support_level.36+
                                      ventilation_support_level.36*(inspired_oxygen.36+
                                        med_ph.36),family = "binomial"))
refit_bs_3.pool <- pool(refit_bs_3)
bs_3_coef_refit <- refit_bs_3.pool$pooled[,3]
names(bs_3_coef_refit) <- refit_bs_3.pool$pooled[,1]
# evaluation on test
test_pred_bs_3_refit <- predict.by.coef(test_long,bs_3_coef_refit) # prediction
# data frame for roc
roc_bs_3_refit <- roc(test_long$outcome,test_pred_bs_3_refit)
roc_curve_bs_3_refit <- data.frame(FPR = 1-roc_bs_3_refit$specificities, 
                                   TPR = roc_bs_3_refit$sensitivities)
# UPDATED: get best threshold
train_pred_bs_3_refit <- predict.by.coef(training_long,bs_3_coef_refit)
thres_bs_3_refit<- best_threshold(roc(training_long$outcome,train_pred_bs_3_refit))

# evaluation metrics
evaluation_bs_3_refit <- evaluation(test_pred_bs_3_refit,test_long$outcome, threshold = thres_bs_3_refit)
evaluation_bs_3_refit

###### Method 3: include the variables has non-zero coefficients within all models
rownames(bs_coef)[which(bs_coef_num==5)]
refit_bs_5 <- with(training_mice,exp=glm(outcome~center+gender*ventilation_support_level.36+
                                           ventilation_support_level.36*(inspired_oxygen.36+med_ph.36),family = "binomial"))
refit_bs_5.pool <- pool(refit_bs_5)
bs_5_coef_refit <- refit_bs_5.pool$pooled[,3]
names(bs_5_coef_refit) <- refit_bs_5.pool$pooled[,1]
# evaluation on test
test_pred_bs_5_refit <- predict.by.coef(test_long,bs_5_coef_refit) # prediction
# data frame for roc
roc_bs_5_refit <- roc(test_long$outcome,test_pred_bs_5_refit)
roc_curve_bs_5_refit <- data.frame(FPR = 1-roc_bs_5_refit$specificities, 
                                   TPR = roc_bs_5_refit$sensitivities)
# UPDATED: get best threshold
train_pred_bs_5_refit <- predict.by.coef(training_long,bs_5_coef_refit)
thres_bs_5_refit<- best_threshold(roc(training_long$outcome,train_pred_bs_5_refit))

# evaluation metrics
evaluation_bs_5_refit <- evaluation(test_pred_bs_5_refit,test_long$outcome, threshold = thres_bs_5_refit)
evaluation_bs_5_refit

###### combined coefficients
# combination
combined_coef_bs <-bind_rows("Times"=bs_coef_num[which(bs_coef_num>=1)],
                             "M1"=bs_1_coef_refit,"M2"=bs_3_coef_refit,"M3"=bs_5_coef_refit) %>%
  round(2)
combined_coef_bs
saveRDS(combined_coef_bs,"coefficients_bs.RDS")

###### combined evaluation
combined_eva_bs <- cbind(evaluation_bs_1_refit,evaluation_bs_3_refit,evaluation_bs_5_refit) %>% round(4)
saveRDS(combined_eva_bs,"evaluation_bs.RDS")

###### ROC curves data
roc_data_bs <- rbind(roc_curve_bs_1_refit,roc_curve_bs_3_refit,roc_curve_bs_5_refit)
roc_data_bs$model <- c(rep("Best Subset (Method 1)",nrow(roc_curve_bs_1_refit)),
                          rep("Best Subset (Method 2)",nrow(roc_curve_bs_3_refit)),
                          rep("Best Subset (Method 3)",nrow(roc_curve_bs_5_refit)))
roc_data_bs$center <- c(rep("Fixed-Effect",nrow(roc_curve_bs_1_refit)+
                             nrow(roc_curve_bs_3_refit)+nrow(roc_curve_bs_5_refit)))
saveRDS(roc_data_bs,"roc_bs.RDS")

cp_bs <- calibration_plot(test_pred_bs_1_refit,test_long$outcome) +
  ggtitle('Calibration Plot for Best Subset',subtitle = "Fixed-Effect, M1")
saveRDS(cp_bs,"calibraion_bs.RDS")

# confidence interval for auc
ci(roc_bs_1_refit,conf.level = 0.95)

# for interpret final best subset
round(exp(bs_nc_1_coef_refit[-1]),2)
# significance
summary(refit_bs_nc_1.pool)
