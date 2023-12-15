# load the library
library(tidyverse) # version 1.3.2
library(glmnet) # version 4.1-6
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

lasso <- function(df) { 
  #' Runs 10-fold CV for lasso and returns corresponding coefficients 
  #' @param df, data set
  #' @return coef, coefficients for minimum cv error
  
  # Matrix form for ordered variables 
  x.ord <- model.matrix(outcome~ .+ventilation_support_level.36*(ga+del_method+prenat_ster+gender+sga+
                                                                   weight_today.36+inspired_oxygen.36+
                                                                   p_delta.36+peep_cm_h2o_modified.36+med_ph.36), data = df)[,-1]
  y.ord <- df$outcome
  
  # Generate folds
  k <- 10 
  set.seed(1) # consistent seeds between imputed data sets
  folds <- sample(1:k, nrow(df), replace=TRUE)
  
  # Lasso model
  lasso_mod_cv <- cv.glmnet(x.ord, y.ord, nfolds = 10, foldid = folds, 
                            alpha = 1, family = "binomial") 
  
  lasso_mod <- glmnet(x.ord, y.ord, nfolds=10, foldid=folds,
                      alpha=1, family = "binomial",
                      lambda = lasso_mod_cv$lambda.min)
  # Get coefficients 
  coef <- coef(lasso_mod) 
  return(coef) 
} 
predict.by.coef <- function(newdata,coef){
  #' get probability prediction for test data based on coefficients
  #' binary outcome
  #' @param newdata, test data
  #' @param coef, coefficients
  #' @return predictions 
  x_vars <- model.matrix(outcome~ .+ventilation_support_level.36*(ga+del_method+prenat_ster+gender+sga+
                                                                    weight_today.36+inspired_oxygen.36+
                                                                    p_delta.36+peep_cm_h2o_modified.36+med_ph.36), newdata)[,]
  return(as.vector(plogis(x_vars[,names(coef)] %*% coef)))
}


###################################################### 
#### Lasso with center #### 
###################################################### 
# Find average lasso coefficients over imputed datasets
lasso_coef1 <- lasso(training_imp[[1]]) 
lasso_coef2 <- lasso(training_imp[[2]]) 
lasso_coef3 <- lasso(training_imp[[3]]) 
lasso_coef4 <- lasso(training_imp[[4]]) 
lasso_coef5 <- lasso(training_imp[[5]]) 
lasso_coef <- cbind(lasso_coef1, lasso_coef2, lasso_coef3, 
                    lasso_coef4, lasso_coef5)
###### Method 1: take the average of coefficients directly
avg_coefs_lasso <- apply(lasso_coef, 1, mean) 
avg_coefs_lasso[which(avg_coefs_lasso!=0)]

# evaluation on test
test_pred_lasso <- predict.by.coef(test_long,avg_coefs_lasso)
roc_lasso <- roc(test_long$outcome,test_pred_lasso)
roc_curve_lasso <- data.frame(FPR = 1-roc_lasso$specificities, 
                              TPR = roc_lasso$sensitivities)
# UPDATED: get best threshold
train_pred_lasso <- predict.by.coef(training_long,avg_coefs_lasso)
thres_lasso <- best_threshold(roc(training_long$outcome,train_pred_lasso))

# evaluation metrics
evaluation_lasso <- evaluation(test_pred_lasso,test_long$outcome, threshold = thres_lasso)
evaluation_lasso

###### Method 2: only include the variables has non-zero coefficients within at least a half of imputed data (3)
lasso_coef_num <- apply(lasso_coef,1,function(x) sum(x!=0))
avg_coefs_lasso_3 <- avg_coefs_lasso[which(lasso_coef_num>=3)]
# evaluation on test
test_pred_lasso_3 <- predict.by.coef(test_long,avg_coefs_lasso_3) # prediction
# data frame for roc
roc_lasso_3 <- roc(test_long$outcome,test_pred_lasso_3)
roc_curve_lasso_3 <- data.frame(FPR = 1-roc_lasso_3$specificities,
                                   TPR = roc_lasso_3$sensitivities)

# UPDATED: get best threshold
train_pred_lasso_3 <- predict.by.coef(training_long,avg_coefs_lasso_3)
thres_lasso_3 <- best_threshold(roc(training_long$outcome,train_pred_lasso_3))

# evaluation metrics
evaluation_lasso_3 <- evaluation(test_pred_lasso_3,test_long$outcome,threshold = thres_lasso_3)
evaluation_lasso_3
# Method 2 has the same result as method 1

###### Method 3: only include the variables has non-zero coefficients within all models
avg_coefs_lasso_5 <- avg_coefs_lasso[which(lasso_coef_num==5)]
# evaluation on test
test_pred_lasso_5 <- predict.by.coef(test_long,avg_coefs_lasso_5) # prediction
# data frame for roc
roc_lasso_5 <- roc(test_long$outcome,test_pred_lasso_5)
roc_curve_lasso_5 <- data.frame(FPR = 1-roc_lasso_5$specificities,
                                TPR = roc_lasso_5$sensitivities)
# UPDATED: get best threshold
thres_lasso_5 <- best_threshold(roc_lasso_5)

# UPDATED: get best threshold
train_pred_lasso_5 <- predict.by.coef(training_long,avg_coefs_lasso_5)
thres_lasso_5 <- best_threshold(roc(training_long$outcome,train_pred_lasso_5))

# evaluation metrics
evaluation_lasso_5 <- evaluation(test_pred_lasso_5,test_long$outcome, threshold = thres_lasso_5)
evaluation_lasso_5

# combination
combined_coef_lasso <-bind_rows("Times"=lasso_coef_num[which(lasso_coef_num>=1)],
                                "M1"=avg_coefs_lasso[which(avg_coefs_lasso!=0)],
                                "M2"=avg_coefs_lasso_3,"M3"=avg_coefs_lasso_5) %>%
  round(2)
saveRDS(combined_coef_lasso,"coefficients_lasso.RDS")

###### combined evaluation
combined_eva_lasso <- cbind(evaluation_lasso,evaluation_lasso_3,evaluation_lasso_5) %>% round(4)
# saveRDS(combined_eva_lasso,"evaluation_lasso.RDS")

###### ROC curves data
roc_data_lasso <- rbind(roc_curve_lasso,roc_curve_lasso_3,roc_curve_lasso_5)
roc_data_lasso$model <- c(rep("Lasso (Method 1)",nrow(roc_curve_lasso)),
                    rep("Lasso (Method 2)",nrow(roc_curve_lasso_3)),
                    rep("Lasso (Method 3)",nrow(roc_curve_lasso_5)))
roc_data_lasso$center <- c(rep("Fixed-Effect",nrow(roc_curve_lasso)+
                                nrow(roc_curve_lasso_3)+nrow(roc_curve_lasso_5)))
# saveRDS(roc_data_lasso,"roc_lasso.RDS")

# calibration plot for final lasso
cp_lasso <- calibration_plot(test_pred_lasso,test_long$outcome) + 
  ggtitle('Calibration Plot for Lasso',subtitle = "Including Center, Method 1")
# saveRDS(cp_lasso,"calibraion_lasso.RDS")

# confidence interval for auc
ci(roc_lasso,conf.level = 0.95)
# interpret by sign
avg_coefs_lasso
