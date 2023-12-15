library(tidyverse) # version 1.3.2
library(pROC) # version 1.18.0
library(lme4) # version 1.1-31
source("evaluation_updated.R")

# read the training
training_mice <- readRDS("training_mice.RDS")
# Store each imputed data set
training_imp <- vector("list",5)
for (i in 1:5){
  training_imp[[i]] <- mice::complete(training_mice,i) 
}

# read the test
test_mice <- readRDS("test_mice.RDS")
test_long <- mice::complete(test_mice,action="long") 
test_long <- test_long[,-c(1,2)] # drop the number of imputation set and id

###################################################### 
#### Variables selected by Lasso #### 
######################################################
# list for mixed-effect models
mm_lasso <- list()
# fixed-effects
mm_lasso_coef<- c()
# fit on each imputed data set
for(i in 1:5){
  model <- glmer(outcome~mat_ethn+bw+birth_hc+del_method+prenat_ster+com_prenat_ster+
                   mat_chorio+sga+weight_today.36+
                   (ga+del_method+prenat_ster+gender+sga+weight_today.36) * ventilation_support_level.36+inspired_oxygen.36+p_delta.36+
                   ventilation_support_level.36*(inspired_oxygen.36+med_ph.36) + (1 | center), family = binomial(), 
                 data = training_imp[[i]])
  mm_lasso[[i]] <- model
  mm_lasso_coef <- cbind(mm_lasso_coef,fixef(model))
}
# get average of fixed-effects
mm_lasso_avg_coef <- apply(mm_lasso_coef, 1, mean)

# evaluation on test
test_pred_mm_lasso <- c()
for(i in 1:5){
  print(i)
  test_pred_mm_lasso <- c(test_pred_mm_lasso,predict(mm_lasso[[i]],newdata=mice::complete(test_mice,i),type="response"))
}
# data frame for roc
roc_mm_lasso <- roc(test_long$outcome,test_pred_mm_lasso)
plot(roc_mm_lasso)
roc_curve_mm_lasso <- data.frame(FPR = 1-roc_mm_lasso$specificities, 
                              TPR = roc_mm_lasso$sensitivities)

# get best threshold
train_pred_mm_lasso <- c()
for(i in 1:5){
  train_pred_mm_lasso <- c(train_pred_mm_lasso,predict(mm_lasso[[i]],newdata=training_imp[[i]],type="response"))
}
thres_mm_lasso<- best_threshold(roc(training_long$outcome,train_pred_mm_lasso))

# evaluation metrics
evaluation_mm_lasso <- evaluation(test_pred_mm_lasso,test_long$outcome, threshold = thres_mm_lasso)
evaluation_mm_lasso

### pool random effects' variance
random_effects_variances_lasso <- vector("list", 5)

for (i in 1:5) {
  random_effects_variances_lasso[[i]] <- VarCorr(mm_lasso[[i]])$center[1]^2
}
within_imputation_variance_lasso <- sapply(random_effects_variances_lasso, function(v) v[1])
mean_within_imputation_variance_lasso <- mean(within_imputation_variance_lasso)
between_imputation_variance_lasso <- var(within_imputation_variance_lasso)
total_variance_lasso <- mean_within_imputation_variance_lasso + (1 + 1/5) * between_imputation_variance_lasso
### random effect
coef_center_lasso <- c()
for(i in 1:5){
  coef_center_lasso <- cbind(coef_center_lasso, ranef(mm_lasso[[i]])$center[,1])
}
rownames(coef_center_lasso) <- paste("Center",c(1:5,7,12,16))
colnames(coef_center_lasso)<- paste("Imputed Data",c(1:5))
coef_center_lasso <- as.data.frame(coef_center_lasso)
coef_center_lasso$Average <- apply(coef_center_lasso,1,mean)

###################################################### 
#### Variables selected by Best Sulassoet #### 
######################################################
# list for mixed-effect models
mm_lasso <- list()
# fixed-effects
mm_lasso_coef <- c()
# fit on each imputed data set
for(i in 1:5){
  model <- glmer(outcome~mat_ethn+prenat_ster+weight_today.36+
                   gender * ventilation_support_level.36+
                   ventilation_support_level.36*(inspired_oxygen.36+med_ph.36) + (1 | center), family = binomial(), 
                 data = training_imp[[i]])
  mm_lasso[[i]] <- model
  mm_lasso_coef <- cbind(mm_lasso_coef,fixef(model))
}
# get average of fixed-effects
mm_lasso_avg_coef <- apply(mm_lasso_coef, 1, mean)

# evaluation on test
test_pred_mm_lasso <- c()
for(i in 1:5){
  test_pred_mm_lasso <- c(test_pred_mm_lasso,predict(mm_lasso[[i]],newdata=mice::complete(test_mice,i),type="response"))
}
# data frame for roc
roc_mm_lasso <- roc(test_long$outcome,test_pred_mm_lasso)
roc_curve_mm_lasso <- data.frame(FPR = 1-roc_mm_lasso$specificities, 
                                   TPR = roc_mm_lasso$sensitivities)

# get best threshold
train_pred_mm_lasso <- c()
for(i in 1:5){
  train_pred_mm_lasso <- c(train_pred_mm_lasso,predict(mm_lasso[[i]],newdata=training_imp[[i]],type="response"))
}
thres_mm_lasso<- best_threshold(roc(training_long$outcome,train_pred_mm_lasso))

# evaluation metrics
evaluation_mm_lasso <- evaluation(test_pred_mm_lasso,test_long$outcome, threshold = thres_lasso_1_refit)
evaluation_mm_lasso

# confidence interval for auc
ci(roc_mm_lasso,conf.level = 0.95)
# calibration plot
cp_mm <- calibration_plot(test_pred_mm_lasso,test_long$outcome) +
  ggtitle('Calibration Plot for Best Sulassoet',subtitle = "Mixed-Effect")
saveRDS(cp_mm,"calibraion_mm.RDS")

### pool random effects' variance
random_effects_variances_lasso <- vector("list", 5)

for (i in 1:5) {
  random_effects_variances_lasso[[i]] <- VarCorr(mm_lasso[[i]])$center[1]^2
}
within_imputation_variance_lasso <- sapply(random_effects_variances_lasso, function(v) v[1])
mean_within_imputation_variance_lasso <- mean(within_imputation_variance_lasso)
between_imputation_variance_lasso <- var(within_imputation_variance_lasso)
total_variance_lasso <- mean_within_imputation_variance_lasso + (1 + 1/5) * between_imputation_variance_lasso

### random effect
coef_center_lasso <- c()
for(i in 1:5){
  coef_center_lasso <- cbind(coef_center_lasso, ranef(mm_lasso[[i]])$center[,1])
}
rownames(coef_center_lasso) <- paste("Center",c(1:5,7,12,16))
colnames(coef_center_lasso)<- paste("Imputed Data",c(1:5))
coef_center_lasso <- as.data.frame(coef_center_lasso)
coef_center_lasso$Average <- apply(coef_center_lasso,1,mean)


###################################################### 
#### save results #### 
######################################################
###### combined coefficients
combined_coef_mm <- bind_rows("Mixed-Effect Best Sulassoet"=mm_lasso_avg_coef,
                              "Mixed-Effect Lasso"=mm_lasso_avg_coef) %>%
  round(2)
combined_coef_mm
saveRDS(combined_coef_mm,"coefficients_mm.RDS")

###### combined evaluation
combined_eva_mm <- cbind(evaluation_mm_lasso,evaluation_mm_lasso) %>% round(4)
saveRDS(combined_eva_mm,"evaluation_mm.RDS")

###### ROC curves data
roc_data_mm <- rbind(roc_curve_mm_lasso,roc_curve_mm_lasso)
roc_data_mm$model <- c(rep("Best Sulassoet (Mixed-Effect)",nrow(roc_curve_mm_lasso)),
                       rep("Lasso (Mixed-Effect)",nrow(roc_curve_mm_lasso)))
roc_data_mm$center <- c(rep("Mixed-Effect",nrow(roc_curve_mm_lasso)+
                              nrow(roc_curve_mm_lasso)))
saveRDS(roc_data_mm,"roc_mm.RDS")

###### mean of random intercept
saveRDS(coef_center_lasso,"coef_center_lasso.RDS")
saveRDS(coef_center_lasso,"coef_center_lasso.RDS")
