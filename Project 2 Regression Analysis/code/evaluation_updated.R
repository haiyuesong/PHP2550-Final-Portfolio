library(tidyverse) # version 1.3.2
library(pROC) # version 1.18.0
library(ggplot2) #  version 3.4.2
# model evaluation indicators
# AUC, sensitivity, specificity, accuracy, precision and Brier score values of the fitted model
evaluation <- function(pred,y.test,threshold=0.5){
  #' get AUC, sensitivity, specificity, accuracy, precision and Brier scorevalues of the fitted model
  #' @param pred, the prediction values
  #' @param y.test, the labels of test dataset
  #' @param threshold, a numeric number of threshold to classify the probability to classes
  #' @return AUC, sensitivity, specificity, accuracy, and precision values
  roc_object <- roc(y.test, as.vector(pred),levels=levels(y.test),direction="<")
  # auc
  auc <- roc_object$auc
  
  df <- data.frame(pred=as.numeric(pred>threshold),label=as.numeric(y.test)-1)
  TP <- dim(df[(df$pred==1&df$label==1),])[1]
  TN <- dim(df[(df$pred==0&df$label==0),])[1]
  FP <- dim(df[(df$pred==1&df$label==0),])[1]
  FN <- dim(df[(df$pred==0&df$label==1),])[1]
  Brier_score <- mean((pred-(as.numeric(y.test)-1))^2)
  return(c(AUC=auc,Sensitivity=TP/(TP+FN),Specificity=TN/(TN+FP),
           Accuracy=(TP+TN)/(TP+TN+FP+FN),Precision=TP/(TP+FP),
           "Brier Score"=Brier_score,Threshold=threshold))
}

# calibration plot
calibration_plot <- function(prob,class,num_cuts=10){
  #' return the calibration plot based on ggplot2
  #' refer to Dr. Alice Paul's book "Health Data Science in R"
  #' @param prob, the predicted probability
  #' @param class, the true labels
  #' @param num_cuts, the number of cuts to the probabilities
  #' @return the calibration plot based on ggplot2 class
  num_cuts <- num_cuts
  calib_data <-  data.frame(prob = prob,
                            bin = cut(prob, breaks = num_cuts),
                            class = as.numeric(class)-1)
  calib_data <- calib_data %>% 
    group_by(bin) %>% 
    dplyr::summarize(observed = sum(class)/n(), 
                     expected = sum(prob)/n(), 
                     se = sqrt(observed*(1-observed)/n()))
  return(ggplot(calib_data) + 
    geom_abline(intercept = 0, slope = 1, color="red") + 
    geom_errorbar(aes(x = expected, ymin=observed-1.96*se, 
                      ymax=observed+1.96*se), 
                  colour="black", width=.01)+
    geom_point(aes(x = expected, y = observed)) +
    labs(x="Expected Proportion", y="Observed Proportion") +
    theme_minimal())
}

# choose best threshold based on roc object
best_threshold <- function(roc){
  #' @param roc, the roc object returned from roc() function from pROC package
  #' @return optimal_threshold, the optimal threshold of categorize predicted outcome
  
  # Calculate the Euclidean distances from each point to the top-left corner (0,1)
  distances <- sqrt((1 - roc$sensitivities)^2 + (roc$specificities - 1)^2)
  
  # Find the index of the point with the minimum distance
  min_index <- which.min(distances)
  
  # get the optimal threshold
  optimal_threshold <- roc$thresholds[min_index]
  
  return(optimal_threshold)
}
