###############################################################################################################################
# leave-one-stratum-out cross-validation, only for Model BTT(Zagheni) & BTF(Zagheni + interaction of age&gender)
#BTT: Zagheni's model
#BTF: Zagheni with interration
###############################################################################################################################

options(warn=-1)

library(lme4)
library(data.table)
# fb_data3 is  the dataframe for the models assuming inhomogeneous bias
fb_data3 =  read.csv("data/P_nuts3_df_data_NiXZ.csv")

computeMAPE <- function(y_true, y_pred){
  mean(abs((y_true - y_pred)/ y_true)) * 100
}

compute_mean_per_region <- function(y_true, no_comb_attr=8){
  true_mean <- mean(rowSums(matrix(y_true, ncol=no_comb_attr, byrow=TRUE)))
}

compute_sum_per_region <- function(y_true, no_comb_attr=8){
  sum_per_city <- rowSums(matrix(y_true, ncol=no_comb_attr, byrow=TRUE))
}

start_idx <- 1
end_idx <- nrow(fb_data3)
no_splits <- nrow(fb_data3)
no_samples = end_idx-start_idx+1

# Leave-one-out CV
leave_one_out <- function(fb_data1, formular, isLMER=FALSE, anti_log=FALSE, by_region=FALSE){
  n <- 8 # 8 demographic attributes
  
  y_true_list <- rep(0, no_splits)
  y_pred_list <- rep(0, no_splits)
  mape_list <- rep(0, no_splits)
  
  for(i in start_idx:no_splits){
    
    if(by_region == TRUE){
      training <- fb_data1[-seq(n*(i-1)+1, n*i),]
      test <- fb_data1[seq(n*(i-1)+1, n*i),]
    }
    else{
      training <- fb_data1[-i,]
      test <- fb_data1[i,]
    }
    
    if(isLMER == TRUE){
      m <- lmer(formular, data=training)
    }
    else{
      m <- lm(formular, data=training)
    }
    
    test_pred <- predict(m, test)
    
    if(anti_log == TRUE){
      y_true <- exp(test$census)
      y_pred <- exp(test_pred)
      y_mean = mean(exp(training$census))
      
      if(by_region == TRUE){
        y_true <- sum(exp(test$census))
        y_pred <- sum(exp(test_pred))
        y_mean = compute_mean_per_city(exp(training$census))
      }
      
    }
    else{
      y_true <- test$census
      y_pred <- test_pred
      y_mean = mean(training$census)
    }
    
    
    mape <- computeMAPE(y_true, y_pred)
    
    y_true_list[i] <- y_true
    y_pred_list[i] <- y_pred
    mape_list[i] <- mape
    
    if(i%%100==0){
      print(i)
    } 
  } 
  
  res <- matrix( rep( 0, len=no_splits*2), nrow = no_splits)
  res[,1] <- y_pred_list
  res[,2] <- mape_list

  res
  
}

run_joint_count_model <- function(fb_data1, formular){
  
  m2 <- lmer(formular, data=fb_data1)
  coefs <- data.frame(coef(summary(m2)))
  y_true = exp(fb_data1$census)
  y_pred = exp(predict(m2))
  y_true <- compute_sum_per_region(y_true)
  y_pred <- compute_sum_per_region(y_pred)
  true_mean <- mean(y_true)
  
  res_sample <- leave_one_out(fb_data1, formular, isLMER=TRUE, anti_log=TRUE, by_region=FALSE)
}


# Zagheni+interactions
formular <- 'census ~ twitter + age*gender + (0+twitter |country) + (0+age*gender|country)'
eval_result_btf <- run_joint_count_model(fb_data3, formular)

# Zagheni
formular <- 'census ~ twitter + age+gender + (0+twitter |country) + (0+age+gender|country)'
eval_result_btt <- run_joint_count_model(fb_data3, formular)


no_samples = end_idx-start_idx+1
no_metrics <- 2
no_models <- 2

eval_results <- matrix( rep( 0, len=no_samples*no_metrics*no_models), nrow = no_samples)
eval_results[,c(1:2)] <- eval_result_btt[c(start_idx:end_idx),]
eval_results[,c(3:4)] <- eval_result_btf[c(start_idx:end_idx),]


eval_results =  data.frame(eval_results)
eval_results$nuts3 = fb_data3$nuts3[c(start_idx:end_idx)]
eval_results$ytrue = exp(fb_data3[c(start_idx:end_idx),'census'])
colnames(eval_results) <- c("btt_ypred", "btt_mape",
                            "btf_ypred", "btf_mape",
                            "ModelName", "ytrue")

setcolorder(eval_results, c("ModelName", "ytrue",
                            "btt_ypred", "btt_mape",
                            "btf_ypred","btf_mape"))


print(eval_results)
write.csv(eval_results, file = "result/Eval_results_sample.csv")
