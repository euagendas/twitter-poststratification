###############################################################################################################################
# leave-one-region-out cross-validation for all the models in the paper
#BTT: Zagheni's model
#BTF: Zagheni with interration
###############################################################################################################################
options(warn=-1)

library(lme4)
library(data.table)

# fb_data1 is  the dataframe for the models assuming homogeneous bias
fb_data1 =  read.csv("data/P_nuts3_df_data_Ni.csv")

# fb_data3 is  the dataframe for the models assuming inhomogeneous bias
fb_data3 =  read.csv("data/P_nuts3_df_data_NiXZ.csv")

computeMAPE <- function(y_true, y_pred){
  mean(abs((y_true - y_pred)/ y_true)) * 100
}

exp1 <- function(vec){
  exp(vec)-1
}

compute_mean_per_region <- function(y_true, no_comb_attr=8){
  true_mean <- mean(rowSums(matrix(y_true, ncol=no_comb_attr, byrow=TRUE)))
}

compute_sum_per_region <- function(y_true, no_comb_attr=8){
  sum_per_city <- rowSums(matrix(y_true, ncol=no_comb_attr, byrow=TRUE))
}

start_idx <- 1
end_idx <- nrow(fb_data1)
no_splits <- nrow(fb_data1)
no_cities = end_idx-start_idx+1

# Leave-one-out CV
leave_one_out <- function(fb_data1, formular, isLMER=FALSE, anti_log=FALSE, by_region=FALSE, nval=8){
  n <- nval # 8 demographic attributes
  
  no_splits = nrow(fb_data1) 
  if(by_region == TRUE){
    no_splits <- no_splits / n 
  }
  
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
        y_mean = compute_mean_per_region(exp(training$census))
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

run_lm_model <- function(fb_data1, formular, byRegion, nvalue=4){
  m2 <- lmer(formular, data=fb_data1)
  coefs <- data.frame(coef(summary(m2)))
  y_true = fb_data1$census
  y_pred = predict(m2)
  
  res2 <- leave_one_out(fb_data1, formular, isLMER=TRUE, by_region=byRegion, nval=nvalue)
}


run_joint_count_model <- function(fb_data1, formular){
  m2 <- lmer(formular, data=fb_data1)
  coefs <- data.frame(coef(summary(m2)))
  y_true = exp(fb_data1$census)
  y_pred = exp(predict(m2))
  y_true <- compute_sum_per_region(y_true)
  y_pred <- compute_sum_per_region(y_pred)
  true_mean <- mean(y_true)
  
  res2 <- leave_one_out(fb_data1, formular, isLMER=TRUE, anti_log=TRUE, by_region=TRUE)
}


# Model 0:
formular <- 'census ~ twitter + (twitter+0|country)'
eval_result0 <- run_lm_model(fb_data1, formular, byRegion=FALSE, nvalue=1)


# Model MG:
formular <- 'census ~ gender_F + gender_M + (0+gender_F |country) + (0+gender_M |country)'
eval_result_mg <- run_lm_model(fb_data1, formular, byRegion=FALSE, nvalue=1)


# Model MAge:
formular <- 'census ~ age_0017 + age_1829 + age_3039 + age_4099 + (0+age_0017 |country) + (0+age_1829 |country) + (0+age_3039 |country) + (0+age_4099 |country)'
eval_result_mage <- run_lm_model(fb_data1, formular, byRegion=FALSE, nvalue=1)

# Model 1:
formular <- 'census ~ tw_0017F + tw_1829F + tw_3039F + tw_4099F + tw_0017M + tw_1829M + tw_3039M + tw_4099M + (0+tw_0017F |country) + (0+tw_1829F |country) + (0+tw_3039F |country) + (0+tw_4099F |country) + (0+tw_0017M |country) + (0+tw_1829M |country) + (0+tw_3039M |country) + (0+tw_4099M |country) '
eval_result_1 <- run_lm_model(fb_data1, formular, byRegion=FALSE, nvalue=1)

# Zagheni
formular <- 'census ~ twitter + age+gender + (0+twitter |country) + (0+age+gender|country)'
eval_result_btt <- run_joint_count_model(fb_data3, formular)

# Zagheni+interactions
formular <- 'census ~ twitter + age*gender + (0+twitter |country) + (0+age*gender|country)'
eval_result_btf <- run_joint_count_model(fb_data3, formular)

regions <- fb_data1['nuts3']
no_regions = nrow(regions)
no_metrics <- 2
no_models <- 6


eval_results <- matrix( rep( 0, len=no_regions*no_metrics*no_models), nrow = no_regions)
eval_results[,c(1:2)] <- eval_result0
eval_results[,c(3:4)] <- eval_result_mg
eval_results[,c(5:6)] <- eval_result_mage
eval_results[,c(7:8)] <- eval_result_1
eval_results[,c(9:10)] <-  eval_result_btt
eval_results[,c(11:12)] <- eval_result_btf


eval_results =  data.frame(eval_results)
eval_results$nuts3 = fb_data1$nuts3[c(start_idx:end_idx)]
eval_results$ytrue = fb_data1[c(start_idx:end_idx),'census']
colnames(eval_results) <- c("model0_YPred", "model0_mape",
                            "mg_YPred", "mg_mape",
                            "mage_YPred", "mage_mape", 
                            "model1_YPred", "model1_mape",
                            "btt_YPred", "btt_mape",
                            "btf_YPred", "btf_mape", 
                            "nuts3", "ytrue")

setcolorder(eval_results, c("nuts3", "ytrue", 
                            "model0_YPred", "model0_mape",
                            "mg_YPred", "mg_mape",
                            "mage_YPred", "mage_mape", 
                            "model1_YPred", "model1_mape", 
                            "btt_YPred", "btt_mape",
                            "btf_YPred", "btf_mape"))


print(eval_results)
write.csv(eval_results, file = "result/Eval_results_byCity.csv")

