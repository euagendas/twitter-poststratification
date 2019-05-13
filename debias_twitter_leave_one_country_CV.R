###############################################################################################################################
# leave-one-country-out cross-validation, only for Model BTT(Zagheni) & BTF(Zagheni + interaction of age&gender)
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


exp1 <- function(vec){
  exp(vec)-1
}

compute_mean_per_region <- function(y_true, no_comb_attr=8){
  true_mean <- mean(rowSums(matrix(y_true, ncol=no_comb_attr, byrow=TRUE)))
}

compute_sum_per_region <- function(y_true, no_comb_attr=8){
  sum_per_city <- rowSums(matrix(y_true, ncol=no_comb_attr, byrow=TRUE))
}


# Leave-one-country-out CV
leave_one_country_out <- function(fb_data1, formular, anti_log=FALSE){
  
  countries <- unique(fb_data1$country)
  
  no_splits = length(countries) 
  
  mape_list <- rep(0, no_splits)
  
  for(i in 1:length(countries)){
    training <- subset(fb_data1, country!=countries[i])
    test <- subset(fb_data1, country==countries[i])
    
    m <- lmer(formular, data=training)
    test_pred <- predict(m, test, re.form=~0)
    
    if(anti_log == TRUE){
      y_true <- exp(test$census)
      y_pred <- exp(test_pred)
      y_mean = mean(exp(training$census))
    }
    else{
      y_true <- test$census
      y_pred <- test_pred
      y_mean = mean(training$census)
    }
    
    mape <- computeMAPE(y_true, y_pred)
    
    mape_list[i] <- mape
    print(i)
  } 
  
  res <- matrix( rep( 0, len=no_splits*1), nrow = no_splits)
  res[,1] <- mape_list
  
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
  
  res_country <- leave_one_country_out(fb_data1, formular, anti_log=TRUE)
}

# Zagheni
formular <- 'census ~ twitter + age+gender + (0+twitter |country) + (0+age+gender|country)'
eval_result_btt <- run_joint_count_model(fb_data3, formular)

# Zagheni+interactions
formular <- 'census ~ twitter + age*gender + (0+twitter |country) + (0+age*gender|country)'
eval_result_btf <- run_joint_count_model(fb_data3, formular)


countries <- unique(fb_data1$country)
no_countries = length(countries) 
no_metrics <- 1
no_models <- 2

eval_results <- matrix( rep( 0, len=no_countries*no_metrics*no_models), nrow = no_countries)
eval_results[,1] <- eval_result_btt
eval_results[,2] <- eval_result_btf

library(data.table)
eval_results =  data.frame(eval_results)
eval_results$country = countries
colnames(eval_results) <- c("btt_mape", "btf_mape", "ModelName")

setcolorder(eval_results, c("ModelName", "btt_mape", "btf_mape"))


print(eval_results)
write.csv(eval_results, file = "result/Eval_results_byCountry.csv")