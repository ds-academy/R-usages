comment = "
1. 구글 트렌드를 이용하여 Seasonal variations이 있는 데이터 찾기 https://www.google.com/trends/?hl=ko
 (1)  다음 방법들을 적용하고 예측력 비교해 보세요 (최소 10시점 이후 예측할 것)
  - Dummy variable, trigonometric models, Additive Holt-Winters, Multiplicative Holt-Winters.
 (2) Additive Holt-Winters와 Multiplicative Holt-Winters 방법의 Weighting parameter (alpha, gamma, delta) 변경하여 예측력 비교해 보세요.

2. 구글 트렌드를 이용하여 Trend가 존재하는 데이터 찾기  https://www.google.com/trends/?hl=ko
 (1) Simple exponential smoothing과 Double exponential smoothing 방법 적용하고 예측력 비교해 보세요 (최소 10시점 예측)
"

install.packages("plotly")

require(stats)
require(lubridate)
require(anytime)
require(forecast)
require(plotly)




# read case
# tab 형태로 분리된 데이터 읽기 
read.table(file_path, header= T, sep = "\t") 

# package 뭐가 불러왔는지 확인
search()


# Question 01

# Dummy Variable
# trigonometric models
# Additive Holt-Winters, Multiplicative Holt-Winters (alpha, gamma, delta)


toeic_data <- read.csv(paste(getwd(), "ts_case2", "toeic_search.csv", sep = "/"),
                       header = T, sep = ",", strip.white = T, colClasses = c("character", "numeric"))


head(toeic_data) # Start Date : 2014-10-05
tail(toeic_data) # End Date : 2019-09-29


plotly::plot_ly( x = ~ anytime::anydate(toeic_data$Weeks),
                 y = ~ toeic_data$Counts, mode = 'lines') %>% 
  plotly::layout(xaxis = list(title = 'Date'), 
                 yaxis = list(title = 'Count of Toeic'))


ts_toeic <- as.ts(toeic_data$Counts)

# index setting for train / valid
split_idx <- ceiling(0.8*dim(toeic_data)[1])
train_idx <- 1:split_idx
valid_idx <- c((split_idx+1):dim(toeic_data)[1])

train_data <- ts(toeic_data$Counts[train_idx], frequency = 52, start = c(2014,10))
valid_data <- ts(toeic_data$Counts[valid_idx], frequency = 52, start= c(2018, 10))


all_data <- ts(toeic_data$Counts, frequency = 52, start = c(2014,10))



m <- stats::HoltWinters(train_data, alpha = 0.5, 
                        beta = 0.5, gamma =  0.5)


hw_predict <- forecast::forecast(m, h=length(valid_idx))
hw_predict$mean
valid_data




alphas <- seq(0.1, 0.9, 0.1)

alphas

gammas <- seq(0.1, 0.9, 0.1)
deltas <- seq(0.1, 0.9, 0.1)


model_erros <- list()
for(alpha in alphas){
  print(alpha)
  for(gamma in gammas){
    print(gamma)
    for(delta in deltas){
      m_name <- paste0("alpha:",alpha," gamma:",gamma," delta:",delta)
      hw_model <- stats::HoltWinters(train_data, alpha = alpha,  
                                     beta = gamma, gamma =  delta, seasonal = "multiplicative")
      hw_predict <- forecast::forecast(hw_model, h=length(valid_idx))
      value_mae <- mean_absolute_error(pred = hw_predict$mean, real = valid_data)
      value_mse <- mean_squared_error(pred = hw_predict$mean, real = valid_data)
      value_mape <- mean_absolute_percentage_error(pred = hw_predict$mean, real = valid_data)
      value_rmse <- root_mean_squared_error(pred = hw_predict$mean, real = valid_data)
      model_erros[[m_name]] <- list("MAE" = value_mae, "MSE" = value_mse, "MAPE" = value_mape, "RMSE" = value_rmse)
      
    }
  }
}


hw_result_table <- do.call('rbind', model_erros)


hw_result_table2 <- do.call('rbind', model_erros)

which.min(hw_result_table)

which.min(hw_result_table2)
head(hw_result_table2, 22)


hw_result_table2[which.min(hw_result_table2),]

head(hw_result_table, 21)

tail(hw_result_table, 10)

# trigonomal data
toeic_data

tri_model1_data <- data.frame(y=log(toeic_data$Counts), transform_trigonometric_data(dim(toeic_data)[1], L = 4, '1'))
tri_model2_data <- data.frame(y=log(toeic_data$Counts), transform_trigonometric_data(dim(toeic_data)[1], L = 4, '2'))

head(tri_model1_data)
head(tri_model2_data)


tri_m1_train <- tri_model1_data[train_idx,]
tri_m1_valid <- tri_model1_data[valid_idx,]

tri_m2_train <- tri_model2_data[train_idx,]
tri_m2_valid <- tri_model2_data[valid_idx,]


trigono_model1 <- lm(formula = y ~ ., data = tri_m1_train)
summary(trigono_model1)

trigono_model2 <- lm(formula = y ~ ., data = tri_m2_train)
summary(trigono_model2)


summary(trigono_model1)
summary(trigono_model2)


tri_m1_valid


trigono_pred1 <- exp(predict(trigono_model1, 
                             newdata = tri_m1_valid[,c("time", "var_1", "var_2")]))

trigono_pred2 <- exp(predict(trigono_model2, 
                             newdata = tri_m2_valid[,c("time", "var_1", "var_2", "var_3", "var_4")]))

trigono_pred1
trigono_pred2



#### required functions ####
transform_trigonometric_data <- function(observations, L, model_type = '1'){
  time <- seq(1, observations)
  
  common_value <- (2*base::pi*time)/L
  var_1 <- sin(common_value)
  var_2 <- cos(common_value)
  
  data = data.frame(time, var_1, var_2)
  
  if(model_type == '2'){
    var_3 <- sin(common_value*2)
    var_4 <- cos(common_value*2)
    data$var_3 <- var_3
    data$var_4 <- var_4
  }
  return(data)
}




require(lubridate)
require(dummy)


# Month Column 생성
bin_toeic_data <- toeic_data
bin_toeic_data$M <-lubridate::month(bin_toeic_data$Weeks, abbr = T)
bin_toeic_data$M <- as.factor(bin_toeic_data$M)
bin_toeic_data$seq_time <- seq(1, dim(bin_toeic_data)[1])
bin_toeic_data <- bin_toeic_data[-1]
str(bin_toeic_data)



# 변수M에 대해서는 One-Hot Encoding으로 변환하고, 나머지 값들은 그대로 담는다.
binary_data_frame <- data.frame(
  time = bin_toeic_data$seq_time, # 1,2,3, .... 246, 247로 구성된 time 변수
  dummy::dummy(bin_toeic_data, p = 'all')[c(1:11)],   # 각 월 데이터를 0, 1 변수로 생성한다. 이후 1 ~ 11까지만 선택한다. 
  y=log(bin_toeic_data$Counts)) # 종속변수(y) 검색횟수 이다.  

head(binary_data_frame, 20)


binary_train_data <- binary_data_frame[train_idx,]
binary_valid_data <- binary_data_frame[valid_idx,]

binary_model <- lm(formula = y ~ ., data = binary_train_data)
summary(binary_model)






hw_additive_model <- stats::HoltWinters(train_data, alpha = 0.1, beta = 0.3, gamma =  0.3, 
                                        seasonal = "additive")

hw_additive_model_predict <- forecast::forecast(hw_additive_model, h=length(valid_idx))


hw_multiple_model <- stats::HoltWinters(train_data, alpha = 0.1, beta = 0.3, gamma =  0.4, 
                                        seasonal = "multiplicative")

hw_multiple_model_predict <- forecast::forecast(hw_multiple_model, h=length(valid_idx))

trigono_pred1 <- exp(predict(trigono_model1, 
                             newdata = tri_m1_valid[,c("time", "var_1", "var_2")]))

trigono_pred2 <- exp(predict(trigono_model2, 
                             newdata = tri_m2_valid[,c("time", "var_1", "var_2", "var_3", "var_4")]))

binary_pred <- exp(predict(binary_model, 
                           newdata =binary_valid_data[,c(1:12)]))

trigono_pred1

# real
toeic_data$Counts[valid_idx]

time(hw_additive_model_predict)

hw_additive_model_predict$x

time(toeic_data$Weeks)

time(hw_additive_model_predict$mean)

time(toeic_data$Weeks)

valid

toeic_data[train_idx,]
toeic_data[valid_idx,]

plot_ly() %>% 
  add_lines(x = time(all_data), y = toeic_data$Counts, 
            color = I("black"), name = "Real") %>%
  add_lines(x = time(hw_additive_model_predict$mean),
            y = hw_additive_model_predict$mean,
            color = I("blue"), name = "HW_additive") %>%
  add_lines(x = time(hw_multiple_model_predict$mean),
            y = hw_multiple_model_predict$mean,
            color = I("red"), name = "HW_multiple")



plotly::plot_ly( x = ~ anytime::anydate(toeic_data$Weeks),
                 y = ~ toeic_data$Counts, mode = 'lines') %>% 
  plotly::layout(xaxis = list(title = 'Date'), 
                 yaxis = list(title = 'Count of Toeic'))





# 각 예측결과와 실제값을 저장하는 Data Frame를 새롭게 생성한다.
ts_result_table <- data.frame(real = toeic_data$Counts[valid_idx], 
                              hw_additive = hw_additive_model_predict$mean,
                              hw_multiple = hw_multiple_model_predict$mean,
                              tri_pred1 = trigono_pred1, 
                              tri_pred2 = trigono_pred2,
                              bin_pred = binary_pred)

head(ts_result_table)


ts_result_table %>% dplyr::summarise(
  MAE_hw_additive = mean_absolute_error(hw_additive, real),
  MAE_hw_multiple = mean_absolute_error(hw_multiple, real),
  MAE_tri1 = mean_absolute_error(tri_pred1, real),
  MAE_tri2 = mean_absolute_error(tri_pred2, real),
  MAE_bin  = mean_absolute_error(bin_pred, real),
  MSE_hw_additive = mean_squared_error(hw_additive, real),
  MSE_hw_multiple = mean_squared_error(hw_multiple, real),
  MSE_tri1 = mean_squared_error(tri_pred1, real),
  MSE_tri2 = mean_squared_error(tri_pred2, real),
  MSE_bin  = mean_squared_error(bin_pred, real),
  RMSE_hw_additive = root_mean_squared_error(hw_additive, real),
  RMSE_hw_multiple = root_mean_squared_error(hw_multiple, real),
  RMSE_tri1 = root_mean_squared_error(tri_pred1, real),
  RMSE_tri2 = root_mean_squared_error(tri_pred2, real),
  RMSE_bin  = root_mean_squared_error(bin_pred, real),
  MAPE_hw_additive = mean_absolute_percentage_error(hw_additive, real),
  MAPE_hw_multiple = mean_absolute_percentage_error(hw_multiple, real),
  MAPE_tri1 = mean_absolute_percentage_error(tri_pred1, real),
  MAPE_tri2 = mean_absolute_percentage_error(tri_pred2, real),
  MAPE_bin  = mean_absolute_percentage_error(bin_pred, real)) %>% select(-real, -hw_additive, -hw_multiple, -tri_pred1, -tri_pred2, -bin_pred) %>% distinct()


mean_absolute_percentage_error(real = toeic_data$Counts[valid_idx],
                               pred = hw_predict$mean)
mean_absolute_percentage_error(real = toeic_data$Counts[valid_idx],
                               pred = binary_pred)
mean_absolute_percentage_error(real = toeic_data$Counts[valid_idx],
                               pred = trigono_pred2)
mean_absolute_percentage_error(real = toeic_data$Counts[valid_idx],
                               pred = trigono_pred1)



# Error functions
mean_absolute_error <- function(pred, real){
  e = abs(pred - real)
  return(mean(e))
}

mean_squared_error <- function(pred, real){
  e = pred - real
  return(mean(e^2))
}

root_mean_squared_error <- function(pred, real){
  e <- (real - pred)
  return(sqrt(mean(e^2)))
}

mean_absolute_percentage_error <- function(pred, real){
  nonZeroIdx <- which(real != 0, arr.ind = T)
  return(mean(abs((real[nonZeroIdx]-pred[nonZeroIdx])/real[nonZeroIdx]))*100)
}




########################## task 3

search_dl_data <- read.csv(paste(getwd(), "TimeSeriesAnalysisCase2", "deeplearning_search.csv", sep = "/"),
                           header = T, sep = ",", strip.white = T, colClasses = c("character", "numeric"))

dim(search_dl_data)

plotly::plot_ly( x = ~ anytime::anydate(search_dl_data$Weeks),
                 y = ~ search_dl_data$Counts, mode = 'lines') %>% 
  plotly::layout(xaxis = list(title = 'Date'), 
                 yaxis = list(title = 'Count of Deep Learning'))


# index setting for train / valid
split_idx <- ceiling(0.8*dim(search_dl_data)[1])
train_idx <- 1:split_idx
valid_idx <- c((split_idx+1):dim(search_dl_data)[1])


## Manually Calculation Simple Exponential Smoothing
# given alpha
es_alpha <- 0.3
# D_i
data_points <- search_dl_data$Counts[train_idx]
# all Level (L_1 , L_2, ... L_t)
Ls <- list()
for(i in 1:length(data_points)){
  if(i == 1){
    Ls[[i]] = es_alpha * data_points[i] + (1-es_alpha) * mean(data_points)
  }else{
    Ls[[i]] = es_alpha * data_points[i] + (1-es_alpha) * Ls[[i-1]]
  }
}

Ls_table <- do.call('rbind', Ls)
Ls_table

## Using ststs::HoltWinters for Simple Exponential Smoothing
search_dl_data

train_data <- ts(search_dl_data$Counts[train_idx], frequency = 52, start = c(2014, 10))
valid_data <- ts(search_dl_data$Counts[valid_idx], frequency = 52, start= c(2018, 10))

ses_model <- stats::HoltWinters(train_data, alpha = 0.3, beta = F, gamma =  F)
ses_model



ses_predict <- forecast::forecast(ses_model, h=length(valid_idx))

ses_predict$mean
length(ses_predict$mean)
length(valid_idx)


length(valid_data)


toeic_data[train_idx, ]

# Simple Exponential Smoothing
sem_errors = list()
for(alpha in alphas){
  m_name <- paste0("alpha:",alpha)
  sem_model <- stats::HoltWinters(train_data, alpha = alpha,  
                                  beta = F, gamma =  F)
  sem_predict <- forecast::forecast(sem_model, h=length(valid_idx))
  value_mae <- mean_absolute_error(pred = sem_predict$mean, real = valid_data)
  value_mse <- mean_squared_error(pred = sem_predict$mean, real = valid_data)
  value_mape <- mean_absolute_percentage_error(pred = sem_predict$mean, real = valid_data)
  value_rmse <- root_mean_squared_error(pred = sem_predict$mean, real = valid_data)
  sem_errors[[m_name]] <- list("MAE" = value_mae, "MSE" = value_mse, "MAPE" = value_mape, "RMSE" = value_rmse)
}
sem_result_table <- do.call('rbind', sem_errors)

sem_result_table

# Double Exponential Smoothing
dem_model <- stats::HoltWinters(train_data, alpha = alpha,  
                                beta = beta, gamma =  F)


dem_errors = list()
for(alpha in alphas){
  for(gamma in gammas){
    m_name <- paste0("alpha:",alpha, " gamma:",gamma)
    dem_model <- stats::HoltWinters(train_data, alpha = alpha, beta = gamma, gamma =  F)
    dem_predict <- forecast::forecast(dem_model, h=length(valid_idx))
    value_mae <- mean_absolute_error(pred = dem_predict$mean, real = valid_data)
    value_mse <- mean_squared_error(pred = dem_predict$mean, real = valid_data)
    value_mape <- mean_absolute_percentage_error(pred = dem_predict$mean, real = valid_data)
    value_rmse <- root_mean_squared_error(pred = dem_predict$mean, real = valid_data)
    dem_errors[[m_name]] <- list("MAE" = value_mae, "MSE" = value_mse, "MAPE" = value_mape, "RMSE" = value_rmse)
    
  }
}

dem_result_table <- do.call('rbind', dem_errors)
dem_result_table


which.min(dem_result_table)


head(dem_result_table, 53)


# 

sem_model <- stats::HoltWinters(train_data, alpha = 0.9,  
                                beta = F, gamma =  F)

dem_model <- stats::HoltWinters(train_data, alpha = 0.6,  
                                beta = 0.8, gamma =  F)


sem_model_predict <- forecast::forecast(sem_model, h=length(valid_idx))


dem_model_predict <- forecast::forecast(dem_model, h=length(valid_idx))


dem_model_predict$mean

all_data

plot_ly() %>% 
  add_lines(x = time(all_data), y = search_dl_data$Counts, 
            color = I("black"), name = "Real") %>%
  #  add_lines(x = time(sem_model_predict$mean),
  #            y = sem_model_predict$mean,
  #            color = I("blue"), name = "Simple_Exponential_Smoothing") %>%
  add_lines(x = time(dem_model_predict$mean),
            y = dem_model_predict$mean,
            color = I("red"), name = "Double_Exponential_Smoothing")




