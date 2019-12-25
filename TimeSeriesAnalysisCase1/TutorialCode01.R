comment = "
주제 : 시계열 데이터 중 Seasonal Variational을 갖는 데이터를 찾고 해당 데이터에 대한 기초 통계 분석, 
       Trigonometric Modeling, Binary Modeling을 수행하고 각 모델에 대한 예측력을 평가한다

(1) 기초통계량 분석
(2) Trigonometric 모델링
(3) Binary 변수 모델링
(4) MAE, MSE, MAPE 등 이용하여 모델 평가

 author : jeonjoohyoung@gmail.com
"

# 분석에 필요한 패키지를 설치한다. 
install.packages(c("dplyr","plotly", "anytime", "lubridate", "dummy", "lmtest"))
# 1. dplyr : 데이터 집계를 위한 패키지
# 2. plotly : 시각화 툴
# 3. anytime :Character 형태의 Date 변수를 Date 유형의 변수로 전환해주는 패키지. 일자 관련 처리할 수 있는 다양한 패키지가 존재한다.  
# 4. lubridate : Date변수에서 월, 주차, 년주차 등 일자정보를 추출하는 패키지
# 5, dummy : One-Hot Encoding 변수를 만들어 주는 패키지
# 6. lmtest : Durbin-Watson 검증을 위한 패키지

# 패키지 로드
require(dplyr)
require(plotly)
require(anytime)
require(lubridate)
require(dummy)
require(lmtest)

# Set working environment
setwd(paste(getwd(), 'TimeSeriesAnalysisCase1', sep = '/'))

# read dataset
data <- read.csv('search_rhinitis.csv', 
                 header = T, sep = ',', strip.white = T, colClasses = c('character', 'numeric'))

head(data, 10)


# plotting
plotly::plot_ly( x = ~ anytime::anydate(data$time),
                 y = ~data$x, mode = 'lines') %>% 
  plotly::layout(xaxis = list(title = 'Date'), 
                 yaxis = list(title = 'Count of Rhinitis'))

# Basic Statistics
summary(data)
dim(data) # Num of Observations 247, Feature 2 (Time, X)

mean(data$x) #43.48988
var(data$x) #217.0964
sd(data$x) #14.73419

# Monthly Statistics
data$time <- anytime::anydate(data$time)
data %>% dplyr::group_by(Month = lubridate::floor_date(time, "1 month")) %>% 
  dplyr::summarise(mean_x = mean(x), sd_x = sd(x),
                   q1_x = quantile(x, 0.25), 
                   median_x = median(x),
                   q3_x = quantile(x, 0.75)) %>% print(n=100)


# Durbin-Watson Test
data$seq_time <- seq(1, dim(data)[1])
data$seq_time
lmtest::dwtest(lm(formula = x ~ seq_time, data = data), alternative = 'two.sided')


############## Autocorrelation check by manually #############
# x와 lag 변수를 저장할 list 변수 선언
corr_list = list()
for(i in 1:dim(data)[1]){
  # 데이터 길이 전체를 순회하면서, x와 i-th log of x 의 correlation을 계산하여 저장
  corr_list[[i]] = stats::cor(data$x, dplyr::lag(data$x, i, 0))
}

# 하나의 행 데이터로 만듦
corr_mat = do.call('rbind', corr_list)
# 맨 마지막 N of lag은 모든 값이 NA 이므로 제거
corr_mat = corr_mat[complete.cases(corr_mat)]
# 자기상관성 계수가 가장 큰 값은 1번째 lag 변수라고 나옴.
which.max(corr_mat)
##############################################################


# function of Transformation Trigonometric dataset
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



trigonometic_data <- data.frame(y=log(data$x), transform_trigonometric_data(dim(data)[1], L = 12, '2'))
head(trigonometic_data)




# data split
# 1 to 222 (90% of Obersvations) -> train
# 223 to remainer (10% of Observations) -> valid
train_idx <- seq(1, round(dim(trigonometic_data)[1]*0.9))
valid_idx <- seq(round(dim(trigonometic_data)[1]*0.9)+1, dim(trigonometic_data)[1])

train_trigono <- trigonometic_data[train_idx,]
valid_trigono2 <- trigonometic_data[valid_idx,]
valid_trigono1

trigono_model1 <- lm(formula = y ~ ., data = train_trigono)
trigono_model2 <- lm(formula = y ~ ., data = train_trigono)


summary(trigono_model1)
summary(trigono_model2)





require(lubridate)
require(dummy)

head(data)

# Month Column 생성
data$M <-lubridate::month(data$time, abbr = T)

head(data)


# 요인 변수로 전환
data$M <- as.factor(data$M)

# 변수 선택 x and Month
m_binary_data <- data[,c("x","seq_time", "M")]

v <- 1:3

names(v) <- c("a", "b", "c")
v[4] <- 4

names(v[4])

y <- 1:4
attr(y, "new_arrtibute") <- "Here's a vector"
attr(y, "new_arrtibute")
y <- 1:5
attr(y, "new_arrtibute")

mtrx

x <- c(12L, 6L, 10L, 8L, 15L)
typeof(median(x))


late_delivery <- data$x >= 30
index(late_delivery)

indat <- c("Ash Rd", "Ash Ave", "Ash St")
grep("[Rd|Ave|Dr|St]", indat)

v1 <- c(1,2,3)
v2 <- list(4, 5, 6)
cbind(v1, v2)

v1 <- c(1,2,3)
v2 <-c(4, 5, 6)
cbind(v1, v2)

v1 <- list(1,2,3)
v2 <-list(c(4, 5, 6))
cbind(v1, v2)

ls(pat="data")



mtrx <- matrix(c(3,5,8,4), nrow=2, ncol=2, byrow = T)
mtrx * mtrx


head(m_binary_data)

# 변수M에 대해서는 One-Hot Encoding으로 변환하고, 나머지 값들은 그대로 담는다.
binary_data_frame <- data.frame(
  time = m_binary_data$seq_time, # 1,2,3, .... 246, 247로 구성된 time 변수
  dummy::dummy(m_binary_data, p = 'all')[c(1:11)],   # 각 월 데이터를 0, 1 변수로 생성한다. 이후 1 ~ 11까지만 선택한다. 
  y=log(m_binary_data$x)) # 종속변수(y) 검색횟수 이다.  

head(binary_data_frame, 20)


binary_train_data <- binary_data_frame[train_idx,]
binary_valid_data <- binary_data_frame[valid_idx,]

binary_model <- lm(formula = y ~ ., data = binary_train_data)
summary(binary_model)

# loss functions

#
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




trigono_pred1 <- exp(predict(trigono_model1, 
                             newdata = valid_trigono1[,c("time", "var_1", "var_2")]))
trigono_pred2 <- exp(predict(trigono_model2, 
                             newdata = valid_trigono2[,c("time", "var_1", "var_2", "var_3", "var_4")]))

binary_pred <- exp(predict(binary_model, 
                           newdata =binary_valid_data[,c(1:12)]))




ts_result_table <- data.frame(real = data$x[valid_idx], 
                              tri_pred1 = trigono_pred1, 
                              tri_pred2 = trigono_pred2,
                              bin_pred = binary_pred)

ts_result_table %>% dplyr::summarise(
  MAE_tri1 = mean_absolute_error(tri_pred1, real),
  MAE_tri2 = mean_absolute_error(tri_pred2, real),
  MAE_bin  = mean_absolute_error(bin_pred, real),
  MSE_tri1 = mean_squared_error(tri_pred1, real),
  MSE_tri2 = mean_squared_error(tri_pred2, real),
  MSE_bin  = mean_squared_error(bin_pred, real),
  RMSE_tri1 = root_mean_squared_error(tri_pred1, real),
  RMSE_tri2 = root_mean_squared_error(tri_pred2, real),
  RMSE_bin  = root_mean_squared_error(bin_pred, real))