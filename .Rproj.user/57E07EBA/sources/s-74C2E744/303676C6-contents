## Chapter 02. Data-Type-Change
## 작성자 : Joohyoung Jeon  
## 목  적 : R에서 주로 다루는 변수 유형을 소개하며, 각 유형별로 변환하는 방법과 변환시 주의사항에 대해 다룬다.

## 0. Introduction ##

# 데이터를 리딩 했다면, 다음에는 데이터를 분석 유형에 맞게 형변환 하는 것이다.
# R에서 주로 사용하는 Data Type으로는 
# Date(날짜), factor(범주형), character(문자형), numeric(수치형) 이 있다.


## 1. factor Variable ##


# 범주형 변수는 정수, 혹은 문자로 표현되며 그 크기에 의미가 없고 단순 데이터 분류를 위해 사용된다.
# 상품코드, 점포코드, 점포명, 대분류, 요일, 팀명, 부서명, 그룹, 고객유형 등이 있다
# PC에서 읽을 경우 정수, 문자열로 읽히지만 실제로 분석시에는 정수, 문자열로 취급하면 안된다.
# 데이터를 보고 해당 변수가 범주형 변수일 경우에는 factor 변수로 변환해서 분석한다. 
# 또한, factor 변수는 Machine Learning/Deep Learning 분석 과정에서 One-Hot encoding 된다.

#@@ One-Hot Encoding Examples
# it x = c("사과", "딸기", "배")
# encodex_x = [1,0,0]
#             [0,1,0]
#             [0,0,1]
# N is the element of list x -> after one-hot ->  N by N square matrix (1/N * 100 = sparsity)

###### 가. OneHot Encoding 변경하는 방법 1 ######

# dummy package 사용하는 방법, https://cran.r-project.org/web/packages/dummy/dummy.pdf
library(dummy)

# categories함수를 통해 입력변수 중 factor 변수명, 변수 수준 리스트를 가져온다.
# 변수 유형 중 Character, Factor 변수들을 가져온다.

#@@ 참고 : Date 변수의 경우 Factor 변수로 설정되면 안되므로, 
##         Date 변수를 Character -> Date 유형으로 바꿔준다.
wm_features$Date <- as.Date(wm_features$Date)

# 위의 방법을 쓰거나, 떄로는 아래 방법이 더 낫기도 하다. 
library(lubridate); wm_features$Date <- lubridate::as_date(wm_features$Date)

# categories 함수를 이용해서 factor 변수 정보를 담는다.
factor_info <- dummy::categories(wm_features, p = "all")

# One-Hot Encoded된 변수를 기존 데이터에 적용하여, ML/DL에 맞는 입력데이터로 변경한다.
one_hot_wm_features <- dummy::dummy(wm_features, p = "all", object = factor_info)

# 위의 one_hot_wm_features에는 factor 변수의 One-Hot Values만 있으므로, 나머지 변수들에 대해서도
# 다음과 같이 가져온다.
numeric_features <- names(wm_features[, -which(names(wm_features) %in% names(factor_info))])

# one_hot_wm_features + numeric_features를 합쳐서 최종 ML/DL에 맞는 train 데이터로 변경 
one_hot_wm_features_final <- cbind(one_hot_wm_features,  wm_features[,numeric_features])
dim(one_hot_wm_features_final)


###### 나. OneHot Encoding 변경하는 방법 2 ######

# fastDummies 사용하는 방법 https://cran.r-project.org/web/packages/fastDummies/fastDummies.pdf
install.packages("fastDummies")
library(fastDummies)

# 기존 feature + One-Hot Encoding features
wm_one_hot_feautres <- fastDummies::dummy_cols(wm_features)

# 만약, 특정 변수만 선택적으로 변경하고 싶다면?
fastDummies::dummy_cols(wm_features, select_columns = "IsHoliday")

# wm_train_data내에 기존 factor 변수로 활용된 것 제외 
wm_one_hot_feautres <- wm_one_hot_feautres[,names(wm_one_hot_feautres)[-which(sapply(wm_one_hot_feautres, is.factor))]]


###### 다. OneHot Encoding 변경하는 방법 3 ######
# 외부라이브러리를 사용하지 않고 변경하는 방법

# wm_features에서 factor 변수들에 대해, 각 변수명과 수준을 리스트로 가져온다.
factor_levels <- lapply(wm_features[,sapply(wm_features, is.factor), drop = FALSE], function(x) {levels(x)})

##@@ 동작방법
# 1. sapply(sample, is.factor) -> 변수들 중에 어떤 변수가 factor 인지 확인한다. 
# 2. wm_features[,sapply(sample, is.factor), drop = FALSE] -> 위에서 true 로 나온(factor) 변수에 대해서
#    해당 컬럼 전체를 가져온다. 
# 3. lapply(wm_features[,sapply(sample, is.factor), drop = FALSE], function(x) {levels(x)})
#    해당 변수들의 levels(요인 수준)들을 전부 가져옴. 

# factor_levels에는 보유한 데이터 중 factor 변수 명, 변수별 수준을 담고 있는 리스트이다. 
factor_levels

factor_levels$Store
factor_levels$IsHoliday

# 데이터 수준들을 발라냈으면, One-Hot Encoded 시킨 테이블로 변환시킨다.
encoded_wm_features <- model.matrix(~. -Date, wm_features, xlev = factor_levels)

#@@ model.matrix 함수는 formula 인자를 받는다. 
# formula 인자는 문자열 문법으로 독립변수들간의 관계, 변수 추가, 제거, 곱, 나누기 등을 할 수 있도록 한다.
# 위에서는 ~. <- 이라고 표현했기 때문에 모든 독립변수가 활용된다.
# 만약, 특정 변수를 포함하거나 제한하고 싶다면 +추가변수명, -제거변수명 과 같이 기호와 이름으로 표현 가능하다.
# 참조 : https://datascienceschool.net/view-notebook/34a54aad27a94121a3ee0b81f3c8380b/

# 위의 Model.matrix는 기본적으로 변수유형이 character 이거나 factor 이면 자동으로 One-Hot 인코더로 치환한다.
# Date 변수의 경우 Character 이기 때문에 제거한다. 


## 2. Character Variable ##

# 문자열 변수. Python의 str, String 등 문자열 변수이다. 
# 문자열 변수의 경우 데이터를 읽을 때 encoding이 중요하다. 
# 문자열 변수의 경우 데이터 분석에 바로 활용되진 않고, 
# character -> factor -> one-hot 으로 변환된다.

# factor 변수의 levels 가 다를 경우 제대로 붙지 않는 문제가 있다.
str(wm_stores)   # Store -> Char "1", "2", .... 
str(wm_features) # Store -> Factor w/ 45 levels

# 위의 두 변수를 Store 기준으로 left_join 하기 위해서는 wm_features의 Store 변수를 Character 변수로
# 변환 해주어야 한다.

wm_features$Store <- as.character.factor(wm_features$Store)

library(dplyr)
wm_features_store <- left_join(wm_features, wm_stores, by = c("Store" = "Store"))

sum(is.na(wm_features_store$Type))
sum(is.na(wm_features_store$Size))


## 3. Numeric Variable 수치형 변수 ##

# 수치형 변수는 숫자의 크기 차이가 있는 변수이다. 온도, 기온, 키, 판매량, 주식가격 등 
# 독립변수, 종속변수 내 수치형 변수가 있고 해당 변수의 크기가 서로 다를 경우에는 정규화가 필요하다.

# 표준정규분포로 정규화 :  평균이 0, 표준편차가 1 의 분포를 가지는 값으로 변경함
# 평균 0을 기준으로 -1, +1 의 범위로 숫자가 생기기 때문에 마이너스가 포함된다. 
wm_features$Fuel_Price <- scale(wm_features$Fuel_Price)

# Min-Max 정규화 : 최대값이 1, 최소값이 0으로 변환되는 정규화
# 0 과 1사이의 값을 가지기 때문에 음수가 없다. 
scales::rescale(wm_features$Fuel_Price, to=c(0,1))


#@@ 정규화를 쓰는 경우
# 정규화의 경우 loss function의 최적화시 한쪽 가중치가 비대칭적으로 부여되어, gradient 크기의 왜곡됨을
# 방지하여, 최적해로의 수렴이 잘 되도록 돕는다.



## 4. Tutorial ##

# 변수 유형 확인 
str(wm_features)

# 변수가 많을 경우 str이 짤려서 출력된다. 이를 방지하기 위해서는 다음과 같이
# list.len을 선언하면 된다. 
str(wm_features, list.len = ncol(wm_features))

# 데이터를 눈으로 보면서 확인한다.
# 데이터 최초 10개만 확인
head(wm_features, 10)

# 뒤에서 부터
tail(wm_features, 10)

# 데이터 통계값 확인 
summary(wm_features)

# 결측치가 존재하는지 확인
sum(is.na(wm_features))

 






# 특정 컬럼만 형변환
wm_features$IsHoliday <- as.character(wm_features$IsHoliday)

# 특정 컬럼만 일괄 형변환
# 1~3번, 5번 컬럼만 변경함.
wm_features[,names(wm_features)[c(1:3,5)]] <- lappy(wm_features[, names(wm_features)[c(1:3,5)]], as.character)


