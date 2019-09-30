##
## Chapter 01. Data-Reading 
## 작성자 : Joohyoung Jeon  
## 목  적 : R에서 파일을 읽는 여러가지 방법들에 대해 알아본다. 
##          colClass 변수를 이해하고, 대용량 데이터를 읽을 때 효율적으로 읽는 방법을 생각한다.


# 1. 파일이 위치하는 Base Path 설정
basePath <- paste(getwd(), "01-data-prepare", "Data", sep = "/")

# 2. 단일 데이터 파일 읽기
tv_sales_df <- read.csv(paste(basePath, "tv-sales-forecast.csv", sep = "/"))

# 3. 리스트 파일로 불러와서 읽기
wallmart_data <- list.files(paste(basePath, "wallmart-sales-forecast-datasets", sep= "/"), full.names = T)
print(wallmart_data)


# 4. colClass 변수 선언 및 wallmart dataset reading
#    colClass는 Feature가 많을 때 필요없는 Feature는 버릴 수 있게 해준다. 또한 Feature Type 사전 지정 가능 

# 4.1 features.csv

# Store, Date, Temperature, Fuel_Price, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5,
# CPI, Unemployment, IsHoliday

feature_colClass <- c("character", "character", "numeric", "numeric", "numeric", "numeric", 
                      "numeric", "numeric", "numeric", "numeric", "numeric", "factor")

# 만약 MarkDown2,3,4,5를 읽고 싶지 않다면 다음과 같이 지정 가능
feature_no_MD <- c("character", "character", "numeric", "numeric", "NULL", "NULL", 
                      "NULL", "NULL", "numeric", "numeric", "numeric", "factor")

# All features
wm_features <- read.csv(wallmart_data[grep("features.csv", wallmart_data)], 
                        header = T, sep = ",", colClasses = feature_colClass )

# Select Features (Exclude Markdown2, 3, 4, 5) 
wm_features <- read.csv(wallmart_data[grep("features.csv", wallmart_data)], 
                        header = T, sep = ",", colClasses = feature_no_MD )

# Check after reading 
sum(is.na(wm_features))   #24,040
dim(wm_features)          # nrow : 8190,  ncol : 12


# 4.2 stores.csv
# Store, Type, Size 
wm_stores <- read.csv(wallmart_data[grep("stores.csv", wallmart_data)], 
                              header = T, sep = ",", colClasses = c("character", "factor", "numeric") )

# Check after reading 
sum(is.na(wm_stores))  # is Zero
dim(wm_stores)   # nrow 45, ncol 3
  

# 4.3 train.csv, test.csv
# train.csv : Store, Dept, Date, Weekly_Sales, IsHoliday
# test.csv : Store, Dept, Date, IsHoliday

train_colclass <- c("character", "character", "character", "numeric", "factor")
test_colclass <- c("character", "character", "character", "factor")

wm_train <- read.csv(wallmart_data[grep("train.csv", wallmart_data)], 
                      header = T, sep = ",", colClasses = train_colclass)

wm_test <- read.csv(wallmart_data[grep("test.csv", wallmart_data)], 
                     header = T, sep = ",", colClasses = test_colclass)

sum(is.na(wm_train)) # is Zero
sum(is.na(wm_test))  # is Zero
dim(wm_train)  # nrow 421,570, ncol 5
dim(wm_test)   # nrow 115,064  ncol 4


# 5. 폴더 기반 파일 읽기

# 때로는 DWH, DB를 통해 TXT 파일을 IF받는 경우도 있다. 이럴 경우에는
# 동일한 파일이 여러개 쪼개져서 특정 폴더로 다 떨어진다. 해당 폴더 안에 있는
# 구조는 동일하다고 가정시, 폴더 안에 있는 데이터를 한번에 읽는 코드를 작성한다.


# 6. read.csv기능의 Wrapper 역할을 수행하는 함수 정의  
my_read_csv <- function(x, colClass){
  x <- read.csv(x, sep = ',', header = F, colClasses = colClass)
  return(x)
}


# 7. csv list 폴더 읽기

# 경로지정
ecommerce_data_path <- paste(basePath, "ecommerce-data", sep = "/")

# 해당 경로에 있는 .csv 파일 전체 경로 리스트 
ecommerce_data_files <- list.files(ecommerce_data_path, full.names = T, pattern = '.csv')

# 읽을 파일 형태는 다음과 같다
# InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country
# colclass를 사전에 정의한다. 
# 데이터가 많기 때문에 불필요한 컬럼은 colClass에서 해당 위치를 NULL로 선언해야 한다.
# 여기에서는 Description 컬럼을 읽지 않도록 한다.
ecommerce_colClass <- c("character", "character", "NULL", "numeric", "character", 
                        "numeric", "character", "character")

# 리스트 파일 읽기
ecommerce_data <- do.call(rbind, lapply(ecommerce_data_files, my_read_csv, colClass = ecommerce_colClass))

# 
sum(is.na(ecommerce_data))
dim(ecommerce_data)   # nrow 541,909  ,  ncol 7


####### Summary ######

# 1. 파일을 읽을 때 colClass 변수를 활용하면 불필요한 Column은 읽지 않거나
#    Column 별 형식을 별도로 정할 수 있다. (효율적으로 읽는다)

# 2. do.call(rbind, lapply ~ )를 쓰면 한번에 읽을 수 있다. (dplyr의 bind_rows를 활용해도 된다)
#    eg) ecommerce_data <- bind_rows(frist_data, list(second_data, third_data, ....))

# 참고. grep("features.csv", wallmart_data) 와 같이 정규식표현으로 찾는 것이 있는데
#       100% 매칭시키고 싶으면 grep("\\bfeatures.csv\\b", wallmart_data)
#   eg) table_name <- features.csv
#       wallmart_data[grep(paste0("\\b",table_name,"\\b"), wallmart_data)]


