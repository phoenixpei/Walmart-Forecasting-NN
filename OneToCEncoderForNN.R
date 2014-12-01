# Title: OneToCEncoderForNN.R
# Author: Ssurey Moon
#
# Reference: 
#       [1] https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting
#       [2] How To Standardize Data for Neural Networks: http://visualstudiomagazine.com/articles/2014/01/01/how-to-standardize-data-for-neural-networks.aspx
#
# Preprocessing before applying Neural network model and Support Vector machines model - Update(2014/12/01)
#  1. One to C Dummy Encoding on Fatoral data, Since Neural network only accepts numeric data
#     Support vector machines also only accepts numeric data
#  2. Min-max normalization on features to help the neural models and SVM models to converse well, and to avoid the model from being divergent
#
# Input file : stores.csv.csv
#              train.csv
#              test.csv
#              features_complete.csv <- output file of MissingDataImputation.R
#
# output file : Under the directory './inputfiles/'
#               Training set file: train_data_with_dept1_for_python.csv
#                                  .....................
#                                  train_data_with_dept99_for_python.csv
#               
#               Test set file: test_data_with_dept1_for_python.csv
#                              .....................
#                              test_data_with_dept99_for_python.csv


#rm(list=ls())
setwd("~/Documents/workspace/Walmart-Forecasting-NN")

# 1 to C encoding for python Neural network library
stores = read.csv("stores.csv", header=TRUE)
train = read.csv("train.csv", header=TRUE)
test = read.csv("test.csv", header = TRUE)
features = read.csv("features_complete.csv", header = TRUE)
#sample = read.csv("sampleSubmission.csv", header=TRUE)

features$Date <- as.Date(features$Date)
train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)
train$Weekly_Sales[train$Weekly_Sales<0]
library(dplyr)
library(lubridate)
features_cor <- merge(features, stores, by="Store", all.x=T)


train_with_features <- merge(x = train, y = features_cor, by=c("Store","Date"), all.x=T)
sum(is.na(train_with_features))
# [1] 0   No missing data
train_with_features_sorted <- train_with_features[order(train_with_features$Store, train_with_features$Dept),]

colnames(train_with_features_sorted)
# [1] "Store"        "Date"         "Dept"         "Weekly_Sales" "IsHoliday.x" 
# [6] "Temperature"  "Fuel_Price"   "MarkDown1"    "MarkDown2"    "MarkDown3"   
# [11] "MarkDown4"    "MarkDown5"    "CPI"          "Unemployment" "IsHoliday.y" 
# [16] "Type"         "Size"

train_with_features_sorted$IsHoliday.y <- NULL # remove duplicates

# One To Two Encoding of the factor data Is.Holiday
# True => 1, False => -1 
train_with_features_sorted$IsHoliday.x <- ifelse(train_with_features_sorted$IsHoliday.x, 1, -1)

# One To 45 Encoding of the factor data Store

# Encoded Type data
# Type A
# 0.99  0.01  0.01
# Type B
# 0.01  0.99  0.01
# Type C
# 0.01  0.01  0.99
train_with_features_Type <- data.frame(model.matrix(~factor(train_with_features_sorted$Type)-1))
train_with_features_Type[train_with_features_Type == 1] <- 0.99
train_with_features_Type[train_with_features_Type == 0] <- 0.01


# Encoded Store 1 data
# 0.99  0.01  .......  0.01
# Encoded Store 2 data
# 0.01  0.99  .......  0.01
#..........
# Encded Store 45 data
# 0.01  0.01  .......  0.99
train_with_features_Store <- data.frame(model.matrix(~factor(train_with_features_sorted$Store)-1))
train_with_features_Store[train_with_features_Store == 1] <- 0.99
train_with_features_Store[train_with_features_Store == 0] <- 0.01


# Encoded Deaprtment 1 data
# 0.99  0.01  .......  0.01
# Encoded Deaprtment 2 data
# 0.01  0.99  .......  0.01
#..........
# Encoded Deaprtment 99 data
# 0.01  0.01  .......  0.99
#
# Enven if a store does not have department 99 data, this code always generates 99 columns
train_with_features_Dept<-NULL
for(i in 1:99){
  train_with_features_Dept <- data.frame(cbind(train_with_features_Dept, ifelse(train_with_features_sorted$Dept==i, 0.99, 0.01)))
}


train_cat_Type_Store_Dept <- cbind(train_with_features_Type, train_with_features_Store, train_with_features_Dept)

######## Train Data is done.
##################################################################

test_with_features <- merge(x = test, y = features_cor, by=c("Store","Date"), all.x=T)

sum(is.na(test_with_features))
test_with_features_sorted <- test_with_features[order(test_with_features$Store, test_with_features$Dept),]
colnames(test_with_features_sorted)
# [1] "Store"        "Date"         "Dept"         "IsHoliday.x"  "Temperature" 
# [6] "Fuel_Price"   "MarkDown1"    "MarkDown2"    "MarkDown3"    "MarkDown4"   
# [11] "MarkDown5"    "CPI"          "Unemployment" "IsHoliday.y"  "Type"        
# [16] "Size"

test_with_features_sorted$IsHoliday.y <- NULL
test_with_features_sorted$IsHoliday.x <- ifelse(test_with_features_sorted$IsHoliday.x, 1, -1)

test_with_features_Type <- data.frame(model.matrix(~factor(test_with_features_sorted$Type)-1))
test_with_features_Type[test_with_features_Type == 1] <- 0.99
test_with_features_Type[test_with_features_Type == 0] <- 0.01

test_with_features_Store <- data.frame(model.matrix(~factor(test_with_features_sorted$Store)-1))
test_with_features_Store[test_with_features_Store == 1] <- 0.99
test_with_features_Store[test_with_features_Store == 0] <- 0.01

test_with_features_Dept<-NULL
for(i in 1:99){
  test_with_features_Dept <- data.frame(cbind(test_with_features_Dept ,ifelse(test_with_features_sorted$Dept==i, 0.99, 0.01)))
}

test_cat_Type_Store_Dept <- cbind(test_with_features_Type, test_with_features_Store, test_with_features_Dept)

######## Test Data is done.
##################################################################

train_with_features_sorted <- train_with_features_sorted[,c(1:3,5:16,4)] # arrange clomuns

#####Check before we go futher#####
colnames(train_with_features_sorted)
# [1] "Store"        "Date"         "Dept"         "IsHoliday.x"  "Temperature" 
# [6] "Fuel_Price"   "MarkDown1"    "MarkDown2"    "MarkDown3"    "MarkDown4"   
# [11] "MarkDown5"    "CPI"          "Unemployment" "Type"         "Size"        
# [16] "Weekly_Sales"
colnames(test_with_features_sorted)
# [1] "Store"        "Date"         "Dept"         "IsHoliday.x"  "Temperature" 
# [6] "Fuel_Price"   "MarkDown1"    "MarkDown2"    "MarkDown3"    "MarkDown4"   
# [11] "MarkDown5"    "CPI"          "Unemployment" "Type"         "Size"
#
#Columns should be the same except for the target column Weekly_Sales


################# Change Date data into fatoral data.
train_with_features_sorted$Year <- year(train_with_features_sorted$Date)-2009
test_with_features_sorted$Year <- year(test_with_features_sorted$Date)-2009

train_with_features_sorted$Week <- week(train_with_features_sorted$Date)
test_with_features_sorted$Week <- week(test_with_features_sorted$Date)
colnames(train_with_features_sorted)

# [1] "Store"        "Date"         "Dept"         "IsHoliday.x"  "Temperature" 
# [6] "Fuel_Price"   "MarkDown1"    "MarkDown2"    "MarkDown3"    "MarkDown4"   
# [11] "MarkDown5"    "CPI"          "Unemployment" "Type"         "Size"        
# [16] "Weekly_Sales" "Year"         "Week"   

train_with_features_sorted$Year <- year(train_with_features_sorted$Date)-2009
test_with_features_sorted$Year <- year(test_with_features_sorted$Date)-2009

nrow(test_with_features_sorted[test_with_features_sorted$Store==1,])
# [1] 2783
nrow(test[test$Store==1,])
# [1] 2783


Super_Bowl = as.Date(c("12-02-10", "11-02-11", "10-02-12", "08-02-13"), "%d-%m-%y")
Labor_Day = as.Date(c("10-09-10", "09-09-11", "07-09-12", "06-09-13"), "%d-%m-%y")
Thanksgiving = as.Date(c("26-11-10", "25-11-11", "23-11-12", "29-11-13"), "%d-%m-%y")
Christmas = as.Date(c("31-12-10", "30-12-11", "28-12-12", "27-12-13"), "%d-%m-%y")

week(Super_Bowl)
# [1] 7 7 6 6
week(Labor_Day)
# [1] 37 37 36 36
week(Thanksgiving)
# [1] 48 48 47 48
week(Christmas)
# [1] 53 53 52 52

##To get a better result we try to shift some week numbers.
train_with_features_sorted$Week <- with(train_with_features_sorted, ifelse(Year<3, week(Date), week(Date)+1))
test_with_features_sorted$Week <- with(test_with_features_sorted, ifelse(Year==3, week(Date)+1, ifelse(week(Date)>40 & week(Date)<51, week(Date), week(Date)+1)))

#Should we consider Easter days?
Easter = as.Date(c("04-04-10", "24-04-11", "28-12-12", "27-12-13"), "%d-%m-%y")

train_with_features_sorted <- train_with_features_sorted[,c(1:15,17:18,16)]
colnames(train_with_features_sorted)
# Output is supposed to look like below:
# [1] "Store"        "Date"         "Dept"         "IsHoliday.x"  "Temperature" 
# [6] "Fuel_Price"   "MarkDown1"    "MarkDown2"    "MarkDown3"    "MarkDown4"   
# [11] "MarkDown5"    "CPI"          "Unemployment" "Type"         "Size"        
# [16] "Year"         "Week"         "Weekly_Sales"

train_with_features_sorted$Date <- NULL
test_with_features_sorted$Date <- NULL

colnames(train_with_features_sorted)
# [1] "Store"        "Dept"         "IsHoliday.x"  "Temperature"  "Fuel_Price"  
# [6] "MarkDown1"    "MarkDown2"    "MarkDown3"    "MarkDown4"    "MarkDown5"   
# [11] "CPI"          "Unemployment" "Type"         "Size"         "Year"        
# [16] "Week"         "Weekly_Sales"
colnames(test_with_features_sorted)
# [1] "Store"        "Dept"         "IsHoliday.x"  "Temperature"  "Fuel_Price"  
# [6] "MarkDown1"    "MarkDown2"    "MarkDown3"    "MarkDown4"    "MarkDown5"   
# [11] "CPI"          "Unemployment" "Type"         "Size"         "Year"        
# [16] "Week" 

y_test <- rep(0, nrow(test_with_features_sorted))
y_train <- train_with_features_sorted$Weekly_Sales

x_train = train_with_features_sorted[,-17]
x_test = test_with_features_sorted


x_train$Type <- NULL
x_test$Type <-NULL
#Data for Python module
x_train4py<-x_train
x_test4py<-x_test

man_scale <- function(col){
  return((col-mean(col))/sd(col))
}
get_mean_sd <- function(col){
  out <- c(mean(col), sd(col))
  return(out)
}
x_train_scaled <- data.frame(apply(x_train,2,man_scale))
x_train_param <- data.frame(apply(x_train,2,get_mean_sd))

nrow(test_with_features_sorted[test_with_features_sorted$Store==1,])
nrow(test[test$Store==1,])
#Both should be 2783


##Gaussian Normalization
# for(i in 3:ncol(x_train)){
#   x_train4py[,i] <- (x_train[,i]-x_train_param[1,i])/x_train_param[2,i]
#   x_test4py[,i] <- (x_test[,i]-x_train_param[1,i])/x_train_param[2,i]
# }


##Min MAx Normalization
for(i in 1:2){
  x_train4py[,i] <- x_train[,i]
  x_test4py[,i] <- x_test[,i]
}
for(i in 3:ncol(x_train)){
  x_train4py[,i] <- (x_train[,i]-min(x_train[,i]))/(max(x_train[,i])-min(x_train[,i]))
  x_test4py[,i] <- (x_test[,i]-min(x_train[,i]))/(max(x_train[,i])-min(x_train[,i]))
}

min(x_train4py[,-1:-2])  # 0 <- Min-Max normalization success.
max(x_train4py[,-1:-2])  # 1

# Encoded Week 1 data
# 0.99  0.01  .......  0.01
# Encoded Week 2 data
# 0.01  0.99  .......  0.01
#..........
# Encoded Week 53 data
# 0.01  0.01  .......  0.99
train_with_features_Week<-NULL
for(i in 1:53){
  train_with_features_Week <- data.frame(cbind(train_with_features_Week ,ifelse(train_with_features_sorted$Week==i, 0.99, 0.01)))
}

test_with_features_Week<-NULL
for(i in 1:53){
  test_with_features_Week <- data.frame(cbind(test_with_features_Week ,ifelse(test_with_features_sorted$Week==i, 0.99, 0.01)))
}

# Binds all data frames
data4py <- cbind(x_train4py[,1:2], train_cat_Type_Store_Dept, train_with_features_Week, x_train4py[,3:ncol(x_train4py)], y_train)
data4py_test <- cbind(x_test4py[,1:2], test_cat_Type_Store_Dept, test_with_features_Week , x_test4py[,3:ncol(x_test4py)], y_test)

data4py$Week <- NULL
data4py_test$Week <- NULL

ncol(data4py) #215 coloumns
ncol(data4py_test) #215 columns


########## Creates files in the favor of departments ##########
sum(sort(unique(data4py$Dept)) != sort(unique(data4py_test$Dept)))
# [1] 0 <= Departments in taining set also in the test set. 

current_wd <- getwd()
setwd('./inputfiles')
colnames(data4py_Deptx)

for(i in unique(data4py$Dept)){
  filename = paste('train_data_with_dept', i, '_for_python.csv', sep="")
  cat(sprintf("processing.... %dth", i),'\n')
  data4py_Deptx <- data4py[data4py$Dept == i, -1:-2]
  write.table(data4py_Deptx, file=filename, col.names=FALSE, row.names=FALSE)
}

for(i in unique(data4py_test$Dept)){
  filename = paste('test_data_with_dept', i, '_for_python.csv', sep="")
  cat(sprintf("processing.... %dth", i),'\n')
  data4py_test_Deptx <- data4py_test[data4py_test$Dept == i, -1:-2]
  write.table(data4py_test_Deptx, file=filename, col.names=FALSE, row.names=FALSE)
}

setwd(current_wd)
