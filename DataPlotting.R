# Title: DataPlotting.R
# Author: Ssurey Moon
#
# Reference: 
#       https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting
#
# Plotting data of Walmart store 1 deapartment 1 


#rm(list=ls())
setwd(".")

## @knitr check_na
stores = read.csv("stores.csv", header=TRUE)
train = read.csv("train.csv", header=TRUE)
test = read.csv("test.csv", header = TRUE)
features = read.csv("features.csv", header = TRUE)
sample = read.csv("sampleSubmission.csv", header=TRUE)

sprintf("%d NAs in train data", sum(is.na(train)))
sprintf("%d NAs in store data", sum(is.na(test)))
sprintf("%d NAs in test data", sum(is.na(stores)))
sprintf("%d NAs in feature data", sum(is.na(features)))

for(i in 1:length(features)){
  cat(sprintf("%d NAs in %s attribute",
              sum(is.na(features[,i])), colnames(features)[i]), "\n")
}


#Let's take a look at data which CPI or Unemployment is NA
CPI_NA = features[which(is.na(features$CPI)),]
sprintf("We do not have any CPI and Unployement rate date from %s to %s", min(as.Date(CPI_NA$Date)), max(as.Date(CPI_NA$Date)))
feat_date_from = min(as.Date(CPI_NA$Date))
feat_date_to = max(as.Date(CPI_NA$Date))

#The period which has at least one Markdown
non_NA_date = as.Date(features$Date[which(features$Store == 1 & !(is.na(features[,5]) & 
                                            is.na(features[,6]) & is.na(features[,7]) & 
                                            is.na(features[,8]) & is.na(features[,9])))])

sprintf("The period which has at least one Markdown is from %s to %s", min(non_NA_date), max(non_NA_date))


x_axis = as.Date(train$Date[which(train$Store==1 & train$Dept==1)])
y_axis = train$Weekly_Sales[which(train$Store==1 & train$Dept==1)]

plot(x_axis, y_axis, ylab="sales", xlab="date", xlim=range(min(as.Date(train$Date)):feat_date_to), type="n")
lines(x_axis, y_axis) #Weekly sales of the store1 departments 1
polygon(c(min(non_NA_date), min(non_NA_date), max(non_NA_date), max(non_NA_date)),
        c(0,70000,70000,0), col='orange', density=50) #Area where markdowns are available

polygon(c(feat_date_from, feat_date_from, feat_date_to, feat_date_to),
        c(0,50000,50000,0), col='grey30', density=70) #Area where CPI and Unemployment are missing

test_date_from= min(as.Date(test$Date))
test_date_to= max(as.Date(test$Date))
polygon(c(test_date_from, test_date_from, test_date_to, test_date_to),
        c(0,35000,35000,0), col='blue', density=50) #Area we need to predict the weekly sales
lines(x_axis, y_axis)



#Plots about relations bwtween sales and holiday
plot(x_axis, y_axis, ylab="sales", xlab="date", xlim=range(min(as.Date(train$Date)):feat_date_to), type="n")

Super_Bowl = as.Date(c("12-02-10", "11-02-11", "10-02-12", "08-02-13"), "%d-%m-%y")
Labor_Day = as.Date(c("10-09-10", "09-09-11", "07-09-12", "06-09-13"), "%d-%m-%y")
Thanksgiving = as.Date(c("26-11-10", "25-11-11", "23-11-12", "29-11-13"), "%d-%m-%y")
Christmas = as.Date(c("31-12-10", "30-12-11", "28-12-12", "27-12-13"), "%d-%m-%y")
Holidays <- t(data.frame(Super_Bowl, Labor_Day, Thanksgiving, Christmas))
isholiday = as.Date((train$Date[which(train$Store==1 & train$Dept==1 & train$IsHoliday==TRUE)]))

print(Holidays)

polygon(c(min(non_NA_date), min(non_NA_date), max(non_NA_date), max(non_NA_date)),
        c(0,70000,70000,0), col='orange', density=50)
polygon(c(feat_date_from, feat_date_from, feat_date_to, feat_date_to),
        c(0,50000,50000,0), col='grey30', density=70)
polygon(c(test_date_from, test_date_from, test_date_to, test_date_to),
        c(0,35000,35000,0), col='blue', density=50)

#Plot holidays
for(i in 1:4){
  lines(rep(Super_Bowl[i], times=4), seq(from=0,to=70000,length=4), col='blue')
  lines(rep(Labor_Day[i], times=4), seq(from=0,to=70000,length=4), col='red')
  lines(rep(Thanksgiving[i], times=4), seq(from=0,to=70000,length=4), col='green')
  lines(rep(Christmas[i], times=4), seq(from=0,to=70000,length=4), col='purple')
}

lines(x_axis, y_axis)
lines(x_axis, y_axis)

polygon(as.Date(c("2010-04-02", "2010-04-02", "2010-04-16", "2010-04-16")),
        c(0,70000,70000,0), col='grey5', density=40)
polygon(as.Date(c("2010-10-22", "2010-10-22", "2010-11-05", "2010-11-05")),
        c(0,70000,70000,0), col='grey5', density=40)
polygon(as.Date(c("2011-04-22", "2011-04-22", "2011-05-06", "2011-05-06")),
        c(0,70000,70000,0), col='grey5', density=40)

