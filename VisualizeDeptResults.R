# Title: VisualizeDeptResults.R
# Author: Ssurey Moon
#
# Reference: 
#       [1] https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting
#
# Visualizing the result
#  1. Training Error and Cross Validation Error Rate
#  2. Target and Prediction plot cross departments
#
# Input file : Under the directory './outputfiles/'
#              nn_deptX_epochYYY_info_train.txt : training errors for department X after Iteration YYY
#              nn_deptX_epochYYY_info_cv.txt : test errors for department X after Iteration YYY
#              walmart_sales_deptX_train_test_result.csv' : predicton on training set and test set
#              walmart_sales_deptX_test_result.csv' : predicton on test set
#




#rm(list=ls())
setwd(".")
train = read.csv("train.csv", header=TRUE) 
test = read.csv("test.csv", header = TRUE)

library(ggplot2)

err_rate <- list()

#Choose the range of departments to plot
dept_from = 1
dept_to = 99

for(dept in dept_from:dept_to){
  filename_for_trainingerr = paste('result_dept', dept, '_epoch500_info_train.txt', sep="")
  filename_for_cverr = paste('nn_dept', dept, '_epoch500_info_cv.txt', sep="")
  
  err_rate[[dept]] =  data.frame(train_err = as.vector(t(read.table(filename_for_trainingerr, sep=",", nrows=1))),
                                  cv_err = as.vector(t(read.table(filename_for_cverr, sep=",", nrows=1))))

  sp <- ggplot(err_rate[[dept]][-1:-2,], aes(x=3:nrow(err_rate[[dept]]), y=cv_err))
  sp <- sp + geom_line(colour = 'pink') + stat_smooth(colour = 'red')
  sp <- sp + geom_line(aes(y=train_err), color = 'blue') + xlab("iterations") + ylab("error rate")
  sp <- sp + labs(title = c(sprintf("dept%d error rate", dept)))
  plot(sp)
}



setwd(".")
total_result <- list()
test_result <- list()
currend_wd <- getwd()



for(dept in dept_from:dept_to){
  filename_for_total_result = paste(currend_wd, '/outputfiles/walmart_sales_dept', dept, '_train_test_result.csv', sep="")
  
  filename_for_test_result = paste(currend_wd, '/outputfiles/walmart_sales_dept', dept, '_test_result.csv', sep="")
  total_result[[dept]] =  as.vector(as.matrix(read.csv(file=filename_for_total_result, header=FALSE)))
  test_result[[dept]] =  as.vector(as.matrix(read.csv(file=filename_for_test_result, header=FALSE)))
  
  plot(total_result[[dept]], type='n', main=c(sprintf('Weekly sales of departments %d of all stores', dept)))
  lines(total_result[[dept]], col='red')
  lines(train$Weekly_Sales[train$Dept==dept], col='blue')
}



for(dept in dept_from:dept_to){
  for(i in unique(test$Store[test$Dept==dept])){
    train_complete_deptX = train[train$Dept==dept,]
    train_complete_deptX$result <- total_result[[dept]][1:nrow(train[train$Dept==dept,])]


    test_complete_deptX <- test[test$Dept==dept,]
    test_complete_deptX$Weekly_Sales <- test_result[[dept]]
    
    if(i %in% unique(train$Store[train$Dept==dept])){
      plot_points_train <- with(train_complete_deptX, c(min(which(Store == i & Dept == dept)), max(which(Store == i & Dept == dept))))
    }
    
    plot_points_test <- with(test_complete_deptX, c(min(which(Store == i & Dept == dept)), max(which(Store==i & Dept == dept))))
    if(i %in% unique(train$Store[train$Dept==dept])){
      prediction <- c(train_complete_deptX$result[plot_points_train[1]:plot_points_train[2]], test_complete_deptX$Weekly_Sales[plot_points_test[1]:plot_points_test[2]])
    }
    else
    {
      prediction <- c(test_complete_deptX$Weekly_Sales[plot_points_test[1]:plot_points_test[2]])
    }
    
    plot(prediction, type = 'n', main=c(sprintf("Department %d, Store %d", dept, i)))
    if(i %in% unique(train$Store[train$Dept==dept])){
      lines(train_complete_deptX$Weekly_Sales[plot_points_train[1]:plot_points_train[2]])
    }
    lines(prediction, col='red')
  }
  
}




#save(list=ls(), file='Visualization.Rdata')

#load(file='Visualization.Rdata')
