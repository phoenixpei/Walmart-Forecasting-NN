# Title: MissingDataImputation.R
# Author: Ssurey Moon
#
# Reference: 
#       [1] https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting
#       [2] Andrew Gelman, Jennifer Hill (2006), "Missing-data imputation", Chapter 25 in Data Analysis Using Regression and Multilevel/Hierarchical Models, Cambridge University Press
#
# Missing Data inputation of feature data of Walmart stores.
# Input file : features.csv
#               Missingness in the fields of Markdown 1~5, CPI and Unemployment Rate
#               Number of missingness: 24040
# output file : features_complete.csv
#               Imputation finished.
#                 1. ARIMA Model for CPI and Unemployment
#                 2. Iterative regression for Markdowns
#               Number of missingness: 0
#
# If you just need data imputed, execute the line below to load workspace.
# load('mi_workspace_13Nov2013.RData')


#rm(list=ls())

setwd(".")
library('mi')
library(forecast)

# To save time..
# ## @knitr check_na
stores = read.csv("stores.csv", header=TRUE)
train = read.csv("train.csv", header=TRUE)
test = read.csv("test.csv", header = TRUE)
features = read.csv("features.csv", header = TRUE)
sample = read.csv("sampleSubmission.csv", header=TRUE)
features$Date <- as.Date(features$Date)

#Missing data imputation process
#1.ARIMA for CPI, Unemployment
#2.Iterative Regression for Markdowns
#3.Clone Markdowns one after

#finds the period when MarkDown data exist.
non_NA_date=list()
for(i in 1:max(features$Store)){
  
  non_NA_date[[i]] <- as.Date(features$Date[which(features$Store == i &
                                                    !(is.na(features[,5]) & 
                                                    is.na(features[,6]) & is.na(features[,7]) & 
                                                    is.na(features[,8]) & is.na(features[,9])))])
}


#Returns dataframe which at least one MK is NA during the period between mindate and maxdate.
DrawFeatNA <- function(features, store, mindate, maxdate){
  features_sx = features[which(features$Store==store),]
  features_sx$Date <- as.Date(features_sx$Date)
  non_NA_date = as.Date(features_sx$Date[which(features$Store == store & !(is.na(features[,5]) & 
                                                                             is.na(features[,6]) & is.na(features[,7]) &
                                                                             is.na(features[,8]) & is.na(features[,9])))])
  features_sx_ma = features_sx[which(features_sx$Date >= mindate &
                                       features_sx$Date <= maxdate),]
  
  features_sx_ma_NA = features_sx_ma[apply(features_sx_ma[,5:9], 1, function(x) any(is.na(x))),]
  
  return(features_sx_ma_NA)
}


##Fist, Just print CPI, UnempRate.
forecastCPIandUnempRate <- function (df, store=1, graph=TRUE){
  out <- list()
  
  features_sN <- features[features$Store==store,]
  par(mar=c(2,2,2,2))
  par(mfrow=c(2,1))
  
  time_NA <- features_sN[is.na(features_sN$CPI),]$Date
  time_OK <- features_sN[!is.na(features_sN$CPI),]$Date
  myts_sN_CPI <- ts(features_sN$CPI[is.element(features_sN$Date, time_OK)], start = c(2010,5), frequency = 52)
  myts_sN_UnEmp <- ts(features_sN$Unemployment[is.element(features_sN$Date, time_OK)], start = c(2010,5), frequency = 52)
  fit_sN_CPI <- auto.arima(myts_sN_CPI)
  fit_sN_UnEmp <- auto.arima(myts_sN_UnEmp)
  
  if(graph){
    plot(forecast(fit_sN_CPI, length(time_NA)), main=c(sprintf("CPI, Store %d", store)))
    plot(forecast(fit_sN_UnEmp, length(time_NA)), main=c(sprintf("Unemployment, Store %d", store)))
  }
  out[["CPI"]] <- forecast(fit_sN_CPI, length(time_NA))
  out[["Unemployment"]] <- forecast(fit_sN_UnEmp, length(time_NA))
  return(out)
}

features_after_CPI_UnEmp <- features
for(i in 1:45){ 
  sprintf("Now, processing... store %d", i)
  store_sN_CPI_UnEmp <- forecastCPIandUnempRate(df=features, store=i, graph=TRUE)
  store_sN_CPI_UnEmp$CPI$mean
  store_sN_CPI_UnEmp$Unemployment$mean
  condition <- (features_after_CPI_UnEmp$Store==i) & is.na(features_after_CPI_UnEmp$CPI)
  features_after_CPI_UnEmp$CPI[condition] <- store_sN_CPI_UnEmp$CPI$mean
  features_after_CPI_UnEmp$Unemployment[condition] <- store_sN_CPI_UnEmp$Unemployment$mean
}

############################################################################################

#df, method, ref
#   If you want stronly restict your markdowns positive, you should pre-process the markdowns.
MiForMarkDowns <- function(df, store, method="Iterative Random Regression", ref_store=NULL, hist=FALSE){
  
  df_sN_ref<-NULL
  if(!is.null(ref_store)){
    df_sN_ref <- df[df$Store==ref_store,]
    df_sN_ref <- df_sN_ref[(is.element(df_sN_ref$Date,non_NA_date[[ref_store]])),]
  }
  
  if(method=="Iterative Random Regression"){
    df_sN <- df[df$Store==store,]
    imp_sN <- df_sN[(is.element(df_sN$Date,non_NA_date[[store]])),]
    n_df_sN <- nrow(imp_sN)
    
    date_mk1 <- imp_sN[is.na(imp_sN$MarkDown1),]$Date
    date_mk2 <- imp_sN[is.na(imp_sN$MarkDown2),]$Date
    date_mk3 <- imp_sN[is.na(imp_sN$MarkDown3),]$Date
    date_mk4 <- imp_sN[is.na(imp_sN$MarkDown4),]$Date
    date_mk5 <- imp_sN[is.na(imp_sN$MarkDown5),]$Date


    imp_sN <- rbind(imp_sN, df_sN_ref)
    aaaa= features_after_CPI_UnEmp[features_after_CPI_UnEmp$Store==25,]
    inf <- mi.info(imp_sN)
#   

#     inf <- update(inf, "type", list(MarkDown1="positive-continuous", MarkDown2="positive-continuous", MarkDown3="positive-continuous", MarkDown4="positive-continuous", MarkDown5="positive-continuous"))
#     if(store in c(10,25)){
#       inf <- update(inf, "type", list(MarkDown1="positive-continuous", MarkDown2="continuous", MarkDown3="positive-continuous", MarkDown4="positive-continuous", MarkDown5="positive-continuous"))
#     }
#     if(store == 21){
#       inf <- update(inf, "type", list(MarkDown1="positive-continuous", MarkDown2="continuous", MarkDown3="continuous", MarkDown4="positive-continuous", MarkDown5="positive-continuous"))
#     }
    

    
    inf <- update(inf, "include", list(Store=FALSE, Date=FALSE))
    
    dat.transformed.sN <- mi.preprocess(imp_sN, inf)
    IMP_sN <- mi(dat.transformed.sN,  n.iter=10, check.coef.convergence=TRUE, add.noise=FALSE)

    out_df <- mi.completed(IMP_sN)
    out_df <- mi.postprocess(out_df, inf)
    print(out_df)

    out_df <- out_df[[2]][1:n_df_sN,]
     if(length(date_mk1)!=0){
        mi.hist(imp(IMP_sN,1)[[names(IMP_sN@mi.info)[5]]], imp_sN$MarkDown1,
                breaks = pretty(imp_sN$MarkDown1), main=c(sprintf("Markdown 1, Store %d", store)))
     }
     if(length(date_mk2)!=0){
        mi.hist(imp(IMP_sN,1)[[names(IMP_sN@mi.info)[6]]], imp_sN$MarkDown2,
                breaks = pretty(imp_sN$MarkDown2), main=c(sprintf("Markdown 2, Store %d", store)))
     }
     if(length(date_mk3)!=0){
        mi.hist(imp(IMP_sN,1)[[names(IMP_sN@mi.info)[7]]], imp_sN$MarkDown3,
                breaks = pretty(imp_sN$MarkDown3), main=c(sprintf("Markdown 3, Store %d", store)))
     }
     if(length(date_mk4)!=0){
        mi.hist(imp(IMP_sN,1)[[names(IMP_sN@mi.info)[8]]], imp_sN$MarkDown4,
                breaks = pretty(imp_sN$MarkDown4), main=c(sprintf("Markdown 4, Store %d", store)))
     }
     if(length(date_mk5)!=0){
        mi.hist(imp(IMP_sN,1)[[names(IMP_sN@mi.info)[9]]], imp_sN$MarkDown5,
                breaks = pretty(imp_sN$MarkDown5), main=c(sprintf("Markdown 5, Store %d", store)))
     }
  
  }
  return(out_df);
}

############################################################################################


## Let's test for store 1
df_sN_11 <- features_after_CPI_UnEmp[features_after_CPI_UnEmp$Store==1,]
imp_sN_11 <- df_sN_11[(is.element(df_sN_11$Date,non_NA_date[[1]])),]
print(imp_sN_11)
# the output of rows from 114 to 132 is supposed to look like below:
# imp_sN_11 <- imp_sN_11[!is.na(imp_sN_11$CPI),]
# 114     1 2012-04-06       70.43      3.891  10121.97        NA     77.98   3750.59
# 115     1 2012-04-13       69.07      3.891   6186.19   3288.69     17.07   1822.55
# 116     1 2012-04-20       66.76      3.877   2230.80    612.02     19.75    275.13
# 117     1 2012-04-27       67.23      3.814   3221.25        NA     35.49    577.14
# 118     1 2012-05-04       75.55      3.749  21290.13        NA     69.89   4977.35
# 119     1 2012-05-11       73.77      3.688   8351.40        NA     10.52   2443.14
# 120     1 2012-05-18       70.33      3.630   6154.14        NA     45.11   1675.49
# 121     1 2012-05-25       77.22      3.561   4039.39        NA    745.19   1429.96
# 122     1 2012-06-01       77.95      3.501   6086.21     12.00    370.51    148.75
# 123     1 2012-06-08       78.30      3.452   8813.81    116.80     64.55   2652.04
# 124     1 2012-06-15       79.35      3.393   5621.99    109.60      0.25   2420.28
# 125     1 2012-06-22       78.39      3.346   8624.56    171.25      3.05   2524.07
# 126     1 2012-06-29       84.88      3.286   3965.73    161.60        NA    435.99
# 127     1 2012-07-06       81.57      3.227  12218.76     94.40    192.83   4066.01
# 128     1 2012-07-13       77.12      3.256   7218.13     45.20     36.24   3909.38
# 129     1 2012-07-20       80.42      3.311   3213.00    313.72      9.53   2262.02
# 130     1 2012-07-27       82.66      3.407   7146.90    389.02      1.59  10267.54
# 131     1 2012-08-03       86.11      3.417  27584.78    119.98     30.23  12007.04
# 132     1 2012-08-10       85.05      3.494  11436.22    245.00      6.85   6964.26



MiForMarkDowns(df=features_after_CPI_UnEmp, store=1, method="Iterative Random Regression", ref_store=NULL)
# the output of rows from 114 to 132 is supposed to look like below:
#    "Store"  "Date"  "Temperature" "Fuel_Price"   "MK1"     "MK2"     "MK3"     "MK4"     "MK5"
# 114     1 2012-04-06       70.43      3.891  10121.97    97.00804    77.98000   3750.59
# 115     1 2012-04-13       69.07      3.891   6186.19  3288.69000    17.07000   1822.55
# 116     1 2012-04-20       66.76      3.877   2230.80   612.02000    19.75000    275.13
# 117     1 2012-04-27       67.23      3.814   3221.25    67.93801    35.49000    577.14
# 118     1 2012-05-04       75.55      3.749  21290.13    13.96069    69.89000   4977.35
# 119     1 2012-05-11       73.77      3.688   8351.40    36.57256    10.52000   2443.14
# 120     1 2012-05-18       70.33      3.630   6154.14    85.00087    45.11000   1675.49
# 121     1 2012-05-25       77.22      3.561   4039.39   353.66978   745.19000   1429.96
# 122     1 2012-06-01       77.95      3.501   6086.21    12.00000   370.51000    148.75
# 123     1 2012-06-08       78.30      3.452   8813.81   116.80000    64.55000   2652.04
# 124     1 2012-06-15       79.35      3.393   5621.99   109.60000     0.25000   2420.28
# 125     1 2012-06-22       78.39      3.346   8624.56   171.25000     3.05000   2524.07
# 126     1 2012-06-29       84.88      3.286   3965.73   161.60000    33.31745    435.99
# 127     1 2012-07-06       81.57      3.227  12218.76    94.40000   192.83000   4066.01
# 128     1 2012-07-13       77.12      3.256   7218.13    45.20000    36.24000   3909.38
# 129     1 2012-07-20       80.42      3.311   3213.00   313.72000     9.53000   2262.02
# 130     1 2012-07-27       82.66      3.407   7146.90   389.02000     1.59000  10267.54
# 131     1 2012-08-03       86.11      3.417  27584.78   119.98000    30.23000  12007.04
# 132     1 2012-08-10       85.05      3.494  11436.22   245.00000     6.85000   6964.26


df_sN_3636 <- features_after_CPI_UnEmp[features_after_CPI_UnEmp$Store==36,]
imp_sN_3636 <- df_sN_3636[(is.element(df_sN_3636$Date,non_NA_date[[36]])),]
#imp_sN_3636 <- imp_sN_3636[!is.na(imp_sN_3636$CPI),]
print(imp_sN_3636)
#    "Store"   "Date"  "Temperature" "Fuel_Price"   "MK1"     "MK2"     "MK3"     "MK4"     "MK5"
# 6484    36 2012-04-06       73.95      3.934    375.20        NA        NA     12.41   1398.67
# 6485    36 2012-04-13       72.54      3.919     85.47    651.77        NA        NA    487.07
# 6486    36 2012-04-20       70.87      3.918    177.77    317.91        NA    231.31   1473.96
# 6487    36 2012-04-27       70.06      3.888    252.98     36.03        NA        NA   6621.24
# 6488    36 2012-05-04       77.17      3.835    621.82        NA        NA     18.38   1191.47
# 6489    36 2012-05-11       76.55      3.764    475.61        NA        NA        NA    818.06
# 6490    36 2012-05-18       73.70      3.713    367.23        NA        NA        NA   1704.18
# 6491    36 2012-05-25       78.94      3.636    517.28        NA        NA      0.92   1767.58
# 6492    36 2012-06-01       80.74      3.567    187.88        NA        NA      4.02   1674.50
# 6493    36 2012-06-08       81.50      3.513    226.87        NA        NA      5.00    597.68
# 6494    36 2012-06-15       82.15      3.407    945.96        NA        NA        NA   2709.66
# 6495    36 2012-06-22       80.40      3.358    318.51        NA        NA        NA   1805.33
# 6496    36 2012-06-29       86.68      3.273   1169.41        NA        NA        NA   2549.31
# 6497    36 2012-07-06       81.52      3.232    249.73        NA      0.04        NA    659.16
# 6498    36 2012-07-13       78.15      3.245    769.50        NA      0.01        NA   1530.69
# 6499    36 2012-07-20       81.76      3.301     38.04        NA        NA        NA    886.99
# 6500    36 2012-07-27       84.00      3.392    144.59        NA        NA        NA   1545.26
# 6501    36 2012-08-03       85.56      3.404    243.47        NA      2.40     64.47   1077.74
# 6502    36 2012-08-10       84.41      3.490     25.14        NA        NA      1.98   1638.93

#MiForMarkDowns(df=features, store=36, method="Iterative Random Regression", ref_store=NULL)

#These are vacant stores
# The store 30 has 49 rows which have multiple variables
# The store 33 has 69 rows which have multiple variables
# The store 36 has 72 rows which have multiple variables
# The store 37 has 55 rows which have multiple variables
# The store 38 has 51 rows which have multiple variables
# The store 42 has 58 rows which have multiple variables
# The store 43 has 59 rows which have multiple variables
# The store 44 has 56 rows which have multiple variables

find_cloest <- function (df, store=1, ignore.vacant=FALSE) {
  
  ck_type <- unique(df$Type[df$Store==store])
  size <- scale(df$Size[!duplicated(df$Store)])
  max_cor <- data.frame(rep(0,5))
  
  vacant.list<-NULL
  if(ignore.vacant){
    vacant.list<-c(-30, -33, -36, -37, -38, -42, -43, -44)
  }
  n_store <- length(unique(df$Store))
  ck_store <- c(1:n_store)
  ck_store <- ck_store[c(-store, vacant.list)]
  
  for (i in ck_store){
    cor_mat <- as.data.frame(t(diag(cor(na.omit(df[df$Store==store,c(-1, -2, -5:-9, -12:-14)]), 
                                        na.omit(df[df$Store==i,c(-1, -2, -5:-9, -12:-14)])))))
    cor_mat$Size <- 1-abs(size[store]-size[i])/(max(size)-min(size))
    cor_mat$is.Type <- ifelse(ck_type %in% df$Type[df$Store==store] , TRUE, FALSE)
    
    if(sum(cor_mat) > sum(max_cor)){
      closest_store <- i
      max_cor <- cor_mat
    } 
  }
  max_cor$Store <- closest_store
  max_cor$Type <- ck_type
  return(data.frame(with(max_cor, cbind(Store, Type, is.Type ,Temperature, Fuel_Price, CPI, Unemployment, Size))))
}

features_cor <- merge(features_after_CPI_UnEmp, stores, by="Store", all.x=T)

find_cloest (features_cor, store=36, ignore.vacant = F)
#       Store Type is.Type Temperature Fuel_Price CPI Unemployment Size
# [1,]    37    1       1           1  0.9979958   1            1    1

# => The store 37 has 55 rows which have multiple variables

find_cloest (features_cor, store=36, ignore.vacant = T)
#      Store Type is.Type Temperature Fuel_Price       CPI Unemployment      Size
# [1,]     3    1       1   0.9887639  0.9977378 0.9999591    0.9814056 0.9863705

MiForMarkDowns(df=features_after_CPI_UnEmp, store=36, method="Iterative Random Regression", ref_store=NULL)
#     "Store"  "Date"  "Temperature" "Fuel_Price"   "MK1"       "MK2"        "MK3"       "MK4"      "MK5"
# 6484    36 2012-04-06       73.95      3.934  375.20000 7.339826e-01 2.301450e+01 1.241000e+01   1398.67
# 6485    36 2012-04-13       72.54      3.919   85.47000 6.517700e+02 3.326842e+01 3.370921e+02    487.07
# 6486    36 2012-04-20       70.87      3.918  177.77000 3.179100e+02 7.226891e+00 2.313100e+02   1473.96
# 6487    36 2012-04-27       70.06      3.888  252.98000 3.603000e+01 1.380979e+01 1.087627e+01   6621.24
# 6488    36 2012-05-04       77.17      3.835  621.82000 8.116983e-01 1.258563e+00 1.838000e+01   1191.47
# 6489    36 2012-05-11       76.55      3.764  475.61000 3.376693e+00 1.000512e+00 3.994249e+00    818.06
# 6490    36 2012-05-18       73.70      3.713  367.23000 3.628022e+00 1.111546e+01 1.017250e+00   1704.18
# 6491    36 2012-05-25       78.94      3.636  517.28000 4.467063e-01 2.353891e-03 9.200000e-01   1767.58
# 6492    36 2012-06-01       80.74      3.567  187.88000 1.242963e+00 7.871822e+00 4.020000e+00   1674.50
# 6493    36 2012-06-08       81.50      3.513  226.87000 2.164653e-01 7.158858e-02 5.000000e+00    597.68
# 6494    36 2012-06-15       82.15      3.407  945.96000 2.513287e-03 3.132680e-03 3.365912e-01   2709.66
# 6495    36 2012-06-22       80.40      3.358  318.51000 1.422186e-02 1.444056e-03 2.423605e+01   1805.33
# 6496    36 2012-06-29       86.68      3.273 1169.41000 4.813595e-02 5.152052e-03 1.524278e+00   2549.31
# 6497    36 2012-07-06       81.52      3.232  249.73000 2.335917e+00 4.000000e-02 8.770761e+00    659.16
# 6498    36 2012-07-13       78.15      3.245  769.50000 9.838314e+00 1.000000e-02 4.601178e+00   1530.69
# 6499    36 2012-07-20       81.76      3.301   38.04000 1.948888e+02 9.701399e-02 7.387690e+00    886.99
# 6500    36 2012-07-27       84.00      3.392  144.59000 5.739933e-01 1.325447e-01 1.639373e+01   1545.26
# 6501    36 2012-08-03       85.56      3.404  243.47000 3.778460e+00 2.400000e+00 6.447000e+01   1077.74
# 6502    36 2012-08-10       84.41      3.490   25.14000 7.657846e+00 2.384958e+01 1.980000e+00   1638.93


MiForMarkDowns(df=features_after_CPI_UnEmp, store=36, method="Iterative Random Regression", ref_store=3)
#   "Store"  "Date"   "Temperature" "Fuel_Price"    "MK1"        "MK2"      "MK3"      "MK4"        "MK5"
# 6484    36 2012-04-06       73.95      3.934  375.2000 1.031212e+03 6.092577e+02 1.241000e+01   1398.67
# 6485    36 2012-04-13       72.54      3.919   85.4700 6.517700e+02 3.713823e-02 6.987074e+00    487.07
# 6486    36 2012-04-20       70.87      3.918  177.7700 3.179100e+02 6.774075e-02 2.313100e+02   1473.96
# 6487    36 2012-04-27       70.06      3.888  252.9800 3.603000e+01 2.442650e-03 1.669446e-03   6621.24
# 6488    36 2012-05-04       77.17      3.835  621.8200 3.363147e-02 3.043125e+02 1.838000e+01   1191.47
# 6489    36 2012-05-11       76.55      3.764  475.6100 7.045692e+03 2.100996e+00 7.866443e+00    818.06
# 6490    36 2012-05-18       73.70      3.713  367.2300 1.407984e+03 5.255140e-02 2.950167e+00   1704.18
# 6491    36 2012-05-25       78.94      3.636  517.2800 6.698661e+00 1.005967e+03 9.200000e-01   1767.58
# 6492    36 2012-06-01       80.74      3.567  187.8800 8.889094e-04 1.280026e+02 4.020000e+00   1674.50
# 6493    36 2012-06-08       81.50      3.513  226.8700 2.317801e+00 2.600912e+00 5.000000e+00    597.68
# 6494    36 2012-06-15       82.15      3.407  945.9600 3.760360e+00 2.560421e-03 9.290263e+01   2709.66
# 6495    36 2012-06-22       80.40      3.358  318.5100 1.211428e-01 9.663208e-01 1.193141e+01   1805.33
# 6496    36 2012-06-29       86.68      3.273 1169.4100 4.052721e+00 8.203186e-01 1.930526e+00   2549.31
# 6497    36 2012-07-06       81.52      3.232  249.7300 2.669510e-01 4.000000e-02 4.227099e+01    659.16
# 6498    36 2012-07-13       78.15      3.245  769.5000 4.604813e-01 1.000000e-02 4.989605e-01   1530.69
# 6499    36 2012-07-20       81.76      3.301   38.0400 2.122057e+01 1.075099e-02 4.606143e-01    886.99
# 6500    36 2012-07-27       84.00      3.392  144.5900 2.589535e+00 3.667566e+00 6.648629e+01   1545.26
# 6501    36 2012-08-03       85.56      3.404  243.4700 1.193903e+03 2.400000e+00 6.447000e+01   1077.74
# 6502    36 2012-08-10       84.41      3.490   25.1400 3.460219e-01 9.593425e-04 1.980000e+00   1638.93

MiForMarkDowns(df=features_after_CPI_UnEmp, store=36, method="Iterative Random Regression", ref_store=3)


MiForMarkDowns(df=features_after_CPI_UnEmp, store=10, method="Iterative Random Regression", ref_store=NULL)


features_after_CPI_UnEmp_MKs <- features_after_CPI_UnEmp
features_cor <- merge(features_after_CPI_UnEmp, stores, by="Store", all.x=T)

for(i in 1:45){
  sprintf("Now, processing... store %d", i)
  if(i %in% c(30, 33, 36, 37, 38, 42, 43, 44)){
    reference_store <- find_cloest (features_cor, store=36, ignore.vacant = T)$Store
    output <- MiForMarkDowns(df=features_after_CPI_UnEmp, store=i, method="Iterative Random Regression", ref_store=reference_store)
  } else {
    output <- MiForMarkDowns(df=features_after_CPI_UnEmp, store=i, method="Iterative Random Regression", ref_store=NULL)
  }
  features_after_CPI_UnEmp_MKs[(features_after_CPI_UnEmp_MKs$Date %in% output$Date) & (features_after_CPI_UnEmp_MKs$Store==i),] <- output[(output$Date %in% output$Date) & (output$Store==i),]
}


NA_date <- list()
for(i in 1:max(features$Store)){
  
  NA_date[[i]] <- as.Date(features$Date[which(features$Store == i & (is.na(features[,5]) & 
                                                                            is.na(features[,6]) & is.na(features[,7]) & 
                                                                            is.na(features[,8]) & is.na(features[,9])))])
}

features_complete <- features_after_CPI_UnEmp_MKs

for(i in 1:45){
  sprintf("Now, processing... store %d", i)
  for(j in rev(NA_date[[i]])){
    features_complete$MarkDown1[which(features_complete$Date == j & features_complete$Store==i)] <- features_complete$MarkDown1[which(features_complete$Date == j & features_complete$Store==i)+52]
    features_complete$MarkDown2[which(features_complete$Date == j & features_complete$Store==i)] <- features_complete$MarkDown2[which(features_complete$Date == j & features_complete$Store==i)+52]
    features_complete$MarkDown3[which(features_complete$Date == j & features_complete$Store==i)] <- features_complete$MarkDown3[which(features_complete$Date == j & features_complete$Store==i)+52]
    features_complete$MarkDown4[which(features_complete$Date == j & features_complete$Store==i)] <- features_complete$MarkDown4[which(features_complete$Date == j & features_complete$Store==i)+52]
    features_complete$MarkDown5[which(features_complete$Date == j & features_complete$Store==i)] <- features_complete$MarkDown5[which(features_complete$Date == j & features_complete$Store==i)+52]
#     features_after_CPI_UnEmp_MKs[features_after_CPI_UnEmp_MKs$Store==i,]$MarkDown2[which(features_after_CPI_UnEmp_MKs$Date == j & features_after_CPI_UnEmp_MKs$Store==i)] <- features_after_CPI_UnEmp_MKs[features_after_CPI_UnEmp_MKs$Store==i,]$MarkDown2[which(features_after_CPI_UnEmp_MKs$Date == j & features_after_CPI_UnEmp_MKs$Store==i)+52]
#     features_after_CPI_UnEmp_MKs[features_after_CPI_UnEmp_MKs$Store==i,]$MarkDown3[which(features_after_CPI_UnEmp_MKs$Date == j & features_after_CPI_UnEmp_MKs$Store==i)] <- features_after_CPI_UnEmp_MKs[features_after_CPI_UnEmp_MKs$Store==i,]$MarkDown3[which(features_after_CPI_UnEmp_MKs$Date == j & features_after_CPI_UnEmp_MKs$Store==i)+52]
#     features_after_CPI_UnEmp_MKs[features_after_CPI_UnEmp_MKs$Store==i,]$MarkDown4[which(features_after_CPI_UnEmp_MKs$Date == j & features_after_CPI_UnEmp_MKs$Store==i)] <- features_after_CPI_UnEmp_MKs[features_after_CPI_UnEmp_MKs$Store==i,]$MarkDown4[which(features_after_CPI_UnEmp_MKs$Date == j & features_after_CPI_UnEmp_MKs$Store==i)+52]
#     features_after_CPI_UnEmp_MKs[features_after_CPI_UnEmp_MKs$Store==i,]$MarkDown5[which(features_after_CPI_UnEmp_MKs$Date == j & features_after_CPI_UnEmp_MKs$Store==i)] <- features_after_CPI_UnEmp_MKs[features_after_CPI_UnEmp_MKs$Store==i,]$MarkDown5[which(features_after_CPI_UnEmp_MKs$Date == j & features_after_CPI_UnEmp_MKs$Store==i)+52]
  }
}

sum(is.na(features_complete))
###Now we finish filling missing data up in Markdown fields.
### Time to do something on CPI, and .. so on..


write.csv(features_complete, "features_complete.csv", row.names = FALSE)

#just test..
features_test = read.csv("features_complete.csv", header = TRUE)

sum(is.na(features_test))
# [1] 0  No missing data!!!!

#save(list=ls(),file='mi_workspace_13Nov2013.RData')


# you don't want wait for the imputation's done,
#just excuate the file below to load the data file:
#load('mi_workspace_13Nov2013.RData') #Saved data file


#Code below is to check if ARIMA works on the weekly salse data 