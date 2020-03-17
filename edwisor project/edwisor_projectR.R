# Credit Card Segmentation
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

install.packages(x)
lapply(x, require, character.only = TRUE)

#remove all the objects stored
rm(list=ls())
#get current working directory
getwd()
#set current working directory
setwd("C:/Users/jagadeesh/Desktop")
#reading the csv file
customer_data=read.csv("credit-card-data.csv")
#reading top 6 values from he dataset customer_data
head(customer_data)
# viewing structure of data
str(customer_data)
#checking number of rows and columns
nrow(customer_data)
ncol(customer_data)
#viewing summary of data
summary(customer_data)
#viewing column names
colnames(customer_data)
#performing data cleaning operations
#checking for missing values
df=data.frame(apply(customer_data,2,function(x){sum(is.na(x))}))
#checking for negative values
f<-data.frame(a=1:10,b=c(1:3,-4,5:10),c=c(-1,2:10))
f
#taking example of a dataframe
c<-apply(f,1,function(x){any(x<0)})
sum(c==FALSE)
sum(c==TRUE)
c<-apply(customer_data,1,function(x){any(x<0)})
sum(c==TRUE)
sum(c==FALSE)
#removing qualitative variable
df1<-df[-c(1)]
head(df1)
c<-apply(df1,1,function(x){any(x<0)})
#ommiting na values
df1<-na.omit(df1)
total_missing<-sum(is.na(customer_data))
#calculating percent of missing values
percent<-data.frame(apply(customer_data,2,function(x){sum(is.na(x))})/nrow(customer_data))
percent
percent<-data.frame(apply(customer_data,2,function(x){sum(is.na(x))})/total_missing)
#datavisulaization of merged data of percentage and  total missing values
ggplot(data=merged_data[1:3,],aes(x=reorder(names,-percent),y=percent))+
  geom_bar(stat="identity",fill="grey")+xlab("Parameter")+ggtitle("Missing data")+theme_bw()
#imputation using KNN
customer_train <-knnImputation(customer_data,k=3)
cnames<-colnames(customer_train)
#plotting histogram
hist(customer_train$MINIMUM_PAYMENTS)
boxplot(customer_train$BALANCE,horizontal=T)
#outlieranalysis

for(i in cnames){
  print(i)
  val = customer_train[,i][customer_train[,i] %in% boxplot.stats(customer_train[,i])$out]
  print(length(val))
  customer_train = customer_train[which(!customer_train[,i] %in% val),]
}
#correlation plot
corrplot(M,method="circle")
head(customer_train)
colnames(customer_train)
# Monthly average purchase_limit
mon_avg_purchase<-customer_train$PURCHASES/customer_train$TENURE
summary(mon_avg_purchase)
boxplot(mon_avg_purchase,horizontal = T)
#cash_advance_amounts
cash_adv_amount<-customer_train$CASH_ADVANCE/customer_train$TENURE
boxplot(cash_adv_amount,horizontal = T)
summary(cash_adv_amount)
#Average amount per purchase
avg_amount_purchase<-customer_train$PURCHASES/customer_train$PURCHASES_TRX
title("Average amount per purchase")
boxplot(avg_amount_purchase,horizontal = T)
summary(avg_amount_purchase)
#Average cash advance per purchase
avg_cash_purchase<-customer_train$CASH_ADVANCE/customer_train$CASH_ADVANCE_TRX
title("Average cash advance  per purchase")
boxplot(avg_cash_purchase,horizontal = T)
summary(avg_cash_purchase)
#Limit Usage
limit_usage<-customer_train$BALANCE/customer_train$CREDIT_LIMIT
title("limit_usage")
boxplot(limit_usage,horizontal = T)
summary(limit_usage)
pay_to_min_ratio<-customer_train$PAYMENTS/customer_train$MINIMUM_PAYMENTS
boxplot(pay_to_min_ratio, horizontal=T)
title("payments to minimum payments ratio")
summary(pay_to_min_ratio)
head(customer_train)
purchases_by_type<-customer_train$ONEOFF_PURCHASES/customer_train$ONEOFF_PURCHASES_FREQUENCY
title("purchases_by_type")
boxplot(purchases_by_type,horizontal = T)
summary(purchases_by_type)
# CLUSTERING ANALYSIS
customer_train1<-customer_train[c(-1)]
#Standaradization of data frame to mean =0 and deviation =1
scaled_df<-scale(customer_train1)
scaled_df[,-17]->scale_df
colnames(scaled_df)
sum(is.na(scaled_df))
head(scale_df)
#FACTOR ANALYSIS
factorana<-factanal(scale_df,factors = 2)
newfact1<-data.frame(factorana$uniquenesses)
head(newfact)
rownames(newfact)->row1
newfact$factorana.uniquenesses->row2
newfact<-cbind(row1,row2)
head(newfact)
newfact1
x=c(1,2,3,5)
library(RColorBrewer)
barplot(as.matrix(newfact1),legend=rownames(newfact1),col=brewer.pal(6,"Pastel1"))
sample1<-c("PURCHASES","ONEOFF_PURCHASES","PURCHASES_TRX","ONEOFF_PURCHASES_FREQUENCY",
"INSTALLMENTS_PURCHASES","PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CREDIT_LIMIT","PAYMENTS", 
"CASH_ADVANCE_FREQUENCY")
subset(scale_df,select=sample1)->cluster_sample
head(cluster_sample)
sample2
k_means_cluster<-kmeans(cluster_sample,5,nstart=50,iter.max=15)
as.factor(k_means_cluster$cluster)
set.seed(123)
data<-cluster_sample
max<-15
wss<-sapply(1:max,function(k){kmeans(data,k,nstart=50,iter.max=15)$tot.withinss})
wss
#plotting for optimal k value
plot(1:max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#clusters
k_means_cluster$size
k_means_cluster$centers
install.packages("factoextra")

sil <- silhouette(k_means_cluster$cluster, dist(cluster_sample))
fviz_silhouette(sil)