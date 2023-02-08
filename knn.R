###set env
getwd()
setwd('D:\\scientific\\IT Engineering\\training\\دیتا ساینس - شریف\\Machine Learning-R\\14011106\\KNN-malignant cancer')
dir()

##load csv file
#library(dplyr)
df <- read.csv('KnnDataset.csv') %>% tibble() 

head(df)
str(df)
summary(df)
##drop id feature 
df <- df[-1]

table(df$diagnosis)
prop.table(table(df$diagnosis))

###preprocessing => recode diagnosis as factor
## step1
df$diagnosis <- factor(x = df$diagnosis,levels = c('B','M'), labels = c('khosh','bad'))
str(df)


#prop.table(table(df$diagnosis))
#step 2
#(scale(c(1,2,3,4,5)))

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

c(normalize(c(10,20,30,40)))

df_n <- as.data.frame( lapply(df[2:31], normalize))
View (df_n)


#slicing
df_train <- df_n[1:469 , ]
df_test <- df_n[470:569 , ]
df_train_labels <- df[1:469,1]
#View(df_train_labels)

df_test_labels <- df[470:569 ,1]
#View(df_test_labels)
##modeling
library(class)
df_test_pred <- knn(train = df_train,test = df_test,cl = df_train_labels,k = 21)

# test or evaluation
library(gmodels)


CrossTable(x=df_test_labels , y = df_test_pred)


###############performance
df_test_pred_5 <- knn(train = df_train,test = df_test,cl=df_train_labels,k=5)
CrossTable(x=df_test_labels , y = df_test_pred_5)

df_test_in <- df_n[569,]
typeof(df_test_in)
typeof(df_test_pred)

predict(df_test_in,df_test_pred)
table(df_test_labels,df_test_pred)

library(caret)
cm <- confusionMatrix(df_test_pred,df_test_pred)
print(cm$table)





############# find optimum k
error.rate=0

for(i in 1:50){
  df_test_pred <- knn(train = df_train,test=df_test,cl=df_train_labels,k=i)
  error.rate[i] <- mean(df_test_labels!= df_test_pred)
}



library(ggplot2)
k.values <- 1:50
error.df <- data.frame(error.rate,k.values)
ggplot(error.df,aes(k.values,error.rate))+geom_point()+geom_line(lty='dotted')

