#Feature Engineering for San Franscisco Crime Classification
# Som possible Features are : 
#Whether crimne was committed on a weekday or a weekend
#Whether the crime was committed in the early mornings, or in the night or Rest of the day.
#(My guess is more crime is committed during the early morning and in the night)
# Which day of the week the crime was committed.
# Is there any particular month where the crime rate is high

library(data.table)
library(lubridate)
library(foreach)
library(randomForest)
library(dplyr)
convertDate <- function(df){
  df$Dates<-fast_strptime(x = df$Dates,format = '%Y-%m-%d %H:%M:%S',tz="UTC")
}

extractFeatures <- function(df){
  df$Hour<-hour(df$Dates)
  df$Year<-as.factor(year(df$Dates))
  df$Month<-as.factor(month(df$Dates))
  #Field Weekend<- Weekend=1 and Weekday=0
  df$Weekend<-as.factor(ifelse(df$DayOfWeek=='Friday'|df$DayOfWeek=='Saturday'|df$DayOfWeek=='Sunday',1,0))
  #Field TimeOfDay: 0 for night: between 10-6 and 1 for evening between 4-10 and 2 for restofThe Day
  df$TimeOfDay<-as.factor(with(df,ifelse(df$Hour>=22 | df$Hour <6,0,ifelse(df$Hour>=16 & df$Hour <22,1,2 ) )))
  df$Intersection<-as.factor(ifelse(grepl("/",df$Address),1,0))
  df$Hour<-as.factor(df$Hour)
  df$PdDistrict<-as.factor(df$PdDistrict)
  return(df)
}

categoryMat<- function(df)
{
  categoryMatrix<-data.frame(with(df,model.matrix(~Category+0)))
  names(categoryMatrix)<-sort(unique(df$Category)) 
  return(categoryMatrix)
}
train<-fread("train.csv")
head(train)
#make the Dates column into Date Time format
train$Dates<-convertDate(train)
#Extract month,day,hour, TimeOfDay,WhetherWeekendOrNot and Whether crime was committed in intersection or Not
train<-extractFeatures(train)
#categoryMatrix<-categoryMat(train)
# append category matrix to train
#train<-cbind(categoryMatrix,train)
#Apply Random Forest on train data 
#Split training into 70,30 split

train.tr.index<-sample(1:nrow(train),0.7*nrow(train)) 
train.tr<-train[train.tr.index,] 
train.tr<-train.tr[,c("Category","DayOfWeek","PdDistrict","Hour","Month","Weekend","TimeOfDay","Intersection"),with=FALSE]
train.test<-train[-train.tr.index,]
train.test<-train.test[,c("Category","DayOfWeek","PdDistrict","Hour","Month","Weekend","TimeOfDay","Intersection"),with=FALSE]
#Apply Random forest using for each package

rf<-foreach(ntree=rep(10,4),.combine = combine,.packages = 'randomForest') %dopar%
{
  training.Category<-train.tr[,c("Category"),with=FALSE]
  training<-train.tr[,-c(1),with=FALSE]
  randomForest(training, factor(training.Category),
               ntree=ntree,importance=TRUE)
}



