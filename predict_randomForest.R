setwd("/home/balki/kaggle/SanFransiscoCrime Classification/")

map_date <-function(crime_df){
  #Convert into POSIXct date time object
  crime_df$Dates<-ymd_hms(crime_df$Dates)
  #get the Year,Month and Hour
  crime_df$Hour<-factor(hour(crime_df$Dates))
  crime_df$Year<-factor(year(crime_df$Dates))
  crime_df$Month<-factor(month(crime_df$Dates))
  return(crime_df)
}
# Load the train data into R
data<-read.csv("train.csv",sep=",")
library(dplyr)
library(ggplot2)
library(lubridate)
sfcrime_tbl<-map_date(tbl_df(data))
#get the summary of data
glimpse(sfcrime_tbl)
#Get the count of each Crime and arrange in descending order and get the top 20 crimes
count<-sfcrime_tbl %>% group_by(Category) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(n=20)
# Plot bar graph of Top 20 Crimes
ggplot(data=count, aes(x=reorder(Category, -count),y=count,color=Category,fill=Category)) +geom_bar(stat="identity")+coord_flip()+xlab("Type Of Crime")+ylab("Number of Crimes")+ggtitle("Number of Crime Incidents by Category")
ggsave(filename="CrimeByCategory.png",plot=last_plot(),scale=2)
#What are the kind of events in Other Offences
#Get Descript for the other offences
other_offences<-filter(sfcrime_tbl,Category=="OTHER OFFENSES") %>% select(Descript)
count_other_offences<- other_offences %>% group_by(Descript) %>% summarise(count=n()) %>% arrange(desc(count))
#Drivers Licence,Suspended or Revoked, Traffic Violation,Traffic Violaion Arrest all come under Traffic Crime
ggplot(data=count_other_offences, aes(x=reorder(Descript, -count),y=count,fill=Descript,color=Descript)) +geom_bar(stat="identity")+coord_flip()+xlab("Type Of Other Offences")+ylab("Number of Crimes")+ggtitle("Other Offences")
ggsave(filename="OtherOffences.png",plot=last_plot(),scale=2)

count_time<-sfcrime_tbl %>% group_by(DayOfWeek,Month,Year) %>%summarise(count=n())
# Whatis the effect of day of the week on Crime Rates
ggplot(count_time, aes(x=DayOfWeek, y=count, fill=DayOfWeek)) + geom_boxplot()+ylab("Number of Incidents")+ggtitle("Crime Rates Vs Day Of Week")
ggsave(filename="CrimeByDayOfWeek.png",plot=last_plot(),scale=2)

# Has crime rate increased or decreased over the year
ggplot(count_time,aes(x=Year,y=count,fill=Year))+geom_boxplot()+ylab("Number of Incidents")+ggtitle("CrimeRates vs Year")
ggsave(filename="CrimeByYear.png",plot=last_plot(),scale=2)


#Is there any seasonal pattern in Crimes?? 
ggplot(count_time,aes(x=Month,y=count,fill=Month))+geom_boxplot()+ylab("Number of Incidents")+ggtitle("CrimeRates vs Month")
ggsave(filename="CrimeByMonth.png",plot=last_plot(),scale=2)

#How the top 20 crimes vary on a monthly and hourly basis?
#Get the top 20 crime categories
top_20_crimes<-select(count,Category)
data_plot<-sfcrime_tbl %>% group_by(DayOfWeek,Month,Hour,Category)
