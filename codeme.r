install.packages("ggmap")
library(jsonlite)
library(maps)
library(ggplot2)
library(ggmap)

df<- fromJSON("data.json")

View(df)
str(df)

df1<- data.frame(df$data)
df2<- data.frame(df1$location) 
df1$location<-NULL

df_combine<- cbind(df1,df2)

for(i in c(1:5))
{
df_combine[,i]<- as.factor(df_combine[,i])
}

a<- c(6,7,11,12,14)
for(i in a)
{
  df_combine[,i]<- as.factor(df_combine[,i])
}
d<- df_combine

# feature Engineering
list_ofnumber<- d$client_time 
difference<-1
for(i in 1:50000){
difference[i]<- list_ofnumber[i+1] - list_ofnumber[i] 
}
d$time_spent<- difference 


ggplot(data=d,aes(d$category))+
  geom_bar(stat = "count",fill="red",color="black",alpha=.6)+
  labs(x="category" ,y ="count",title="Number of observations per Category")


ggplot(data=d,aes(d$event_name))+
  geom_bar(stat = "count",aes(fill=d$category))+
  labs(x="Event name " ,y ="count",title="Category Vs Event type",fill="Category")


ggplot(data=d,aes(d$event_name))+
  geom_bar(stat = "count",aes(fill=d$gender))+
  labs(x="Event Type" ,y ="Count",title="Type of project VS Gender",fill="Gender")


ggplot(data=d,aes(d$event_name))+
  geom_bar(stat = "count",aes(fill=d$age))+
  labs(x="Event Type" ,y ="Count",title="Type of project VS Age",fill="Age")


ggplot(data=d,aes(d$gender))+
  geom_bar(stat = "count",aes(fill=d$device))+
  labs(x="Gender" ,y ="Count",title="Type of device VS Gender",fill="Type of device")


ggplot(data=d,aes(d$device))+
  geom_bar(stat = "count",aes(fill=d$age))+
  labs(x="Type of device" ,y ="Count",title="Type of device VS Age",fill="Age")

ggplot(data=d,aes(x= c(1:50000),y=d$time_spent))+
  geom_point(aes(color=d$event_name))+
  labs(x="client number",y = "time spent by client",color= "Event type",
       title="Time Spent By Client")

summary(d$time_spent)
# max = 696.00
# mean = 53.57
#median = 34
#1st Quartile = 14
#3rd Quartile = 72
