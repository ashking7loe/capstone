# metapackage with lots of helpful functions
library(tidyverse) 
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(plotly)

#Reading the Data
data=read.csv(file.choose(), header = TRUE)
View(data)

#Data Preprocessing
nrow(data)
dim(data)
colnames(data)
head(data)
tail(data)
str(data)
glimpse(data)

#Getting summary of Data
summary(data)
#Missing Values in Dataset
sapply(data, function(x) sum(is.na(x)))
sapply(data, FUN = class)

#New dataset without missing values
newdataset = data[complete.cases(data),]
View(newdataset)

#Exploratory Data Analysis
#Browsing the Dataset
library(DT)
datatable(data =newdataset , 
          rownames = FALSE, 
          filter = "top",
          options = list(autoWidth = TRUE))

#plotting
table(data$Passholder.Type)
qplot(data$Passholder.Type)
table(data$Trip.Route.Category)
qplot(data$Trip.Route.Category)
head(data)
head(data$Start.Time)
library(lubridate)
data$usetime<-ymd_hms(data$End.Time)-ymd_hms(data$Start.Time)
data$usetime<-as.numeric(data$usetime)
print(data$usetime)
summary(data$usetime)
#boxplot
boxplot(data$usetime)$stats
#Checking NA values
data$usetime<-ifelse(data$usetime<=1|data$usetime>=36,NA, data$usetime)
print(data$usetime)
table(is.na(data$usetime))

bike<- data %>% filter(!is.na(usetime))
print(bike)
ggplot(data=data, aes(x=Passholder.Type, y=usetime))+geom_boxplot()+ggtitle("usetime by pass holder")

#Histogram
hist(data$usetime, breaks = 30,
     main = "Using time", col = "Grey", xlab="time", ylab = "number")

#Scatterplot between duration and starting date
attach(data)
plot(Stating.Date,Duration,main="Scatterplot",xlab="Duration",ylab="Starting Date",pch=10)

#group_by 
ggplot(data=data, aes(x=Passholder.Type, y=usetime))+geom_boxplot()+ggtitle("summary using time by Passholder Type")

#Roundtrip
round_journey<- bike %>%
        filter(Trip.Route.Category=="Round Trip")
round_journey %>%
        group_by(Passholder.Type) %>%
        summarise(mean=mean(usetime))
head(round_journey)
#passholder type round_journey 
ggplot(data=round_journey, aes(x=Passholder.Type, y=usetime))+geom_boxplot()+ggtitle("round_journey")

#Onewaytrip 
oneway_journey<- bike %>%
        filter(Trip.Route.Category=="One Way")

oneway_journey %>%
        group_by(Passholder.Type) %>%
        summarise(mean=mean(usetime))
#passholder type Oneway_journey 
ggplot(data=oneway_journey, aes(x=Passholder.Type, y=usetime))+geom_boxplot()+ggtitle("oneway_journey")

#oneway_journey starting station & Ending station
tail(oneway_journey)
table(oneway_journey$Starting.Station.ID)
table(oneway_journey$Ending.Station.ID)


oneway_starting_station_freq<-ggplot(data=oneway_journey,
                                     aes(x=oneway_journey$Starting.Station.ID))+geom_bar(fill='#FF9900')+
        coord_cartesian(xlim = c(3000, 3100))
ggplotly(oneway_starting_station_freq)


oneway_ending_station_freq<-ggplot(data=oneway_journey,
                                   aes(x=oneway_journey$Ending.Station.ID))+geom_bar()+coord_cartesian(xlim = c(3000, 3100))

ggplotly(oneway_ending_station_freq)

#oneway starting.station id | passholder type
oneway_starting_station_id<- bike %>%
        filter(Trip.Route.Category=="One Way") %>%
        select(Starting.Station.ID,Passholder.Type)

#starting station ID 
oneway_starting_station_count<-oneway_starting_station_id %>%
        group_by(Starting.Station.ID) %>%
        tally()

#oneway ending station id | passholder type
oneway_ending_station_id<- bike %>%
        filter(Trip.Route.Category=="One Way") %>%
        select(Ending.Station.ID, Passholder.Type)
#ending station id
oneway_ending_station_count<-oneway_ending_station_id %>%
        group_by(Ending.Station.ID) %>%
        tally()

#
starting_count_new<-oneway_starting_station_count 

starting_count_new<-rename(starting_count_new, station = Starting.Station.ID) 
starting_count_new<-rename(starting_count_new, startcount = n)
#
ending_count_new<-oneway_ending_station_count 
#
ending_count_new<-rename(ending_count_new, station = Ending.Station.ID)
ending_count_new<-rename(ending_count_new, endingcount = n)
#
starting_count_new<-starting_count_new[-c(65,66),]
ending_count_new<-ending_count_new[-c(65,66),]
#Left Join
total<-left_join(starting_count_new, ending_count_new, by="station")


library(reshape2)
total_1<-melt(total, id=(c("station")))

f1<-ggplot(data=total_1, aes(x=station))
f2<-f1+geom_bar(aes(y=startcount), colour="Red", size=1)
f3<-f2+geom_bar(aes(y=endingcount), colour="blue", size=1)

head(total_1)
h1<-ggplot(data=total_1, aes(x=station, y=value, fill=variable))+geom_bar(stat="identity", position = "dodge")+xlim(x=c(3000,3100))

ggplotly(h1)           
h1

#Linear Regression
x<-c(data$Ending.Station)
print(x)
y<-c(data$Duration)
print(y)
relation<-lm(y~x)
summary(relation)

result<-predict(relation)
print(result)

#Plotting graphs using plotly
library(plotly)
plot_ly(data=data,x=~Trip.ID,y=~Duration,type ="scatter",mode="markers")
plot_ly(data=data,x=~Trip.ID,y=~Duration,type ="scatter",mode="markers",
        color=I("black"))

plot_ly(data=data,x=Trip.ID,y=~Duration,type ="scatter",mode="markers",
        marker=list
        (color="green",size=2))

plot_ly(data=data,x=~Trip.ID,y=~Duration,type ="scatter",mode="markers",
        marker = list(size = 7,
                      color = 'rgba(255, 182, 193, .9)',
                      line = list(color = 'rgba(152, 0, 0, .8)',
                                  width = 0.5)))

route=plot_ly(data=data, x = ~Trip.Route.Category, color = ~Stating.Date, type = "box")
print(route)
ID=plot_ly(data=data, x = ~Trip.ID, color = ~Stating.Date, type = "box")
print(ID)
Pass=plot_ly(data=data, x = ~Passholder.Type, color = ~Stating.Date, type = "box")
print(Pass)


