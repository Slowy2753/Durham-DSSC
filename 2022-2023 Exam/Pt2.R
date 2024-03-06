library("ggplot2")
library(stringr)
library(dplyr)
library("lubridate")

#11.1
ggplot(ufo, aes(y=ufo_shape))+
  geom_bar()

#11.2
ufo2<-ufo
ufo2$ufo_shape[is.na(ufo2$ufo_shape)]<-"unknown"
ufo2$ufo_shape<-str_to_sentence(ufo2$ufo_shape)
ufo2$ufo_shape<-as.factor(ufo2$ufo_shape)
ufo2$ufo_shape<-reorder(ufo2$ufo_shape, ufo2$ufo_shape, FUN = length, decreasing=TRUE)


ggplot(ufo2, aes(y=ufo_shape))+
  labs(x='Number of observerd UFOs', y='UFO Shape')+
  geom_bar()

#11.3
ufo3<-ufo2
ufo3$date_time<-strptime(as.character(ufo3$date_time), "%m/%d/%Y %H:%M")

ufo3$year<-year(ufo3$date_time)
ufo3$month<-month(ufo3$date_time, label=T, abbr=F)
ufo3$day<-mday(ufo3$date_time)
ufo3$wday<-wday(ufo3$date_time, label=T)
ufo3$hour<-hour(ufo3$date_time)
ufo3$min<-minute(ufo3$date_time)

ggplot(ufo3)+
  geom_histogram(aes(year),binwidth = 5)+
  labs(x='Year')+
  geom_vline(xintercept=1993, color='red')
  

#11.4
ufo4<-subset(ufo3, ufo$latitude>0)


for (i in 1:length(ufo4$date_time)) {
  if(i==1){seasons=c()}
  if(ufo4$month[i] %in% c('December','January','February')){
    seasons<-append(seasons, 'Winter')}
  else if(ufo4$month[i] %in% c('March','April','May')){
    seasons<-append(seasons, 'Spring')}
  else if(ufo4$month[i] %in% c('June','July','August')){
    seasons<-append(seasons, 'Summer')}
  else if(ufo4$month[i] %in% c('September','October','November')){
    seasons<-append(seasons, 'Autumn')}
  else{print(as.character(ufo4$month[i]))}
}
ufo4$season<-seasons

#2rhs is up

#need to group by season and hour then calculate counts

#11.5

#Go through each observation take the country, find it in the counteries csv then take the numerical code
#then find the region with that sub code (not oo bad tbh)


#11.6

#subset of the citing with the right country codes
#plot the longatudes and latitudes  




