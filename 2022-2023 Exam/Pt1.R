

library("plyr")

data<-data(baseball)

summary(baseball)

unique(baseball$id)

for (i in 1:length(unique(baseball$id))){
  if(i==1){no_ab<-c()}
  if(sum(subset(baseball, id== unique(baseball$id)[i])$ab)==0){
    no_ab<-append(no_ab,unique(baseball$id)[i])
  }
}



baseball$decade<- 10*trunc(baseball$year/10)
length(unique(subset(baseball, decade==2000)$id))
hist(baseball$decade)


for (i in 1:length(unique(baseball$id))){
  if(i==1){mobile<-c()}
  if(length(unique(subset(baseball, id== unique(baseball$id)[i])$team))>=10){
    mobile<-append(mobile,unique(baseball$id)[i])
  }
}


for (i in max(baseball$year):min(baseball$year)) {
  if(i==1){
    year<-c()
    numb_unique_players<-c()
    total_hr<-c()
  }
  
  year<-append(year,i)
  
  year_data<-subset(baseball, year==i)
  
  numb_unique_players<-append(numb_unique_players, length(unique(year_data$id)))
  total_hr<-append(total_hr, sum(year_data$hr))
}
yearly_data<-as.data.frame(cbind(year,numb_unique_players,total_hr))
yearly_data$mean_hr_pp<-yearly_data$total_hr/yearly_data$numb_unique_players
