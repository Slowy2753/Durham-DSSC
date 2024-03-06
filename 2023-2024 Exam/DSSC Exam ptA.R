library('tidyr')
library('ggplot2')

#1
mean(x =c(1,2,3,NA), na.rm=T)

?complete

#4
data(txhousing)

#7
table(txhousing$city)

#8
k<-subset(txhousing, city=='Port Arthur')
k<-subset(k, year==2007)
k<-subset(k, month==6)

#9
txhousing$mean <- txhousing$volume/txhousing$sales
txhousing$ind<- txhousing$mean>txhousing$median
table(txhousing$ind )

#10
for (i in 2000:2015){
  if(i==2000){yearly_sales<-c()}
  
  temp<-subset(txhousing, year==i)
  temp$sales<-temp$sales %>% replace(is.na(.), 0)
  
  yearly_sales<-append(yearly_sales, sum(temp$sales))
}
anual_sales<-as.data.frame(cbind(2000:2015, yearly_sales))

