#install.packages("ggplot2movies")

library(ggplot2)
library(ggplot2movies)

movies <- movies

#4.45
max(movies$budget[is.na(movies$budget)==F])

#4.46
movies.wbudges <- movies$budget[is.na(movies$budget)==F]
  
#4.47
hist(movies$length)
hist(movies$length[movies$length<300])

#4.48
boxplot(movies$length)
boxplot(movies$length[movies$length<300])

#4.49
hist(movies$length[movies$length<180],breaks=190)

#4.50
?hist
#(a,b]
table(movies$length)
#85 mins long


#4.51
plot(movies$year, movies$length, ylim=c(0,500))

#4.52

for (i in 1:12){
  if (i==1){
    start_yr=1891
    medians<-c(1:12)
    straps<-c(1:12)
    years<-c(1:12)
    upper<-c(1:12)
    lower<-c(1:12)
    }
  
  assign(paste0("Movies",start_yr), subset(movies$length, movies$year %in% c(start_yr:(start_yr+9))))
  x<-subset(movies$length, movies$year %in% c(start_yr:(start_yr+9)))
  
  years[i] <- as.integer(start_yr)
  start_yr = start_yr+10
  
  
  N=10000
  for (j in 1:N){
    if (j==1){bootstraps <- c(1:N)}
    
    bootstraps[j] <- median(sample(x, replace=TRUE))
  }
  straps[i] <- sd(bootstraps)
  medians[i] <- median(x)
  
  lower[i]<- as.integer(quantile(bootstraps, probs=0.005))
  upper[i]<- as.integer(quantile(bootstraps, probs=0.995))
}

data <- as.data.frame(cbind(years, straps, medians,lower,upper))

plot(data$years, data$lower, type="l", col='red')
points(data$years, data$medians, type="l", col='black')
points(data$years, data$upper, type="l", col='green')
