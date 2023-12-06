library("tidyverse")
library("lubridate")

data("flights", package = "nycflights13")



flights$sched_dep_hour<-floor(flights$sched_dep_time/100)
flights$sched_dep_min<-(flights$sched_dep_time-100*flights$sched_dep_hour)

flights <- flights |> 
  mutate(sched_dep = make_datetime(year, month, day, sched_dep_hour, sched_dep_min, tz = "America/New_York")) |> 
  select(-sched_dep_hour, -sched_dep_min)


flights |> 
  mutate(day_of_week = wday(sched_dep, label = TRUE)) |> 
  group_by(day_of_week) |> 
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE))



bootstrap.se<- function(x){
  
  N=200
  
  S <- mean
  
  samples <- c(1:N)
  for (i in 1:N) {
    samples[i]<-S( sample(x, replace=T) )
  }
  
  return(sd(samples))
}


mean_ci <- flights |> 
  filter(dep_delay >= 0) |> 
  mutate(day_of_week = wday(sched_dep, label = TRUE)) |> 
  group_by(day_of_week) |> 
  summarise(mean_delay = mean(dep_delay, na.rm = TRUE),
            bootstrap_se = bootstrap.se(dep_delay)) |> 
  mutate(mean_ci_lower = mean_delay - 1.96*bootstrap_se,
         mean_ci_upper = mean_delay + 1.96*bootstrap_se)

ggplot(mean_ci, aes(x=mean_delay, y= day_of_week))+
  geom_point()+
  geom_errorbarh(aes(xmin=mean_ci_lower, xmax=mean_ci_upper))


