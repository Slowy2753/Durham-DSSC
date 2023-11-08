library(tidyverse)
library(nycflights13)

#4.53
#nope. should have votes and totals in different columns
# ie candidate, location, votes, total

#4.54

pres.res <- data.frame(
  Candidate = c("Clinton", "Trump", "Other"),
  California = c("8753788/14181595", "4483810/14181595", "943997/14181595"),
  Arkansas = c("380494/1130676", "684872/1130676", "65310/1130676")
)



pres.res2 <- pivot_longer(pres.res,
                          c("California","Arkansas"),
                          names_to = 'State',
                          values_to = 'Proportion')

#4.55
pres.res3 <- separate (pres.res2,
                         'Proportion',
                         c("Votes","Total"))


pres.res4 <- mutate(pres.res3, Votes = as.numeric(Votes), Total = as.numeric(Total))

#4.56
pres.res5 <- cbind(pres.res4, "Precentage"=pres.res4$Votes/pres.res4$Total )

#4.57
View(flights)

#4.58
table(flights$year)

num.flights <- flights |> 
  group_by(year, month, day) |> 
  summarise(count = n()) |> 
  arrange((day))|> 
  arrange((month))

#4.59
hrs_late_2 <- subset(flights, arr_delay>=120)
percentage_late <- as.data.frame(100* table(hrs_late_2$carrier)/table(flights$carrier))

View(airlines)
#Preference is US airways inc
#(this doesnt accoubnt for cancelled flights etc)

#4.60
hist(flights$dep_delay)
hrs_late_2_c <- subset(flights, arr_delay<120)
hist(hrs_late_2_c$dep_delay)

#4.61
summary(flights$dep_delay)

depp_delay_no_cheat <- flights
depp_delay_no_cheat$dep_delay[flights$dep_delay<0] <- 0

summary(depp_delay_no_cheat$dep_delay)

#4.62
clock_to_minutes <- function(x){
  hr <- trunc(x/100)
  min <- x-100*trunc(x/100)
  
  return((60*hr+min))
}

#4.63
flights$dep_time_mins <- clock_to_minutes(flights$dep_time)
flights$arr_time_mins <- clock_to_minutes(flights$arr_time)

#4.64
cancelled <- subset(flights, is.na(flights$dep_time)==T)

hrs_cancelled <- cbind( as.data.frame( table(cancelled$hour)), as.data.frame(table(flights$hour)))
hrs_cancelled$Var1 <- as.numeric(hrs_cancelled$Var1)
hrs_cancelled$prop <- hrs_cancelled$Freq/hrs_cancelled[4]

for (i in 1:length(hrs_cancelled$Var1)) {
  if(i==1){
    total_pre_lunch<-0
    total_post_lunch<-0
    cancelled_pre_lunch<-0
    cancelled_post_lunch<-0
  }
  if(hrs_cancelled$Var1[i]<12){
    total_pre_lunch = total_pre_lunch + hrs_cancelled[4][i]
    cancelled_pre_lunch = cancelled_pre_lunch + hrs_cancelled[2][i]
  }
  else{
    total_post_lunch = total_post_lunch + hrs_cancelled[4][i]
    cancelled_post_lunch = cancelled_post_lunch + hrs_cancelled[2][i]
  }
}
print(cancelled_pre_lunch/total_pre_lunch)
print(cancelled_post_lunch/total_post_lunch)
