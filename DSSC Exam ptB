library(ggplot2)
library(gridExtra)
library(ggpubr)
library(dplyr)
library(lubridate)

#11.1
colony2<-colony
colony2$months<-as.factor(colony2$months)
colony2$months<-ordered(colony2$months, 
                                    levels=c('January-March',
                                   'April-June',
                                   'July-September',
                                   'October-December'))
names(colony2)[names(colony2)=='months']<-'quarter'
colony2<-subset(colony2, state!='United States')

stressor2<-stressor
stressor2$months<-as.factor(stressor2$months)
stressor2$months<-ordered(stressor2$months, 
                        levels=c('January-March',
                                 'April-June',
                                 'July-September',
                                 'October-December'))
names(stressor2)[names(stressor2)=='months']<-'quarter'
stressor2<-subset(stressor2, state!='United States')

#11.2

ggplot(colony2, aes(x=quarter, y=colony_reno))+
  geom_boxplot()
  
ggplot(colony2, aes(x=quarter, y=colony_reno))+
  scale_y_log10()+
  labs(x='Quarter', y='Number of bee colony renovatiovations')+
  geom_boxplot()

#11.3
stressor2_half1<-subset(stressor2, stressor %in% c('Disesases','Other','Other pests/parasites'))
stressor2_half2<-subset(stressor2, stressor %in% c('Pesticides','Unknown','Varroa mites'))

q1<-ggplot(stressor2_half1, aes(x=stress_pct))+
  geom_histogram(binwidth = 5)+
  xlim(0,100)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_grid(~stressor)

q2<-ggplot(stressor2_half2, aes(x=stress_pct))+
  geom_histogram(binwidth = 5)+
  xlim(0,100)+
  labs(x='Percentage of colonies')+
  facet_grid(~stressor)

ggarrange(q1, q2, nrow=2)


#11.4
stressor_wide<-pivot_wider(stressor2, names_from = stressor, values_from = stress_pct)

colony_full<-left_join(colony2, stressor_wide, by = join_by(year, quarter, state)) 

#11.5
colony3<-colony2

colony3$date<-as.Date(lubridate::ymd(colony3$year, truncated = 2L) %m+% months( 3*(match(colony3$quarter, levels(colony3$quarter))-1)   )  )

temp11_5<-subset(colony3, state %in% c('California', 'Connecticut', 'Texas'))

ggplot(temp11_5,aes(x=date, y=colony_n))+
  geom_line(aes(color=state))

#11.6

for (i in 1:length(unique(colony3$date))){
  if(i==1){
    added<-c()
    lost<-c()
  }
  
  temp11_6<-subset(colony3, date==as.Date(as.character(unique(colony3$date)[i])))
  temp11_6$colony_added[is.na(temp11_6$colony_added)] <- 0
  temp11_6$colony_lost[is.na(temp11_6$colony_lost)] <- 0
  
  added<-append(added, sum(temp11_6$colony_added))
  lost<-append(lost, sum(temp11_6$colony_lost))
}
temp11_6_2<-as.data.frame(cbind(added,lost))
temp11_6_2$date<-as.Date(as.character(unique(colony3$date)))
temp11_6_3<-pivot_longer(temp11_6_2, cols=c('added','lost'),names_to = 'type', values_to = 'value')

ggplot(temp11_6_3)+
  geom_line(aes(y=value,x=date,col=type))

#(almost there but I cant get it to format as a barchart,
#additionally need to add Q1 and Q2 of each year to get the bar
#chart to have the desired number of bars per year)


  
