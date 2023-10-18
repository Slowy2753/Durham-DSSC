
#3.20
sugars <- c(1.9,2.6,2.3,1.9)
pH <- c(3.51,3.20,3.26,3.16)
alcohol <- c(9.4,9.8,9.8,9.8)
Wine <- data.frame(sugars,pH,alcohol)

summary(Wine)


#3.21
#3.22
#3.23
plot(winequality.red$sulphates,winequality.red$quality)

#3.24
summary(winequality.red)
high.sulphate.red <- subset(winequality.red, sulphates>0.62)
low.sulphate.red <- subset(winequality.red, sulphates<=0.62)

#3.25
plot(ecdf(low.sulphate.red$quality))

#3.26
plot(ecdf(low.sulphate.red$quality))
lines(ecdf(high.sulphate.red$quality),col='red')
legend('topleft', c('High Sulphate', 'Low Sulphate'),
       fill=c('red','black'))

#3.27
low.ecdf <- ecdf(low.sulphate.red$quality)
high.ecdf <- ecdf(high.sulphate.red$quality)
low.ecdf(5)
1-high.ecdf(7-1)

#3.28
t.test(low.sulphate.red$quality)
t.test(high.sulphate.red$quality)

#3.29
high.sds <- rep(0,1000)
low.sds <- rep(0,1000)

for (i in 1:1000){
  sample.low <- sample(low.sulphate.red$quality, replace=T)
  low.sds[i] <- sd(sample.low)

  sample.high <- sample(high.sulphate.red$quality, replace=T)
  high.sds[i] <- sd(sample.high)
}
sd(low.sulphate.red$quality)
sd(low.sds)

#3.30
winequality.red$col=factor('Red')
winequality.white$col=factor('White')

winequality <- rbind(winequality.red, winequality.white)

#3.31
summary(winequality)
