#3.3
pbirthday(221, classes=30, coincident = 12)


#3.4
?rnorm
rnorm(10, mean=2, sd=sqrt(2))

#3.5
x<-rnorm(100000, mean=2, sd=1)
xgrt2.13<-subset(x, x>=2.13)
100*length(xgrt2.13)/length(x)

?pnorm
#P(X>2.13)
pnorm(2.13, mean=2, sd=1, lower.tail=F)
#X<x=0.123
qnorm(0.123, mean=2, sd=1, lower.tail=T)


#3.6

n <- 10

type.I <- rep(0, 10000)
for(i in 1:10000) {
  # Generate some data
  x <- rnorm(n, mean = 3.14, sd = 1)
  
  # Compute test statistic
  test.stat <- (mean(x) - 3.14)/(1/sqrt(n))
  
  # Find the p-value of this test statistic
  p.val <- pnorm(-abs(test.stat))*2
  
  # Was this a type I error?
  type.I[i] <- (p.val<0.05)
}
#This gives the proportion of the time where the test had insignificant evidence
mean(type.I)


#3.7


n <- 10

type.II <- rep(0, 10000)
for(i in 1:10000) {
  # Generate some data
  #This would represent population mean==5 (unknown if data x is generated
  #through real-world sampling, or our h0)
  x <- rnorm(n, mean = 5, sd = 1)
  
  # Compute test statistic
  #the 3.14 represents our current estimate of the mean (our h1)
  test.stat <- (mean(x) - 3.14)/(1/sqrt(n))
  
  # Find the p-value of this test statistic
  p.val <- pnorm(-abs(test.stat))*2
  
  # Was this a type II error?
  type.II[i] <- (p.val>0.05)
}
#This gives the proportion of the time where the test incorrectly rejects the null
mean(type.II)