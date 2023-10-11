#3.8
x <- 1
n <- 100
for(k in 1:n) {
  x <- x*k
}
print(x)

#3.9
prod(1:100)

#3.10
factorial(100)

#3.11
exact<-1:100
for (i in 1:100){
  exact[i]<-factorial(i)
}

estim<-1:100
for (i in 1:100){
  estim[i]<-(i/exp(1))^(i)*sqrt(2*pi*i)
}


#3.12
rel.err <- abs(exact-estim)/exact
plot(1:length(rel.err), rel.err, xlab = "k")
plot(1:length(rel.err), log10(rel.err), xlab = "k")


#3.13
obs <- sample(6,60,replace=T, prob=c(0.15,0.15,0.15,0.15,0.15,0.25))
table(obs)


#3.14
obs.test.stat <- mean(obs)

n=1000000  
test.stat <- rep(0, n)
for(i in 1:n) {
  sim <- sample(1:6, 60, replace = TRUE)
  test.stat[i] <- mean(sim)
}
#hist(test.stat)
mean(test.stat > obs.test.stat)


#3.15
#actual p value=0.055


#3.16
N=1000
pvals <- rep(0,N)

for (j in 1:N){

M <- 1000

predicted <- rep(0,M)
for (i in 1:M){
  predicted[i] <- max(sample(107, 5, replace=F))
}
pvals[j] <- mean(61>=predicted)
}

hist(pvals)
abline(v=0.05596113,col='red')
abline(v=mean(pvals), col='blue')


#3.18
mean(pvals<0.05)


#3.19(More MC samples)
N=1000
pvals <- rep(0,N)

for (j in 1:N){
  
  M <- 2000
  
  predicted <- rep(0,M)
  for (i in 1:M){
    predicted[i] <- max(sample(107, 5, replace=F))
  }
  pvals[j] <- mean(61>=predicted)
}

hist(pvals)
abline(v=0.05596113,col='red')
abline(v=mean(pvals), col='blue')

#Doubbline the number of MC rounds (1000 -> 2000)
#increased the accuracy of the test (0.182 -> 0.12)
mean(pvals<0.05)
