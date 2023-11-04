B=10000

v <- function(x){
  
  if (x<1 & x>=0){ 0}
  else if (x<12 & x>=1){ (200*x-200)}
  else {2200}
  
}




for (i in 1:B){
  if (i==1){list=1:B}
  
  samp1 <- rexp(1, 1/2.088253613)
  samp2 <- rexp(1, 1/2.088253613)
  samp3 <- rexp(1, 1/2.088253613)
  samp4 <- rexp(1, 1/2.088253613)
  
  V_samp <- c( v(samp1),  v(samp2), v(samp3), v(samp4) )
  
  list[i] <- mean(V_samp)
  
}

sd(list)
