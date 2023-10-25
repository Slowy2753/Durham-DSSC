
library(ggplot2)

Diamond_Data <- diamonds

#3.33/#3.35
summary(Diamond_Data)

#3.34
Diamond_Data[1:10]

#3.36
D.1 <- subset(Diamond_Data, cut=='Ideal' & color=='D' & depth<=60)
sum(D.1$price)

#3.37
Diamond_Data$ppc <- Diamond_Data$price / Diamond_Data$carat
summary(Diamond_Data$ppc)

#3.38
plot(Diamond_Data$carat, Diamond_Data$ppc)

#3.39
abline(v=1)
abline(v=2)
abline(v=3)
abline(h=mean(Diamond_Data$ppc))

#3.40
D.2 <- subset(Diamond_Data, ppc> 10000 & carat>1 & carat<2)
D.3 <- subset(Diamond_Data, ppc <= 10000 & carat>1 & carat<2)

par(mfrow=c(2,2))

barplot(table(D.2$clarity), main='ppc>10k clarity')
barplot(table(D.3$clarity), main='ppc<10k clarity')
barplot(table(D.2$color), main='ppc>10k color')
barplot(table(D.3$color), main='ppc<10k color')


#3.41
#Use data viewer

# 2.29
# Premium
# I
# VS2
# 60.8
# 60.0
# 18823
# 8.50
# 8.47
# 5.16
# 8219.651
table(Diamond_Data$clarity)

#3.42
D.4 <- subset(Diamond_Data, carat>2.29 & cut=='Ideal' & clarity %in% c('VS1','VVS2','VVS1','IF'))

18823-min(D.4$price)
