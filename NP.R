#### Pract1
#1
rm(list = ls())
x=c(4.17,8.42,3.02,2.89,9.77,6.06,2.72,5.12,6,4.78,2.62,7.20,1.61,5.92,7.25,8.01,4.76,5.36,5.34,7.59,0.66,7.27,3.39,1.40)
q=seq(-1,11,0.01)
ptri=function(a,b,c,x){
  out=c()
  for (i in 1:length(x)) {
  if (x[i] <= a) {
    out[i]=0
  } else if (a<x[i] & x[i]<=c) {
    out[i]=((x[i]-a)^2)/((b-a)*(c-a))
  } else if (c<x[i] & x[i]<b) {
    out[i]=(1-((b-x[i])^2)/((b-a)*(b-c)))
  } else if (b<=x[i]) {
    out[i]=(1)
  } else out[i]=0}
  return(out)
}
library(ggplot2)
ggplot(data = NULL,aes(x)) + stat_ecdf(geom = "step") + geom_line(data = NULL, aes(q,punif(q,0,10)), col="red") + geom_line(data = NULL, aes(q,ptri(0,10,5,q)), col="blue")
#Both Uniform Distribution and Triangular Distributions seem to be good fit

#2
rm(list = ls())
McAlpha=c(0,0,1,2,3,9,14,22,23,29,33,41,41,42,44,52,56,57,58,
          58,60,62,63,64,65,69,72,72,73,74,74,75,76,75,77,77,
          78,78,79,79,80,81,81,81,81,82,82,
          83,84,84,85,86,87,87,88,90,92,93,95)
McBeta=c(0,19,22,30,31,37,55,56,66,66,67,67,68,71,73,75,75,78,79,82,83,83,88,96)
McGamma=c(13,13,22,26,33,33,59,72,72,72,77,78,78,80,81,82,85,85,85,86,88)
McDelta=c(1,11,13,13,16,34,65,68,74,77,83,83,87)
summary(McAlpha)
summary(McBeta)
summary(McGamma)
summary(McDelta)

par(mfrow=c(2,4))
boxplot(McAlpha, main="Boxplot of McAlpha")
boxplot(McBeta, main="Boxplot of McBeta")
boxplot(McGamma, main="Boxplot of McGamma")
boxplot(McDelta, main="Boxplot of McDelta")

hist(McAlpha, prob=TRUE)
lines(density(McAlpha),col="red")

hist(McBeta, prob=TRUE)
lines(density(McBeta),col="red")

hist(McGamma, prob=TRUE)
lines(density(McGamma),col="red")

hist(McDelta, prob=TRUE)
lines(density(McDelta),col="red")

library(ggplot2)
ggplot(data = NULL,aes(McAlpha)) + stat_ecdf(geom = "step")
ggplot(data = NULL,aes(McBeta)) + stat_ecdf(geom = "step")
ggplot(data = NULL,aes(McGamma)) + stat_ecdf(geom = "step")
ggplot(data = NULL,aes(McDelta)) + stat_ecdf(geom = "step")

######### Pract 2
rm(list=ls())
par(mfrow=c(
x=c(21,44,141,6,18,39,48,11,42,11,12,35,24,83,50,91,81,42,61,147,12,94,35,83,94,2,111,91,117,11,73,48,88,33,5,22,210,91,44,17)
hist(x)

######### Pract-8 ##########
#Bootstrap
rm(list = ls())
set.seed(100)
heights=c(175,168,170,168,162,165,167,155)
B=1000 #No. of bootstrap sample
bootstrap_medians=replicate(B,{sample_data=
  sample(heights,size = length(heights),replace = TRUE)
median(sample_data)})
bootstrap_median=median(bootstrap_medians);bootstrap_median
bootstrap_se=sd(bootstrap_medians);bootstrap_se
CI=quantile(bootstrap_medians,c(0.025,0.975));CI
#plot distn of bootstrap medians
hist(bootstrap_medians,breaks=30,main="Bootstrap distn of median",xlab="Median")
abline(v=CI)
abline(v=bootstrap_medians)

############ Pract-10 ###############  
#Jacknife Estimator
#1
rm(list = ls())
dat=c(2.1,2.4,2.8,3.0,3.3,3.7,3.9)
n=length(dat)
original_mean=mean(dat)
leave_one_out_means=numeric(n)
for(i in 1:n)
  leave_one_out_means[i]=mean(dat[-i])
jacknife_mean=mean(leave_one_out_means);jacknife_mean
bias=(n-1)*(jacknife_mean-original_mean);bias
jacknife_variance=((n-1)/n)*sum((jacknife_mean-original_mean)^2);jacknife_variance



#2
rm(list = ls())
set.seed(100)
n=10
x=sample(1:100,size = n)
#Jacknife estimator SE
M=numeric(n)
for(i in 1:n){
  y=x[-i]
  M[i]=median(y)
}
Mbar=mean(M)
jacknife_se=((n-1)/n)*sum((Mbar-M)^2)

#Bootstrap SE
B=1000
bootstrap_medians=replicate(B,{sample_data=sample(x,size = length(x),replace = TRUE)
median(sample_data)})
bootstrap_median=median(bootstrap_medians)
bootstrap_se=sd(bootstrap_medians)
cat("Jacknife estimate of SE:",jacknife_se,"\n","Bootstrap estimate of SE:",bootstrap_se,"\n")
