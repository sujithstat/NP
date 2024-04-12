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

boxplot(McAlpha)
boxplot(McBeta)
boxplot(McGamma)
boxplot(McDelta)

hist(McAlpha)
lines(McAlpha)

hist(McBeta)
lines(McBeta)

hist(McGamma)
lines(McGamma)

hist(McDelta)
lines(McDelta)

library(ggplot2)
ggplot(data = NULL,aes(McAlpha)) + stat_ecdf(geom = "step")
ggplot(data = NULL,aes(McBeta)) + stat_ecdf(geom = "step")
ggplot(data = NULL,aes(McGamma)) + stat_ecdf(geom = "step")
ggplot(data = NULL,aes(McDelta)) + stat_ecdf(geom = "step")
