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

