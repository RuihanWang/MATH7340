#Problem 2
rm(list = ls()) 
set.X<-c(seq(1500,2000),by = 1)
sum(dbinom(set.X,size=2000,p=0.7))

#Problem 3
n = 20
rm(list = ls()) 
require(mvtnorm) 
set.p<-c()
for(j in 1:run_count) {

count<-0
total <- 10000
for (i in 1:total) {
   
   my_data<-rmvnorm(50, mean=c(9,10), sigma=matrix(c(3,2,2,5), nrow=2))
   mean.y<-apply(my_data,2,mean)
   mean
   if (mean.y[1]+0.5<mean.y[2]) {
     count = count + 1
   }
  
   
   
}

p = count/total 

set.p<-append(set.p,p)


}

error <- qnorm(0.975)*var(set.p)/sqrt(n)
mean(set.p)-error;mean(set.p)+error


#Problem 4
rm(list = ls()) 

x1<-matrix(rchisq(10000*1000, df = 10),nrow=1000)

x2<- matrix(rgamma(10000*1000, shape = 1,scale = 2), nrow=1000) 

x3<- matrix(rt(10000*1000,df=3), nrow=1000) 
y<-(x1^(1/2))*x2+4*(x3^2)
mean.y<-apply(y,1,mean) 
var.y<- apply(y,1,var)
mean(mean.y) + c(-1,1)*1.96*sqrt(var(mean.y)/1000) 

mean(var.y) + c(-1,1)*1.96*sqrt(var(var.y)/1000) 

