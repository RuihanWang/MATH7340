rm(list = ls()) 
#Problem 2

f_x <- function(x)2^x/gamma(x+1)*exp(-2);
f_x(1);
x_range <- seq(0,3,by=1)
p_03 <- sum(f_x(x_range))
p_03

#Problem 4
rm(list = ls())
1-sum(dbinom(3,size=3,p=0.25));

X.range<-(0:3);
EX <- sum(X.range*dbinom(X.range,size=3,p=0.25));
VarX<-sum((X.range-EX)^2*dbinom(X.range,size=3,p=0.25));

#Problem 5
rm(list = ls())
integrate(function(x) dchisq(x,df=3), lower=1, upper=4)$value

integrate(function(x) x*dchisq(x,df=3), lower=Inf, upper=Inf)$value
integrate(function(x) (x-3)^2*dchisq(x,df=3), lower=Inf, upper=Inf)$value
x<-rchisq(n=100000, df=3)
mean((1<x) & (x<4))

#Problem 7
rm(list = ls())
integrate(function(x) dnorm(x,mean=1.6,sd=0.4), lower=1, upper=1.6)

x<-rnorm(n=500000, mean=1.6,sd=0.4)
mean((1<x) & (x<1.6))

dbinom(2,size=5,p=0.43307)


#Problem 8
rm(list = ls())
integrate(function(x) x*df(x,df1=2,df2=5), lower=Inf, upper=Inf)$value
integrate(function(x) (x-1.6667)^2*df(x,df1=2,df2=5), lower=Inf, upper=Inf)$value

integrate(function(x) x*df(x,df1=10,df2=5), lower=Inf, upper=Inf)$value
integrate(function(x) (x-1.6667)^2*df(x,df1=10,df2=5), lower=Inf, upper=Inf)$value


