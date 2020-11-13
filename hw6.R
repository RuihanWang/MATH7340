#Problem 1
library(multtest)
library(matrixTests)
data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
h4j.ALL <- golub[2972, gol.fac=="ALL"]
h4j.AML <- golub[2972, gol.fac=="AML"]
aps.ALL <- golub[2989, gol.fac=="ALL"]
#a
t.test(h4j.ALL, mu=-0.9, alternative = "greater")

#t.test(h4j.ALL,h4j.AML)
#b
t.test(golub[2972,]~gol.fac)
#c
t.test(h4j.ALL, aps.ALL, alternative = "less",paired=T )
#d
plow <- h4j.ALL<aps.ALL
#sum(plow)
prop.test(sum(plow), length(plow), p=0.5, alternative = "greater")
binom.test(sum(plow), length(plow), p=0.5, alternative = "greater")
#e
ph4j <- (h4j.ALL > (-0.6))
prop.test(sum(ph4j), length(ph4j), p=0.5, alternative = "less")
binom.test(sum(ph4j), length(ph4j), p=0.5, alternative = "less")
#f
ph4j.AML <-(h4j.AML > (-0.6))
prop.test(x=c(sum(ph4j),sum(ph4j.AML)), n=c(length(ph4j),length(ph4j.AML)), alternative="two.sided")


#Problem 2

binom.test(89, 2000, p=0.05,alternative = "less")


#Problem 3

x.sim <- matrix(rnorm(10000*20, mean=3, sd=4), ncol=20)
tstat <- function(x) (mean(x)-3)/sd(x)*sqrt(length(x))
tstat.sim <- apply(x.sim, 1, tstat)
power.sim<-mean(tstat.sim>qt(0.9,df=19)) #Calculate the rejection rate
power.sim+c(-1,0,1)*qnorm(0.975)*sqrt(power.sim*(1-power.sim)/10000)

power.sim<-mean(qt(0.3, df=19)<tstat.sim & tstat.sim < qt(0.4, df=19))
power.sim+c(-1,0,1)*qnorm(0.975)*sqrt(power.sim*(1-power.sim)/10000)


#Problem 4


p.values<-row_t_welch(golub[, gol.fac=="ALL"],golub[, gol.fac=="AML"],alternative = "two.sided", mu = 0,conf.level = 0.95)$pvalue
#p.values <- apply(golub, 1, function(x) t.test(x~gol.fac)$p.value)

p.bon <-p.adjust(p=p.values, method="bonferroni")
p.fdr <-p.adjust(p=p.values, method="fdr")
sum(p.values<0.05)
sum(p.bon<0.05)
sum(p.fdr<0.05)


t <- matrix(c(as.numeric(p.values),golub.gnames[,2]),ncol = 2);
t<-t[order(as.numeric(t[,1])),];
t[1:3,]
t <- matrix(c(as.numeric(p.bon),golub.gnames[,2]),ncol = 2);
t<-t[order(as.numeric(t[,1])),];
t[1:3,]
t <- matrix(c(as.numeric(p.fdr),golub.gnames[,2]),ncol = 2);
t<-t[order(as.numeric(t[,1])),];
t[1:3,]



#Problem 5

Wald.ci=function(x,n,conf.level=0.95){
  (x/n)+c(-1,1)*qnorm(1-(1-conf.level)/2)*sqrt(x/n*(1-x/n)/n) 
}


Wilson.ci <- function(x, n, conf.level) {
  (x+qnorm(1-(1-conf.level)/2)^2/2)/(n+qnorm(1-(1-conf.level)/2)^2) + c(-1, 1)*(qnorm(1-(1-conf.level)/2)*sqrt(n)/(n+qnorm(1-(1-conf.level)/2)^2))*sqrt(x/n*(1-x/n)+qnorm(1-(1-conf.level)/2)^2/(4*n))
}

Agresti.ci <- function(x, n, conf.level) {
  (x + qnorm(1-(1-conf.level)/2)^2/2)/(n + qnorm(1-(1-conf.level)/2)^2) + c(-1, 1)*qnorm(1-(1-conf.level)/2)*sqrt(((x + qnorm(1-(1-conf.level)/2)^2/2)/(n + qnorm(1-(1-conf.level)/2)^2))*(1-(x + qnorm(1-(1-conf.level)/2)^2/2)/(n + qnorm(1-(1-conf.level)/2)^2))/(n + qnorm(1-(1-conf.level)/2)^2))
}

x.sim<-rbinom(10000,size=40,prob=0.2)

Wald.sim<-matrix(Wald.ci(x.sim,n=40, conf.level=0.95),nrow=2)
mean((Wald.sim[1,]<0.2)&(Wald.sim[2,]>0.2)) # 
Wilson.sim <- matrix(Wilson.ci(x.sim,n=40, conf.level=0.95), nrow=2)
mean(Wilson.sim[1,] < 0.2 & Wilson.sim[2,] > 0.2)
Agresti.sim <- matrix(Agresti.ci(x.sim,n=40, conf.level=0.95), nrow=2)
mean(Agresti.sim[1,] < 0.2 & Agresti.sim[2,] > 0.2)


