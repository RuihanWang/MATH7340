#Problem 1

data <- c(1.636, 0.374, 0.534, 3.015, 0.932, 0.179)
1/mean(data)


nloglik <- function(theta) -sum(log(dexp(data, rate = theta)))
optim(par = 1, nloglik)$par

lik <- function(theta) prod(dexp(data, rate = theta))
nlik <- function(theta) -lik(theta)
optim(par = 1, nlik)$par




#Problem 2
100.8+qt(0.1, df=52)*(12.4/sqrt(53))

#Problem 3
data(golub, package = 'multtest')
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
Zyxin.ALL <- golub[ grep("Zyxin", golub.gnames[,2]), gol.fac == "ALL"]
Zyxin.AML <- golub[ grep("Zyxin", golub.gnames[,2]), gol.fac == "AML"]
len.all <- length(Zyxin.ALL)
len.aml <- length(Zyxin.AML)

nboot <- 10000
boot.ALL.mean <- boot.AML.mean <- boot.ALL.var <- boot.AML.var <- boot.ALL.median <- boot.AML.median <- rep(NA, nboot)

for (i in 1:nboot) {
  data.ALL.star <- Zyxin.ALL[sample(1:len.all, replace=TRUE)]
  data.AML.star <- Zyxin.AML[sample(1:len.aml, replace=TRUE)]
  boot.ALL.mean[i] <- mean(data.ALL.star)
  boot.AML.mean[i] <- mean(data.AML.star)
  boot.ALL.var[i] <- var(data.ALL.star)
  boot.AML.var[i] <- var(data.AML.star)
  boot.ALL.median[i] <- median(data.ALL.star)
  boot.AML.median[i] <- median(data.AML.star)
}
quantile(boot.ALL.mean,c(0.025,0.975))
quantile(boot.AML.mean,c(0.025,0.975))
quantile(boot.ALL.var, c(0.025,0.975))
quantile(boot.AML.var, c(0.025,0.975))
quantile(boot.ALL.median,c(0.025,0.975))
quantile(boot.AML.median,c(0.025,0.975))


mean(Zyxin.ALL) + qt(c(0.025,0.975), df=len.all-1)*sd(Zyxin.ALL)/sqrt(len.all)
mean(Zyxin.AML) + qt(c(0.025,0.975), df=len.aml-1)*sd(Zyxin.AML)/sqrt(len.aml)
c (var(Zyxin.ALL)*(len.all-1)/ qchisq(0.975, df=len.all-1),var(Zyxin.ALL)*(len.all-1)/ qchisq(0.025, df=len.all-1))
c (var(Zyxin.AML)*(len.aml-1)/ qchisq(0.975, df=len.aml-1),var(Zyxin.AML)*(len.aml-1)/ qchisq(0.025, df=len.aml-1))


#Problem 4
n = 50
nsim = 1000
CICoverage <- function(lam){
  dataset <- matrix(rpois(nsim*n, lambda = lam), nrow = nsim)
  
  mean.success <- var.success <- rep(0, nsim)

  
  t0.05 <- qt(0.05, n-1)
  t0.95 <- -t0.05
  
  k0.95 <- qchisq(0.95, n-1)
  k0.05 <- qchisq(0.05, n-1)
  
  mean <- apply(dataset, 1, mean)
  vars <- apply(dataset, 1, var)
  for (i in 1:nsim) {
    lower.mean <- mean[i] + t0.05*sqrt(mean[i]/n)
    upper.mean <- mean[i] + t0.95*sqrt(mean[i]/n)
    lower.var <- (n-1)*vars[i]/ k0.95
    upper.var <- (n-1)*vars[i]/k0.05
    if (lower.mean < lam&lam < upper.mean) {
      mean.success[i] <- 1
    }
    if (lower.var < lam&lam < upper.var ) {
      var.success[i] <- 1
    }
  }
  print(mean(mean.success))
  print(mean(var.success))
}
lambda <- c(0.1,1,10)
for(lam in lambda) {
  CICoverage(lam)
}


