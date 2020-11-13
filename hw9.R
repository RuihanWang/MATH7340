library(multtest)
data(stackloss)
library(lmtest)
data(golub)
GRO2 <- golub[grep("GRO2", golub.gnames[,2]),]
GRO3 <- golub[grep("GRO3", golub.gnames[,2]),]
#a
cor(GRO2,GRO3)
#b
cor.test(GRO2,GRO3,conf.level = 0.90)
#c
nboot <- 2000
boot.cor <- matrix(0,nrow = nboot,ncol = 1)
data <- cbind(GRO2,GRO3)

for (i in 1:nboot) {
  data.cor <- data[sample(1:nrow(data),replace = TRUE),]
  boot.cor[i] <- cor(data.cor[,1],data.cor[,2])
    
}
quantile(boot.cor[,1],probs = c(0.05,0.95))


#Problem 2

Zyxin <- golub[grep("Zyxin",golub.gnames[,2]),]
#amount = 0
#for (i in 1:nrow(golub)) {
#  data.cor <- cor(golub[i,],Zyxin)
#  if (data.cor < -0.5) {
#    amount <- amount +1
#  }
#}
#a
cor <- apply(golub, 1, function(x) cor(x,Zyxin))
sum(cor < -0.5)
#b
golub.gnames[,2][order(cor,decreasing = FALSE)][1:5]
#c
p.values <- apply(golub, 1, function(x) cor.test(Zyxin, x, alternative = "less")$p.value)
p.fdr <- p.adjust(p=p.values, method="fdr")
sum(p.fdr<0.05)



#Problem 3
#a
reg.fit <- lm(GRO3 ~ GRO2)
summary(reg.fit)
#b
confint(reg.fit, level=0.9)
#c
predict(reg.fit, newdata=data.frame(GRO2=0), interval="prediction", level = 0.80)

#d
shapiro.test(resid(reg.fit))
bptest(reg.fit, studentize = FALSE)


#Problem 4
#a
lin.reg<-lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc., data=stackloss)
summary(lin.reg)

#c
predict(lin.reg, newdata=data.frame(Air.Flow=60, Water.Temp=20, Acid.Conc.=90), 
        interval="confidence", level = 0.9)
predict(lin.reg, newdata=data.frame(Air.Flow=60, Water.Temp=20, Acid.Conc.=90), 
        interval="prediction", level = 0.9)
