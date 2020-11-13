library(ALL)
library(lmtest)
data(ALL)
#Problem 1
ALLB1234 <- ALL[,ALL$BT %in% c("B", "B1", "B2", "B3", "B4")]
y <- exprs(ALLB1234)["109_at",]
anova( lm( y ~ ALLB1234$BT ) )

summary( lm( y ~ ALLB1234$BT -1) )

pairwise.t.test(y, ALLB1234$BT)

pairwise.t.test(y,ALLB1234$BT, p.adjust.method='fdr')

shapiro.test( residuals( lm( y ~ ALLB1234$BT )))
bptest( lm( y ~ ALLB1234$BT ), studentize = FALSE)


#Problem 2
y <- exprs(ALLB1234)
p.values <- apply(y, 1, function(x) kruskal.test(x ~ ALLB1234$BT)$p.value)
p.fdr <-p.adjust(p=p.values, method="fdr")
sum(p.fdr < 0.05)

t<-exprs(ALLB1234)[order(p.fdr,decreasing = FALSE),];
t[1:5,1]

#Problem 3
ALLB1234 <- ALL[,which(ALL$BT %in% c("B1","B2","B3","B4"), ALL$sex %in% c("F", "M"))]
y <- exprs(ALLB1234)["38555_at",]
Bcell <- ALLB1234$BT
sex <- ALLB1234$sex
anova( lm( y ~ Bcell*sex ) )

shapiro.test( residuals( lm( y ~ Bcell*sex )))
bptest(lm(y ~ Bcell*sex), studentize = FALSE)

#Problem 4
ALLB123 <- ALL[,ALL$BT %in% c("B1", "B2", "B3")]
data<- exprs(ALLB123)["1242_at",]
group<-ALLB123$BT[,drop=T]
n <- length(data)
current_mean <- mean(by(data, group, mean))
T.obs <- sum( (by(data, group, mean) - current_mean) ^ 2) / 2
n.perm <- 2000
T.perm <- rep(NA, n.perm)
for(i in 1:n.perm) {
  data.perm <- sample(data, n, replace=F) #permute data
  new_mean <- mean( by(data.perm, group, mean) )
  T.perm[i] <- sum( (by(data.perm, group, mean) - new_mean) ^ 2) / 2 #Permuted statistic
}
mean(T.perm >= T.obs) #p-value
