#required library
library(affy)
library(ArrayExpress)
library(genefilter)
library(yeast2.db)
library(annotate)
library(GO.db)
library(ALL)
library(limma)
library(hgu95av2.db)

data(ALL)
annotation(ALL)

#Problem 1
#a
#require downloaded into ./yeast folder
#getAE('E-MEXP-1551', path = './yeast',  type = "full")
yeast.raw <-  ReadAffy(celfile.path= './yeast' )
eset <- expresso(yeast.raw, 
                 bgcorrect.method="mas", 
                 normalize.method="quantiles", 
                 pmcorrect.method="pmonly", 
                 summary.method="medianpolish")

#b
apply(eset[1:5,], 1, mean)

#c
dim(eset)


#Problem 2
#a
annotation(yeast.raw)

#b
GO <- get("1769308_at", env = yeast2GO)
gonr <- getOntology(GO, "MF")
gonr

#c
gP <- getGOParents(gonr)
pa <- sapply(gP, function(x) x$Parents)
pa

#d
gC <- getGOChildren(gonr)
ch <- sapply(gC, function(x) x$Children)
ch
length(unique(ch))

#Problem 3
#a
patientB2 <- factor(ALL$BT %in% c("B2"))
#length(patientB2)
patientB3 <- factor(ALL$BT %in% c("B3"))
#length(patientB3)
wilcox <- function(x) (wilcox.test (x[patientB2 == TRUE], x[patientB3 == TRUE], paired = F, exact = F)$p.value < 0.001 )
sel1 <- genefilter(exprs(ALL), filterfun(wilcox))

welch <- function(x) (t.test(x[patientB2 == TRUE], x[patientB3 == TRUE], paired = F)$p.value < 0.001) #pass 2-sample t-test
#function(x) (t.test(x ~ patientB)$p.value < 0.05) 
sel2 <- genefilter(exprs(ALL), filterfun(welch))

#b
x <- apply(cbind(sel1, sel2), 2, as.integer) #Combine 3 filter results (as 3 columns) and convert entries (TRUE/FALE) to integers (0 and 1)
vc <- vennCounts(x, include = "both") #Calculate counts for Venn Diagram
vennDiagram(vc)

#c
ALL1 <- ALL[sel1,]
nrow(exprs(ALL1))

ALL2 <- ALL[sel1 & sel2,]
nrow(exprs(ALL2))

#d
GOTerm2Tag <- function(term) {
  GTL <- eapply(GOTERM, function(x) {grep(term, x@Term,value=TRUE)}) #grep GOTERM on each element in list
  Gl <- sapply(GTL, length) #count how many are found 
  names(GTL[Gl>0]) #return those nonempty lists (length>0)
}
GOTerm2Tag("oncogene")

#e
tran <- hgu95av2GO2ALLPROBES$"GO:0090402"  #probes for this GO number
inboth <- tran %in% row.names(exprs(ALL2)) #ALLs probes for this GO number
ALLtran <- ALL2[tran[inboth],] #pick out probes data for this GO number
dim(exprs(ALLtran))


#Problem 4
#a
allB <- ALL[, which(ALL$BT %in% c("B1", "B2", "B3"))]

#b
design.ma <- model.matrix(~0 + factor(allB$BT)) #Design matrix forANOVA without intercept
colnames(design.ma) <- c("B1", "B2", "B3") #variable names
fit <- lmFit(allB, design.ma)
fit <- eBayes(fit)
print(topTable(fit, coef=3, number=5, adjust.method="fdr"), digits=4)

#c
cont.ma <- makeContrasts(B1-B2,B2-B3, levels=factor(allB$BT)) #design matrix built from the two contrasts
fit1 <- contrasts.fit(fit, cont.ma)
fit1 <- eBayes(fit1)
dim(topTable(fit1, number=Inf, p.value=0.01, adjust.method="fdr"))
print(topTable(fit1, number=5, adjust.method="fdr"), digits=4) #Print the top 5 selected for 2nd coefficient with FDR adjustment, show only 4 digits.
