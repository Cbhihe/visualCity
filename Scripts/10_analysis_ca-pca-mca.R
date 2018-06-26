# #############################
## MIRI/MVA   :     Analysis of NYC-311 Service Request Calls
## Author:          Cedric Bhihe, Santi Calvo
## Delivery:        2018.06.26
## Script:          10_analysis_ca-pca-mca.R
# #############################


rm(list=ls(all=TRUE))

setwd("~/Documents/Work/Academic-research/NYC311/")
#setwd("C:/Users/calvo/Desktop/UPC/Courses/Third_semester/MVA/NYC-complaints-master")

set.seed(932178)

options(scipen=6) # R switches to sci notation above 5 digits on plot axes
ccolors=c("red","green","blue","orange","cyan","tan1","darkred","honeydew2","violetred",
          "palegreen3","peachpuff4","lavenderblush3","lightgray","lightsalmon","wheat2")

datestamp <- format(Sys.time(),"%Y%m%d-%H%M%S"); 


# #############################
## Source parameter file
# #############################

source(file="Scripts/00_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...setwd("~/Documents/Work/Academic-research/NYC-complaints/")


# #############################
## Libraries
# #############################

setRepositories(ind = c(1:6,8))
library(FactoMineR)     # to use canned PCA and CA methods. 

require(graphics)       # enhanced graphics
library(ggplot2)        # to enhance graph plotting
library(ggrepel)        # to plot with well behaved labeling


# #############################
## Functions
# #############################

csvSaveF <- function(dataObj,targetfile) {
  write.table(dataObj,
              targetfile,
              append=F,
              sep=",",
              eol="\n",
              na ="NA",
              dec=".",
              row.names=F,
              col.names=T)
}    # save cvs to file


# #############################
## Load data
# #############################

source_file <- paste0(yearNbr,
                      ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                      "00_nyc_whole-data-set.csv"
)
X <- read.csv(paste0("Data/",source_file),
              header=T,
              sep=",",
              quote="\"",
              dec=".",
              skip=0)
rownames(X) <- X[,1]; X <- X[,-1] 
names(X)

# #############################
##  Contingency table
# #############################

## Build table

# choose columns 2 to 14 for CA on 2D contingency table

# row 208 = 99999 bogus zip for missing
ZIPsupress <- which(rownames(X) %in% c("99999"))
Y <- X[-ZIPsupress,2:14]  # 208 is a supplementary factor among row profiles

# total nbr of counts
cntTot <- sum(Y)
# frequency matrix
fij <- Y/cntTot

# row weights in terms of service calls for each ZIP
fi <- rowSums(fij)
# col weights of service call modality accross all ZIPs
fj <- colSums(fij)
sum(fi) ; sum(fj) # check = 1

## Explore table

# Identify & suppress rows (i.e. ZIPs) with no reported SRC
zeroSRCzip <- labels(fi[fi <= 5/cntTot]) # identify
Y <- Y[!rownames(Y) %in% zeroSRCzip,]  # suppress
fij <- Y/cntTot
fi <- rowSums(fij)
fj <- colSums(fij)
rowCentroid <- sqrt(fj) 
colCentroid <- sqrt(fi)

# #############################
## Test of independence 1
# #############################

# CAUTION !!
# Check that cell with low count values (<5) do not contribute significantly to computed chi-sq.
# compute nbr of cells with count <5 
pbmCntCellLength <- length(which(as.numeric(unlist(Y))<5))
cat("nbr of cells with count <5:",pbmCntCellLength,"\n")

# 1/ Pearson chi-square test for significant association (dependence) between row & col categs
# H0: In the population, the two categorical variables are independent.
# joint cell counts distr. in 2D contingency table = (row marginals) x (col marginals)
# DFs = 180x12 = 2160 is not taken into account for test statistics computed by Monte-Carlo simulations.
(chiSqResult <- chisq.test(Y,
                           simulate.p.value=T,
                           B=10000)
)
# attributes(chiSqResult)

# compute contribution to chi-sq of low frequency cells
chiSqContrib <- vector(mode="numeric", length=pbmCntCellLength)
kk <-0
for (jj in 1:ncol(Y)) {
  for (ii in 1:nrow(Y)) {
    if (Y[ii,jj] <5){
      kk <- kk+1
      chiSqContrib[kk] <- (chiSqResult$residual[ii,jj])^2 /chiSqResult$statistic 
    }
  }
}  

# # check that sum of all so-calculated contributions to Chi-square = 1
# chiSqContribTest <- 0
# for (jj in 1:ncol(Y)) {
#     for (ii in 1:nrow(Y)) {
#             chiSqContribTest <- chiSqContribTest+(chiSqResult$residual[ii,jj])^2 /chiSqResult$statistic
#     }
# }
# chiSqContribTest

length(which(chiSqContrib >=0.01))   # result: 0 cells, good
length(which(chiSqContrib >=0.001))  # result: 8 cells, totally acceptable

# Final conclusion
# chi-sq=43630; p=1e-4; We can reject the null hypothesis at the rist 1% of erring
# There is significant association of row and column categorical variables, i.e. we can say that 
# service calls to NYC 311 does depend on ZIP codes.


# #############################
## Test of independence 2 (on ZIPs to be suppressed)
# #############################

# test that modalities and suppressed ZIP codes hace no association
U <- X[(rownames(X) %in% zeroSRCzip) & (rowSums(X[,2:14]) != 0),2:14]
U <- U[,colSums(U) !=0]
(chiSqResult <- chisq.test(U,
                           simulate.p.value=T,
                           B=10000)
)
# p-value = 0.054 ;  we cannot reject the null hypothesis and conclude that we can suppress ZIP codes.
# #############################
## Balloon plot
# #############################

# library ("gplots")
# par(mfrow=c(1,2))
# dt <- as.table(as.matrix(Y))
# balloonplot(t(dt), main ="NYC311 service calls pero ZIP",
#             xlab ="", ylab="",
#             rowsrt=0, colsrt=90, asp=1,
#             rowmar=2.2,colmar=2.2,
#             label = FALSE,
#             text.size=0.5,
#             show.margins = T)


# ############################# 
## Mosaic plot
# #############################

# mosaicplot(dt,
#            shade=T,
#            las=2,
#            cex.axis=0.5,
#            main ="")
# # Neither representations are useful given data size
# 
# par(mfrow = c(1,1))


# #############################
## Plot & interpret cloud projections
# #############################

## Compute centered cloud for row profiles
# row profile cloud with incorporated metric effect 
# -> deformed cloud of points
Di <- diag(fi)
Dj <- diag(fj)

# define cloud of rows, with matrix of conditional frequencies of j (col) given i (row)
CFj_given_i <- solve(Di) %*% as.matrix(fij)

# # check that each row sums to 1
# apply(CFj_given_i,1,sum)

# coordinates of rows with embedded chi-sq metric
#Fim <- solve(Di) %*% as.matrix(fij) %*% solve(sqrt(Dj))
Fim <- CFj_given_i %*% solve(sqrt(Dj))
# consistency check: compute center of gravity of rows in 2 ways; test component wise
ifelse(round(sqrt(fj) - apply(Fim, 2, weighted.mean, w=fi),3)== 0,"ok","not ok") 

# center cloud and define new centered row profiles' data set, Y_ctd
unitaryM <- matrix(1,nrow=nrow(Fim), ncol=ncol(Fim), byrow=T)
Y_ctd <- Fim - unitaryM %*% sqrt(Dj)  # centering

rownames(Y_ctd) <- rownames(Y)
colnames(Y_ctd) <- colnames(Y)


# ... for which we can calculate eigenvalues and eigenvectors
Z <- sqrt(Di) %*% Y_ctd
eigZrows <- eigen( t(Z) %*% Z) 
evecRows <- eigZrows$vectors        # eigenvectors for row profiles
evalRows <- eigZrows$values         # eigenvalues
# ifelse(sum(diag(t(Z) %*%Z)) == sum(evals1),"ok", "not ok") #  check consistency

eigZcols <- eigen( Z %*% t(Z)) 
evecCols <- eigZcols$vectors        # eigenvectors
evalCols <- eigZcols$values         # eigenvalues  same first 12 eigenvalues as evalRows


# #############################
## Repeat analysis with FactoMineR
# #############################

# plot **factor** maps in factorial planes PC1-2, PC2-3, PC1-3 where row and col factors 
# are printed together with distinct colors for easier differentiation. 
# Factors, psi_alpha, are projections of either centered row "i" profiles, X_ctd, or of 
# centered col "j" profiles on PC direction u_alpha (evec)
# psi <- t(X_ctd) %*% evecs   # for row profiles

par(mfrow = c(1,3))
# dim(X[!rownames(X) %in% zeroSRCzip,1:14])
# caY <- CA(X[!rownames(X) %in% zeroSRCzip,1:14],
#           ncp=ncol(X)-2,
#           row.sup= 196,
#           quali.sup = 1,
#           graph=T,
#           axes = c(1,2),
#           excl=NULL)
# creates error as col_1 is categorical. 
# nevertheless helps in that in appraising for missings' bogus ZIP "99999"
# its quality of representation (cos2) in 1st 3 dimensions. It is bad.


# PC1-2 factorial plane
caY <- CA(Y,
          ncp=ncol(Y)-1,
          graph=T,
          axes = c(1,2),
          excl=NULL)
# Do not specify option 'row.w=fi' 
# By default, vector of 1's and each row has weight equals to its margin;
# Weights given only for active rows

# PC2-3 fatorial plane
CA(Y,
   ncp=ncol(Y)-1,
   graph=T,
   axes = c(2,3),
   excl=NULL)

# PC1-3 fatorial plane
CA(Y,
   ncp=ncol(Y)-1,
   graph=T,
   axes = c(1,3),
   excl=NULL)

# PC1-4 factorial plane
CA(Y,
   ncp=ncol(Y)-1,
   graph=T,
   axes = c(1,4),
   excl=NULL)

# PC2-4 fatorial plane
CA(Y,
   ncp=ncol(Y)-1,
   graph=T,
   axes = c(2,4),
   excl=NULL)

# PC3-4 fatorial plane
CA(Y,
   ncp=ncol(Y)-1,
   graph=T,
   axes = c(3,4),
   excl=NULL)

par(mfrow = c(1,1))


# ctr: contributions to the construction of PCs (%) (intra dimension sum=1)
# cos2: factor's representation quality along one PC  [0,1] 
attributes(caY)
summary.CA(caY)  

## for column profiles
# list all contributions above 10%
tab <- caY$col$contrib[,1:3] # col profiles contributions to 1st 3 PCs.
(colctr.gt.10 <- tab[tab[,1] > 10 | tab[,2] > 10 | tab[,3] > 10 ,])

# quality of representation of col profiles with biggest contrib to PC formation
ctrNames <- rownames(colctr.gt.10)
tab <- caY$col$cos2[,1:3]
( colctr.gt.10_rep <- tab[rownames(tab) %in% ctrNames,] )

## for row profiles
# list all contributions above 10%
tab <- round(caY$row$contrib[,1:3],1) # col profiles contributions to 1st 3 PCs.
(rowctr.gt.2 <- tab[tab[,1] > 2 | tab[,2] > 2 | tab[,3] > 2 ,])
(maxCtrDim1 <- tab[tab[,1] == max(tab[,1]),])   # max contributor to construct of  Dim 1
(maxCtrDim2 <- tab[tab[,2] == max(tab[,2]),])   # max contributor to construct of  Dim 2
(maxCtrDim3 <- tab[tab[,3] == max(tab[,3]),])   # max contributor to construct of  Dim 3
tab[rownames(tab)== "10463",]  # investigate ZIP 10463 (Riverdale)


caY$eig                   
evalRows2 <- caY$eig[,1]  
# last eval = 0, 0; along with last column in evecs, correspond to the centroid 'sqrt(fj)'

cumvarexp <- c(100*evalRows[1]/sum(evalRows))
for (ii in 2:(ncol(Y))) {
  if(ii==2) cat("Eigenvalues","\t","Cumulative variance (%)\n")
  cumvarexp <- c(cumvarexp,cumvarexp[ii-1] + 100*evalRows[ii]/sum(evalRows))
  cat(format(evalRows[ii-1],scientific=T),"\t",round(cumvarexp[ii-1],6),"\n")
}


par(mfrow = c(1,2))

# plot row profiles cloud and colum profile cloud separately
plot(caY,invisible="col", axes = c(1,2),label="row",shadow=T,cex=0.8,selectRow="cos2 0.6")
# project row centroid on PC12 
# logically it coincides with origin of axes of factorial plane.
points(rowCentroid %*% evecRows[,1:2],pch=18,type='p',col='green',cex = 2)   

plot(caY,invisible="row", axes = c(1,2), label="col",shadow=T, cex=0.8,selectCol="cos2 0.4")
# project col centroid on PC12 
# logically it coincides with origin of axes of factorial plane.
points(colCentroid %*% evecCols[,1:2],pch=18,type='p',col='green',cex = 2)  

par(mfrow = c(1,1))

# screeplot
plottitle = sprintf("PC explanatory power\n(\"NYC 311 SCR vs zip codes\" data set)")
plot(seq(1:length(evalRows)),evalRows,
     pch=15, 
     cex=1,
     col="blue",
     type="b",
     main=plottitle,
     xlab="Index (sorted)",
     ylab="Eigenvalues"
)
text(x=1:length(evalRows), y=evalRows, 
     labels=paste0(as.character(round(caY$eig[,3],1)),"%"),
     cex=0.9,
     pos=4,
     col="red") # add labels
text(x=0.5*length(evalRows),y=evalRows[1]*0.9,
     labels="(Red labels show cumulative\nvariance explanatory power)",
     cex=1,
     pos=3,
     col="black")
abline(h=mean(evalRows),col="gray")
text(x=2/3*length(evalRows), y=mean(evalRows)-0.0001, 
     labels=paste0("Eigenvalue mean: ",as.character(format(mean(evalRows),scientific=T))),
     cex=0.75,
     pos=3,
     col="red")
grid()



# ############################
# Full PCA
# ############################

# ####################
## by hand

Z <- X[!rownames(X) %in% zeroSRCzip,1:14]  # suppress
bogus_idx <- which(rownames(Z)%in%c("99999"))
Z_ctd <- cbind(Borough=as.character(Z[-bogus_idx,1]),Y_ctd)
rownames(Z_ctd) <- rownames(Z[-bogus_idx,])
colnames(Z_ctd) <- colnames(Z)
Zborough <- Z[-bogus_idx,1]
Z_tmp <- apply(as.matrix(Z_ctd[,-1]),2, as.numeric)  # transform data.frame in numeric matrix
covZ <- t(Z_tmp) %*% diag(fi)  %*% Z_tmp   # compute cov matrix with row marginals as obs weights

evalsZ <- eigen(covZ)$values
cat("Eigenvalues (obs): ",round(evalsZ,4),"\n")  # ok
evecsZ <- eigen(covZ)$vectors   
cat("Eigenvectors (obs):","\n"); evecsZ   # same as 'evecsRows' above

psiZ <- Z_tmp %*% evecsZ  # matrix of scores, values of each individual's p components along the PC axes 
colnames(psiZ) <- paste0("PC",1:ncol(psiZ))

borough_col=as.factor(Zborough)


par(mfrow = c(1,1))

plottitle=sprintf("Row profiles\' projection in PC1-2 factorial plane")
plotdata <- data.frame(PC1=-psiZ[,1],PC2=-psiZ[,2],z=rep("",nrow(Z_tmp)))
#plotdata <- data.frame(PC1=psiZ[,1],PC2=psiZ[,2],z=matrix(rep("",nrow(Z)),nrow=nrow(Z),byrow=T))
ggplot(data = plotdata) + 
  theme_bw() +
  geom_vline(xintercept = 0, col="gray") +
  geom_hline(yintercept = 0, col="gray") +
  # geom_text_repel(aes(PC1,PC2,label = z),
  #                 size=3,
  #                 point.padding = 0.5,
  #                 box.padding = unit(0.55, "lines"),
  #                 segment.size = 0.3,
  #                 segment.color = 'grey') +
  geom_point(aes(PC1,PC2,col = factor(borough_col)), size = 2) +
  scale_color_discrete(name = 'Borough') +
  labs(title = plottitle)

par(mfrow = c(1,1))

# ####################
## with FactorMineR

par(mfrow = c(1,2))
pcaY <- PCA(Y_ctd,
            scale.unit=FALSE,
            ind.sup = NULL, 
            quanti.sup = NULL, 
            quali.sup = NULL, 
            row.w = fi, col.w = NULL, 
            graph = TRUE, axes = c(1,2))
pcaY$eig
# etc...

par(mfrow = c(1,1))

Psi <- pcaY$ind$coord # psi matrix


#############################################
## MCA
#############################################

## use contingency table W
W <- X[!rownames(X) %in% c(zeroSRCzip,"99999"),1:19]
W_bak <- W

## discretize modality counts with bins, containing roughly same number of ZIP codes 
{
# HousCond
summary(W$HousCond)
bins_HousCond <- c(3,7,13)
mods_HousCond <- c("VL","L","M","H")
cat("HousCond:\n   below or equal to",bins_HousCond[1],"RFCs - bin count:",length(which(W$HousCond<=bins_HousCond[1])),"\n")
cat("   between",bins_HousCond[1]+1,"and",bins_HousCond[2],"RFCs - bin count:", length(which(W$HousCond>bins_HousCond[1] & W$HousCond<=bins_HousCond[2])),"\n")
cat("   between",bins_HousCond[2]+1,"and",bins_HousCond[3],"RFCs- bin count:", length(which(W$HousCond>bins_HousCond[2] & W$HousCond<=bins_HousCond[3])),"\n")
cat("   above",bins_HousCond[3],"RFCs - bin count:", length(which(W$HousCond>bins_HousCond[3])),"\n")

W$HousCond[which(W_bak$HousCond<=bins_HousCond[1])] <- mods_HousCond[1]
W$HousCond[which(W_bak$HousCond>bins_HousCond[1] & W_bak$HousCond<=bins_HousCond[2])] <- mods_HousCond[2] 
W$HousCond[which(W_bak$HousCond>bins_HousCond[2] & W_bak$HousCond<=bins_HousCond[3])] <- mods_HousCond[3]
W$HousCond[which(W_bak$HousCond>bins_HousCond[3])] <- mods_HousCond[4]

W$HousCond <- as.factor(W$HousCond)

# Sani
summary(W$Sani)
bins_Sani <- c(18,31,54)
mods_Sani <- c("L","M","H","VH")
cat("Sani:\n   below or equal to",bins_Sani[1],"RFCs - bin count:",length(which(W$Sani<=bins_Sani[1])),"\n")
cat("   between",bins_Sani[1]+1,"and",bins_Sani[2],"RFCs - bin count:", length(which(W$Sani>bins_Sani[1] & W$Sani<=bins_Sani[2])),"\n")
cat("   between",bins_Sani[2]+1,"and",bins_Sani[3],"RFCs- bin count:", length(which(W$Sani>bins_Sani[2] & W$Sani<=bins_Sani[3])),"\n")
cat("   above",bins_Sani[3],"RFCs - bin count:", length(which(W$Sani>bins_Sani[3])),"\n")

W$Sani[which(W_bak$Sani<=bins_Sani[1])] <- mods_Sani[1]
W$Sani[which(W_bak$Sani>bins_Sani[1] & W_bak$Sani<=bins_Sani[2])] <- mods_Sani[2] 
W$Sani[which(W_bak$Sani>bins_Sani[2] & W_bak$Sani<=bins_Sani[3])] <- mods_Sani[3]
W$Sani[which(W_bak$Sani>bins_Sani[3])] <- mods_Sani[4]

W$Sani <- as.factor(W$Sani)

# NoiseResid
summary(W$NoiseResid)
bins_NoiseResid <- c(29,61,123)
mods_NoiseResid <- c("L","M","H","VH")
cat("NoiseResid:\n   below or equal to",bins_NoiseResid[1],"RFCs - bin count:",length(which(W$NoiseResid<=bins_NoiseResid[1])),"\n")
cat("   between",bins_NoiseResid[1]+1,"and",bins_NoiseResid[2],"RFCs - bin count:", length(which(W$NoiseResid>bins_NoiseResid[1] & W$NoiseResid<=bins_NoiseResid[2])),"\n")
cat("   between",bins_NoiseResid[2]+1,"and",bins_NoiseResid[3],"RFCs- bin count:", length(which(W$NoiseResid>bins_NoiseResid[2] & W$NoiseResid<=bins_NoiseResid[3])),"\n")
cat("   above",bins_NoiseResid[3],"RFCs - bin count:", length(which(W$NoiseResid>bins_NoiseResid[3])),"\n")

W$NoiseResid[which(W_bak$NoiseResid<=bins_NoiseResid[1])] <- mods_NoiseResid[1]
W$NoiseResid[which(W_bak$NoiseResid>bins_NoiseResid[1] & W_bak$NoiseResid<=bins_NoiseResid[2])] <- mods_NoiseResid[2] 
W$NoiseResid[which(W_bak$NoiseResid>bins_NoiseResid[2] & W_bak$NoiseResid<=bins_NoiseResid[3])] <- mods_NoiseResid[3]
W$NoiseResid[which(W_bak$NoiseResid>bins_NoiseResid[3])] <- mods_NoiseResid[4]

W$NoiseResid <- as.factor(W$NoiseResid)

# NoiseConst
summary(W$NoiseConst)
bins_NoiseConst <- c(1,5,20)
mods_NoiseConst <- c("VL","L","M","H")
cat("NoiseConst:\n   below or equal to",bins_NoiseConst[1],"RFCs - bin count:",length(which(W$NoiseConst<=bins_NoiseConst[1])),"\n")
cat("   between",bins_NoiseConst[1]+1,"and",bins_NoiseConst[2],"RFCs - bin count:", length(which(W$NoiseConst>bins_NoiseConst[1] & W$NoiseConst<=bins_NoiseConst[2])),"\n")
cat("   between",bins_NoiseConst[2]+1,"and",bins_NoiseConst[3],"RFCs- bin count:", length(which(W$NoiseConst>bins_NoiseConst[2] & W$NoiseConst<=bins_NoiseConst[3])),"\n")
cat("   above",bins_NoiseConst[3],"RFCs - bin count:", length(which(W$NoiseConst>bins_NoiseConst[3])),"\n")

W$NoiseConst[which(W_bak$NoiseConst<=bins_NoiseConst[1])] <- mods_NoiseConst[1]
W$NoiseConst[which(W_bak$NoiseConst>bins_NoiseConst[1] & W_bak$NoiseConst<=bins_NoiseConst[2])] <- mods_NoiseConst[2] 
W$NoiseConst[which(W_bak$NoiseConst>bins_NoiseConst[2] & W_bak$NoiseConst<=bins_NoiseConst[3])] <- mods_NoiseConst[3]
W$NoiseConst[which(W_bak$NoiseConst>bins_NoiseConst[3])] <- mods_NoiseConst[4]

W$NoiseConst <- as.factor(W$NoiseConst)

# NoiseBiz
summary(W$NoiseBiz)
bins_NoiseBiz <- c(3,9,27)
mods_NoiseBiz <- c("VL","L","M","H")
cat("NoiseBiz:\n   below or equal to",bins_NoiseBiz[1],"RFCs - bin count:",length(which(W$NoiseBiz<=bins_NoiseBiz[1])),"\n")
cat("   between",bins_NoiseBiz[1]+1,"and",bins_NoiseBiz[2],"RFCs - bin count:", length(which(W$NoiseBiz>bins_NoiseBiz[1] & W$NoiseBiz<=bins_NoiseBiz[2])),"\n")
cat("   between",bins_NoiseBiz[2]+1,"and",bins_NoiseBiz[3],"RFCs- bin count:", length(which(W$NoiseBiz>bins_NoiseBiz[2] & W$NoiseBiz<=bins_NoiseBiz[3])),"\n")
cat("   above",bins_NoiseBiz[3],"RFCs - bin count:", length(which(W$NoiseBiz>bins_NoiseBiz[3])),"\n")

W$NoiseBiz[which(W_bak$NoiseBiz<=bins_NoiseBiz[1])] <- mods_NoiseBiz[1]
W$NoiseBiz[which(W_bak$NoiseBiz>bins_NoiseBiz[1] & W_bak$NoiseBiz<=bins_NoiseBiz[2])] <- mods_NoiseBiz[2] 
W$NoiseBiz[which(W_bak$NoiseBiz>bins_NoiseBiz[2] & W_bak$NoiseBiz<=bins_NoiseBiz[3])] <- mods_NoiseBiz[3]
W$NoiseBiz[which(W_bak$NoiseBiz>bins_NoiseBiz[3])] <- mods_NoiseBiz[4]

W$NoiseBiz <- as.factor(W$NoiseBiz)

# UrbInf
summary(W$UrbInf)
bins_UrbInf <- c(33,55,87)
mods_UrbInf <- c("L","M","H","VH")
cat("UrbInf:\n   below or equal to",bins_UrbInf[1],"RFCs - bin count:",length(which(W$UrbInf<=bins_UrbInf[1])),"\n")
cat("   between",bins_UrbInf[1]+1,"and",bins_UrbInf[2],"RFCs - bin count:", length(which(W$UrbInf>bins_UrbInf[1] & W$UrbInf<=bins_UrbInf[2])),"\n")
cat("   between",bins_UrbInf[2]+1,"and",bins_UrbInf[3],"RFCs- bin count:", length(which(W$UrbInf>bins_UrbInf[2] & W$UrbInf<=bins_UrbInf[3])),"\n")
cat("   above",bins_UrbInf[3],"RFCs - bin count:", length(which(W$UrbInf>bins_UrbInf[3])),"\n")

W$UrbInf[which(W_bak$UrbInf<=bins_UrbInf[1])] <- mods_UrbInf[1]
W$UrbInf[which(W_bak$UrbInf>bins_UrbInf[1] & W_bak$UrbInf<=bins_UrbInf[2])] <- mods_UrbInf[2] 
W$UrbInf[which(W_bak$UrbInf>bins_UrbInf[2] & W_bak$UrbInf<=bins_UrbInf[3])] <- mods_UrbInf[3]
W$UrbInf[which(W_bak$UrbInf>bins_UrbInf[3])] <- mods_UrbInf[4]

W$UrbInf <- as.factor(W$UrbInf)

# Traffic
summary(W$Traffic)
bins_Traffic <- c(24,54,87)
mods_Traffic <- c("L","M","H","VH")
cat("Traffic:\n   below or equal to",bins_Traffic[1],"RFCs - bin count:",length(which(W$Traffic<=bins_Traffic[1])),"\n")
cat("   between",bins_Traffic[1]+1,"and",bins_Traffic[2],"RFCs - bin count:", length(which(W$Traffic>bins_Traffic[1] & W$Traffic<=bins_Traffic[2])),"\n")
cat("   between",bins_Traffic[2]+1,"and",bins_Traffic[3],"RFCs- bin count:", length(which(W$Traffic>bins_Traffic[2] & W$Traffic<=bins_Traffic[3])),"\n")
cat("   above",bins_Traffic[3],"RFCs - bin count:", length(which(W$Traffic>bins_Traffic[3])),"\n")

W$Traffic[which(W_bak$Traffic<=bins_Traffic[1])] <- mods_Traffic[1]
W$Traffic[which(W_bak$Traffic>bins_Traffic[1] & W_bak$Traffic<=bins_Traffic[2])] <- mods_Traffic[2] 
W$Traffic[which(W_bak$Traffic>bins_Traffic[2] & W_bak$Traffic<=bins_Traffic[3])] <- mods_Traffic[3]
W$Traffic[which(W_bak$Traffic>bins_Traffic[3])] <- mods_Traffic[4]

W$Traffic <- as.factor(W$Traffic)

# NoiseTraf
summary(W$NoiseTraf)
bins_NoiseTraf <- c(4,11,23)
mods_NoiseTraf <- c("VL","L","M","H")
cat("NoiseTraf:\n   below or equal to",bins_NoiseTraf[1],"RFCs - bin count:",length(which(W$NoiseTraf<=bins_NoiseTraf[1])),"\n")
cat("   between",bins_NoiseTraf[1]+1,"and",bins_NoiseTraf[2],"RFCs - bin count:", length(which(W$NoiseTraf>bins_NoiseTraf[1] & W$NoiseTraf<=bins_NoiseTraf[2])),"\n")
cat("   between",bins_NoiseTraf[2]+1,"and",bins_NoiseTraf[3],"RFCs- bin count:", length(which(W$NoiseTraf>bins_NoiseTraf[2] & W$NoiseTraf<=bins_NoiseTraf[3])),"\n")
cat("   above",bins_NoiseTraf[3],"RFCs - bin count:", length(which(W$NoiseTraf>bins_NoiseTraf[3])),"\n")

W$NoiseTraf[which(W_bak$NoiseTraf<=bins_NoiseTraf[1])] <- mods_NoiseTraf[1]
W$NoiseTraf[which(W_bak$NoiseTraf>bins_NoiseTraf[1] & W_bak$NoiseTraf<=bins_NoiseTraf[2])] <- mods_NoiseTraf[2] 
W$NoiseTraf[which(W_bak$NoiseTraf>bins_NoiseTraf[2] & W_bak$NoiseTraf<=bins_NoiseTraf[3])] <- mods_NoiseTraf[3]
W$NoiseTraf[which(W_bak$NoiseTraf>bins_NoiseTraf[3])] <- mods_NoiseTraf[4]

W$NoiseTraf <- as.factor(W$NoiseTraf)

# WaterSyst
summary(W$WaterSyst)
bins_WaterSyst <- c(19,29,44)
mods_WaterSyst <- c("L","M","MH","H")
cat("WaterSyst:\n   below or equal to",bins_WaterSyst[1],"RFCs - bin count:",length(which(W$WaterSyst<=bins_WaterSyst[1])),"\n")
cat("   between",bins_WaterSyst[1]+1,"and",bins_WaterSyst[2],"RFCs - bin count:", length(which(W$WaterSyst>bins_WaterSyst[1] & W$WaterSyst<=bins_WaterSyst[2])),"\n")
cat("   between",bins_WaterSyst[2]+1,"and",bins_WaterSyst[3],"RFCs- bin count:", length(which(W$WaterSyst>bins_WaterSyst[2] & W$WaterSyst<=bins_WaterSyst[3])),"\n")
cat("   above",bins_WaterSyst[3],"RFCs - bin count:", length(which(W$WaterSyst>bins_WaterSyst[3])),"\n")

W$WaterSyst[which(W_bak$WaterSyst<=bins_WaterSyst[1])] <- mods_WaterSyst[1]
W$WaterSyst[which(W_bak$WaterSyst>bins_WaterSyst[1] & W_bak$WaterSyst<=bins_WaterSyst[2])] <- mods_WaterSyst[2] 
W$WaterSyst[which(W_bak$WaterSyst>bins_WaterSyst[2] & W_bak$WaterSyst<=bins_WaterSyst[3])] <- mods_WaterSyst[3]
W$WaterSyst[which(W_bak$WaterSyst>bins_WaterSyst[3])] <- mods_WaterSyst[4]

W$WaterSyst <- as.factor(W$WaterSyst)

# ConsumProt
summary(W$ConsumProt)
bins_ConsumProt <- c(5,13,23)
mods_ConsumProt <- c("VL","L","M","H")
cat("ConsumProt:\n   below or equal to",bins_ConsumProt[1],"RFCs - bin count:",length(which(W$ConsumProt<=bins_ConsumProt[1])),"\n")
cat("   between",bins_ConsumProt[1]+1,"and",bins_ConsumProt[2],"RFCs - bin count:", length(which(W$ConsumProt>bins_ConsumProt[1] & W$ConsumProt<=bins_ConsumProt[2])),"\n")
cat("   between",bins_ConsumProt[2]+1,"and",bins_ConsumProt[3],"RFCs- bin count:", length(which(W$ConsumProt>bins_ConsumProt[2] & W$ConsumProt<=bins_ConsumProt[3])),"\n")
cat("   above",bins_ConsumProt[3],"RFCs - bin count:", length(which(W$ConsumProt>bins_ConsumProt[3])),"\n")

W$ConsumProt[which(W_bak$ConsumProt<=bins_ConsumProt[1])] <- mods_ConsumProt[1]
W$ConsumProt[which(W_bak$ConsumProt>bins_ConsumProt[1] & W_bak$ConsumProt<=bins_ConsumProt[2])] <- mods_ConsumProt[2] 
W$ConsumProt[which(W_bak$ConsumProt>bins_ConsumProt[2] & W_bak$ConsumProt<=bins_ConsumProt[3])] <- mods_ConsumProt[3]
W$ConsumProt[which(W_bak$ConsumProt>bins_ConsumProt[3])] <- mods_ConsumProt[4]

W$ConsumProt <- as.factor(W$ConsumProt)

# SocServ
summary(W$SocServ)
bins_SocServ <- c(2,6,11)
mods_SocServ <- c("VL","L","M","MH")
cat("SocServ:\n   below or equal to",bins_SocServ[1],"RFCs - bin count:",length(which(W$SocServ<=bins_SocServ[1])),"\n")
cat("   between",bins_SocServ[1]+1,"and",bins_SocServ[2],"RFCs - bin count:", length(which(W$SocServ>bins_SocServ[1] & W$SocServ<=bins_SocServ[2])),"\n")
cat("   between",bins_SocServ[2]+1,"and",bins_SocServ[3],"RFCs- bin count:", length(which(W$SocServ>bins_SocServ[2] & W$SocServ<=bins_SocServ[3])),"\n")
cat("   above",bins_SocServ[3],"RFCs - bin count:", length(which(W$SocServ>bins_SocServ[3])),"\n")

W$SocServ[which(W_bak$SocServ<=bins_SocServ[1])] <- mods_SocServ[1]
W$SocServ[which(W_bak$SocServ>bins_SocServ[1] & W_bak$SocServ<=bins_SocServ[2])] <- mods_SocServ[2] 
W$SocServ[which(W_bak$SocServ>bins_SocServ[2] & W_bak$SocServ<=bins_SocServ[3])] <- mods_SocServ[3]
W$SocServ[which(W_bak$SocServ>bins_SocServ[3])] <- mods_SocServ[4]

W$SocServ <- as.factor(W$SocServ)

# IAO
summary(W$IAO)
bins_IAO <- c(14,23,33)
mods_IAO <- c("L","M","MH","H")
cat("IAO:\n   below or equal to",bins_IAO[1],"RFCs - bin count:",length(which(W$IAO<=bins_IAO[1])),"\n")
cat("   between",bins_IAO[1]+1,"and",bins_IAO[2],"RFCs - bin count:", length(which(W$IAO>bins_IAO[1] & W$IAO<=bins_IAO[2])),"\n")
cat("   between",bins_IAO[2]+1,"and",bins_IAO[3],"RFCs- bin count:", length(which(W$IAO>bins_IAO[2] & W$IAO<=bins_IAO[3])),"\n")
cat("   above",bins_IAO[3],"RFCs - bin count:", length(which(W$IAO>bins_IAO[3])),"\n")

W$IAO[which(W_bak$IAO<=bins_IAO[1])] <- mods_IAO[1]
W$IAO[which(W_bak$IAO>bins_IAO[1] & W_bak$IAO<=bins_IAO[2])] <- mods_IAO[2] 
W$IAO[which(W_bak$IAO>bins_IAO[2] & W_bak$IAO<=bins_IAO[3])] <- mods_IAO[3]
W$IAO[which(W_bak$IAO>bins_IAO[3])] <- mods_IAO[4]

W$IAO <- as.factor(W$IAO)

# EnvProt
summary(W$EnvProt)
bins_EnvProt <- c(16,26,41)
mods_EnvProt <- c("L","M","MH","H")
cat("EnvProt:\n   below or equal to",bins_EnvProt[1],"RFCs - bin count:",length(which(W$EnvProt<=bins_EnvProt[1])),"\n")
cat("   between",bins_EnvProt[1]+1,"and",bins_EnvProt[2],"RFCs - bin count:", length(which(W$EnvProt>bins_EnvProt[1] & W$EnvProt<=bins_EnvProt[2])),"\n")
cat("   between",bins_EnvProt[2]+1,"and",bins_EnvProt[3],"RFCs- bin count:", length(which(W$EnvProt>bins_EnvProt[2] & W$EnvProt<=bins_EnvProt[3])),"\n")
cat("   above",bins_EnvProt[3],"RFCs - bin count:", length(which(W$EnvProt>bins_EnvProt[3])),"\n")

W$EnvProt[which(W_bak$EnvProt<=bins_EnvProt[1])] <- mods_EnvProt[1]
W$EnvProt[which(W_bak$EnvProt>bins_EnvProt[1] & W_bak$EnvProt<=bins_EnvProt[2])] <- mods_EnvProt[2] 
W$EnvProt[which(W_bak$EnvProt>bins_EnvProt[2] & W_bak$EnvProt<=bins_EnvProt[3])] <- mods_EnvProt[3]
W$EnvProt[which(W_bak$EnvProt>bins_EnvProt[3])] <- mods_EnvProt[4]

W$EnvProt <- as.factor(W$EnvProt)

# Violation
summary(W$Violation)
bins_Violation <- c(7,20,38)
mods_Violation <- c("L","M","MH","H")
cat("Violation:\n   below or equal to",bins_Violation[1],"RFCs - bin count:",length(which(W$Violation<=bins_Violation[1])),"\n")
cat("   between",bins_Violation[1]+1,"and",bins_Violation[2],"RFCs - bin count:", length(which(W$Violation>bins_Violation[1] & W$Violation<=bins_Violation[2])),"\n")
cat("   between",bins_Violation[2]+1,"and",bins_Violation[3],"RFCs- bin count:", length(which(W$Violation>bins_Violation[2] & W$Violation<=bins_Violation[3])),"\n")
cat("   above",bins_Violation[3],"RFCs - bin count:", length(which(W$Violation>bins_Violation[3])),"\n")

W$Violation[which(W_bak$Violation<=bins_Violation[1])] <- mods_Violation[1]
W$Violation[which(W_bak$Violation>bins_Violation[1] & W_bak$Violation<=bins_Violation[2])] <- mods_Violation[2] 
W$Violation[which(W_bak$Violation>bins_Violation[2] & W_bak$Violation<=bins_Violation[3])] <- mods_Violation[3]
W$Violation[which(W_bak$Violation>bins_Violation[3])] <- mods_Violation[4]

W$Violation <- as.factor(W$Violation)

# Misdemeanor
summary(W$Misdemeanor)
bins_Misdemeanor <- c(33,87,178)
mods_Misdemeanor <- c("M","H","VH","OC")
cat("Misdemeanor:\n   below or equal to",bins_Misdemeanor[1],"RFCs - bin count:",length(which(W$Misdemeanor<=bins_Misdemeanor[1])),"\n")
cat("   between",bins_Misdemeanor[1]+1,"and",bins_Misdemeanor[2],"RFCs - bin count:", length(which(W$Misdemeanor>bins_Misdemeanor[1] & W$Misdemeanor<=bins_Misdemeanor[2])),"\n")
cat("   between",bins_Misdemeanor[2]+1,"and",bins_Misdemeanor[3],"RFCs- bin count:", length(which(W$Misdemeanor>bins_Misdemeanor[2] & W$Misdemeanor<=bins_Misdemeanor[3])),"\n")
cat("   above",bins_Misdemeanor[3],"RFCs - bin count:", length(which(W$Misdemeanor>bins_Misdemeanor[3])),"\n")

W$Misdemeanor[which(W_bak$Misdemeanor<=bins_Misdemeanor[1])] <- mods_Misdemeanor[1]
W$Misdemeanor[which(W_bak$Misdemeanor>bins_Misdemeanor[1] & W_bak$Misdemeanor<=bins_Misdemeanor[2])] <- mods_Misdemeanor[2] 
W$Misdemeanor[which(W_bak$Misdemeanor>bins_Misdemeanor[2] & W_bak$Misdemeanor<=bins_Misdemeanor[3])] <- mods_Misdemeanor[3]
W$Misdemeanor[which(W_bak$Misdemeanor>bins_Misdemeanor[3])] <- mods_Misdemeanor[4]

W$Misdemeanor <- as.factor(W$Misdemeanor)

# Felony
summary(W$Felony)
bins_Felony <- c(17,45,91)
mods_Felony <- c("ML","M","H","VH")
cat("Felony:\n   below or equal to",bins_Felony[1],"RFCs - bin count:",length(which(W$Felony<=bins_Felony[1])),"\n")
cat("   between",bins_Felony[1]+1,"and",bins_Felony[2],"RFCs - bin count:", length(which(W$Felony>bins_Felony[1] & W$Felony<=bins_Felony[2])),"\n")
cat("   between",bins_Felony[2]+1,"and",bins_Felony[3],"RFCs- bin count:", length(which(W$Felony>bins_Felony[2] & W$Felony<=bins_Felony[3])),"\n")
cat("   above",bins_Felony[3],"RFCs - bin count:", length(which(W$Felony>bins_Felony[3])),"\n")

W$Felony[which(W_bak$Felony<=bins_Felony[1])] <- mods_Felony[1]
W$Felony[which(W_bak$Felony>bins_Felony[1] & W_bak$Felony<=bins_Felony[2])] <- mods_Felony[2] 
W$Felony[which(W_bak$Felony>bins_Felony[2] & W_bak$Felony<=bins_Felony[3])] <- mods_Felony[3]
W$Felony[which(W_bak$Felony>bins_Felony[3])] <- mods_Felony[4]

W$Felony <- as.factor(W$Felony)
}
cat("Active categorical factors:",names(W[which(sapply(W, is.factor))]),"\n")
length(names(W[which(sapply(W, is.factor))]))

par(mfrow=c(1,1))

mcaNYC311 <- MCA(W,ncp=5,
               quanti.sup=c(18,19),
               quali.sup=c(1),
               excl=NULL,
               graph = T,
               level.ventil = 0.00,
               axes = c(1,2),
               row.w = NULL,
               method="Indicator",
               na.method="NA",
               tab.disj=NULL)
plot.MCA(mcaNYC311,
         choix="ind",
         autoLab="yes",
         cex=0.8,
         col.ind = "blue", col.var = "red", col.quali.sup = "darkgreen",
         col.ind.sup = "darkblue", col.quanti.sup = "blue",
         label = c("var","quali.sup","quanti.sup"),
         habillage=c(1),
         #palette=palette(rainbow(30))
         )
summary(mcaNYC311,nbelements=12)

fviz_mca_ind(mcaNYC311,
             axes = c(1, 2),
             geom="point",
             col.ind = "cos2",
             label="none", habillage=c(1),   # specifies index of factor var in data, used for color and ellipses
             addEllipses=TRUE, ellipse.level=0.95)
fviz_mca_ind(mcaNYC311,
             axes = c(1, 2),
             geom="point",
             col.ind = "cos2")






