# #############################
## Project:     Analysis of NYC-311 Service Request Calls
## Script:      11_ca-pca-varimax.R
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Last edit:   Sept 27, 2018
# #############################

rm(list=ls(all=TRUE))

# #############################
## Source parameter file
# #############################

setwd("~/Documents/Work/Academic-research/visualCity/")

set.seed(932178)
options(scipen=6) # R switches to sci notation above 5 digits on plot axes

source(file="Scripts/01_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...


# #############################
## Libraries
# #############################

setRepositories(ind = c(1:6,8))
library(FactoMineR)     # to use canned PCA and CA methods. 
require(factoextra)     # to use enhanced graph functions
require(graphics)       # enhanced graphics
library(ggplot2)        # to enhance graph plotting
library(ggrepel)        # to plot with well behaved labeling
library(MASS)           # to calculate a matrix' pseudoinverse with ginv()


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

exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
}    # exit function. Use with caution: not standard, depends on OS's internals

# #############################
## Load data
# #############################

source_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                      "00_nyc_simple-whole-data-set.csv"
)
X <- read.csv(source_file,
              header=T,
              sep=",",
              quote="\"",
              dec=".",
              skip=0)

# #############################
##  Contingency table   --   w/o ghost ZIP "99999"
# #############################

## Build table

rownames(X) <- X[,1]
X <- X[,-1]
Y <- X[! rownames(X) %in% c("99999"),2:14]  # bogus ZIP row is suppressed
# (not a supplementary factor among row profiles)

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
cntTot <- sum(Y)
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
# DFs (= 180x12 = 2148, for the April-2014 data set) is not taken into account for test statistics 
# computed by Monte-Carlo simulations.
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

# check that sum of all so-calculated contributions to Chi-square = 1
chiSqContribTest <- 0
for (jj in 1:ncol(Y)) {
    for (ii in 1:nrow(Y)) {
            chiSqContribTest <- chiSqContribTest+(chiSqResult$residual[ii,jj])^2 /chiSqResult$statistic
    }
}
chiSqContribTest

length(which(chiSqContrib >=0.01))   # result: 0 cells, good
length(which(chiSqContrib >=0.001))  # result: 8 cells, totally acceptable

# Final conclusion for April 2010 and April 2014
# chi-sq=43294; p =~ 1e-4; We can reject the null hypothesis at the risk 1% of erring
# There is significant association of row and column categorical variables, i.e. we can say that
# service calls to NYC 311 does depend on ZIP codes.


# #############################
## Test of independence 2 (on ZIPs to be suppressed)
# #############################

# test that modalities and suppressed ZIP codes have no association
U <- X[(rownames(X) %in% zeroSRCzip) & (rowSums(X[,2:14]) != 0),2:14]
U <- U[,colSums(U) !=0]
(chiSqResult <- chisq.test(U,
                           simulate.p.value=T,
                           B=10000)
)

# April 2010
# Nbr of cells to supress: 453
# p-value =~ 0.316 >> 0.01;  so again we cannot reject the null hypothesis (H0) and conclude that we the
# suppressed ZIP codes have a random structure wrt to SRCs' modalities.

# April 2014
# Nbr of cells to supress: ~ 520
# p-value =~ 0.0071 < 0.01;  so again we reject the null hypothesis (H0) and conclude that we the
# suppressed ZIP codes have a non random structure wrt to SRCs' modalities.

# April 2018
# Nbr of cells to supress: 
# p-value =~ 0002 < 0.01;  so again we reject the null hypothesis (H0) and conclude that we the
# suppressed ZIP codes have a non random structure wrt to SRCs' modalities.

# #############################
## Balloon plot
# #############################

# library ("gplots")
# par(mfrow=c(1,2))
# dt <- as.table(as.matrix(Y))
# balloonplot(t(dt), main ="NYC311 service calls pero ZIP",
#    nyc311_report_tfm         xlab ="", ylab="",
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
apply(CFj_given_i,1,sum)

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
eigZrows <- eigen( t(Z) %*% Z )
evecRows <- eigZrows$vectors    # eigenvectors for row profiles
evalRows <- eigZrows$values     # eigenvalues
ifelse(sum(diag(t(Z) %*%Z)) - sum(evalRows == 0),"ok", "not ok") # check consistency

eigZcols <- eigen( Z %*% t(Z)) 
evecCols <- eigZcols$vectors        # eigenvectors
evalCols <- eigZcols$values         # eigenvalues  same first 12 eigenvalues as evalRows

# ##############################
## Detect outlier "10281" for April 2010 data ?? maybe
## Detect outlier "10463" for April 2014 data ?? confirmed
## Detect outlier "_____" for April 2018 data ?? __
# ##############################

dev.off()
par(mfrow = c(2,2))
#ind.sup_idx = which(rownames(Y_ctd) %in% c("99999"))
pcaY <- PCA(Y_ctd,
            ncp=5,
            scale.unit=F,
            #ind.sup = c(ind.sup_idx), 
            quanti.sup = NULL, 
            quali.sup = NULL, 
            #row.w = fi[-ind.sup_idx], 
            row.w = fi, 
            col.w = NULL,
            graph = T, axes = c(1,2)
)
pcaY$eig   

if (yearNbr == 2010) {
    ind.sup_idx = which(rownames(Y_ctd) %in% c("10281"))   # April 2010
} else if (yearNbr == 2014) {
    ind.sup_idx = which(rownames(Y_ctd) %in% c("10463"))   # April 2014
} else if (yearNbr == 2018) {
    ind.sup_idx = which(rownames(Y_ctd) %in% c("11430", "11371"))   # April 2018
    # 11430 = JFK airport, Queens
    # 11371 = La Guardia airport, Queens
} else {
    cat("\n\n---------------\nUNKNOWN YEAR\n Abort\n--------------------\n\n")
    exit()
}

pcaY <- PCA(Y_ctd,
            ncp=5,
            scale.unit=F,
            ind.sup = c(ind.sup_idx), 
            quanti.sup = NULL, 
            quali.sup = NULL, 
            row.w = fi[-ind.sup_idx], 
            col.w = NULL, 
            graph = T, axes = c(1,2)
)
pcaY$eig

# ################################

#ind.sup_idx = which(rownames(Y_ctd) %in% c("99999"))
pcaY <- PCA(Y_ctd,
            ncp=5,
            scale.unit=F,
            # ind.sup = c(ind.sup_idx), 
            quanti.sup = NULL, 
            quali.sup = NULL, 
            #row.w = fi[-ind.sup_idx],
            row.w = fi,
            col.w = NULL,
            graph = T, axes = c(1,3)
)

pcaY <- PCA(Y_ctd,
            ncp=5,
            scale.unit=F,
            ind.sup = c(ind.sup_idx), 
            quanti.sup = NULL, 
            quali.sup = NULL, 
            row.w = fi[-ind.sup_idx], 
            col.w = NULL, 
            graph = T, axes = c(1,3)
)

# ################################

#ind.sup_idx = which(rownames(Y_ctd) %in% c("99999"))
pcaY <- PCA(Y_ctd,
            ncp=5,
            scale.unit=F,
            #ind.sup = c(ind.sup_idx), 
            quanti.sup = NULL, 
            quali.sup = NULL, 
            #row.w = fi[-ind.sup_idx], 
            row.w = fi,
            col.w = NULL,
            graph = T, axes = c(2,3)
)

pcaY <- PCA(Y_ctd,
            ncp=5,
            scale.unit=F,
            ind.sup = c(ind.sup_idx), 
            quanti.sup = NULL, 
            quali.sup = NULL, 
            row.w = fi[-ind.sup_idx], 
            col.w = NULL, 
            graph = T, axes = c(2,3)
)
par(mfrow = c(1,1))



## Determine nbr of significant dimensiones, nd, automatically with supplementary individuals 
# in 'ind.sup_idx'.
# 1/ Point successively to every component of the list of evals sorted by decreasing value 
# 2/ When total explained inertia exceeds 70% chose eval index as nbr of significant dims
n_dim <- ncol(Y_ctd)-1
for (nd in 1:n_dim) {  if(pcaY$eig[nd,3] > 72) break  }
cat("Number of significant dimensions:",nd)


## Correlation with PCs and contribution to the construction of each dimension
pcaY$var$cor[,1:nd] # cor = normalized projection of variable on PCs in R^p
                    #     = coord/norm(projected var)

# cos2 = cor^2  ; apply(cos2,1,sum)=1 ; quality of representation of the variable
# contrib = explanatory power of each variable in terms of variance, = "inertia",
#           for each dimension

pcaY$var$contrib[,1:nd]
apply(pcaY$var$contrib[,1:nd],2,sum)  # = 100%, check !
pcaY$eig[,2]

# #############################
## Repeat analysis with FactoMineR, w/o "10463", 
#+ i.e. considering 10463 as supplementary individual
# #############################

# plot **factor** maps in factorial planes PC1-2, PC2-3, PC1-3 where row and col factors 
# are printed together with distinct colors for easier differentiation. 
# Factors, psi_alpha, are projections of either centered row "i" profiles, X_ctd, or of 
# centered col "j" profiles on PC direction u_alpha (evec)
# psi <- t(X_ctd) %*% evecs   # for row profiles

par(mfrow = c(2,2))
# Plotted and appraised bogus ZIP "99999" for missings 
# |_ quality of representation (cos2) in 1st 3 dimensions is bad.

# PC1-2 factorial plane
if (yearNbr == 2010) {
    ind.sup_idx = which(rownames(Y_ctd) %in% c("99999","10281"))  # April 2010
} else if (yearNbr == 2014) {
    ind.sup_idx = which(rownames(Y_ctd) %in% c("99999","10463"))  # April 2014
} else if (yearNbr == 2018) {
    ind.sup_idx = which(rownames(Y_ctd) %in% c("99999","11430","11371"))   # April 2018
} else {
    cat("\n\n---------------\nUNKNOWN YEAR\n Abort\n--------------------\n\n")
    exit()
}

plottitle <- paste0("CA factor map: ",month.abb[monthNbr],".",yearNbr,
                    " NYC311 SRC data/n(before feature selection)")
caY <- CA(Y,
          ncp=13,
          row.sup=ind.sup_idx,
          graph=T,
          axes=c(1,2),
          #title=plottitle, # ineffectual
          excl=NULL)
# Do not specify option 'row.w=fi' 
# By default, vector of 1's and each row has weight equals to its margin;
# Weights given only for active rows

caY <- CA(Y,
          ncp=13,
          row.sup=ind.sup_idx,
          graph=T,
          axes = c(1,3),
          excl=NULL)

caY <- CA(Y,
          ncp=13,
          row.sup=ind.sup_idx,
          graph=T,
          axes = c(2,3),
          excl=NULL)

caY$eig
evalRows <- caY$eig[,1]
# last eval = 0, 0; along with last column in evecs, correspond to the centroid 'sqrt(fj)'
# cumvarexp <- c(100*evalRows[1]/sum(evalRows))
# for (ii in 2:(ncol(Y))) {
#     if(ii==2) cat("Eigenvalues","\t","Cumulative variance (%)\n")
#     cumvarexp <- c(cumvarexp,cumvarexp[ii-1] + 100*evalRows[ii]/sum(evalRows))
#     cat(format(evalRows[ii-1],scientific=T),"\t",round(cumvarexp[ii-1],6),"\n")
# }

## screeplot
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
text(x=0.7*length(evalRows),y=evalRows[1]*0.9,
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

par(mfrow = c(1,1))


# ctr: contributions to the construction of PCs (%) (intra dimension sum=1)
# cos2: factor's representation quality along one PC  [0,1] 
attributes(caY)
summary.CA(caY)  

## for column profiles
# list all factor (categorical modalities) contributions above 10%
tab <- caY$col$contrib[,1:3] # col profiles contributions to 1st 3 PCs.
(colctr.gt.10 <- tab[tab[,1] > 10 | tab[,2] > 10 | tab[,3] > 10 ,])       
(colctr <- tab[tab[,1] > 0 | tab[,2] > 0 | tab[,3] > 0 ,]) # same as issuing: > (colctr <- tab)

## Compute Inertia Explanatory Power (IEP) of each modality
iep_alldim <- c(); iep_sigdim <- c()
Nmod <- length(caY$col$contrib[,1]) # Nbr of SRCs' modalities 
for (mm in 1:Nmod) {
    modLabel <- rownames(caY$col$contrib)[mm]
    # For all dim
    iep <- 0
    for (dd in 1:(ncol(caY$col$contrib))) {
        iep <- iep+caY$col$contrib[mm,dd]*caY$eig[dd,2]/100
    }
    iep_alldim  <- c(iep_alldim,iep)
    
    # For sd dimendions (significant dimensions only)
    iep <- 0
    for (dd in 1:nd) {
        iep <- iep+caY$col$contrib[mm,dd]*caY$eig[dd,2]/sum(caY$eig[1:nd,2])
    }
    iep_sigdim  <- c(iep_sigdim,iep)
}
# Display
iep.df <- cbind("iep_alldim" = round(iep_alldim,1), "iep_sigdim" = round(iep_sigdim,1))
rownames(iep.df) <- rownames(caY$col$contrib)
show(iep.df)

# quality of representation of col profiles with biggest contrib to PC formation
ctrNames <- rownames(colctr.gt.10)
tab <- caY$col$cos2[,1:3]
( colctr.gt.10_rep <- tab[rownames(tab) %in% ctrNames,] )

## for row profiles
# list all contributions above 2%
tab <- round(caY$row$contrib[,1:3],1) # col profiles contributions to 1st 3 PCs.
(rowctr.gt.2 <- tab[tab[,1] > 2 | tab[,2] > 2 | tab[,3] > 2 ,])
(maxCtrDim1 <- tab[tab[,1] == max(tab[,1]),])   # max contributor to construct of  Dim 1
(maxCtrDim2 <- tab[tab[,2] == max(tab[,2]),])  # max contributor to construct of  Dim 2
(maxCtrDim3 <- tab[tab[,3] == max(tab[,3]),])   # max contributor to construct of  Dim 3

if (yearNbr == 2010) {
    tab[rownames(tab)== "10281",]  # April 2010 -- investigate ZIP 10281 (Battery Park City)
} else if (yearNbr == 2014) {
    tab[rownames(tab)== "10463",]  # April 2014 -- investigate ZIP 10463 (Riverdale)
} else if (yearNbr == 2018) {
    tab[rownames(tab) %in% c("11430","11371"),]  # April 2018 -- investigate ZIP _____ ()
} else {
    cat("\n\n---------------\nUNKNOWN YEAR\n Abort\n--------------------\n\n")
    exit()
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

evalRows <- caY$eig[,1]
    
# screeplot
plottitle = sprintf("PC explanatory power")
subtitle = paste0("(",month.abb[monthNbr],". ",yearNbr," NYC311 SRC data/nbefore feature selection)")
plot(seq(1:length(evalRows)),evalRows,
     pch=15, 
     cex=1,
     col="blue",
     type="b",
     main=plottitle,
     xlab="",
     ylab="Eigenvalue",
     sub=subtitle
)
text(x=1:length(evalRows), y=evalRows, 
     labels=paste0(as.character(round(caY$eig[,3],1)),"%"),
     cex=0.9,
     pos=4,
     col="red") # add labels
text(x=0.7*length(evalRows),y=evalRows[1]*0.9,
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


# ########################
## Feature extraction & selection
if (yearNbr == 2010) {
    ## April 2010
    # ########################
    # "EnvProt" and "WaterSyst" are strongly correlated in PC1-2-3
    # "IAO", "NoiseTraf" and "SocServ" are weakly correlated with PCs and can be dispensed with.
    
    Ysimple <- cbind(HousCond=Y[,1],Y[,2:7],EPwater=Y[,8]+Y[,12],Y[,c(9:11,13)])
    sup_cols <- c(2,11,12)
    
} else if (yearNbr == 2014) {
    ## April 2014
    # ########################
    # "EnvProt" and "Sani"  are weakly correlated with 3 first PCs and are collinear
    #   => add corresponding frequencies in new feature "EPsani", to decrease dimensionality
    # EnvProt and WaterSyst appear strongly correlated in planes PC1-2 and PC1-3, but anti-
    #   correlated in PC2-3. WaterSyst has a poor Inertia Explanatory Power with IEP < 5%
    #   for the retained significant dimensions.  Eliminate "WaterSyst" to decrease dimensionality.
    Ysimple <- cbind(HousCond=Y[,1],EPsani=Y[,2]+Y[,13],Y[,3:8],Y[,9:12])

        # "HousCond" as well as "SocServ" and "IAO" are weakly coorelated with 3 first PCs
    #   and are eliminated. 
    # "ConsumProt" however, although apparently strongly correlated with "NoiseConst" cannot be 
    #   joined with it as no satisfactory justification was found to explain the apparent correlation.  
    sup_cols <- c(1,9,11,12)
    
} else if (yearNbr == 2018) {
    Ysimple <- Y
    sup_cols <- c(5,10,11)
    
} else {
    cat("\n\n---------------\nUNKNOWN YEAR\n Abort\n--------------------\n\n")
    exit()
}


par(mfrow = c(1,2))
caYsimple <- CA(Ysimple,
                ncp=ncol(Ysimple)-1,
                row.sup=ind.sup_idx,
                col.sup=sup_cols,
                graph=T,
                axes = c(1,2),
                excl=NULL)
# caYsimple <- CA(Ysimple,
#                 ncp=ncol(Ysimple)-1,
#                 row.sup=ind.sup_idx,
#                 col.sup=sup_cols,
#                 graph=T,
#                 axes = c(1,3),
#                 excl=NULL)
# caYsimple <- CA(Ysimple,
#                 ncp=ncol(Ysimple)-1,
#                 row.sup=ind.sup_idx,
#                 col.sup=sup_cols,
#                 graph=T,
#                 axes = c(2,3),
#                 excl=NULL)
caYsimple$eig
evalRows <- caYsimple$eig[,1]
# last eval = 0, 0; along with last column in evecs, correspond to the centroid 'sqrt(fj)'


## Determine nbr of significant dimensiones, ndSimple, automatically
# 1/ Point successively to every component of the list of evals sorted by decreasing value 
# 2/ When total explained inertia exceeds inertia_threshold (%) chose eval index as nbr of
#    significant dims
inertia_threshold=72
n_dim <- ncol(Ysimple) - length(sup_cols)
for (ndSimple in 1:n_dim) {  if(caYsimple$eig[ndSimple,3] >= inertia_threshold) break  }
cat("Number of significant dimensions:",ndSimple,"\n")

# cumvarexp <- c(100*evalRows[1]/sum(evalRows))
# for (ii in 2:(ncol(Y))) {
#     if(ii==2) cat("Eigenvalues","\t","Cumulative variance (%)\n")
#     cumvarexp <- c(cumvarexp,cumvarexp[ii-1] + 100*evalRows[ii]/sum(evalRows))
#     cat(format(evalRows[ii-1],scientific=T),"\t",round(cumvarexp[ii-1],6),"\n")
# }

# ########################
## screeplot
# ########################

plottitle = sprintf("PC explanatory power")
subtitle = paste0("(",month.abb[monthNbr],". ",yearNbr," NYC311 SRC data\nafter feature selection)")
plot(seq(1:length(evalRows)),evalRows,
     pch=15, 
     cex=1,
     col="blue",
     type="b",
     main=plottitle,
     #sub="(after feature selection)",
     sub=subtitle,
     xlab="",
     ylab="Eigenvalue"
)
text(x=1:length(evalRows), y=evalRows, 
     labels=paste0(as.character(round(caYsimple$eig[,3],1)),"%"),
     cex=0.9,
     pos=4,
     col="red") # add labels
text(x=0.7*length(evalRows),y=evalRows[1]*0.9,
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

par(mfrow = c(1,1))

caYsimple$eig

plot.CA(caYsimple,
        axes = c(1,2),
        invisible="row"
)

# ctr: contributions to the construction of PCs (%) (intra dimension sum=1)
# cos2: factor's representation quality along one PC  [0,1] 
attributes(caYsimple)
summary.CA(caYsimple)

## for column profiles
# list all contributions above 10%
tab <- caYsimple$col$contrib[,1:3] # col profiles contributions to 1st 3 PCs.
(colctr.gt.10 <- tab[tab[,1] > 10 | tab[,2] > 10 | tab[,3] > 10 ,])

# quality of representation of col profiles with biggest contrib to PC formation
ctrNames <- rownames(colctr.gt.10)
tab <- caYsimple$col$cos2[,1:3]
( colctr.gt.10_rep <- tab[rownames(tab) %in% ctrNames,] )

## for row profiles
# list all contributions above 10%
tab <- round(caYsimple$row$contrib[,1:3],1) # col profiles contributions to 1st 3 PCs.
(rowctr.gt.2 <- tab[tab[,1] > 2 | tab[,2] > 2 | tab[,3] > 2 ,])
(maxCtrDim1 <- tab[tab[,1] == max(tab[,1]),])   # max contributor to construct of  Dim 1
(maxCtrDim2 <- tab[tab[,2] == max(tab[,2]),])   # max contributor to construct of  Dim 2
(maxCtrDim3 <- tab[tab[,3] == max(tab[,3]),])   # max contributor to construct of  Dim 3

if (yearNbr == 2010) {
    tab[rownames(tab)== "10281",]  # April 2010 -- investigate ZIP 10281 (Battery Park City)
} else if (yearNbr == 2014) {
    tab[rownames(tab)== "10463",]  # April 2014 -- investigate ZIP 10463 (Riverdale)
} else if (yearNbr == 2018) {
    tab[rownames(tab) %in% c("11430","11371"),]  # April 2018 -- investigate ZIP _____ ()
} else {
    cat("\n\n---------------\nUNKNOWN YEAR\n Abort\n--------------------\n\n")
    exit()
}

## Compute Inertia Explanatory Power (IEP) of each modality
iep_alldim <- c(); iep_sigdim <- c()
Nmod <- length(caYsimple$col$contrib[,1]) # Nbr of SRCs' modalities 
for (mm in 1:Nmod) {
    modLabel <- rownames(caYsimple$col$contrib)[mm]
    # For all dim
    iep <- 0
    for (dd in 1:(ncol(caYsimple$col$contrib))) {
        iep <- iep+caYsimple$col$contrib[mm,dd]*caYsimple$eig[dd,2]/100
    }
    iep_alldim  <- c(iep_alldim,iep)
    
    # For ndSimple dimendions (significant dimensions only)
    iep <- 0
    for (dd in 1:ndSimple) {
        iep <- iep+caYsimple$col$contrib[mm,dd]*caYsimple$eig[dd,2]/sum(caYsimple$eig[1:ndSimple,2])
    }
    iep_sigdim  <- c(iep_sigdim,iep)
}
# Save two lists in df
iep.df <- cbind("iep_alldim" = round(iep_alldim,1), "iep_sigdim" = round(iep_sigdim,1))
rownames(iep.df) <- rownames(caYsimple$col$contrib)
show(iep.df)


# ########################
## ind + var plot with FactoMineR
# ########################


if (yearNbr == 2010) {
    ## April 2010
    # ########################
    # "EnvProt" and "WaterSyst" are strongly correlated in PC1-2-3
    # "IAO", "NoiseTraf" and "SocServ" are weakly correlated with PCs and can be dispensed with.
    
    Ysimple_ctd <- cbind(HousCond=Y_ctd[,1],EPsani=Y_ctd[,2]+Y_ctd[,13],Y_ctd[,3:8],Y_ctd[,9:12])
    ind.sup_idx <- which(rownames(Ysimple_ctd) %in% c("99999","11430", "10281"))
    col.sup_idx <- which(colnames(Ysimple_ctd) %in% c("IAO","SocServ","WaterSyst","HousCond"))
    
} else if (yearNbr == 2014) {
    ## April 2014
    # ########################
    # "EnvProt" and "Sani"  are weakly correlated with 3 first PCs and are collinear
    #   => add corresponding frequencies in new feature "EPsani", to decrease dimensionality
    # EnvProt and WaterSyst appear strongly correlated in planes PC1-2 and PC1-3, but anti-
    #   correlated in PC2-3. WaterSyst has a poor Inertia Explanatory Power with IEP < 5%
    #   for the retained significant dimensions.  Eliminate "WaterSyst" to reduce dimensionality.
    Ysimple_ctd <- cbind(EPsani=Y_ctd[,1]+Y_ctd[,13],Y_ctd[,c(2:12)])
    ind.sup_idx <- which(rownames(Ysimple_ctd) %in% c("99999","10430", "10048"))
    col.sup_idx <- which(colnames(Ysimple_ctd) %in% c("IAO","SocServ","WaterSyst"))
    
} else if (yearNbr == 2018) {
    ## April 2018
    # ########################
    # "EnvProt", "Sani" and "WaterSyst" are not collinear pairwise
    # On the other hand IAO, SOcServ and WaterSyst seems both weakly correlated and rather 
    # poorly represented with significant dimensions.
    # Eliminate them to reduce dimensionality.
    Ysimple_ctd <- Y_ctd
    ind.sup_idx <- which(rownames(Ysimple_ctd) %in% c("99999","11371", "11430"))
    col.sup_idx <- which(colnames(Ysimple_ctd) %in% c("IAO","SocServ","WaterSyst"))
    
} else {
    cat("\n\n---------------\nUNKNOWN YEAR\n Abort\n--------------------\n\n")
    exit()
}


par(mfrow = c(1,3))
componentNbr <- 9
pcaYsimple <- PCA(Ysimple_ctd,
                  ncp=componentNbr,
                  scale.unit=F,
                  ind.sup = ind.sup_idx, 
                  quali.sup = col.sup_idx,
                  quanti.sup=NULL,
                  row.w = fi[-ind.sup_idx], 
                  col.w = NULL, 
                  graph = T, axes = c(1,2))

plot(pcaYsimple,
     axes = c(1,2),
     choix = "ind",
     # habillage=1,
     # col.hab=c("black","blue","yellow","orange","green","violet"),
     col.ind="blue",
     col.quali="green",
     col.ind.sup="red",
     label="ind.sup",
     unselect=0.8,
     cex=0.85
     # selectRow="cos2 0.75",
     # selectCol="cos2 0.2"
)
grid()
par(mfrow = c(1,1))


par(mfrow = c(1,2))
plottitle <-paste("PCA - Biplot -", month.name[monthNbr],yearNbr ,"(All individuals)")
fviz_pca_biplot(pcaYsimple,
                # col.ind = "blue",
                col.ind ="cos2",
                col.ind.sup="orange",
                col.var = "red",
                label="var",
                xlim=c(-1,1),ylim=c(-1.5,1.5),
                title=plottitle
)

fviz_pca_biplot(pcaYsimple,
                # col.ind = "blue",
                col.ind ="cos2",
                col.ind.sup="orange",
                col.var = "red",
                label="var",
                xlim=c(-1,1),ylim=c(-1.5,1.5),
                select.ind=list(contrib = 80),
                title="(Individual contribution >= 80%)"
)
par(mfrow = c(1,1))

# ########################
## Plot of individuals' projection - by hand
# ########################

# suppress bogus ZIPXs, outliers as sup indivs.
Z <- X[!rownames(X) %in% c("99999","11371", "11430",zeroSRCzip),1:14]  

# feature selection + extraction
if (yearNbr == 2010) {
    # suppress bogus ZIPXs,and low count cells.
    Z <- X[!rownames(X) %in% c("99999",zeroSRCzip),1:14] 
    # feature selection + extraction
    Z <- cbind(Borough=Z[,1],Z[,4:9],EPsani=Z[,3]+Z[,14],ConsumProt=Z[,11]) # feature selection + extraction
} else if (yearNbr == 2014) {
    # suppress bogus ZIPXs, outliers as sup indivs.
    Z <- X[!rownames(X) %in% c("99999",zeroSRCzip),1:14] 
    # feature selection + extraction
    # Z <- cbind()
    exit()
} else if (yearNbr == 2018) {
    # suppress bogus ZIPXs, outliers (?) and low count cells.
    Z <- X[!rownames(X) %in% c("99999","11371", "11430",zeroSRCzip),1:14]  
    # do nothing
} else {
    cat("\n\n---------------\nUNKNOWN YEAR\n Abort\n--------------------\n\n")
    exit()
}
    
cntTot <- sum(Z[,-1])
fij <- Z[,-1]/cntTot
fi <- rowSums(fij)
fj <- colSums(fij)
rowCentroid <- sqrt(fj) 
colCentroid <- sqrt(fi)
Di <- diag(fi)
Dj <- diag(fj)
CFj_given_i <- solve(Di) %*% as.matrix(fij)
Fim <- CFj_given_i %*% solve(sqrt(Dj))
unitaryM <- matrix(1,nrow=nrow(Fim), ncol=ncol(Fim), byrow=T)
Z_ctd <- Fim - unitaryM %*% sqrt(Dj)  # centering
rownames(Z_ctd) <- rownames(Z)
colnames(Z_ctd) <- colnames(Z[,-1])
Z_ctd <- cbind(Borough=as.character(Z[-ind.sup_idx,1]),Z_ctd[-ind.sup_idx,])
Z_tmp <- apply(as.matrix(Z_ctd[,-1]),2, as.numeric)  # transform data.frame in numeric matrix

covZ <- t(Z_tmp) %*% diag(fi[-ind.sup_idx])  %*% Z_tmp   # compute cov matrix with row marginals as obs weights

evalsZ <- eigen(covZ)$values
cat("Eigenvalues (obs): ",round(evalsZ,4),"\n")  # ok
evecsZ <- eigen(covZ)$vectors   
cat("Eigenvectors (obs):","\n"); evecsZ   # same as 'evecsRows' above

psiZ <- Z_tmp %*% evecsZ  # matrix of scores, values of each individual's p components along the PC axes 
colnames(psiZ) <- paste0("PC",1:ncol(psiZ))
rownames(psiZ) <- rownames(Z_ctd)

Boroughs <- as.factor(Z_ctd[,1])

par(mfrow = c(1,1))

plottitle=sprintf("Row profiles\' projection in PC1-2 factorial plane")
plottitle <- paste0(plottitle,"\n(",month.abb[monthNbr],". ",yearNbr," SRC data after feature selection)")
plotdata <- data.frame(PC1=psiZ[,1],PC2=psiZ[,2],z=rep("",nrow(Z_tmp)))
#plotdata <- data.frame(PC1=psiZ[,1],PC2=psiZ[,2],z=matrix(rep("",nrow(Z)),nrow=nrow(Z),byrow=T))
plot1 <- ggplot(data = plotdata) + 
    theme_bw() +
    geom_vline(xintercept = 0, col="gray") +
    geom_hline(yintercept = 0, col="gray") +
    # geom_text_repel(aes(PC1,PC2,label = z),
    #                 size=3,
    #                 point.padding = 0.5,
    #                 box.padding = unit(0.55, "lines"),
    #                 segment.size = 0.3,
    #                 segment.color = 'grey') +
    geom_point(aes(PC1,PC2,col=Boroughs),size = 2) +
    scale_color_discrete(name = 'Borough') +
    labs(title = plottitle)

plot1 +
    scale_color_manual(values=ccolors[1:length(levels(Boroughs))])

par(mfrow = c(1,1))

# ########################
## Compute contributions of each borough to inertia over all dimensions
#  and in 1st factorial plane over 2 significant dimensions
# ########################

for ( bb in unique(as.character(factor(Boroughs))) ) {
    Nindiv <- nrow(Z_ctd[Z_ctd[,1] == bb,])
    indIEP <- matrix(NA,ncol=2,nrow=nrow(pcaYsimple$ind$contrib[which(Z_ctd[,1] == bb),]))
    rownames(indIEP) <- rownames(pcaYsimple$ind$contrib[which(Z_ctd[,1] == bb),])
    colnames(indIEP) <- c("All_dim","PC1-2")
    cnt <- 0
    for ( indiv_idx in as.numeric(which(Z_ctd[,1] == bb)) ) {
        indIEPall <- 0
        indIEPsignif <- 0
        cnt <- cnt+ 1
#        for ( eval_idx in 1:(nrow(pcaYsimple$eig)-1) ) {
        for ( eval_idx in 1:componentNbr ) {
            indIEPall = indIEPall + pcaYsimple$ind$contrib[indiv_idx,eval_idx] * pcaYsimple$eig[eval_idx,2]/100
            if (eval_idx <= ndSimple) {
                indIEPsignif = indIEPsignif + pcaYsimple$ind$contrib[indiv_idx,eval_idx] * pcaYsimple$eig[eval_idx,2]/100
            }
        }
        indIEP[cnt,1] <- round(indIEPall,1)
        indIEP[cnt,2] <- round(indIEPsignif,1)
    }
    cat("\nBorough: ",bb,"\n","Number of ZIP codes: ",Nindiv,"\n")
    cat("Borough's ZIPs' % contribution to inertia (overall and in PC1-2 factorial plane):\n")
    print(apply(indIEP,2,sum))
    #print(indIEP)
}


# ########################
## Topographic plot of color-coded ZIPs (PC1-2 projections before varimax)
# ########################
library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")      # to read shapefile (translate GPS into ZIP)
library("fastshp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")
#library("colorspace")

#pal <- choose_palette()

# Use zipped achive at https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
mapZIP <- readOGR(dsn="./Data/Geolocation", layer="ZIP_CODE_040114")
shp <- read.shp("Data/Geolocation/ZIP_CODE_040114.shp", format="list")

ZIP_lst <- c()
ZIP_lst <- c(rownames(Z),zeroSRCzip)
#tail(ZIP_lst,27)
# retrieve mapZIP's ZIP indices to be mapped
mapZIP_idx <- which(mapZIP$ZIPCODE %in% ZIP_lst)

mapZIPborough <- c()
for (index in mapZIP_idx) {
    mapZIPborough <- c(mapZIPborough,as.character(X[as.character(mapZIP$ZIPCODE[index]) == rownames(X),1]))
}
mapZIPborough <- as.factor(as.numeric(as.factor(mapZIPborough)))
# Compute & plot overall area bounding box
xMinBox <- c() ; yMinBox <- c() ; xMaxBox <- c() ; yMaxBox <- c();
for (index in mapZIP_idx) {
    xMinBox <- c(xMinBox,shp[[index]]$box[1])
    yMinBox <- c(yMinBox,shp[[index]]$box[2])
    xMaxBox <- c(xMaxBox,shp[[index]]$box[3])
    yMaxBox <- c(yMaxBox,shp[[index]]$box[4])
}

xMin <- min(xMinBox); yMin <- min(yMinBox)
xMax <- max(xMaxBox); yMax <- max(yMaxBox)

# color-code ZIP codes according to quadrant position on PC1-2 row profile projection 
# coordinates are: x=-psiZ[,1]; y=-psiZ[,2]
# 1st quadrant x>=0, y>=0  ---- orchid2
# 2nd quadrant x>=0, y<=0  ---- orangered3
# 3rd quadrant x<=0, y<=0  ---- tan
# 4th quadrant x<=0, y>=0  ---- green
zipQuadrant <- ifelse(-psiZ[,1] >=0,ifelse(psiZ[,2] >=0,"orchid2","orangered3"),ifelse(psiZ[,2] >=0,"green","tan"))
zipWeight <- fi[-ind.sup_idx] / max(fi[-ind.sup_idx])

# draw initial ghost ZIP perimeter
plottitle <- "Mapped NYC ZIP codes (5 boroughs)"
plottitle <- paste0(plottitle,"\n(",month.abb[monthNbr],". ",yearNbr," SRC data after feature selection)")
plot(x=c((xMin+xMax)/2),y=c((yMin+yMax)/2),
     xlim=c(xMin,xMax),ylim=c(yMin,yMax),
     axes=FALSE,
     type="p",pch=".",col="darkgreen",xlab="",ylab="", main=plottitle)
axis(1,at=c(),labels=F,col.axis="black",tck=0)
axis(2,labels=F,col.axis="black",tck=0)

for ( ii in 1:length(mapZIP_idx) ) {
    index <- mapZIP_idx[ii]
    #lines(shp[[index]]$x, shp[[index]]$y,type="l",col="skyblue",xlab="x",ylab="y") # draw ZIP perimeter
    lines(shp[[index]]$x, shp[[index]]$y,type="l",col=ccolors[mapZIPborough[ii]],xlab="x",ylab="y") # draw ZIP perimeter
    
    # lines(c(shp[[index]]$box[1],shp[[index]]$box[1],shp[[index]]$box[3],shp[[index]]$box[3],shp[[index]]$box[1]),
    #       c(shp[[index]]$box[2],shp[[index]]$box[4],shp[[index]]$box[4],shp[[index]]$box[2],shp[[index]]$box[2]),
    #       type="l", lwd=1,col="red") # draw ZIP box
    
    ##put zip code string at the center of each zip area bounding box
    localZIP <- mapZIP$ZIPCODE[index]
    # text(x=(xMinBox[ii]+xMaxBox[ii])/2,
    #      y=(yMinBox[ii]+yMaxBox[ii])/2,
    #      adj=0.5,labels=localZIP,
    #      col="steelblue",
    #      cex=0.5)
    
    if (as.character(localZIP) %in% rownames(Z_ctd)) {
        #print(ii)
        points(x=(xMinBox[ii]+xMaxBox[ii])/2,y=(yMinBox[ii]+yMaxBox[ii])/2,
               type = "p",
               col = as.character(zipQuadrant[which(labels(zipQuadrant) == as.character(localZIP))]),
               bg = as.character(zipQuadrant[which(labels(zipQuadrant) == as.character(localZIP))]),
               pch = 21,
               cex = 3 * as.numeric(zipWeight[which(labels(zipWeight) == as.character(localZIP))]),
               alpha=I(1/5)
        )
    }
}
legend("topleft",legend=levels(Boroughs),lwd=4,col=ccolors[1:length(levels(Boroughs))])
legend("bottomright",legend=c(1,2,3,4),pch=16,
       col= c("orchid2","orangered3","tan","green"),
       horiz=T,
       title="PC1-2 quadrants:", 
       bty="n", 
       pt.cex=1.3, 
       cex=0.8)

# ########################
## Apply varimax to loadings in PC1-2  (MVA slides 03, pp 20~24)
# ########################
# Recall that loadings are evecs scaled by the square roots of their respective evals
require(stats)  # part of 'base' package

(  pcaYsimple.rot <- varimax(pcaYsimple$var$cor[,1:ndSimple])  )

Zs <- scale(apply(Z_ctd[,-1],2,as.numeric),center=T,scale=F)
rownames(Zs) <- rownames(Z_ctd)
sd_var <- apply(Zs[,-col.sup_idx],2,sd)

p <- ncol(Zs[,-col.sup_idx]) 
Phi.rot <- diag(sd_var) %*% pcaYsimple.rot$loadings[1:p,]
tags <- colnames(Z_ctd[,-c(1,col.sup_idx+1)])

lmb.rot <- diag(t(Phi.rot) %*% Phi.rot)
sum(lmb.rot)
sum(pcaYsimple$eig[1:ndSimple,1])

# Psi_stan.rot = Zs %*% solve(cov(Zs)) %*% Phi.rot
# Psi.rot = Psi_stan.rot %*% diag(sqrt(lmb.rot))
# apply(Psi.rot,2,var) 

axes_orig <- rep(0,p)
plottitle <- "Varimax projection\n of SRCs' modalities in PC1-2 factorial plane"
subtitle <- paste("(",month.name[monthNbr],yearNbr,")")
plot(Phi.rot,
     type="n",
     xlim=c(1.2*min(Phi.rot[,1]),1.2*max(Phi.rot[,1])),
     ylim=c(1.2*min(Phi.rot[,2]),1.2*max(Phi.rot[,2])),
     main=plottitle, 
     xlab="PC1",
     ylab="PC2",
     sub=subtitle)
text(Phi.rot,labels=tags, col="blue", pos=1)
arrows(axes_orig, axes_orig, Phi.rot[,1], Phi.rot[,2], length = 0.07,col="blue")
abline(h=0,v=0,col="gray")
grid()


# ########################
## Borough-based color-coded individual observation projections on PC1-2
## AFTER VARIMAX
# ########################

## How to apply the varimax rotation to scores, i.e. to the scores (i.e. projection of individuals) ?
#+ Recall here that individuals are row profiles. For a complete answer by @amoeba, see:
# https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r
# Method summary:
# In varimax() loadings, i.e. evecs scaled by the square roots of their respective evals, are rotated. 
# Doing so on evecs would be unconventional and quite unusual.
# In other words, by convention the evecs of the cov matrix are not directly rotated. 
# So after rotation by varimax(), loading vectors aren't orthogonal anymore (even though the 
# varimax-rotation is said to be "orthogonal"). The upshot is that the orthogonal projections of the
# data onto the rotated loading directions is not computed in a straightforward way.
# Is PCA followed by a rotation (such as varimax) still PCA ? 
# Look at SVD of the data matrix X <- U %*% S %*% t(V). To rotate loadings means inserting R %*% t(R) 
# for some rotation matrix R as follows:
#                  X <- U %*% (R %*% t(R)) %*%  S %*% t(V)
# To find varimax-rotated scores, one can use varimax-rotated loadings. Multiply the data with the
# transposed pseudo-inverse of the rotated loadings, like so:
# library(MASS)
# Note Zs is centered, not normalized
scores.rot <- Zs[,-col.sup_idx] %*% t(MASS::ginv(pcaYsimple.rot$loadings[1:p,]))

colnames(scores.rot) <- paste0("Varimax-PC",1:ncol(scores.rot))
rownames(scores.rot) <- rownames(Zs)

Boroughs <- as.factor(Z_ctd[,1])

par(mfrow = c(1,1))

plottitle=sprintf("Row profiles\' projection in PC1-2 factorial plane\n(after feature selection and varimax rotation)")
#plotdata <- data.frame(PC1=scores.rot[,1],PC2=scores.rot[,2],z=rep("",nrow(Zs)))
plotdata <- data.frame(PC1=-scores.rot[,1],PC2=scores.rot[,2],z=rownames(Zs))
plot1 <- ggplot(data = plotdata) + 
    theme_bw() +
    geom_vline(xintercept = 0, col="gray") +
    geom_hline(yintercept = 0, col="gray") +
    geom_text_repel(aes(PC1,PC2,label = z),
                    size=3,
                    point.padding = 0.5,
                    box.padding = unit(0.55, "lines"),
                    segment.size = 0.3,
                    segment.color = 'grey') +
    geom_point(aes(PC1,PC2,col=Boroughs),size = 2) +
    scale_color_discrete(name = 'Borough') +
    labs(title = plottitle)

plot1 +
    scale_color_manual(values=ccolors[1:length(levels(Boroughs))])

par(mfrow = c(1,1))



# ########################
## Topographic plot of color-coded ZIPs (PC1-2 projections AFTER varimax)
# ########################
library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")      # to read shapefile (translate GPS into ZIP)
library("fastshp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")
#library("colorspace")

#pal <- choose_palette()

# Use zipped achive at https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
mapZIP <- readOGR(dsn="./Data/Geolocation", layer="ZIP_CODE_040114")
shp <- read.shp("Data/Geolocation/ZIP_CODE_040114.shp", format="list")

ZIP_lst <- c()
ZIP_lst <- c(rownames(Z),zeroSRCzip)
#tail(ZIP_lst,27)
# retrieve mapZIP's ZIP indices to be mapped
mapZIP_idx <- which(mapZIP$ZIPCODE %in% ZIP_lst)

mapZIPborough <- c()
for (index in mapZIP_idx) {
    mapZIPborough <- c(mapZIPborough,as.character(X[as.character(mapZIP$ZIPCODE[index]) == rownames(X),1]))
}
mapZIPborough <- as.factor(as.numeric(as.factor(mapZIPborough)))
# Compute & plot overall area bounding box
xMinBox <- c() ; yMinBox <- c() ; xMaxBox <- c() ; yMaxBox <- c();
for (index in mapZIP_idx) {
    xMinBox <- c(xMinBox,shp[[index]]$box[1])
    yMinBox <- c(yMinBox,shp[[index]]$box[2])
    xMaxBox <- c(xMaxBox,shp[[index]]$box[3])
    yMaxBox <- c(yMaxBox,shp[[index]]$box[4])
}

xMin <- min(xMinBox); yMin <- min(yMinBox)
xMax <- max(xMaxBox); yMax <- max(yMaxBox)

# color-code ZIP codes according to quadrant position on PC1-2 row profile projection 
# coordinates are: x=-psiZ[,1]; y=-psiZ[,2]
# varimax-rot PC1+ cone  x > 0, x >=  |y|      QUALITY       ---- orchid2   
# varimax-rot PC2+ cone  y > 0, y >   |x|      CITIZEN WATCH ---- green
# varimax-rot PC1- cone  x < 0, x <= -|y|      NOISE         ---- tan
# varimax-rot PC2- cone  y < 0, y <  -|x|      CITY WATCH    ---- orangered3
zipQuadrant <- ifelse(scores.rot[,1] >= 0 & abs(scores.rot[,2]) <= scores.rot[,1],"orchid2",
                      ifelse( scores.rot[,2] > 0 & scores.rot[,2] > abs(scores.rot[,1]),"green",
                             ifelse( scores.rot[,1] <= 0 & scores.rot[,1] <= -abs(scores.rot[,2]) ,"tan","orangered3")))
zipWeight <- fi[-ind.sup_idx] / max(fi[-ind.sup_idx])

# draw initial ghost ZIP perimeter
plottitle <- "Mapped NYC ZIP codes (5 boroughs)"
plottitle <- paste0(plottitle," - ",month.abb[monthNbr],". ",yearNbr,"\n(after feature selection and varimax rotation)")
plot(x=c((xMin+xMax)/2),y=c((yMin+yMax)/2),
     xlim=c(xMin,xMax),ylim=c(yMin,yMax),
     axes=FALSE,
     type="p",pch=".",col="darkgreen",xlab="",ylab="", main=plottitle)
axis(1,at=c(),labels=F,col.axis="black",tck=0)
axis(2,labels=F,col.axis="black",tck=0)

for ( ii in 1:length(mapZIP_idx) ) {
    index <- mapZIP_idx[ii]
    #lines(shp[[index]]$x, shp[[index]]$y,type="l",col="skyblue",xlab="x",ylab="y") # draw ZIP perimeter
    lines(shp[[index]]$x, shp[[index]]$y,type="l",col=ccolors[mapZIPborough[ii]],xlab="x",ylab="y") # draw ZIP perimeter
    
    # lines(c(shp[[index]]$box[1],shp[[index]]$box[1],shp[[index]]$box[3],shp[[index]]$box[3],shp[[index]]$box[1]),
    #       c(shp[[index]]$box[2],shp[[index]]$box[4],shp[[index]]$box[4],shp[[index]]$box[2],shp[[index]]$box[2]),
    #       type="l", lwd=1,col="red") # draw ZIP box
    
    ##put zip code string at the center of each zip area bounding box
    localZIP <- mapZIP$ZIPCODE[index]
    # text(x=(xMinBox[ii]+xMaxBox[ii])/2,
    #      y=(yMinBox[ii]+yMaxBox[ii])/2,
    #      adj=0.5,labels=localZIP,
    #      col="steelblue",
    #      cex=0.5)
    
    if (as.character(localZIP) %in% rownames(Z_ctd)) {
        #print(ii)
        points(x=(xMinBox[ii]+xMaxBox[ii])/2,y=(yMinBox[ii]+yMaxBox[ii])/2,
               type = "p",
               col = as.character(zipQuadrant[which(labels(zipQuadrant) == as.character(localZIP))]),
               bg = as.character(zipQuadrant[which(labels(zipQuadrant) == as.character(localZIP))]),
               pch = 21,
               cex = 3 * as.numeric(zipWeight[which(labels(zipWeight) == as.character(localZIP))]),
               alpha=I(1/5)
        )
    }
}
legend("topleft",legend=levels(Boroughs),lwd=4,col=ccolors[1:length(levels(Boroughs))])
legend("bottomright",legend=c("PC1+","PC2+","PC1-","PC2-"),pch=16,
       col= c("orchid2","green","tan","orangered3"),
       horiz=T,
       title="PC1-2 conic sectors:", 
       bty="n", 
       pt.cex=1.3, 
       cex=0.8)

# ########################
