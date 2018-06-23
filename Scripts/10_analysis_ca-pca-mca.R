# #############################
## MIRI Project:    Geosociological Analysis of NYC-311 Service Requests
## Author:          Cedric Bhihe, Santi Calvo
## Delivery:        2018.06.26
## Script:          10_analysis_ca-mca.R
# #############################


rm(list=ls(all=TRUE))

setwd("~/Documents/Work/Academic-research/NYC311/")

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

#  apportionment rules for ZIP code "00083"
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
Y <- X[-208,2:14]  # 208 is a supplementary factor among row profiles

# total nbr of counts
N <- sum(Y)
# frequency matrix
fij <- Y/N

# row weights in terms of service calls for each ZIP
( fi <- rowSums(fij) )  
# col weights of service call modality accross all ZIPs
( fj <- colSums(fij) )  # see that catalanes have the lowest perceived similarity across all spanish regions
sum(fi) ; sum(fj) # check = 1

## Explore table

# Identify & suppress rows (i.e. ZIPs) que no tienen service calls
labels(fi[round(fi,5)==0]) # identify
Y <- Y[!rownames(Y) %in% labels(fi[round(fi,5)==0]),]  # suppress
fij <- Y/N
fi <- rowSums(fij)
fj <- colSums(fij)
rowCentroid <- sqrt(fj) 
colCentroid <- sqrt(fi)

# #############################
## Test of independence
# #############################

# CAUTION !!
# Check that cell with low count values (<5) do not contribute significantly to computed chi-sq.
# compute nbr of cells with count <5 
pbmCntCellLength <- length(which(as.numeric(unlist(Y))<5))
cat("nbr of cells with count <5:",pbmCntCellLength,"\n")

# 1/ Pearson chi-square test for significant association (dependence) between row & col categs
# H0: In the population, the two categorical variables are independent.
# joint cell counts distr. in 2D contingency table = (row marginals) x (col marginals)
# DFs = 207x12 = 2484 is not taken into account for test statistics computed by Monte-Carlo simulations.
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

# # 2/ Balloon plot
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
# 
# # 3/ Mosaic plot
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
# sqrt(fj)  # coordinates center of gravity of rows
# apply(Fim, 2, weighted.mean, w=fi) # should be same as above

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
evalCols <- eigZcols$values         # eigenvalues


# #############################
## Same with FactoMineR
# #############################

# plot **factor** maps in factorial planes PC1-2, PC2-3, PC1-3 where row and col factors 
# are printed together with distinct colors for easier differentiation. 
# Factors, psi_alpha, are projections of either centered row "i" profiles, X_ctd, or of 
# centered col "j" profiles on PC direction u_alpha (evec)
# psi <- t(X_ctd) %*% evecs   # for row profiles

par(mfrow = c(1,3))
# PC1-2 factorial plane
caY <- CA(Y,ncp=ncol(Y)-1,
            graph=T,
            axes = c(1,2),
            excl=NULL)
# Do not specify option 'row.w=fi' 
# By default, vector of 1's and each row has weight equals to its margin;
# Weights given only for active rows

# PC2-3 fatorial plane
CA(Y,ncp=ncol(Y)-1,
   graph=T,
   axes = c(2,3),
   excl=NULL)

# PC1-3 fatorial plane
CA(Y,ncp=ncol(Y)-1,
   graph=T,
   axes = c(1,3),
   excl=NULL)

# PC1-4 factorial plane
CA(Y,ncp=ncol(Y)-1,
          graph=T,
          axes = c(1,4),
          excl=NULL)
# Do not specify option 'row.w=fi' 
# By default, vector of 1's and each row has weight equals to its margin;
# Weights given only for active rows

# PC2-4 fatorial plane
CA(Y,ncp=ncol(Y)-1,
   graph=T,
   axes = c(2,4),
   excl=NULL)

# PC3-4 fatorial plane
CA(Y,ncp=ncol(Y)-1,
   graph=T,
   axes = c(3,4),
   excl=NULL)

par(mfrow = c(1,1))

caY$eig                   
evalRows2 <- caY$eig[,1]  # last eval = 0, 0; along with last column in evecs, correspond to
# the centroid 'sqrt(fj)'

cumvarexp <- c(100*evalRows[1]/sum(evalRows))
for (ii in 2:(ncol(Y))) {
    if(ii==2) cat("Eigenvalues","\t","Cumulative variance (%)\n")
    cumvarexp <- c(cumvarexp,cumvarexp[ii-1] + 100*evalRows[ii]/sum(evalRows))
    cat(format(evalRows[ii-1],scientific=T),"\t",round(cumvarexp[ii-1],6),"\n")
}


par(mfrow = c(1,2))

# plot row profiles cloud and colum profile cloud separately
plot(caY,invisible="col", label="row")
# project row centroid on PC12 
# logically it coincides with origin of axes of factorial plane.
points(rowCentroid %*% evecRows[,1:2],pch=18,type='p',col='green',cex = 2)   

plot(caY,invisible="row", label="col")
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


















# save to disk
target_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                      "00_nyc_whole-data-set.csv")
csvSaveF(protoY,target_file)     # csv to disk

