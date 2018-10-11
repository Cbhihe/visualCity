# #############################
## Project:     Analysis of NYC-311 Service Request Calls
## Script:      13_k-means-clustering.R
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Last edit:   
# #############################

rm(list=ls(all=TRUE)) 

# #############################

setwd("~/Documents/Work/Academic-research/NYC311/")

set.seed(932178)
options(scipen=6) # R switches to sci notation above 5 digits on plot axes
ccolors=c("red","green","blue","orange","cyan","tan1","darkred","honeydew2","violetred",
          "palegreen3","peachpuff4","lavenderblush3","lightgray","lightsalmon","wheat2")
datestamp <- format(Sys.time(),"%Y%m%d-%H%M%S")


# #############################
## Source parameter file
# #############################

source(file="Scripts/01_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...


# #############################
## Libraries
# #############################

setRepositories(ind = c(1:6,8))
library(FactoMineR)     # to use canned PCA and CA methods. 
library(factoextra)
require(graphics)       # enhanced graphics
library(ggplot2)        # to enhance graph plotting


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

source_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                      "00_nyc_simple-whole-data-set_binned.csv")

W <- read.csv(source_file,
              header=T,
              sep=",",
              quote="\"",
              dec=".",
              skip=0)


# #############################
## generate MCA
# #############################
rownames(W) <- W[,1]
W <- W[,-1]
#cntTot <- sum(W[,2:14])  # total nbr of counts
W_bak <- W

bogusZIP_idx <- which(rownames(W) %in% c("99999"))  #  bogus ZIP codes' indices

outliersZIP_idx <- c()
if (yearNbr == 2010) {
    # in April 2010, ZIP _____ is a major outlier
    outliersZIP_idx <- which(rownames(W) %in% c())
} else if (yearNbr == 2014) {
    # in April 2014, ZIP "10463" (Riverdale) is a major outlier, 11430" (JFK) a lesser one
    outliersZIP_idx <- which(rownames(W) %in% c("10463","11430"))
} else if (yearNbr == 2018) {
    # in April 2014, ZIP _____ is a major outlier
    outliersZIP_idx <- which(rownames(W) %in% c())
}
### W <- W[-outliersZIP_idx,]    ### do NOT suppress outliers, use them as sup individuals
    
Boroughs <- factor(W$Borough[-bogusZIP_idx])
levels(Boroughs)  # rid factor's levels of "99999" -- ok


mcaNYC311 <- MCA(W[,-1],ncp=5,
                 quanti.sup=c(17,18),
                 ind.sup=c(outliersZIP_idx,bogusZIP_idx),
                 #quali.sup=c(1),
                 excl=NULL,
                 graph = T,
                 level.ventil = 0.00,
                 axes = c(1,2),
                 row.w = NULL,
                 method="Indicator",
                 na.method="NA",
                 tab.disj=NULL)

summary(mcaNYC311,nbelements=12)
dimdesc(mcaNYC311)




# #############################
##  Contingency table
# #############################

# source_file <- paste0("Data/",
#                       yearNbr,
#                       ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
#                       "00_nyc_simple-whole-data-set.csv")

# X <- read.csv(source_file,
#               header=T,
#               sep=",",
#               quote="\"",
#               dec=".",
#               skip=0)
# # Build table, where "99999" row is bogus zip encompassing all previously detected missings
# rownames(X) <- X[,1]; 
# X <- X[,-1] 
# # Y <- X[rowSums(X[,2:14])!=0,2:14]  # keep bogus zip for later as 'sup.ind'
# Y <- X[,c(2:19)]
# cntTot <- sum(Y[,c(1:13)])
# fij <- Y[,c(1:13)]/cntTot
# fi <- rowSums(fij)
# fj <- colSums(fij)
# 
# pbmCntCellLength <- length(which(as.numeric(unlist(Y[,c(1:13)]))<5))
# cat("nbr of cells with count <5:", pbmCntCellLength,"\n")
# zeroSRCzip <- labels(fi[fi <= 5/cntTot]) # identify ZIP codes with very low overall SRCs' counts.
# 
# bogusZIP <- c("10048","00083","99999")
# Y <- Y[!rownames(Y) %in% c(bogusZIP,zeroSRCzip),]    # suppress bogus and ghost ZIPs
# outliersZIP <- c()
# if (yearNbr == 2010) {
#     # in April 2010, ZIP _____ is a major outlier
#     outliersZIP <- c()
#     Y <- Y[!rownames(Y) %in% outliersZIP,]    # suppress outliers
# } else if (yearNbr == 2014) {
#     # in April 2014, ZIP "10463" (Riverdale) is a major outlier, 11430" (JFK) a lesser one
#     outliersZIP <- c("10463","11430")
#     Y <- Y[!rownames(Y) %in% outliersZIP,]        # supress outliers
# } else if (yearNbr == 2018) {
#     # in April 2014, ZIP _____ is a major outlier
#     outliersZIP <- c()
#     Y <- Y[!rownames(Y) %in% outliersZIP,]        # supress outliers
# }
# 
# Yboroughs <- X[!rownames(X) %in% c(bogusZIP,zeroSRCzip, outliersZIP),1]
# Yboroughs <- factor(Yboroughs)
# levels(Yboroughs)  # rid factor's levels of "99999" -- ok
# 
# 
# par(mfrow = c(1,1))
# Ysimple_ctd <- cbind(Y_ctd[,c(1,3:8)],EPwater=Y_ctd[,2]+Y_ctd[,13],Y_ctd[,10:12])
# ind.sup_idx <- which(rownames(Ysimple_ctd) %in% c("99999","10463"))
# col.sup_idx <- which(colnames(Ysimple_ctd) %in% c("IAO","SocServ","WaterSyst","HousCond"))
# 
# pcaYsimple <- PCA(Ysimple_ctd,
#                   ncp=7,
#                   scale.unit=F,
#                   ind.sup = ind.sup_idx, 
#                   quali.sup = col.sup_idx,
#                   quanti.sup=NULL,
#                   row.w = fi[-ind.sup_idx], col.w = NULL, 
#                   graph = T, axes = c(1,2))


###################################################################################################################
## Probabilistic clustering with k-means replications
#   Hierarchical clustering
#   Clustering consolidation, k-means
###################################################################################################################

Psi <- mcaNYC311$ind$coord # psi matrix
Nobs <- nrow(Psi)
#Nobs <- Nobs-length(c(outliersZIP_idx,bogusZIP_idx))    # nbr of obs - nbr of supplementary individuals

# #####################
# Probabilistic k-means approach
# #####################
par(mfrow = c(1,1))

Nr=20 # nbr of replications
Nk=10 # max probed nbr (index) of clusters

in_over_tot <- matrix(NA,Nr,Nk) # ratio of within-cluster sum of squares over total sum of squares
CH_index<- matrix(NA,Nr,Nk) # Calinsky-Harabasz index = corrected ratio of between-cluster over tot within-cluster sum of squares 

for (r in 1:Nr) {
    for (k in 2:Nk) {
        resC <- kmeans(Psi,k,iter.max = 10, nstart = 3)
        in_over_tot[r,k] <- resC$betweenss/resC$totss
        CH_index[r,k] <- (resC$betweenss/(k-1))/(resC$tot.withinss/(Nobs-k))
    }
}

# index has zero value for 1 cluster
in_over_tot[,1] <- 0  
CH_index[,1] <- 0

par(mfrow = c(1,2))
plot(colMeans(in_over_tot),
     ylab='Normalized within-cluster SS',
     type="b",col="blue")
plot(colMeans(CH_index),
     ylab='Calinsky-Harabasz index',
     type="b",
     col="dark red")

# OR ...
# ... plotting on the same graph, with 2 different vertical axes
{
par(mfrow = c(1,1), mar=c(5, 4, 4, 6) + 0.1)
# 1st plot
plot(colMeans(in_over_tot),
     pch=16, 
     axes=F,
     xlab='',ylab='',
     ylim=c(0,1),
     type="b",
     col="blue",
     main="Selection of optimal number of clusters by k-means\n (April 2014 NYC data)")
axis(2,ylim=c(0,1),col='black',col.axis='black',las=1)
mtext('Normalized within-cluster SS',col='blue',side=2,line=2.5)
# 2nd plot
par(new=T)
plot(colMeans(CH_index),
     pch=18, 
     axes=F,
     ylim=c(0,max(colMeans(CH_index))),
     xlab='',ylab='',
     type="b",
     col="dark red")
axis(4,ylim=c(0,max(colMeans(CH_index))),col='dark red',col.axis='dark red',las=1)
mtext('Calinsky-Harabasz index',col='dark red',side=4,line=2.5)
# abcissa
axis(1,pretty(range(1:Nk),10))
mtext("Index (cluster nbr)",side=1,col="black",line=2.5) 
# dashed line at optimal cluster index

# abline(v=which(colMeans(CH_index)==max(colMeans(CH_index))),
#        lty=2,
#        col='gray')
#segments(c(2,5),c(0,0),c(2,5),c(75,72),lty=2,col='orangered2')
arrows(c(2,5),c(0,0),c(2,5),c(75,72),code=1,angle=15,lty=2,col='orangered2')

# legend
legend("bottomright",
       legend=c("Norm within-SS","CH index"),
       text.col=c("blue","dark red"),
       pch=c(16,18),
       col=c("blue","dark red"))

par(mfrow = c(1,1))
}

# #####################
## Use silhouette() method to confirm cluster nbr
# #####################

library(cluster)
cnt <- 0
for (kk in list(2,5)) {
    cnt <- cnt+1
    resC <- kmeans(Psi,kk,iter.max = 10, nstart = 3)
    sil <- silhouette(resC$cluster,dist(Psi))
    plot(sil,col=2:(kk+1), main=paste0(letters[cnt],") Silhouette for ",kk," clusters"))
}


# #####################
## Hierarchical clustering approach
# #####################

# Input for clustering algorithm is psi, even as the PCs form an artificial var space.
distX <- dist(Psi, method = "euclidean")
treeX <- hclust(distX, method = "ward.D2")

#par(mfrow=c(1,2),new=F)

# Hierarchical Clustering plot
plot(treeX,
     main='Hierarchical Clustering (Ward.D2)',
     xlab='Distance',
     cex=0.6)
abline(h=2,col='red')

# Dendogram
barplot(treeX$height,
        xlab='Agglomeration index', 
        main='Clustering heights',
        ylab='Agglomeration criterion\'s value')
abline(h=2,col='red')
text(x=44, y=2-0.04, 
     labels="Dendrogram tree-cut",
     cex=0.75,
     pos=3,
     col="red")

Nclusters = 7
cutX <- cutree(treeX,k=Nclusters)
length(cutX)

# Centroids for x classes
nd = 5
centroids <- aggregate(Psi,list(cutX),mean)

# Quality index
Bss <- sum(rowSums(centroids^2)*as.numeric(table(cutX)))
Tss <- sum(rowSums(Psi^2))
Ib <- 100*Bss/Tss

# Visualize partition
par(mfrow=c(1,1),new=F)

plot(Psi[,1],Psi[,2],
     xlab='PC1',ylab='PC2',
     pch=16,type="n",
     col=cutX,
     main="Clustering of observations in 7 classes",
     sub=""
)
text(Psi[,1],Psi[,2],col=cutX,labels=rownames(Psi),cex = 0.8)
abline(h=0,v=0,col="gray")
# legend("topright",paste0("Class_",1:Nclusters),pch=20,col=c(1:Nclusters))

# OR
plotdata <- data.frame(PC1=Psi[,1],PC2= Psi[,2],z=rownames(Psi))
plottitle <- "Clustering of observations in PC1-2 factorial plane"
ggplot(data = plotdata,col=cutX) +
    theme_bw()+
    # geom_vline(xintercept = 0, col="gray") +
    # geom_hline(yintercept = 0, col="gray") +
    geom_text_repel(aes(PC1,PC2,label = z),
                    col=cutX,
                    size=4,
                    point.padding = 0.5,
                    box.padding = unit(0.55, "lines"),
                    segment.size = 0.3,
                    segment.color = 'grey') +
    geom_point(aes(PC1,PC2),col = "blue", size = 1) +
    labs(title = plottitle)+
    coord_fixed()


# ##########
# Consolidation using k-means
# ##########

resC_consol <- kmeans(Psi,centers=centroids[,2:6])

# Quality index
Bss <- sum(rowSums(resC_consol$centers^2)*resC_consol$size)  # resC_consol$betweenss
Wss <- sum(resC_consol$withinss)                             # resC_consol$tot.withinss
Ib_consol <- 100*Bss/(Bss+Wss)

# Plot
par(mfrow=c(1,1),new=F)
plot(Psi[,1],Psi[,2],
     xlab='PC1',ylab='PC2',
     pch=cutX+16,
     type="p",
     col=cutX+4,
     main="Consolidated clustering of observations in 2 classes",
     sub="(With \'Cuba\' as outlier in PCA)"
)
points(centroids[,2:3],pch=18,type='p',col='blue',cex = 1.5)
text(centroids[,2],centroids[,3],labels=paste0("G",unique(cutX)),pos=1,cex=1,2)
abline(h=0,v=0,col="gray")

# Use silhouette to confirm clustering result
sil <- silhouette(resC_consol$cluster,dist(Psi))
plot(sil,col=1:length(unique(cutX)), 
     main='Silhouette widths for consolidated clustering')


# ############################################
# 5: Using catdes(), interpret cluster and represent them.
# ############################################

# 
# cat_descript <- catdes(cbind(as.factor(resC_consol$cluster),Y_ctd),
#                        1,
#                        proba=0.05,
#                        row.w=NULL)
