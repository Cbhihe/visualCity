# #############################
## MIRI/MVA   :     Analysis of NYC-311 Service Request Calls
## Author:          Cedric Bhihe, Santi Calvo
## Delivery:        2018.06.26
## Script:          11_analysis_clustering-tree.R
# #############################


rm(list=ls(all=TRUE))

#setwd("~/Documents/Work/Academic-research/NYC311/")
setwd("C:/Users/calvo/Desktop/UPC/Courses/Third_semester/MVA/NYC-complaints-master")

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
                      "00_nyc_whole-data-set.csv")

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
# row 208 = 99999 bogus zip for missing
ZIPsupress <- which(rownames(X) %in% c("99999"))
Y <- X[-ZIPsupress,2:14]  # 208 is a supplementary factor among row profiles
cntTot <- sum(Y)
fij <- Y/cntTot
fi <- rowSums(fij)
fj <- colSums(fij)
# Identify & suppress rows (i.e. ZIPs) with no reported SRC
zeroSRCzip <- labels(fi[fi <= 5/cntTot]) # identify
Y <- Y[!rownames(Y) %in% zeroSRCzip,]  # suppress
fij <- Y/cntTot
fi <- rowSums(fij)
fj <- colSums(fij)
rowCentroid <- sqrt(fj) 
colCentroid <- sqrt(fi)
Di <- diag(fi)
Dj <- diag(fj)
CFj_given_i <- solve(Di) %*% as.matrix(fij)
Fim <- CFj_given_i %*% solve(sqrt(Dj))
unitaryM <- matrix(1,nrow=nrow(Fim), ncol=ncol(Fim), byrow=T)
Y_ctd <- Fim - unitaryM %*% sqrt(Dj)  # centering

rownames(Y_ctd) <- rownames(Y)
colnames(Y_ctd) <- colnames(Y)
pcaY <- PCA(Y_ctd,
            scale.unit=F,
            ind.sup = NULL, 
            quanti.sup = NULL, 
            quali.sup = NULL, 
            row.w = fi, col.w = NULL, 
            graph = F, axes = c(1,2)
            )

Psi <- pcaY$ind$coord # psi matrix


###################################################################################################################
# Probabilistic clustering with k-means replications
# Hierarchical clustering, 
# Clustering consolidation, k-means
###################################################################################################################

# ##########
# Probabilistic k-means approach
# ##########
par(mfrow = c(1,1))


Nobs <- nrow(Psi)
#Nobs <- Nobs-length(supInd)    # nbr of obs - nbr of supplementary individuals

Nr=10 # nbr of replications
Nk=10 # max probed nbr (index) of clusters

in_over_tot <- matrix(NA,Nr,Nk) # ratio of within-cluster sum of squares over total sum of squares
CH_index<- matrix(NA,Nr,Nk)     # corrected ratio of between-cluster over tot within-cluster sum of squares 
# = Calinsky - Harabasz index

for (r in 1:Nr) {
    for (k in 2:Nk) {
        resC <- kmeans(Psi,k,iter.max = 10, nstart = 1)
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
par(mfrow = c(1,1), mar=c(5, 4, 4, 6) + 0.1)
# 1st plot
plot(colMeans(in_over_tot),
     pch=16, 
     axes=F,
     xlab='',ylab='',
     ylim=c(0,1),
     type="b",
     col="blue",
     main="Selection of optimal number of clusters (by k-means)")
axis(2,ylim=c(0,1),col='black',col.axis='black',las=1)
mtext('Normalized within-cluster SS',side=2,line=2.5)
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
abline(v=which(colMeans(CH_index)==max(colMeans(CH_index))),
       lty=2,
       col='gray')

# legend
legend("bottomright",
       legend=c("Norm within-SS","CH index"),
       text.col=c("blue","dark red"),
       pch=c(16,18),
       col=c("blue","dark red"))

# Use silhouette() method to confirm cluster nbr
#par(mfrow = c(1,3),new=F)
for (kk in 2:4) {
    resC <- kmeans(Psi,kk,iter.max = 10, nstart = 1)
    sil <- silhouette(resC$cluster,dist(Psi))
    plot(sil,col=1:kk, main='')
}


# ##########
# Hierarchical clustering approach
# ##########

# Input for the clustering algorithm is the psi matrix even as the PCs form an artificial var space.
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
