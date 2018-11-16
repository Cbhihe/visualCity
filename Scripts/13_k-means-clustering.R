# #############################
## Project:     Analysis of NYC-311 Service Request Calls
## Script:      13_k-means-clustering.R
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Last edit:   
# #############################

rm(list=ls(all=TRUE)) 

setwd("~/Documents/Work/Academic-research/NYC311/")

set.seed(932178)
options(scipen=6) # R switches to sci notation above 5 digits on plot axes


# #############################
## Source parameter file
# #############################

source(file="Scripts/01_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...


# #############################
## Libraries
# #############################

setRepositories(ind = c(1:6,8))
library(FactoMineR)     # to use canned MCA, CA and PCA methods. 
library(factoextra)     # methods to extract and visualize MVA results
require(graphics)       # enhanced graphics
library(ggplot2)        # enhanced graphs
library(ggrepel)        # enhanced graphs


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
}    # exit function, not standard, depends on system's internals


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

bogusZIP <- c("99999","10048")
bogusZIP_idx <- which(rownames(W) %in% bogusZIP)  #  bogus ZIP codes' indices

if (yearNbr == 2010) {
    # in April 2010, ZIP 10281 is a major outlier
    outliersZIP <- c("10281","11430")
} else if (yearNbr == 2014) {
    # in April 2014, ZIP "10463" (Riverdale) is a major outlier, 11430" (JFK) a lesser one
    outliersZIP <- c("10463","11430")
} else if (yearNbr == 2018) {
    # in April 2014, ZIP _____ is a major outlier
    outliersZIP <- c("11371","11430")
}
outliersZIP_idx <- which(rownames(W) %in% outliersZIP)
### W <- W[-outliersZIP_idx,]    ### do NOT suppress outliers, use them as sup individuals
    
Boroughs <- factor(W$Borough[-c(outliersZIP_idx,bogusZIP_idx)])
levels(Boroughs)  # rid factor's levels of "99999" -- ok

mcaNYC311 <- MCA(W[,-1],
                 ncp=5,
                 quanti.sup = c(17,18),
                 ind.sup = c(outliersZIP_idx,bogusZIP_idx),
                 excl = NULL,
                 graph = TRUE,
                 level.ventil = 0,
                 axes = c(1,2),
                 row.w = NULL,
                 method = "Indicator",
                 na.method = "NA",
                 tab.disj = NULL
                 )

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
     main=paste0("Selection of optimal number of clusters by k-means\n(",
                 month.name[monthNbr]," ",yearNbr," NYC data)"))
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

if (yearNbr == 2010) {
    arrows(c(6,7),c(0,0),c(6,7),c(70,72),code=1,angle=15,lty=2,col='orangered2') 
} else if (yearNbr == 2014) {
    # abline(v=which(colMeans(CH_index)==max(colMeans(CH_index))),
    #        lty=2,
    #        col='gray')
    #segments(c(2,5),c(0,0),c(2,5),c(75,72),lty=2,col='orangered2')
    arrows(c(2,5),c(0,0),c(2,5),c(75,72),code=1,angle=15,lty=2,col='orangered2') 
} else if (yearNbr == 2018) {
    ## arrows(c(2,__),c(0,0),c(2,__),c(75,72),code=1,angle=15,lty=2,col='orangered2')
} else {
    cat("\n\n-----------------------\nYear number not recognized.\nAbort.\n-----------------------\n ")
    exit()
}


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

if (yearNbr == 2010) {
    clust_lst <- c(2,5,6,7)
} else if (yearNbr == 2014) {
    clust_lst <- c(2,5)
} else if (yearNbr == 2018) {
    clust_lst <- c(2,3,5)
} else {
    cat("\n\n-----------------------\nYear number not recognized.\nAbort.\n-----------------------\n ")
    exit()
}

library(cluster)
cnt <- 0
lenCL <- length(clust_lst)
# Compute grouping of silouhette's graphs
if (lenCL <= 3) {
    par(mfrow = c(1,lenCL))
} else {
    par(mfrow = c(floor(lenCL/2)+lenCL%%2,2))
}

for (kk in clust_lst) {
    cnt <- cnt+1
    resC <- kmeans(Psi,kk,iter.max = 10, nstart = 3)
    sil <- silhouette(resC$cluster,dist(Psi))
    plot(sil,col=2:(kk+1), main=paste0(letters[cnt],") Silhouette for ",kk," clusters"))
}

par(mfrow = c(1,1))

# #####################
## Hierarchical clustering (HC) approach
# #####################

# Input for clustering algorithm is psi, even as the PCs form an artificial var space.
distX <- dist(Psi, method = "euclidean")
treeX <- hclust(distX, method = "ward.D2")
if (yearNbr == 2010) {
    Nclusters = clust_lst[3]
} else if (yearNbr == 2014) {
    Nclusters = clust_lst[2]
} else if (yearNbr == 2018) {
    Nclusters = clust_lst[3]
} else {
    cat("\n\n-----------------------\nYear number not recognized.\nAbort.\n-----------------------\n ")
    exit()
}

#par(mfrow=c(1,2))

# Plot HC dendrogram
plot(treeX,
     main=paste0("Hierarchical clustering\n(",
                 month.name[monthNbr]," ",yearNbr," NYC data)"),
     xlab='Distance',
     cex=0.7)
#abline(h=4,col='red')

if (yearNbr == 2010) {
    heightCut <- 4.7
} else if (yearNbr == 2014) {
    heightCut <- 5
} else if (yearNbr == 2018) {
    heightCut <- 4.9
} else {
    cat("\n\n-----------------------\nYear number not recognized.\nAbort.\n-----------------------\n ")
    exit()
}
segments(0,heightCut,170,heightCut,col='red')
text(x=170, y=heightCut-0.04, 
     labels=paste0("(",Nclusters," classes)"),
     cex=0.75,
     pos=3,
     col="red")

# Plot of cluster heights vs. agglomeration
barplot(tail(treeX$height,50),
        xlab='Agglomeration index', 
        main='Clustering heights',
        ylab='Agglomeration criterion\'s value')
segments(0,heightCut,60,heightCut,col='red',lty=2)
text(x=30, y=heightCut-0.04,
     labels=paste0("Dendrogram tree-cut (",Nclusters," classes)"),
     cex=0.75,
     pos=3,
     col="red")

cutX <- cutree(treeX,k=Nclusters)

# Centroids for x classes
nd = 5  # analysis carried out for 5 first dimensions
centroids <- aggregate(Psi,list(cutX),mean)[,2:(nd+1)]

# Quality index
Bss <- sum(rowSums(centroids^2)*as.numeric(table(cutX)))
Tss <- sum(rowSums(Psi^2))
Ib <- 100*Bss/Tss
cat("\n\nClustering quality Index for",Nclusters,"classes, Ib =",round(Ib,2),"\n\n")

# Visualize shape-coded partitions and color-coded boroughs

# ####  PC1-2
par(mfrow=c(1,1),new=F)
title <- paste0("Clustering of MCA scores in ",
                Nclusters," classes\n(",month.name[monthNbr]," ",yearNbr," NYC data)")
plot(Psi[,1],Psi[,2],
     xlab='PC1',ylab='PC2',
     pch=cutX-1,type="p",
     col=ccolors[as.numeric(Boroughs)],
     main=title,
     sub=""
)
text(Psi[,1],Psi[,2],col=cutX,
     labels = NULL,
     # labels=rownames(Psi),
     cex = 0.6)
abline(h=0,v=0,col="gray")
legend("bottomright",paste0("Clust_",1:Nclusters),pch=unique(cutX)-1,col="black") 
legend("topright",legend=levels(Boroughs),cex=1,pch=16,col=ccolors[1:length((Boroughs))])

# OR

# ####  PC1-2
plotdata <- data.frame(PC1=Psi[,1],PC2= Psi[,2],z=rownames(Psi))
plottitle <- paste0("Clustering of MCA PC1-2 scores in ",
                    Nclusters," classes\n(",month.name[monthNbr]," ",yearNbr," NYC data)")
plot <- ggplot(data = plotdata) + #,col=cutX) +
        theme_bw()+
        geom_vline(xintercept = 0, col="gray") +
        geom_hline(yintercept = 0, col="gray") +
        # geom_text_repel(aes(PC1,PC2,label = z),
        #                 col=ccolors[as.numeric(Boroughs)],
        #                 size=rep(2,length(Boroughs)),
        #                 point.padding = 0.5,
        #                 box.padding = unit(0.55, "lines"),
        #                 segment.size = 0.3,
        #                 segment.color = 'grey') +
        geom_point(aes(PC1,PC2),col = ccolors[as.numeric(Boroughs)],pch=cutX-1, size = 2) +
        #scale_color_discrete(name = 'Boroughs') +
        labs(title = plottitle) +
        coord_fixed(ratio=1)
plot #+  scale_color_manual(values=ccolors[as.numeric(Boroughs)])

# OR

# ####  PC1-2
plotdata <- data.frame(PC1=Psi[,1],PC2= Psi[,2],z=rownames(Psi))
plottitle <- paste0("Clustering of MCA PC1-2 scores in ",
                    Nclusters," classes\n(",month.name[monthNbr]," ",yearNbr," NYC data)")
plot <- ggplot(plotdata,aes(PC1,PC2,
                            col = factor(as.character(Boroughs)),
                            shape = factor(cutX))) +
    theme(legend.position = "right",
          legend.key = element_rect(fill = "white", color = "white"),
          #panel.grid.major = element_line(colour = "gray"),
          panel.background=element_rect(fill = "white",color = "black")) +
    geom_vline(xintercept = 0, col="gray") +
    geom_hline(yintercept = 0, col="gray") +
    # geom_text_repel(aes(PC1,PC2,label = z),
    #                 col=ccolors[as.numeric(Boroughs)],
    #                 size=rep(3,length(Boroughs)),
    #                 point.padding = 0.5,
    #                 box.padding = unit(0.55, "lines"),
    #                 segment.size = 0.3,
    #                 segment.color = 'grey') +
    geom_point(data=NULL,size=3) +
    #,col = as.character(Boroughs),size = rep(2,length(ccolors[as.numeric(Boroughs)]))
    #scale_color_discrete(name = 'Boroughs') +
    labs(title = plottitle,col="Boroughs",shape="Classes") +
    scale_color_manual(values=ccolors[1:length(levels(Boroughs))]) #+
    #coord_fixed(ratio=1)
plot # +  scale_shape_manual (values=c(16,17,15,3,7,8,5))

# ####  PC1-3
plotdata <- data.frame(PC1=Psi[,1],PC3= Psi[,3],z=rownames(Psi))
plottitle <- paste0("Clustering of MCA PC1-3 scores in ",
                    Nclusters," classes\n(",month.name[monthNbr]," ",yearNbr," NYC data)")
plot <- ggplot(plotdata,aes(PC1,PC3,
                            col = factor(as.character(Boroughs)),
                            shape=factor(cutX))) +
    theme(legend.position = "right",
          legend.key = element_rect(fill = "white", color = "white"),
          #panel.grid.major = element_line(colour = "gray"),
          panel.background=element_rect(fill = "white",color = "black")) +
    geom_vline(xintercept = 0, col="gray") +
    geom_hline(yintercept = 0, col="gray") +
    # geom_text_repel(aes(PC1,PC3,label = z),
    #                 col=ccolors[as.numeric(Boroughs)],
    #                 size=rep(3,length(Boroughs)),
    #                 point.padding = 0.5,
    #                 box.padding = unit(0.55, "lines"),
    #                 segment.size = 0.3,
    #                 segment.color = 'grey') +
    geom_point(data=NULL,size=3) +
    #,col = as.character(Boroughs),size = rep(2,length(ccolors[as.numeric(Boroughs)]))
    #scale_color_discrete(name = 'Boroughs') +
    labs(title = plottitle,col="Boroughs",shape="Classes") +
    scale_color_manual(values=ccolors[1:length(levels(Boroughs))]) #+
    #coord_fixed(ratio=1)
plot # +  scale_shape_manual (values=c(16,17,15,3,7,8,5))

# ####  PC2-3
plotdata <- data.frame(PC2=Psi[,2],PC3= Psi[,3],z=rownames(Psi))
plottitle <- paste0("Clustering of MCA PC2-3 scores in ",
                    Nclusters," classes\n(",month.name[monthNbr]," ",yearNbr," NYC data)")
plot <- ggplot(plotdata,aes(PC2,PC3,
                            col = factor(as.character(Boroughs)),
                            shape=factor(cutX))) +
    theme(legend.position = "right",
          legend.key = element_rect(fill = "white", color = "white"),
          #panel.grid.major = element_line(colour = "gray"),
          panel.background=element_rect(fill = "white",color = "black")) +
    geom_vline(xintercept = 0, col="gray") +
    geom_hline(yintercept = 0, col="gray") +
    # geom_text_repel(aes(PC2,PC3,label = z),
    #                 col=ccolors[as.numeric(Boroughs)],
    #                 size=rep(3,length(Boroughs)),
    #                 point.padding = 0.5,
    #                 box.padding = unit(0.55, "lines"),
    #                 segment.size = 0.3,
    #                 segment.color = 'grey') +
    geom_point(data=NULL,size=3) +
    #,col = as.character(Boroughs),size = rep(2,length(ccolors[as.numeric(Boroughs)]))
    #scale_color_discrete(name = 'Boroughs') +
    labs(title = plottitle,col="Boroughs",shape="Classes") +
    scale_color_manual(values=ccolors[1:length(levels(Boroughs))]) #+
#coord_fixed(ratio=1)
plot # +  scale_shape_manual (values=c(16,17,15,3,7,8,5))


# #####  3D plot
library("plot3D")

# static    (phi=azymytal direction, theta=colatitude)
charType <- c()
charCol <- c()
for ( cc in 1:length(cutX) ) {
    if (cutX[cc] == 1) {
        charType <- c(charType,16)
    } else if (cutX[cc] == 2 ) {
        charType <- c(charType,17)
    } else if (cutX[cc] == 3 ) {
        charType <- c(charType,15)
    } else if (cutX[cc] == 4 ){
        charType <- c(charType,3)
    } else {
        charType <- c(charType,7)
    }
    
    if (Boroughs[cc] == "Bronx") {
        charCol <- c(charCol,"darkred") 
    } else if (Boroughs[cc] == "Brooklyn") {
        charCol <- c(charCol,"darkgreen")
    } else if (Boroughs[cc] == "Manhattan") {
        charCol <- c(charCol,"blue")
    } else if (Boroughs[cc] == "Queens" ) {
        charCol <- c(charCol,"orangered")
    } else {
        charCol <- c(charCol,"cyan")
    }
}

###    FUNCTION "my3Dplot()" 
# No argument is optional but all have sensible defaults
my3Dplot <- function(myData=Psi,
                     myCenters=centroids,
                     locMonthNbr=monthNbr,
                     locYearNbr=yearNbr,
                     locBoroughs=Boroughs,
                     Nclass=Nclusters,
                     indClass=cutX,
                     indChar=charType, 
                     indCol=charCol,
                     azymuth=10,latitude=-30)
    {
    maintitle <- paste0("Clustering of MCA PC1-2-3 scores in ",
                        Nclass," classes\n(",month.name[locMonthNbr]," ",locYearNbr," NYC data)")
    subtitle <-  paste0("(phi=",azymuth,"º, theta=",latitude,"º)")
    
    plot1 <- scatter3D(x=myData[,1],y=myData[,2],z=myData[,3],
                       phi = azymuth, theta = latitude,
                       main = maintitle,
                       sub = subtitle,
                       xlab= "PC1",ylab="PC2",zlab="PC3",
                       #col.var = locBoroughs,
                       #col.var = as.integer(locBoroughs),
                       #col = colXp,
                       col = rep("black",length(locBoroughs)), 
                       #col = ccolors[as.integer(locBoroughs)],
                       pch = indChar,
                       cex = 1.2,
                       ticktype = "detailed",
                       cex.axis = 0.6,
                       bty = "b2",
                       add = FALSE,
                       colkey = FALSE
    )
    Xshift <- (max(myData[,1])-min(myData[,1]))/50
    Yshift <- (max(myData[,2])-min(myData[,2]))/50
    Zshift <- (max(myData[,3])-min(myData[,3]))/50
    
    plot1 <- plot1 +
        text3D(x=myData[,1]+Xshift,y=myData[,2]+Yshift,z=myData[,3]+Zshift,
               labels = as.character(indClass),
               col = indCol,
               colkey = FALSE, 
               cex = 1,
               add = TRUE,
               plot=TRUE )
    plot1 <- plot1 +
        points3D(x=myCenters[,1],y=myCenters[,2],z=myCenters[,3],
                 #phi = azymuth, theta = latitude,
                 col = rep("orange",5),
                 cex = rep(3,5),
                 pch = c(16,17,15,3,7,8), #unique(indChar),
                 colkey = FALSE,
                 labels = NULL,
                 add=TRUE,
                 plot=TRUE)
    
    return(plot1)
}

myPlot <- my3Dplot(azymuth=10,latitude=-60)


# ##### Interactive 3D plot
#install.packages("plot3Drgl")  # faulty install
# library("plot3Drgl")   # abort R session
# plotrgl() 



# ########################
## Compute contributions of each borough to inertia over 'nd' dimensions
# ########################

X <- W[-c(outliersZIP_idx,bogusZIP_idx),-c(18,19)]

boroughIEP <- c()

for ( bb in unlist(levels(Boroughs))) {
    boroughObs_idx <- which(X[,1] == bb)
    Nindiv <- length(boroughObs_idx)
    indIEP <- matrix(NA,ncol=1,nrow=Nindiv)
    rownames(indIEP) <- rownames(mcaNYC311$ind$contrib[boroughObs_idx,])
    colnames(indIEP) <- paste0("IEP_over_",nd,"_dim")
    cnt <- 0
    for ( indiv_idx in boroughObs_idx ) {
        indIEPall <- 0
        indIEPsignif <- 0
        cnt <- cnt+ 1
        for ( eval_idx in 1:(min(nrow(mcaNYC311$eig)-1,nd))) {
            indIEPall = indIEPall + mcaNYC311$ind$contrib[indiv_idx,eval_idx] * mcaNYC311$eig[eval_idx,2]/100
        }
        indIEP[cnt,1] <- round(indIEPall,1)
    }
    cat("\nBorough:",bb,"\n","Number of ZIP codes:",Nindiv,"\n")
    cat("Boroughs' % (non-normalized) contribution to inertia: ",apply(indIEP,2,sum),"\n")
    boroughIEP <- c(boroughIEP,apply(indIEP,2,sum))
    #print(indIEP); flush.console() 
}
cnt <- 0
boroughIEP <- boroughIEP/sum(boroughIEP)*100
for ( bb in unlist(levels(Boroughs))) {
    cnt <- cnt+1
    cat("Boroughs' % (normalized) contribution to inertia: ",round(boroughIEP[cnt],1),"%\n")
}
# ########################
## Compute contributions of each cluster class to inertia over 'nd' dimensions
# ########################

clusterIEP <- c()

for ( cc in unique(as.integer(factor(cutX))) ) {
    clusterObs_idx <- which(cutX == cc)
    Nindiv <- length(clusterObs_idx)
    indIEP <- matrix(NA,ncol=1,nrow=Nindiv)
    rownames(indIEP) <- rownames(mcaNYC311$ind$contrib[clusterObs_idx,])
    colnames(indIEP) <- paste0("IEP_over_",nd,"_dim")
    cnt <- 0
    for ( indiv_idx in clusterObs_idx ) {
        indIEPall <- 0
        indIEPsignif <- 0
        cnt <- cnt+ 1
        for ( eval_idx in 1:(min(nrow(mcaNYC311$eig)-1,nd))) {
            indIEPall = indIEPall + mcaNYC311$ind$contrib[indiv_idx,eval_idx] * mcaNYC311$eig[eval_idx,2]/100
        }
        indIEP[cnt,1] <- round(indIEPall,1)
    }
    cat("\nCluster: ",cc,"\n","Number of ZIP codes: ",Nindiv,"\n")
    cat("Clusters' (non-normalized) % contribution to inertia:",apply(indIEP,2,sum),"\n")
    clusterIEP <- c(clusterIEP,apply(indIEP,2,sum))
    print(indIEP); flush.console() 
}

clusterIEP <- clusterIEP/sum(clusterIEP)*100
for ( cc in 1:Nclusters) {
    cat("Cluster",cc,"'s normalized contribution to inertia: ",round(clusterIEP[cc],1),"%\n")
}


# ########################
## Topographic plot of color-coded ZIPs, according to cluster classification
# ########################
library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")      # to read shapefile (translate GPS into ZIP)
library("fastshp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")

# Use zipped achive at https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
mapZIP <- readOGR(dsn="./Data/Geolocation", layer="ZIP_CODE_040114")
shp <- read.shp("Data/Geolocation/ZIP_CODE_040114.shp", format="list")

source_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                      "00_nyc_whole-data-set.csv")

Wp <- read.csv(source_file,
              header=T,
              sep=",",
              quote="\"",
              dec=".",
              skip=0)

ghostZIP_idx <- which(Wp[,1] %in% c("00083","10048"))
bogusZIP_idx <- which(Wp[,1] %in% c("99999"))

ZIP_lst <- Wp[-c(bogusZIP_idx,ghostZIP_idx),1]

rownames(Wp) <- Wp[,1]
Wp <- Wp[-c(bogusZIP_idx,ghostZIP_idx),-1]

# retrieve mapZIP's ZIP indices to be mapped
mapZIP_idx <- which(mapZIP$ZIPCODE %in% ZIP_lst)

mapZIPborough <- c()
for (index in mapZIP_idx) {
    mapZIPborough <- c(mapZIPborough,as.character(Wp[as.character(mapZIP$ZIPCODE[index]) == rownames(Wp),1]))
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

# color-code ZIP codes according to cluster class

# for ( cc in unique(as.integer(factor(cutX))) ) {
#     assign(paste0("zipClass", cc),rownames(X[which(cutX == cc),]))
# }

# #########
#  Define cluster colors (i.e. dot colors in topological mapping representation)
# #########
zipClassCol <- matrix("",ncol=1,nrow=nrow(Wp))
#CSO <- 4   # color scale offset in ccolors[]
#clusterCol <- ccolors[CSO:(CSO+Nclusters-1)]
clusterCol <- c("orchid2","darkturquoise","tan","orangered3","green","steelblue")
## Check that there are enough cluster colors defined.
if (length(clusterCol) < length(levels(as.factor(cutX)))) {
    print("Number of cluster colors is smaller than the number of classes to represent.",quote=FALSE)
    print("Add more colors to 'clusterCol' vector.", quote=FALSE)
    print("Script aborted.",quote=FALSE)
    exit()
}

for (ii in 1:length(zipClassCol)) {
    if ( ! rownames(Wp[ii,]) %in% rownames(X) ) {
        zipClassCol[ii] <- "lightgray"
    } else {
        for (cc in unique(as.integer(cutX))) {
            if (rownames(Wp[ii,]) %in% rownames(X[which(cutX == cc),])) {
                zipClassCol[ii] <- clusterCol[cc]
            }
        }
    }
}

rownames(zipClassCol) <- rownames(Wp)
cntTot <- sum(Wp[,c(2:17)])
fij <- Wp[,c(2:17)]/cntTot
fi <- rowSums(fij)
#fj <- colSums(fij)
zipWeight <- fi / max(fi)

# draw initial ZIP codes perimeters
plottitle <- paste0("Mapped NYC ZIP codes (",Nclusters," class HC)\n")
plottitle <- paste0(plottitle,"(",month.name[monthNbr]," ",yearNbr," SRCs with crime data)")

plot(x=c((xMin+xMax)/2),y=c((yMin+yMax)/2),
     xlim=c(xMin,xMax),ylim=c(yMin,yMax),
     axes=FALSE,
     type="p",pch=".",
     col="darkgreen",
     xlab="",ylab="",
     main=plottitle)

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
    
    if (as.character(localZIP) %in% rownames(Wp)) {
        #print(ii)
        points(x=(xMinBox[ii]+xMaxBox[ii])/2,y=(yMinBox[ii]+yMaxBox[ii])/2,
               type = "p",
               col = zipClassCol[which(rownames(zipClassCol) == as.character(localZIP))],
               bg  = zipClassCol[which(rownames(zipClassCol) == as.character(localZIP))],
               pch = 21,
               cex = 3 * as.numeric(zipWeight[which(labels(zipWeight) == as.character(localZIP))]),
               alpha=I(1/5)
        )
    }
}
legend("topleft",legend=levels(Boroughs),lwd=2,col=ccolors[1:length(levels(Boroughs))], cex=0.8)
legend("bottomright",
       legend=levels(factor(cutX)),
       pch=16,
       col=clusterCol, 
       horiz=T,
       title="Cluster classes", 
       bty="n", 
       pt.cex=1.3, 
       cex=0.8)


# ####################
## Consolidation using k-means
# ####################

consolClust <- kmeans(Psi,centers=centroids)
consolClass <- consolClust$cluster

# -----------------------
# Define plots' character typesand colors
consolCharType <- c()
consolCharCol <- c()
for ( cc in 1:length(consolClass) ) {
    if (consolClass[cc] == 1) {
        consolCharType <- c(consolCharType,16)
    } else if (consolClass[cc] == 2 ) {
        consolCharType <- c(consolCharType,17)
    } else if (consolClass[cc] == 3 ) {
        consolCharType <- c(consolCharType,15)
    } else if (consolClass[cc] == 4 ){
        consolCharType <- c(consolCharType,3)
    } else {
        consolCharType <- c(consolCharType,7)
    }
    
    # this block not useful because charCol = consolCharCol
    if (Boroughs[cc] == "Bronx") {
        consolCharCol <- c(consolCharCol,"darkred") 
    } else if (Boroughs[cc] == "Brooklyn") {
        consolCharCol <- c(consolCharCol,"darkgreen")
    } else if (Boroughs[cc] == "Manhattan") {
        consolCharCol <- c(consolCharCol,"blue")
    } else if (Boroughs[cc] == "Queens" ) {
        consolCharCol <- c(consolCharCol,"orangered")
    } else {
        consolCharCol <- c(consolCharCol,"cyan")
    }
}
    
# -----------------------
# Compute quality index
Bss <- sum(rowSums(consolClust$centers^2)*consolClust$size)  # resC_consol$betweenss
Wss <- sum(consolClust$withinss)                             # resC_consol$tot.withinss
Ib_consol <- 100*Bss/(Bss+Wss)   # Comparison of Ib before and after consolidation can
                                 # only show an improvement

cat("\n\nClustering quality index for",Nclusters,"classes, after k-means consolidation, Ib =",round(Ib_consol,2),"\n\n")

# -----------------------
# Plots

plottitle <- paste0("Clustering of MCA PC1-2 scores in ",Nclusters," classes\n")
plottitle <- paste0(plottitle,"after k-means consolidation.\n(NYC 311 + NYPD 911: ",month.name[monthNbr]," ",yearNbr," data)")
plotdata <- data.frame(PC1=Psi[,1],PC2= Psi[,2],PC3= Psi[,3],
                       z=rownames(Psi))
centroids.df <- data.frame(PC1=centroids[,1],PC2=centroids[,2],PC3=centroids[,3],
                           labels=paste0("G",unique(consolClass)))

# Style 1
par(mfrow=c(1,1),new=F)
plot(plotdata[,1],plotdata[,2],
     xlab='PC1',ylab='PC2',
     pch=consolCharType,
     type="p",
     col=charCol,
     main=plottitle
)
points(centroids[,1:2],type='p',pch=c(16,17,15,3,7,8),col='orange',cex = 2.5)
text(centroids[,1],centroids[,2],labels=paste0("G",unique(consolClass)),pos=1,cex=1,2)
abline(h=0,v=0,col="gray")

# OR

# Style 2
par(mfrow=c(1,1),new=F)
plot <- ggplot(plotdata,aes(PC1,PC2,
                            col = factor(as.character(Boroughs)),
                            shape=factor(consolClass))) +
    theme(legend.position = "right",
          legend.key = element_rect(fill = "white", color = "white"),
          #panel.grid.major = element_line(colour = "gray"),
          panel.background=element_rect(fill = "white",color = "black")) +
    geom_vline(xintercept = 0, col="gray") +
    geom_hline(yintercept = 0, col="gray") +
    # geom_text_repel(aes(PC1,PC2,label = z),
    #                 col=ccolors[as.numeric(Boroughs)],
    #                 size=rep(3,length(Boroughs)),
    #                 point.padding = 0.5,
    #                 box.padding = unit(0.55, "lines"),
    #                 segment.size = 0.3,
    #                 segment.color = 'grey') +
    geom_point(data=NULL,size=3) +
    #,col = as.character(Boroughs),size = rep(2,length(ccolors[as.numeric(Boroughs)]))
    #scale_color_discrete(name = 'Boroughs') +
    labs(title = plottitle,col="Boroughs",shape="Classes") # + coord_fixed(ratio=1)
plot +  scale_color_manual(values=ccolors[1:length(levels(Boroughs))])

# plot + geom_point() +
#     annotate("point", 
#              centroids.df[,1],centroids.df[,2],
#              shape=unique(factor(consolClass)),
#              size=4,
#              col="orange"
#              )


# -----------------------
# Use silhouette to confirm clustering result
sil <- silhouette(consolClust$cluster,dist(Psi))
plot(sil,col=1:length(unique(consolClass)), 
     main='Silhouette widths for consolidated clustering')

# ########################
## Compute contributions of each consolidated cluster's class to inertia 
## over 'Nd' dimensions

clusterIEP <- c()

for ( cc in unique(as.integer(consolClass)) ) {
    clusterObs_idx <- which(consolClass == cc)
    Nindiv <- length(clusterObs_idx)
    indIEP <- matrix(NA,ncol=1,nrow=Nindiv)
    rownames(indIEP) <- rownames(mcaNYC311$ind$contrib[clusterObs_idx,])
    colnames(indIEP) <- paste0("IEP_over_",nd,"_dim")
    cnt <- 0
    for ( indiv_idx in clusterObs_idx ) {
        indIEPall <- 0
        indIEPsignif <- 0
        cnt <- cnt+ 1
        for ( eval_idx in 1:(min(nrow(mcaNYC311$eig)-1,nd))) {
            indIEPall = indIEPall + mcaNYC311$ind$contrib[indiv_idx,eval_idx] * mcaNYC311$eig[eval_idx,2]/100
        }
        indIEP[cnt,1] <- round(indIEPall,1)
    }
    cat("\nCluster class: ",cc,"\n","Number of ZIP codes: ",Nindiv,"\n")
    cat("Clusters' (non-normalized) % contribution to inertia:",apply(indIEP,2,sum),"\n")
    clusterIEP <- c(clusterIEP,apply(indIEP,2,sum))
    print(indIEP); flush.console() 
}

clusterIEP <- clusterIEP/sum(clusterIEP)*100
for ( cc in 1:Nclusters) {
    cat("Cluster",cc,"'s normalized contribution to inertia: ",round(clusterIEP[cc],1),"%\n")
}
# ########################
## Topographic plot of color-coded ZIPs, 
## according to cluster classification
## AFTER CONSOLIDATION
# ########################
library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")  # to read shapefile (translate GPS into ZIP)
library("fastshp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")

# Use zipped achive at https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
mapZIP <- readOGR(dsn="./Data/Geolocation", layer="ZIP_CODE_040114")
shp <- read.shp("Data/Geolocation/ZIP_CODE_040114.shp", format="list")

source_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                      "00_nyc_whole-data-set.csv")

Wp <- read.csv(source_file,
               header=T,
               sep=",",
               quote="\"",
               dec=".",
               skip=0)

ghostZIP_idx <- which(Wp[,1] %in% c("00083","10048"))
bogusZIP_idx <- which(Wp[,1] %in% c("99999"))

ZIP_lst <- Wp[-c(bogusZIP_idx,ghostZIP_idx),1]

rownames(Wp) <- Wp[,1]
Wp <- Wp[-c(bogusZIP_idx,ghostZIP_idx),-1]

# retrieve mapZIP's ZIP indices to be mapped
mapZIP_idx <- which(mapZIP$ZIPCODE %in% ZIP_lst)

mapZIPborough <- c()
for (index in mapZIP_idx) {
    mapZIPborough <- c(mapZIPborough,as.character(Wp[as.character(mapZIP$ZIPCODE[index]) == rownames(Wp),1]))
}
mapZIPborough <- factor(as.numeric(as.factor(mapZIPborough)))
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

# color-code ZIP codes according to cluster class

# for ( cc in unique(as.integer(factor(cutX))) ) {
#     assign(paste0("zipClass", cc),rownames(X[which(cutX == cc),]))
# }

# #########
#  Define cluster colors (i.e. dot colors in topological mapping representation)
# #########
zipClassCol <- matrix("",ncol=1,nrow=nrow(Wp))
#CSO <- 4   # color scale offset in ccolors[]
#clusterCol <- ccolors[CSO:(CSO+Nclusters-1)]
clusterCol <- c("orchid2","darkturquoise","tan","orangered3","green","steelblue")
## Check that there are enough cluster colors defined.
if (length(clusterCol) < length(levels(as.factor(cutX)))) {
    print("Number of cluster colors is smaller than the number of classes to represent.",quote=FALSE)
    print("Add more colors to 'clusterCol' vector.", quote=FALSE)
    print("Script aborted.",quote=FALSE)
    exit()
}

# Define colors of each observation dot (according to class)
for (ii in 1:length(zipClassCol)) {
    if ( ! rownames(Wp[ii,]) %in% rownames(X) ) {
        # ZIP code not classified or not present in observed sample
        zipClassCol[ii] <- "lightgray"
    } else {
        for (cc in unique(as.integer(consolClass))) {
            if (rownames(Wp[ii,]) %in% rownames(X[which(consolClass == cc),])) {
                zipClassCol[ii] <- clusterCol[cc]
            }
        }
    }
}

rownames(zipClassCol) <- rownames(Wp)
cntTot <- sum(Wp[,c(2:17)])
fij <- Wp[,c(2:17)]/cntTot
fi <- rowSums(fij)
#fj <- colSums(fij)
zipWeight <- fi / max(fi)

# draw initial ZIP codes perimeters
plottitle <- paste0("Mapped NYC ZIP codes (",Nclusters," class HC)\n")
plottitle <- paste0(plottitle,"after k-means consolidation.\n(",month.name[monthNbr]," ",yearNbr," SRCs with crime data)")

plot(x=c((xMin+xMax)/2),y=c((yMin+yMax)/2),
     xlim=c(xMin,xMax),ylim=c(yMin,yMax),
     axes=FALSE,
     type="p",pch=".",
     col="darkgreen",
     xlab="",ylab="",
     main=plottitle)

axis(1,at=c(),labels=F,col.axis="black",tck=0)
axis(2,labels=F,col.axis="black",tck=0)

ii <- 0 ; ii <- ii+1

for ( ii in 1:length(mapZIP_idx) ) {
    index <- mapZIP_idx[ii]
    #lines(shp[[index]]$x, shp[[index]]$y,type="l",col="skyblue",xlab="x",ylab="y") # draw ZIP perimeter
    lines(shp[[index]]$x, shp[[index]]$y,
          type="l",
          col=ccolors[mapZIPborough[ii]],
          xlab="x",ylab="y"
          )        # draw ZIP perimeter
    
    ##put zip code string at the center of each zip area bounding box
    localZIP <- as.character(mapZIP$ZIPCODE[index])
    # text(x=(xMinBox[ii]+xMaxBox[ii])/2,
    #      y=(yMinBox[ii]+yMaxBox[ii])/2,
    #      adj=0.5,labels=localZIP,
    #      col="steelblue",
    #      cex=0.5)
    
    if (localZIP %in% as.character(rownames(Wp))) {
        #print(ii)
        points(x=(xMinBox[ii]+xMaxBox[ii])/2,y=(yMinBox[ii]+yMaxBox[ii])/2,
               type = "p",
               col = zipClassCol[which(rownames(zipClassCol) == as.character(localZIP))],
               bg  = zipClassCol[which(rownames(zipClassCol) == as.character(localZIP))],
               pch = 21,
               cex = 3 * as.numeric(zipWeight[which(labels(zipWeight) == as.character(localZIP))]) #,
               #alpha=I(1/5)
               )
    }
}
legend("topleft",legend=levels(Boroughs),lwd=2,col=ccolors[1:length(levels(Boroughs))], cex=0.8)
legend("bottomright",
       legend=levels(factor(cutX)),
       pch=16,
       col=clusterCol[1:Nclusters], 
       horiz=T,
       title="Cluster classes", 
       bty="n", 
       pt.cex=1.3, 
       cex=0.8)



# ############################################
# 5: Using catdes(), interpret cluster and represent them.
# ############################################

class_descript <- catdes(cbind(as.factor(consolClass),W[which(!rownames(W) %in% c(outliersZIP,bogusZIP)),2:17]),
                         1,
                         proba=0.01,
                         row.w=NULL)
#class_descript

#  categorical variables most and least significantly related to the construction cluster classes
percent <- 0.3
Nvar <- length(labels(class_descript$test.chi2[,1]))
cnt_limit <- round(percent*Nvar,0)

cat("Var. most significantly related to construction of classes:\n", 
    labels(class_descript$test.chi2[,1])[1:cnt_limit])

cat("Var. least significantly related to construction of classes:\n",
    labels(class_descript$test.chi2[,1])[(Nvar-cnt_limit+1):Nvar])

# ---------------
# canned plots

par(mfrow=c(1,1),new=F)
plot.catdes(class_descript,
            level=0.1)

par(mfrow=c(2,3))
plot.catdes(class_descript,
            level=0.1,
            show="quali",
            numchar=20,
            barplot=T)

# --------------
# Plot de v.test results for each class

par(mfrow=c(1,2),mai=c(1.8,1,0.75,0.75))
for (cc in 1:length(class_descript$category)) {
    c.col <- vector("character",length(class_descript$category[[cc]][,5]))
    c.col <- ifelse(class_descript$category[[cc]][,5]>0,clusterCol[cc],"gray")
    
    plottitle <- paste0("Consolidated Class ",cc,"\n(",month.name[monthNbr]," ",yearNbr," data)")
    barplot(class_descript$category[[cc]][,5],
            main=plottitle,
            cex.main=1.5,
            #xlab="Significant Modalities",
            ylab="Significance (v.test)",
            col=c.col,
            las=2)
    cat("Class ",cc,"'s most significant modalities:\n",
        rownames(class_descript$category[[cc]][which(class_descript$category[[cc]][,5]>2),]),"\n\n"
        )
}

