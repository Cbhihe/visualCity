# #############################
## MIRI:        Analysis of NYC-311 Service Request Calls
## Author:      Cedric Bhihe
## Date:        Summer 2018
## Script:      11_analysis_clustering-tree.R
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
# Y <- X[! rownames(X) %in% c("99999"),2:14] # 208 is a supplementary factor among row profiles
Y <- X[,2:14]  # keep bogus zip for later as 'sup.ind' 
cntTot <- sum(Y)
fij <- Y/cntTot
fi <- rowSums(fij)
fj <- colSums(fij)

# Identify & suppress rows (i.e. ZIPs) with either very low reporterd SRC count
#   o no reported SRC at all.
zeroSRCzip <- labels(fi[fi <= 5/cntTot]) # identify
Y <- Y[!rownames(Y) %in% zeroSRCzip,]  # suppress
Yboroughs <- X[!rownames(X) %in% zeroSRCzip,1]

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

par(mfrow = c(2,2))
ind.sup_idx = which(rownames(Y_ctd) %in% c("99999"))
pcaY <- PCA(Y_ctd,
            ncp=13,
            scale.unit=F,
            ind.sup = c(ind.sup_idx), 
            quanti.sup = NULL, 
            quali.sup = NULL, 
            row.w = fi[-ind.sup_idx], col.w = NULL, 
            graph = T, axes = c(1,2)
            )

ind.sup_idx = which(rownames(Y_ctd) %in% c("99999","10463"))
pcaY <- PCA(Y_ctd,
            ncp=13,
            scale.unit=F,
            ind.sup = c(ind.sup_idx), 
            quanti.sup = NULL, 
            quali.sup = NULL, 
            row.w = fi[-ind.sup_idx], col.w = NULL, 
            graph = T, axes = c(1,2)
)
#text(x=-0.2,y=-0.2,labels=c("blablabla"),col=ccolors[4])
par(mfrow = c(1,1))


## Determine nbr of significant dimensiones, nd, automatically
# 1/ Point successively to every component of the list of evals sorted by decreasing value 
# 2/ When total explained inertia exceeds 80% chose eval index as nbr of significant dims
n_dim <- ncol(Y_ctd)-1
for (nd in 1:n_dim) {  if(pcaY$eig[nd,3] >= 70) break  }
cat("Number of significant dimensions:",nd)


## Correlation with PCs and contribution to the construction of each dimension
pcaY$var$cor[,1:nd] # cor = normalized projection of variable on PCs in R^p
                    #     = coord/norm(projected var)
                    # cos2 = cor^2  ; apply(cos2,1,sum)=1 ; quality of representation of the variable
                    # contrib = explanatory power of each variable in terms of variance/inertia
                    #           for each dimension

pcaY$var$contrib[,1:nd]
apply(pcaY$var$contrib[,1:nd],2,sum)  # =100%
pcaY$eig[,2]


# ########################
## Compute inertia explanatory power of each variable
# ########################

varVarExpPow <- matrix(NA,ncol=2,nrow=nrow(pcaY$var$contrib)) 
rownames(varVarExpPow) <- rownames(pcaY$var$contrib)
colnames(varVarExpPow) <- c("all_dim","signif_dim")

for (vv in 1:nrow(pcaY$var$contrib)) {
    varVEPall <- 0
    varVEPsignif <- 0
    for ( ev in 1:(nrow(pcaY$var$contrib)-1) ) {
        varVEPall = varVEPall + pcaY$var$contrib[vv,ev] * pcaY$eig[ev,2]/100
        if (ev <= nd) {
            varVEPsignif = varVEPsignif + pcaY$var$contrib[vv,ev] * pcaY$eig[ev,2]/100
        }
    }
    varVarExpPow[vv,1] <- round(varVEPall,1)
    varVarExpPow[vv,2] <- round(varVEPsignif,1)
}
apply(varVarExpPow,2,sum)  # check that it is normalized to 100% and pcaY$eig[nd,3]

varVarExpPow



# ########################
## Represent biplots + scree plot
# ########################
par(mfrow = c(2,2))

## with all columns 
caY <- CA(Y,
          ncp=ncol(Y)-1,
          row.sup=ind.sup_idx,
          graph=T,
          axes=c(1,2),
          excl=NULL)
caY <- CA(Y,
          ncp=ncol(Y)-1,
          row.sup=ind.sup_idx,
          graph=T,
          axes = c(1,3),
          excl=NULL)
caY <- CA(Y,
          ncp=ncol(Y)-1,
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

par(mfrow = c(1,1))

## Feature extraction & selection
# "WaterSyst", "EnvProt" and "Sani"  are weakly correlated with 3 first PCs and are
#   collinear => add corresponding frequencies
Ysimple <- cbind(Y[,c(1,3:8)],EPwater=Y[,2]+Y[,9]+Y[,13],Y[,10:12])
# "HousCond" as well as "SocServ" and "IAO" are weakly coorelated with 3 first PCs
#   and therefore badly represented. 
# "ConsumProt" however, although apparently strongly correlated with "NoiseConst" cannot be 
#   dispensed with as:
#   - no satisfactory justification was found to explain the apparent correlation between the 2 vars
#   - "ConsumProt" is reasonably well represented by dimension 2 (PC2) and captures 7.3% of inertia  
sup_cols <- c(1,10,11)


par(mfrow = c(1,3))
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
inertia_threshold=70
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

plottitle = sprintf("PC explanatory power\n(\"NYC 311 SCR vs zip codes\" data set)")
plot(seq(1:length(evalRows)),evalRows,
     pch=15, 
     cex=1,
     col="blue",
     type="b",
     main=plottitle,
     sub="(after feature selection)",
     xlab="Index (sorted)",
     ylab="Eigenvalues"
)
text(x=1:length(evalRows), y=evalRows, 
     labels=paste0(as.character(round(caYsimple$eig[,3],1)),"%"),
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

par(mfrow = c(1,1))

caYsimple$eig


plot.CA(caYsimple,
        axes = c(1,2),
        invisible="row",
)


# ########################
## ind + var plot with FactoMineR
# ########################

par(mfrow = c(1,3))
Ysimple_ctd =cbind(Y_ctd[,c(1,3:8)],EPwater=Y_ctd[,2]+Y_ctd[,9]+Y_ctd[,13],Y_ctd[,10:12])
ind.sup_idx = which(rownames(Ysimple_ctd) %in% c("99999","10463"))
col.sup_idx = which(colnames(Ysimple_ctd) %in% c("IAO","SocServ","HousCond"))
pcaYsimple <- PCA(Ysimple_ctd,
                  ncp=7,
                  scale.unit=F,
                  ind.sup = ind.sup_idx, 
                  quali.sup = col.sup_idx,
                  quanti.sup=NULL,
                  row.w = fi[-ind.sup_idx], col.w = NULL, 
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
                xlimit=c(-1.5,1.5),ylim=c(-1.5,1.5),
                title=plottitle
)

fviz_pca_biplot(pcaYsimple,
                # col.ind = "blue",
                col.ind ="cos2",
                col.ind.sup="orange",
                col.var = "red",
                label="var",
                xlimit=c(-1.5,1.5),ylim=c(-1.5,1.5),
                select.ind=list(contrib = 80),
                title="(Individual contribution >= 80%)"
)
par(mfrow = c(1,1))

# ########################
## Plot of individuals' projection - by hand
# ########################

Z <- X[!rownames(X) %in% zeroSRCzip,1:14]  # suppress
Z <- cbind(Borough=Z[,1],Z[,4:9],EPwater=Z[,3]+Z[,10]+Z[,14],ConsumProt=Z[,11])
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
rm(Z_tmp)

par(mfrow = c(1,1))

plottitle=sprintf("Row profiles\' projection in PC1-2 factorial plane")
plotdata <- data.frame(PC1=-psiZ[,1],PC2=-psiZ[,2],z=rep("",nrow(Z_tmp)))
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
## compute contributions of each borough's ZIPs to inertia overall and in 1st factorial plane
# ########################

for ( bb in unique(as.character(factor(borough_col))) ) {
    Nindiv <- nrow(Z_ctd[Z_ctd[,1] == bb,])
    indVarExpPow <- matrix(NA,ncol=2,nrow=nrow(pcaYsimple$ind$contrib[which(Z_ctd[,1] == bb),]))
    rownames(indVarExpPow) <- rownames(pcaYsimple$ind$contrib[which(Z_ctd[,1] == bb),])
    colnames(indVarExpPow) <- c("All_dim","PC1-2")
    cnt <- 0
    for ( indiv_idx in as.numeric(which(Z_ctd[,1] == bb)) ) {
        indVEPall <- 0
        indVEPsignif <- 0
        cnt <- cnt+ 1
        for ( ev in 1:(nrow(pcaYsimple$eig)-1) ) {
            indVEPall = indVEPall + pcaYsimple$ind$contrib[indiv_idx,ev] * pcaYsimple$eig[ev,2]/100
            if (ev <= ndSimple) {
                indVEPsignif = indVEPsignif + pcaYsimple$ind$contrib[indiv_idx,ev] * pcaYsimple$eig[ev,2]/100
            }
        }
        indVarExpPow[cnt,1] <- round(indVEPall,1)
        indVarExpPow[cnt,2] <- round(indVEPsignif,1)
    }
    cat("\nBorough: ",bb,"\n","Number of ZIP codes: ",Nindiv,"\n")
    cat("Borough's ZIPs' % contribution to inertia (overall and in PC1-2 factorial plane):\n")
    print(apply(indVarExpPow,2,sum))
    #print(indVarExpPow)
}

# ########################
## Apply varimax to row profiles' projections in PC1-2  (MVA slides 03, pp 20~24)
# ########################
require(stats)  # part of 'base' package

pcaYsimple.rot <- varimax(pcaYsimple$var$cor[,1:ndSimple])  
pcaYsimple.rot

Zs <- scale(apply(Z_ctd[,-1],2,as.numeric),center=T,scale=F)
rownames(Zs) <-rownames(Z_ctd) 
sd_var <- apply(Zs,2,sd)

p <- ncol(Zs) 
Phi.rot <- diag(sd_var) %*% pcaYsimple.rot$loadings[1:p,]
tags <- colnames(Z_ctd[,-1])

lmb.rot <- diag(t(Phi.rot) %*% Phi.rot)
sum(lmb.rot)
sum(pcaYsimple$eig[1:ndSimple,1])

# Psi_stan.rot = Zs %*% solve(cov(Zs)) %*% Phi.rot
# Psi.rot = Psi_stan.rot %*% diag(sqrt(lmb.rot))
# apply(Psi.rot,2,var) 

axes_orig <- rep(0,p)
plottitle <- "Varimax rotation of variable projection\n in PC1-2 factorial plane"
subtitle <- paste("(",month.name[monthNbr],yearNbr,")")
plot(Phi.rot,
     type="n",
     xlim=c(1.2*min(Phi.rot[,1]),1.2*max(Phi.rot[,1])),
     ylim=c(1.2*min(Phi.rot[,2]),1.2*max(Phi.rot[,2])),
     #main=plottitle, 
     sub=subtitle)
text(Phi.rot,labels=tags, col="blue", pos=1)
arrows(axes_orig, axes_orig, Phi.rot[,1], Phi.rot[,2], length = 0.07,col="blue")
abline(h=0,v=0,col="gray")
grid()


# ########################
## Topographic plot of color-coded ZIPs according to varimax'ed PC1-2 projections
# ########################
library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")      # to read shapefile (translate GPS into ZIP)
library("fastshp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")
library("colorspace")
#pal <- choose_palette()

# Use zipped achive at https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
mapZIP <- readOGR(dsn="./Data/Geolocation", layer="ZIP_CODE_040114")
shp <- read.shp("Data/Geolocation/ZIP_CODE_040114.shp", format="list")

ZIP_lst <- c(); ZIP_lst <- c(rownames(Zs),zeroSRCzip,"10463")
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
# 1st quadrant x>=0, y>=0  ---- lightblue
# 2nd quadrant x<=0, y>=0  ---- springgreen
# 3rd quadrant x<=0, y<=0  ---- tan
# 4th quadrant x>=0, y<=0  ---- red 
zipQuadrant <- ifelse(-psiZ[,1] >=0,ifelse(-psiZ[,2] >=0,"lightblue","red"),ifelse(-psiZ[,2] >=0,"springgreen","tan"))
zipWeight <- fi[-ind.sup_idx] / max(fi[-ind.sup_idx])

# draw initial ghost ZIP perimeter
plottitle <- "Mapped NYC ZIP codes (5 boroughs)"
plot(x=c((xMin+xMax)/2),y=c((yMin+yMax)/2),
     xlim=c(xMin,xMax),ylim=c(yMin,yMax),
     type="p",pch=".",col="darkgreen",xlab="plane x",ylab="plane y", main=plottitle)

for ( ii in 1:length(mapZIP_idx) ) {
    index <- mapZIP_idx[ii]
    #lines(shp[[index]]$x, shp[[index]]$y,type="l",col="skyblue",xlab="x",ylab="y") # draw ZIP perimeter
    lines(shp[[index]]$x, shp[[index]]$y,type="l",col=ccolors[mapZIPborough[ii]],xlab="x",ylab="y") # draw ZIP perimeter

        # lines(c(shp[[index]]$box[1],shp[[index]]$box[1],shp[[index]]$box[3],shp[[index]]$box[3],shp[[index]]$box[1]),
    #       c(shp[[index]]$box[2],shp[[index]]$box[4],shp[[index]]$box[4],shp[[index]]$box[2],shp[[index]]$box[2]),
    #       type="l", lwd=1,col="red") # draw ZIP box
    
    # put zip code string at the center of each zip area bounding box 
    localZIP <- mapZIP$ZIPCODE[index]
    # text(x=(xMinBox[ii]+xMaxBox[ii])/2,
    #      y=(yMinBox[ii]+yMaxBox[ii])/2,
    #      adj=0.5,labels=localZIP,
    #      col="steelblue",
    #      cex=0.5)
    
    if (as.character(localZIP) %in% rownames(Z_ctd)) {
        print(ii)
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
legend("topleft",legend=levels(Boroughs),lwd=2,col=ccolors[1:length(levels(Boroughs))])




###################################################################################################################
# Probabilistic clustering with k-means replications
# Hierarchical clustering
# Clustering consolidation, k-means
###################################################################################################################

Psi <- pcaYsimple$ind$coord # psi matrix

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
