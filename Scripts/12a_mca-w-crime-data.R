# #############################
## Project:     Analysis of NYC-311 Service Request Calls
## Script:      12a_mca-w-crime-data.R
## Author:      Cedric Bhihe
## Created:     June 2018
## Last edit:   November 2018
# #############################

rm(list=ls(all=TRUE))

# #############################
## Source parameter file
# #############################

setwd("~/Documents/Work/Academic-research/NYC311/")

set.seed(932178)
options(scipen=6) # R switches to sci notation above 5 digits on plot axes

source(file="Scripts/01_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...setwd("~/Documents/Work/Academic-research/NYC-complaints/")


# #############################
## Libraries
# #############################

setRepositories(ind = c(1:6,8))
library(FactoMineR)     # to use canned PCA and CA methods. 
require(factoextra)     # to use enhanced graph functions
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

exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
}    # script exit function. 
                            # Caution: not standard, depends on OS's internals

# #############################
## Load data
# #############################

source_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                      "00_nyc_simple-whole-data-set.csv")
X <- read.csv(source_file,
              header=T,
              sep=",",
              quote="\"",
              dec=".",
              skip=0)


# ########################
## MCA
# ########################

rownames(X) <- X[,1];
X <- X[!rownames(X) %in% c("10048"),-1]   # former World Trade twin towers' ZIP code eliminatated from row profiles

cntTot <- sum(X[,2:14])  # total nbr of counts
fij <- X[,2:14]/cntTot  # frequency matrix
fi <- rowSums(fij) # row weights in terms of service calls for each ZIP
zeroSRCzip <- labels(fi[fi <= 5/cntTot]) # Identify & suppress rows (i.e. ZIPs) with no reported SRC

W <- X[!rownames(X) %in% c(zeroSRCzip),1:19]
W_bak <- W


if (yearNbr == 2010) {
    zip99999_Widx <- which(rownames(W)=="99999")  #  bogus ZIP code number
    zip10281_Widx <- which(rownames(W)=="10281")  #  Battery Park City in Manhattan
    zip11430_Widx <- which(rownames(W)=="11430")  #  JFK, Queens 
} else if (yearNbr == 2014) {
    zip99999_Widx <- which(rownames(W)=="99999")  #  bogus ZIP code number
    zip10463_Widx <- which(rownames(W)=="10463")  #  Riverdale in the Bronx
    zip11430_Widx <- which(rownames(W)=="11430")  #  JFK, Queens 
} else if (yearNbr == 2018) {
    zip99999_Widx <- which(rownames(W)=="99999")  #  bogus ZIP code number
    zip11430_Widx <- which(rownames(W)=="11430")  #  JFK, Queens 
    zip11371_Widx <- which(rownames(W)=="11371")  #  La Guardia, Queens
} else {
    cat("\n\n---------------\nUNKNOWN YEAR\n Abort\n--------------------\n\n")
    exit()
}

# ###########################
## discretize modality counts with bins, containing roughly same number of ZIP codes 
# ###########################
{
    ## HousCond
    if (!dynAnalysisF) {
        # Isolated time period analysis
        sumWHouscond <- as.numeric(summary(W$HousCond))    ## Use when analyzing one period alone
    } else {
        # Temporal evolution analysis over several time periods
        # Use when conducting period analysis across time 
        ## Use c(0,70,200,220,350,1789) for periods 'April 2010', 'April 2014' and 'April 2018'
        sumWHousCond <- c(0,70,200,220,350,1789)
    }
    bins_HousCond <- c(round(sumWHousCond[2]),round(sumWHousCond[3]),round(sumWHousCond[5]))
            
    mods_HousCond <- c(paste0("HousCond_[",as.character(sumWHousCond[1]),",",as.character(sumWHousCond[2]),"]"),
                       paste0("HousCond_[",as.character(sumWHousCond[2]),",",as.character(sumWHousCond[3]),"]"),
                       paste0("HousCond_[",as.character(sumWHousCond[3]),",",as.character(sumWHousCond[5]),"]"),
                       paste0("HousCond_[",as.character(sumWHousCond[5]),",",as.character(sumWHousCond[6]),"]"))
    
    cat("HousCond:\n   below or equal to",bins_HousCond[1],"SRCs - bin count:",length(which(W$HousCond<=bins_HousCond[1])),"\n")
    cat("   between",bins_HousCond[1]+1,"and",bins_HousCond[2],"SRCs - bin count:", length(which(W$HousCond>bins_HousCond[1] & W$HousCond<=bins_HousCond[2])),"\n")
    cat("   between",bins_HousCond[2]+1,"and",bins_HousCond[3],"SRCs- bin count:", length(which(W$HousCond>bins_HousCond[2] & W$HousCond<=bins_HousCond[3])),"\n")
    cat("   above",bins_HousCond[3],"SRCs - bin count:", length(which(W$HousCond>bins_HousCond[3])),"\n")
    
    W$HousCond[which(W_bak$HousCond<=bins_HousCond[1])] <- mods_HousCond[1]
    W$HousCond[which(W_bak$HousCond>bins_HousCond[1] & W_bak$HousCond<=bins_HousCond[2])] <- mods_HousCond[2]
    W$HousCond[which(W_bak$HousCond>bins_HousCond[2] & W_bak$HousCond<=bins_HousCond[3])] <- mods_HousCond[3]
    W$HousCond[which(W_bak$HousCond>bins_HousCond[3])] <- mods_HousCond[4]
    
    W$HousCond <- as.factor(W$HousCond)

    # Sani
    summary(W$Sani)
    bins_Sani <- c(18,31,54)
    mods_Sani <- c("L","M","H","VH")
    cat("Sani:\n   below or equal to",bins_Sani[1],"SRCs - bin count:",length(which(W$Sani<=bins_Sani[1])),"\n")
    cat("   between",bins_Sani[1]+1,"and",bins_Sani[2],"SRCs - bin count:", length(which(W$Sani>bins_Sani[1] & W$Sani<=bins_Sani[2])),"\n")
    cat("   between",bins_Sani[2]+1,"and",bins_Sani[3],"SRCs- bin count:", length(which(W$Sani>bins_Sani[2] & W$Sani<=bins_Sani[3])),"\n")
    cat("   above",bins_Sani[3],"SRCs - bin count:", length(which(W$Sani>bins_Sani[3])),"\n")
    
    W$Sani[which(W_bak$Sani<=bins_Sani[1])] <- mods_Sani[1]
    W$Sani[which(W_bak$Sani>bins_Sani[1] & W_bak$Sani<=bins_Sani[2])] <- mods_Sani[2] 
    W$Sani[which(W_bak$Sani>bins_Sani[2] & W_bak$Sani<=bins_Sani[3])] <- mods_Sani[3]
    W$Sani[which(W_bak$Sani>bins_Sani[3])] <- mods_Sani[4]
    
    W$Sani <- as.factor(W$Sani)
    
    # NoiseResid
    summary(W$NoiseResid)
    bins_NoiseResid <- c(29,61,123)
    mods_NoiseResid <- c("L","M","H","VH")
    cat("NoiseResid:\n   below or equal to",bins_NoiseResid[1],"SRCs - bin count:",length(which(W$NoiseResid<=bins_NoiseResid[1])),"\n")
    cat("   between",bins_NoiseResid[1]+1,"and",bins_NoiseResid[2],"SRCs - bin count:", length(which(W$NoiseResid>bins_NoiseResid[1] & W$NoiseResid<=bins_NoiseResid[2])),"\n")
    cat("   between",bins_NoiseResid[2]+1,"and",bins_NoiseResid[3],"SRCs- bin count:", length(which(W$NoiseResid>bins_NoiseResid[2] & W$NoiseResid<=bins_NoiseResid[3])),"\n")
    cat("   above",bins_NoiseResid[3],"SRCs - bin count:", length(which(W$NoiseResid>bins_NoiseResid[3])),"\n")
    
    W$NoiseResid[which(W_bak$NoiseResid<=bins_NoiseResid[1])] <- mods_NoiseResid[1]
    W$NoiseResid[which(W_bak$NoiseResid>bins_NoiseResid[1] & W_bak$NoiseResid<=bins_NoiseResid[2])] <- mods_NoiseResid[2] 
    W$NoiseResid[which(W_bak$NoiseResid>bins_NoiseResid[2] & W_bak$NoiseResid<=bins_NoiseResid[3])] <- mods_NoiseResid[3]
    W$NoiseResid[which(W_bak$NoiseResid>bins_NoiseResid[3])] <- mods_NoiseResid[4]
    
    W$NoiseResid <- as.factor(W$NoiseResid)
    
    # NoiseConst
    summary(W$NoiseConst)
    bins_NoiseConst <- c(1,5,20)
    mods_NoiseConst <- c("VL","L","M","H")
    cat("NoiseConst:\n   below or equal to",bins_NoiseConst[1],"SRCs - bin count:",length(which(W$NoiseConst<=bins_NoiseConst[1])),"\n")
    cat("   between",bins_NoiseConst[1]+1,"and",bins_NoiseConst[2],"SRCs - bin count:", length(which(W$NoiseConst>bins_NoiseConst[1] & W$NoiseConst<=bins_NoiseConst[2])),"\n")
    cat("   between",bins_NoiseConst[2]+1,"and",bins_NoiseConst[3],"SRCs- bin count:", length(which(W$NoiseConst>bins_NoiseConst[2] & W$NoiseConst<=bins_NoiseConst[3])),"\n")
    cat("   above",bins_NoiseConst[3],"SRCs - bin count:", length(which(W$NoiseConst>bins_NoiseConst[3])),"\n")
    
    W$NoiseConst[which(W_bak$NoiseConst<=bins_NoiseConst[1])] <- mods_NoiseConst[1]
    W$NoiseConst[which(W_bak$NoiseConst>bins_NoiseConst[1] & W_bak$NoiseConst<=bins_NoiseConst[2])] <- mods_NoiseConst[2] 
    W$NoiseConst[which(W_bak$NoiseConst>bins_NoiseConst[2] & W_bak$NoiseConst<=bins_NoiseConst[3])] <- mods_NoiseConst[3]
    W$NoiseConst[which(W_bak$NoiseConst>bins_NoiseConst[3])] <- mods_NoiseConst[4]
    
    W$NoiseConst <- as.factor(W$NoiseConst)
    
    # NoiseBiz
    summary(W$NoiseBiz)
    bins_NoiseBiz <- c(3,9,27)
    mods_NoiseBiz <- c("VL","L","M","H")
    cat("NoiseBiz:\n   below or equal to",bins_NoiseBiz[1],"SRCs - bin count:",length(which(W$NoiseBiz<=bins_NoiseBiz[1])),"\n")
    cat("   between",bins_NoiseBiz[1]+1,"and",bins_NoiseBiz[2],"SRCs - bin count:", length(which(W$NoiseBiz>bins_NoiseBiz[1] & W$NoiseBiz<=bins_NoiseBiz[2])),"\n")
    cat("   between",bins_NoiseBiz[2]+1,"and",bins_NoiseBiz[3],"SRCs- bin count:", length(which(W$NoiseBiz>bins_NoiseBiz[2] & W$NoiseBiz<=bins_NoiseBiz[3])),"\n")
    cat("   above",bins_NoiseBiz[3],"SRCs - bin count:", length(which(W$NoiseBiz>bins_NoiseBiz[3])),"\n")
    
    W$NoiseBiz[which(W_bak$NoiseBiz<=bins_NoiseBiz[1])] <- mods_NoiseBiz[1]
    W$NoiseBiz[which(W_bak$NoiseBiz>bins_NoiseBiz[1] & W_bak$NoiseBiz<=bins_NoiseBiz[2])] <- mods_NoiseBiz[2] 
    W$NoiseBiz[which(W_bak$NoiseBiz>bins_NoiseBiz[2] & W_bak$NoiseBiz<=bins_NoiseBiz[3])] <- mods_NoiseBiz[3]
    W$NoiseBiz[which(W_bak$NoiseBiz>bins_NoiseBiz[3])] <- mods_NoiseBiz[4]
    
    W$NoiseBiz <- as.factor(W$NoiseBiz)
    
    # UrbInf
    summary(W$UrbInf)
    bins_UrbInf <- c(33,55,87)
    mods_UrbInf <- c("L","M","H","VH")
    cat("UrbInf:\n   below or equal to",bins_UrbInf[1],"SRCs - bin count:",length(which(W$UrbInf<=bins_UrbInf[1])),"\n")
    cat("   between",bins_UrbInf[1]+1,"and",bins_UrbInf[2],"SRCs - bin count:", length(which(W$UrbInf>bins_UrbInf[1] & W$UrbInf<=bins_UrbInf[2])),"\n")
    cat("   between",bins_UrbInf[2]+1,"and",bins_UrbInf[3],"SRCs- bin count:", length(which(W$UrbInf>bins_UrbInf[2] & W$UrbInf<=bins_UrbInf[3])),"\n")
    cat("   above",bins_UrbInf[3],"SRCs - bin count:", length(which(W$UrbInf>bins_UrbInf[3])),"\n")
    
    W$UrbInf[which(W_bak$UrbInf<=bins_UrbInf[1])] <- mods_UrbInf[1]
    W$UrbInf[which(W_bak$UrbInf>bins_UrbInf[1] & W_bak$UrbInf<=bins_UrbInf[2])] <- mods_UrbInf[2] 
    W$UrbInf[which(W_bak$UrbInf>bins_UrbInf[2] & W_bak$UrbInf<=bins_UrbInf[3])] <- mods_UrbInf[3]
    W$UrbInf[which(W_bak$UrbInf>bins_UrbInf[3])] <- mods_UrbInf[4]
    
    W$UrbInf <- as.factor(W$UrbInf)
    
    # Traffic
    summary(W$Traffic)
    bins_Traffic <- c(24,54,87)
    mods_Traffic <- c("L","M","H","VH")
    cat("Traffic:\n   below or equal to",bins_Traffic[1],"SRCs - bin count:",length(which(W$Traffic<=bins_Traffic[1])),"\n")
    cat("   between",bins_Traffic[1]+1,"and",bins_Traffic[2],"SRCs - bin count:", length(which(W$Traffic>bins_Traffic[1] & W$Traffic<=bins_Traffic[2])),"\n")
    cat("   between",bins_Traffic[2]+1,"and",bins_Traffic[3],"SRCs- bin count:", length(which(W$Traffic>bins_Traffic[2] & W$Traffic<=bins_Traffic[3])),"\n")
    cat("   above",bins_Traffic[3],"SRCs - bin count:", length(which(W$Traffic>bins_Traffic[3])),"\n")
    
    W$Traffic[which(W_bak$Traffic<=bins_Traffic[1])] <- mods_Traffic[1]
    W$Traffic[which(W_bak$Traffic>bins_Traffic[1] & W_bak$Traffic<=bins_Traffic[2])] <- mods_Traffic[2] 
    W$Traffic[which(W_bak$Traffic>bins_Traffic[2] & W_bak$Traffic<=bins_Traffic[3])] <- mods_Traffic[3]
    W$Traffic[which(W_bak$Traffic>bins_Traffic[3])] <- mods_Traffic[4]
    
    W$Traffic <- as.factor(W$Traffic)
    
    # NoiseTraf
    summary(W$NoiseTraf)
    bins_NoiseTraf <- c(4,11,23)
    mods_NoiseTraf <- c("VL","L","M","H")
    cat("NoiseTraf:\n   below or equal to",bins_NoiseTraf[1],"SRCs - bin count:",length(which(W$NoiseTraf<=bins_NoiseTraf[1])),"\n")
    cat("   between",bins_NoiseTraf[1]+1,"and",bins_NoiseTraf[2],"SRCs - bin count:", length(which(W$NoiseTraf>bins_NoiseTraf[1] & W$NoiseTraf<=bins_NoiseTraf[2])),"\n")
    cat("   between",bins_NoiseTraf[2]+1,"and",bins_NoiseTraf[3],"SRCs- bin count:", length(which(W$NoiseTraf>bins_NoiseTraf[2] & W$NoiseTraf<=bins_NoiseTraf[3])),"\n")
    cat("   above",bins_NoiseTraf[3],"SRCs - bin count:", length(which(W$NoiseTraf>bins_NoiseTraf[3])),"\n")
    
    W$NoiseTraf[which(W_bak$NoiseTraf<=bins_NoiseTraf[1])] <- mods_NoiseTraf[1]
    W$NoiseTraf[which(W_bak$NoiseTraf>bins_NoiseTraf[1] & W_bak$NoiseTraf<=bins_NoiseTraf[2])] <- mods_NoiseTraf[2] 
    W$NoiseTraf[which(W_bak$NoiseTraf>bins_NoiseTraf[2] & W_bak$NoiseTraf<=bins_NoiseTraf[3])] <- mods_NoiseTraf[3]
    W$NoiseTraf[which(W_bak$NoiseTraf>bins_NoiseTraf[3])] <- mods_NoiseTraf[4]
    
    W$NoiseTraf <- as.factor(W$NoiseTraf)
    
    # WaterSyst
    summary(W$WaterSyst)
    bins_WaterSyst <- c(19,29,44)
    mods_WaterSyst <- c("L","M","MH","H")
    cat("WaterSyst:\n   below or equal to",bins_WaterSyst[1],"SRCs - bin count:",length(which(W$WaterSyst<=bins_WaterSyst[1])),"\n")
    cat("   between",bins_WaterSyst[1]+1,"and",bins_WaterSyst[2],"SRCs - bin count:", length(which(W$WaterSyst>bins_WaterSyst[1] & W$WaterSyst<=bins_WaterSyst[2])),"\n")
    cat("   between",bins_WaterSyst[2]+1,"and",bins_WaterSyst[3],"SRCs- bin count:", length(which(W$WaterSyst>bins_WaterSyst[2] & W$WaterSyst<=bins_WaterSyst[3])),"\n")
    cat("   above",bins_WaterSyst[3],"SRCs - bin count:", length(which(W$WaterSyst>bins_WaterSyst[3])),"\n")
    
    W$WaterSyst[which(W_bak$WaterSyst<=bins_WaterSyst[1])] <- mods_WaterSyst[1]
    W$WaterSyst[which(W_bak$WaterSyst>bins_WaterSyst[1] & W_bak$WaterSyst<=bins_WaterSyst[2])] <- mods_WaterSyst[2] 
    W$WaterSyst[which(W_bak$WaterSyst>bins_WaterSyst[2] & W_bak$WaterSyst<=bins_WaterSyst[3])] <- mods_WaterSyst[3]
    W$WaterSyst[which(W_bak$WaterSyst>bins_WaterSyst[3])] <- mods_WaterSyst[4]
    
    W$WaterSyst <- as.factor(W$WaterSyst)
    
    # ConsumProt
    summary(W$ConsumProt)
    bins_ConsumProt <- c(5,13,23)
    mods_ConsumProt <- c("VL","L","M","H")
    cat("ConsumProt:\n   below or equal to",bins_ConsumProt[1],"SRCs - bin count:",length(which(W$ConsumProt<=bins_ConsumProt[1])),"\n")
    cat("   between",bins_ConsumProt[1]+1,"and",bins_ConsumProt[2],"SRCs - bin count:", length(which(W$ConsumProt>bins_ConsumProt[1] & W$ConsumProt<=bins_ConsumProt[2])),"\n")
    cat("   between",bins_ConsumProt[2]+1,"and",bins_ConsumProt[3],"SRCs- bin count:", length(which(W$ConsumProt>bins_ConsumProt[2] & W$ConsumProt<=bins_ConsumProt[3])),"\n")
    cat("   above",bins_ConsumProt[3],"SRCs - bin count:", length(which(W$ConsumProt>bins_ConsumProt[3])),"\n")
    
    W$ConsumProt[which(W_bak$ConsumProt<=bins_ConsumProt[1])] <- mods_ConsumProt[1]
    W$ConsumProt[which(W_bak$ConsumProt>bins_ConsumProt[1] & W_bak$ConsumProt<=bins_ConsumProt[2])] <- mods_ConsumProt[2] 
    W$ConsumProt[which(W_bak$ConsumProt>bins_ConsumProt[2] & W_bak$ConsumProt<=bins_ConsumProt[3])] <- mods_ConsumProt[3]
    W$ConsumProt[which(W_bak$ConsumProt>bins_ConsumProt[3])] <- mods_ConsumProt[4]
    
    W$ConsumProt <- as.factor(W$ConsumProt)
    
    # SocServ
    summary(W$SocServ)
    bins_SocServ <- c(2,6,11)
    mods_SocServ <- c("VL","L","M","MH")
    cat("SocServ:\n   below or equal to",bins_SocServ[1],"SRCs - bin count:",length(which(W$SocServ<=bins_SocServ[1])),"\n")
    cat("   between",bins_SocServ[1]+1,"and",bins_SocServ[2],"SRCs - bin count:", length(which(W$SocServ>bins_SocServ[1] & W$SocServ<=bins_SocServ[2])),"\n")
    cat("   between",bins_SocServ[2]+1,"and",bins_SocServ[3],"SRCs- bin count:", length(which(W$SocServ>bins_SocServ[2] & W$SocServ<=bins_SocServ[3])),"\n")
    cat("   above",bins_SocServ[3],"SRCs - bin count:", length(which(W$SocServ>bins_SocServ[3])),"\n")
    
    W$SocServ[which(W_bak$SocServ<=bins_SocServ[1])] <- mods_SocServ[1]
    W$SocServ[which(W_bak$SocServ>bins_SocServ[1] & W_bak$SocServ<=bins_SocServ[2])] <- mods_SocServ[2] 
    W$SocServ[which(W_bak$SocServ>bins_SocServ[2] & W_bak$SocServ<=bins_SocServ[3])] <- mods_SocServ[3]
    W$SocServ[which(W_bak$SocServ>bins_SocServ[3])] <- mods_SocServ[4]
    
    W$SocServ <- as.factor(W$SocServ)
    
    # IAO
    summary(W$IAO)
    bins_IAO <- c(14,23,33)
    mods_IAO <- c("L","M","MH","H")
    cat("IAO:\n   below or equal to",bins_IAO[1],"SRCs - bin count:",length(which(W$IAO<=bins_IAO[1])),"\n")
    cat("   between",bins_IAO[1]+1,"and",bins_IAO[2],"SRCs - bin count:", length(which(W$IAO>bins_IAO[1] & W$IAO<=bins_IAO[2])),"\n")
    cat("   between",bins_IAO[2]+1,"and",bins_IAO[3],"SRCs- bin count:", length(which(W$IAO>bins_IAO[2] & W$IAO<=bins_IAO[3])),"\n")
    cat("   above",bins_IAO[3],"SRCs - bin count:", length(which(W$IAO>bins_IAO[3])),"\n")
    
    W$IAO[which(W_bak$IAO<=bins_IAO[1])] <- mods_IAO[1]
    W$IAO[which(W_bak$IAO>bins_IAO[1] & W_bak$IAO<=bins_IAO[2])] <- mods_IAO[2] 
    W$IAO[which(W_bak$IAO>bins_IAO[2] & W_bak$IAO<=bins_IAO[3])] <- mods_IAO[3]
    W$IAO[which(W_bak$IAO>bins_IAO[3])] <- mods_IAO[4]
    
    W$IAO <- as.factor(W$IAO)
    
    # EnvProt
    summary(W$EnvProt)
    bins_EnvProt <- c(16,26,41)
    mods_EnvProt <- c("L","M","MH","H")
    cat("EnvProt:\n   below or equal to",bins_EnvProt[1],"SRCs - bin count:",length(which(W$EnvProt<=bins_EnvProt[1])),"\n")
    cat("   between",bins_EnvProt[1]+1,"and",bins_EnvProt[2],"SRCs - bin count:", length(which(W$EnvProt>bins_EnvProt[1] & W$EnvProt<=bins_EnvProt[2])),"\n")
    cat("   between",bins_EnvProt[2]+1,"and",bins_EnvProt[3],"SRCs- bin count:", length(which(W$EnvProt>bins_EnvProt[2] & W$EnvProt<=bins_EnvProt[3])),"\n")
    cat("   above",bins_EnvProt[3],"SRCs - bin count:", length(which(W$EnvProt>bins_EnvProt[3])),"\n")
    
    W$EnvProt[which(W_bak$EnvProt<=bins_EnvProt[1])] <- mods_EnvProt[1]
    W$EnvProt[which(W_bak$EnvProt>bins_EnvProt[1] & W_bak$EnvProt<=bins_EnvProt[2])] <- mods_EnvProt[2] 
    W$EnvProt[which(W_bak$EnvProt>bins_EnvProt[2] & W_bak$EnvProt<=bins_EnvProt[3])] <- mods_EnvProt[3]
    W$EnvProt[which(W_bak$EnvProt>bins_EnvProt[3])] <- mods_EnvProt[4]
    
    W$EnvProt <- as.factor(W$EnvProt)
    
    # Violation
    summary(W$Violation)
    bins_Violation <- c(7,20,38)
    mods_Violation <- c("L","M","MH","H")
    cat("Violation:\n   below or equal to",bins_Violation[1],"SRCs - bin count:",length(which(W$Violation<=bins_Violation[1])),"\n")
    cat("   between",bins_Violation[1]+1,"and",bins_Violation[2],"SRCs - bin count:", length(which(W$Violation>bins_Violation[1] & W$Violation<=bins_Violation[2])),"\n")
    cat("   between",bins_Violation[2]+1,"and",bins_Violation[3],"SRCs- bin count:", length(which(W$Violation>bins_Violation[2] & W$Violation<=bins_Violation[3])),"\n")
    cat("   above",bins_Violation[3],"SRCs - bin count:", length(which(W$Violation>bins_Violation[3])),"\n")
    
    W$Violation[which(W_bak$Violation<=bins_Violation[1])] <- mods_Violation[1]
    W$Violation[which(W_bak$Violation>bins_Violation[1] & W_bak$Violation<=bins_Violation[2])] <- mods_Violation[2] 
    W$Violation[which(W_bak$Violation>bins_Violation[2] & W_bak$Violation<=bins_Violation[3])] <- mods_Violation[3]
    W$Violation[which(W_bak$Violation>bins_Violation[3])] <- mods_Violation[4]
    
    W$Violation <- as.factor(W$Violation)
    
    # Misdemeanor
    summary(W$Misdemeanor)
    bins_Misdemeanor <- c(33,87,178)
    mods_Misdemeanor <- c("M","H","VH","OC")
    cat("Misdemeanor:\n   below or equal to",bins_Misdemeanor[1],"SRCs - bin count:",length(which(W$Misdemeanor<=bins_Misdemeanor[1])),"\n")
    cat("   between",bins_Misdemeanor[1]+1,"and",bins_Misdemeanor[2],"SRCs - bin count:", length(which(W$Misdemeanor>bins_Misdemeanor[1] & W$Misdemeanor<=bins_Misdemeanor[2])),"\n")
    cat("   between",bins_Misdemeanor[2]+1,"and",bins_Misdemeanor[3],"SRCs- bin count:", length(which(W$Misdemeanor>bins_Misdemeanor[2] & W$Misdemeanor<=bins_Misdemeanor[3])),"\n")
    cat("   above",bins_Misdemeanor[3],"SRCs - bin count:", length(which(W$Misdemeanor>bins_Misdemeanor[3])),"\n")
    
    W$Misdemeanor[which(W_bak$Misdemeanor<=bins_Misdemeanor[1])] <- mods_Misdemeanor[1]
    W$Misdemeanor[which(W_bak$Misdemeanor>bins_Misdemeanor[1] & W_bak$Misdemeanor<=bins_Misdemeanor[2])] <- mods_Misdemeanor[2] 
    W$Misdemeanor[which(W_bak$Misdemeanor>bins_Misdemeanor[2] & W_bak$Misdemeanor<=bins_Misdemeanor[3])] <- mods_Misdemeanor[3]
    W$Misdemeanor[which(W_bak$Misdemeanor>bins_Misdemeanor[3])] <- mods_Misdemeanor[4]
    
    W$Misdemeanor <- as.factor(W$Misdemeanor)
    
    # Felony
    summary(W$Felony)
    bins_Felony <- c(17,45,91)
    mods_Felony <- c("ML","M","H","VH")
    cat("Felony:\n   below or equal to",bins_Felony[1],"SRCs - bin count:",length(which(W$Felony<=bins_Felony[1])),"\n")
    cat("   between",bins_Felony[1]+1,"and",bins_Felony[2],"SRCs - bin count:", length(which(W$Felony>bins_Felony[1] & W$Felony<=bins_Felony[2])),"\n")
    cat("   between",bins_Felony[2]+1,"and",bins_Felony[3],"SRCs- bin count:", length(which(W$Felony>bins_Felony[2] & W$Felony<=bins_Felony[3])),"\n")
    cat("   above",bins_Felony[3],"SRCs - bin count:", length(which(W$Felony>bins_Felony[3])),"\n")
    
    W$Felony[which(W_bak$Felony<=bins_Felony[1])] <- mods_Felony[1]
    W$Felony[which(W_bak$Felony>bins_Felony[1] & W_bak$Felony<=bins_Felony[2])] <- mods_Felony[2] 
    W$Felony[which(W_bak$Felony>bins_Felony[2] & W_bak$Felony<=bins_Felony[3])] <- mods_Felony[3]
    W$Felony[which(W_bak$Felony>bins_Felony[3])] <- mods_Felony[4]
    
    W$Felony <- as.factor(W$Felony)
    
}

cat("Active categorical factors:",names(W[which(sapply(W, is.factor))]),"\n")
length(names(W[which(sapply(W, is.factor))]))


# ###########################
## Save to disk
# ###########################

Wp <- cbind(ZIP=rownames(W),W) 
if (! dynAnalysisF) {
    # Isolated time period analysis
    target_file <- paste0("Data/",
                          yearNbr,
                          ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                          "00_nyc_simple-whole-data-set_binned.csv")   ## Use when analyzing one period alone
} else {
    # Temporal evolution analysis over several time periods
    target_file <- paste0("Data/",
                          yearNbr,
                          ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                          "00_nyc_simple-whole-data-set_te-binned.csv")
}
csvSaveF(Wp, target_file)
rm(Wp)


# ###########################
## Boroughs' Crime Index tables and bar plots
# ###########################

WbogusZIP <- W[which(W$Borough == "99999"), ]
Z <- W[which(W$Borough != "99999"), ]
Z$Borough <- factor(Z$Borough)  
levels(Z$Borough)  # check that "99999" has disappeared as level and observation

par(mfrow = c(1,3))

# VIOLATIONS
crimeDistScale <- c("L","M","MH","H")
(BCIviol <- with(Z,table(Borough,Violation))[,order(match(colnames(with(Z,table(Borough,Violation))),
                                                          crimeDistScale ))])
summar <- summary(Z$Violation)
tableColName <- c()
for (cc in crimeDistScale) {
    tableColName <- c(tableColName,paste(cc,summar[labels(summar)==cc]))
}
colnames(BCIviol) <- tableColName

BCIviol_norm <- sweep(BCIviol,1,margin.table(BCIviol,1),"/") # row-wise normalized segmentation
#BCIviol_norm <- prop.table(BCIviol,1)  # same as above
apply(BCIviol_norm,1,function(x) sum(x)) # check that all rows sum up to 1
#BCIviol_norm <- BCIviol_norm[order(BCIviol_norm[,4],-BCIviol_norm[,1]),]
BCIviol_norm = t(BCIviol_norm)  # transpose to be able to use stacked barplot

title=paste0('Violations (',sum(W_bak$Violation),')',
            '\n(',month.name[monthNbr],' ',yearNbr,')')
barplot(round(BCIviol_norm*100,0),
        ylab="Normalized segmentation (%)",
        main=title,
        sub="",
        las=2,
        cex.names=0.9,
        col=c("blue","cyan","yellow","orange"),
        xlim=c(0, ncol(BCIviol_norm) +2),  # make space for legend
        legend.text = T,
        args.legend=list(
            x=ncol(BCIviol_norm) +4,
            y=100,
            bty = "n" # box type around legend
        ))

# MISDEMEANORS
crimeDistScale <- c("M","H","VH","OC")
(BCImisd <- with(Z,table(Borough,Misdemeanor))
    [,order(match(colnames(with(Z,table(Borough,Misdemeanor))),crimeDistScale))])

summar <- summary(Z$Misdemeanor)
tableColName <- c()
for (cc in crimeDistScale) {
    tableColName <- c(tableColName,paste(cc,summar[labels(summar)==cc]))
}
colnames(BCImisd) <- tableColName

BCImisd_norm <- sweep(BCImisd,1,margin.table(BCImisd,1),"/") # row-wise normalized market segmentation for each brand
apply(BCImisd_norm,1,function(x) sum(x)) # check that all rows sum up to 1
BCImisd_norm = t(BCImisd_norm)  # transpose to be able to use stacked barplot

title=paste0('Misdemeanors (',sum(W_bak$Misdemeanor),')',
             '\n(',month.name[monthNbr],' ',yearNbr,')')
barplot(round(BCImisd_norm*100,0),
        #ylab="Normalized segmentation (%)",
        main=title,
        sub="",
        las=2,
        cex.names=0.9,
        col=c("cyan","orange","red","darkred"),
        xlim=c(0, ncol(BCImisd_norm) + 2),  # make space for legend
        legend.text = T,
        args.legend=list(
            x=ncol(BCImisd_norm) +4,
            y=100,
            bty = "n" # box type around legend
        ))

# FELONIES
crimeDistScale <- c("ML","M","H","VH")
(BCIfelon <- with(Z,table(Borough,Felony))[,order(match(colnames(with(Z,table(Borough,Felony))),
                                                        crimeDistScale))])
summar <- summary(Z$Felony)
tableColName <- c()
for (cc in crimeDistScale) {
    tableColName <- c(tableColName,paste(cc,summar[labels(summar)==cc]))
}
colnames(BCIfelon) <- tableColName

BCIfelon_norm <- sweep(BCIfelon,1,margin.table(BCIfelon,1),"/") # row-wise normalized market segmentation for each brand
apply(BCIfelon_norm,1,function(x) sum(x)) # check that all rows sum up to 1
BCIfelon_norm = t(BCIfelon_norm)  # transpose to be able to use stacked barplot

title=paste0('Felonies (',sum(W_bak$Felony),')',
             '\n(',month.name[monthNbr],' ',yearNbr,')')
barplot(round(BCIfelon_norm*100,0),
        #ylab="Normalized segmentation (%)",
        main=title,
        sub="",
        las=2,
        cex.names=0.9,
        col=c("#009999","cyan","orange","red"),
        xlim=c(0, ncol(BCIfelon_norm) + 2),  # make space for legend
        legend.text = T,
        args.legend=list(
            x=ncol(BCIfelon_norm) +4,
            y=100,
            bty = "n" # box type around legend
        ))

par(mfrow=c(1,1))


# ###########################
## MCA: Multiple Component Analysis
# ###########################

library(nnet)
##  To encode variable modalities according to a one-hot encoding scheme:
#+ 1st compute the total number of modalities for all _active_ categorical variables
#+ 2nd form all modaity-indicator submatrices and cbind() them in one large final indicator matrix

J <-0  # initialize total number of modalities (after binning each active categorical var)
colHeader <- c()
# W <- as.data.frame(rbind(as.matrix(W),as.matrix(WbogusZIP)))
# #W <- cbind(apply(W[,1:17],2,factor),apply(W[,18:19],2,as.integer))
# W <- cbind(apply(W[,1:17],2,factor),W[,18:19])
# length(names(W[which(sapply(W, is.factor))]))

Nmod <- length(which(lapply(W[,-1],is.factor)==TRUE))
for (nn in 1:Nmod) {
    colHeader <- c( colHeader,paste0(colnames(W)[nn+1],"_",levels(W[,nn+1])) )
    J <- J + length(levels(W[,nn+1])) 
    }

Windic <- data.frame(matrix(ncol = 0, nrow = nrow(W)))
for (ii in 1:Nmod) { subWindic <- class.ind(W[,ii+1]); Windic <- cbind(Windic,subWindic) }
rownames(Windic) <- rownames(W)
colnames(Windic) <- colHeader
# check that sum of rows equals nbr of active categorical variables: ok

dim(W[,-1])

if (yearNbr == 2010) {
    supObservations <- c(zip10281_Widx, zip11430_Widx)
} else if (yearNbr == 2014) {
    supObservations <- c(zip10463_Widx, zip11430_Widx)
} else if (yearNbr == 2018) {
    supObservations <- c(zip11371_Widx, zip11430_Widx)
    #Boroughs <- W[-c(zip99999_Widx, zip11430_Widx,),1]

} else {
    cat("\n\n---------------\nUNKNOWN YEAR\n Abort\n--------------------\n\n")
    exit()
}
Boroughs <- W[-zip99999_Widx,1]
Boroughs <- factor(Boroughs)  # clean up factor levels


mcaNYC311 <- MCA(W[-zip99999_Widx,-1],
                 ncp=5,
                 ind.sup=supObservations,
                 quanti.sup=c(17,18),
                 excl=NULL,
                 graph = TRUE,
                 level.ventil = 0.02,
                 axes = c(1,2),
                 #row.w = NULL,
                 method="Indicator",
                 na.method="NA",
                 tab.disj=NULL
                 )

summary(mcaNYC311,nbelements=12)
dimdesc(mcaNYC311)



mcaPlot1 <- plot.MCA(mcaNYC311,
                    #mcaNYC311$ind$coord[,1],mcaNYC311$ind$coord[,2],
                    choix="ind",
                    autoLab="yes",
                    cex=0.9,
                    col.ind = ccolors[as.numeric(factor(Boroughs[-supObservations]))], 
                    col.var = c(rep("lightsalmon3",52),rep("purple",12)),
                    col.ind.sup = "red", col.quanti.sup = "orchid3",
                    label = c("var","ind.sup","quanti.sup"),
                    title = paste0("MCA - Biplot",
                                   '\n(NYC 311 + NYPD 911: ',month.name[monthNbr],' ',yearNbr,')')
)
mcaPlot1 + legend("bottomright",
                  legend=levels(Boroughs),
                  cex=0.8,
                  pt.cex=1.3,
                  pch=16,
                  col=ccolors[1:length(levels(Boroughs))])

mcaPlot2 <- fviz_mca_biplot(mcaNYC311, 
                axes = c(1, 2),
                geom.ind = c("point","text"), 
                geom.var = c("point","text"),
                repel = TRUE, 
                pointsize = 1.3,
                label = "var",
                #var.col=c(rep("gray",52),rep("red,12")),
                invisible = "none", 
                habillage = factor(Boroughs[-supObservations]), 
                addEllipses = FALSE,
                palette = ccolors[1:length(levels(factor(Boroughs[-supObservations])))],
                arrows = c(FALSE, FALSE),
                map = "asymmetric",
                title = paste0("MCA - Biplot",
                               '\n(NYC 311 + NYPD 911: ',month.name[monthNbr],' ',yearNbr,')') #,
                #col.var = c(rep("lightsalmon3",52),rep("purple",12)),
                
                )
mcaPlot2 

mcaPlot3 <- plot.MCA(mcaNYC311,
                    #mcaNYC311$ind$coord[,1],mcaNYC311$ind$coord[,2],
                    title=paste0("Composite variables' bi-plot projection in PC1-2\n(MCA on ",
                                month.name[monthNbr]," ",yearNbr," NYC data)"),
                    choix="var",
                    autoLab="yes",
                    cex=0.9,
                    col.ind = ccolors[as.numeric(Boroughs)], col.var = c(rep("black",13),rep("red",3), c("blue","blue")),
                    col.ind.sup = "red", col.quanti.sup = "orchid3",
                    label = c("var","ind.sup","quanti.sup"))
mcaPlot3+ abline(a=c(0,0),b=0.44,lty=2,lwd=2,col="gray")

mcaPlot4 <- fviz_mca_var(mcaNYC311,
                         title=paste0("a) Variables' projection in PC1-2 (all)\n(MCA on ",
                                      month.name[monthNbr]," ",yearNbr," NYC data)"),
                         axes = c(1, 2),
                         geom=c("arrow","text"),
                         label=c("var","quanti.sup"),
                         invisible="none",
                         #pointsize=2,
                         #col.ind = "coord",
                         #col.var = "contrib",
                         col.var = c(rep("SRCs",52),rep("Crime",12)),
                         #coltext=c(rep("gray",52),rep("red",12)),
                         select.var = list(name = NULL, cos2 = 0, contrib = NULL),
                         habillage="none",
                         #jitter = list(what = "label", width = NULL, height = NULL)   # deprecated, use "repel=" instead
                         repel=TRUE,
                         legend=NULL)
#mcaPlot4 + points(mcaNYC311$var$coord[c(53:64),1],mcaNYC311$var$coord[c(53:64),2],pch=17,pointsize=2,col="red")         
mcaPlot4

mcaPlot5 <- fviz_mca_var(mcaNYC311,
                         title=paste0("b) Variables' projection in PC1-2 (cos2>0.2)\n(MCA on ",
                                      month.name[monthNbr]," ",yearNbr," NYC data)"),
                         axes = c(1, 2),
                         geom=c("arrow","text"),
                         label=c("var","quanti.sup"),
                         invisible="none",
                         #pointsize=2,
                         #col.ind = "coord",
                         #col.var = "contrib",
                         col.var = c(rep("SRCs",52),rep("Crime",12)),
                         #coltext=c(rep("gray",52),rep("red",12)),
                         select.var = list(name = NULL, cos2 = 0.2, contrib = NULL),
                         habillage="none",
                         #jitter = list(what = "label", width = NULL, height = NULL)   # deprecated, use "repel=" instead
                         repel=TRUE,
                         legend=NULL)
mcaPlot5
#mcaPlot5 + points(mcaNYC311$var$coord[c(53:64),1],mcaNYC311$var$coord[c(53:64),2],pch=17,pointsize=2,col="red")         

# simple plot of color-code projection of indiv. in PC1-2, with cos2 thermometer bar 
mcaPlot6 <- fviz_mca_ind(mcaNYC311,
             title=paste0("a) Individuals' projection in PC1-2\n(MCA on ",
                          month.name[monthNbr]," ",yearNbr," NYC data)"),
             axes = c(1, 2),
             geom="point",
             #col.ind = "coord",
             col.ind = "cos2",
             habillage="none" #,
             #alpha.ind="contrib",
             #label=list(as.character(Boroughs)),
             #pt.cex=1.3,
             #cex=1.3,
             # habillage=c(14,15,16),   # specifies index of factor var in data, used for color and ellipses
             # addEllipses=TRUE,
             # ellipse.level=0.75
) + scale_color_gradient2(low="blue", mid="green",high="red", midpoint=0.5)
mcaPlot6

# simple plot of color-code projection of indiv. in PC1-2; color code is by borough 
mcaPlot7 <- fviz_mca_ind(mcaNYC311,
             title=paste0("b) Individuals' projection in PC1-2, by NYC borough\n(MCA on ",
                          month.name[monthNbr]," ",yearNbr," NYC data)"),
             axes = c(1, 2),
             geom="point",
             pointsize=1.5,
             habillage= factor(Boroughs[-supObservations]) #,
             #col.ind = Boroughs,
             #col.ind=ccolors[as.numeric(Boroughs)]
             )
mcaPlot7 +  scale_color_manual(values = ccolors)

# plot 3 crime categorical variables with modalities' levels identified by color-coded ellipses in PC1-2
mcaPlot8 <- fviz_mca_ind(mcaNYC311,
                         title=paste0("Individuals' projection in PC1-2, with crime levels\n(MCA on ",
                                      month.name[monthNbr]," ",yearNbr," NYC data)"),
                         axes = c(1, 2),
                         geom="point",
                         textcol="black",
                         #col.ind = "ctr",
                         #label=Boroughs,
                         pointsize=0.8,
                         #cex=1.3,
                         habillage=c(14,15,16),   # specifies index of factor var in data, used for color and ellipses
                         addEllipses=TRUE,
                         ellipse.level=0.75)
mcaPlot8 # + scale_color_brewer(palette="Set3")



## Identify ZIP codes in 2nd quadrant of PC12 var projection from MCA
#      mcaNYC311$var$coord[,1] <= 0   &&   mcaNYC311$var$coord[,2] >= 0
projVarQ2_ZIPs <- rownames(W[which(ifelse(mcaNYC311$ind$coord[,1] <= 0,ifelse(mcaNYC311$ind$coord[,2] >= 0,TRUE,FALSE),FALSE)),])

## Compute number of violations in 2nd quadrant of PC12 var projection from MCA
#      mcaNYC311$var$coord[,1] <= 0   &&   mcaNYC311$var$coord[,2] >= 0   
sum(W_bak[rownames(W_bak) %in% projVarQ2_ZIPs,15])   # sum of all violation counts in 2nd quadrant 
sum(W_bak[ ! rownames(W_bak) %in% projVarQ2_ZIPs,15])  # sum of all other violation counts 

## Compute number of misdemeanors in 2nd quadrant of PC12 var projection from MCA
sum(W_bak[rownames(W_bak) %in% projVarQ2_ZIPs,16])   # sum of all misdemeanor counts in 2nd quadrant 
sum(W_bak[ ! rownames(W_bak) %in% projVarQ2_ZIPs,16])  # sum of all other misdemeanor counts 

## Compute number of felonies in 2nd quadrant of PC12 var projection from MCA
sum(W_bak[rownames(W_bak) %in% projVarQ2_ZIPs,17])   # sum of all felonie counts in 2nd quadrant 
sum(W_bak[,17])  # sum of all other felonie counts 
