# #############################
## Project:     Analysis of NYC-311 Service Request Calls
## Script:      09b_nyc-zip00083_apportionment.R
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

datestamp <- format(Sys.time(),"%Y%m%d-%H%M%S"); 


# #############################
## Source parameter file
# #############################

source(file="Scripts/01_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...


# #############################
## Libraries
# #############################

setRepositories(ind = c(1:6,8))
#library("ggplot2")
library("VIM")        # Visualization and Imputation of Missing values
# methods aggr(),barmiss(), histmiss() for missing/imputed values

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
source_file <- "nyc311-zip00083-neighbors-common-border.csv"
apportRule.df <- read.csv(paste0("Data/",source_file),
                          header=T,
                          sep=",",
                          quote="\"",
                          dec=".",
                          skip=0)

# load csv data-set to process
source_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                      "00_nyc_whole-data-set.csv")
protoY <- read.csv(source_file,
                   header=T,
                   sep=",",
                   quote="\"",
                   dec=".",
                   skip=0)

# load NYC borough ZIP code info
source_file <- paste0("Data/nyc_borough-zip.csv")

boroughZIP <- read.csv(source_file,
                       header=T,
                       sep=",",
                       quote="\"",
                       dec=".",
                       skip=0)
# boroughZIP <- as.data.frame(cbind(ZIP=as.character(sprintf("%05d",boroughZIP$ZIP)),
#                                   Borough=as.character(boroughZIP[,2])))  # format as 5 character string

# #############################
## Apportion ghost ZIP "00083" observations to surrounding real zips
# #############################

# determine source_ZIP and target_ZIP from rules
#names(apportRule.df)
ghostZIPapport_idx <- which(apportRule.df$len_prop==100)  # ZIP apport file index
ghostZIP <- apportRule.df$ZIP[ghostZIPapport_idx]   # source ghost ZIP code
neighbor_lst <- apportRule.df$ZIP[-ghostZIPapport_idx]  # target_ZIP codes
apportRule_lst <- as.numeric(as.vector(apportRule.df$len_prop[-ghostZIPapport_idx]))/100  # 
sum(apportRule_lst)  # chk_09.02


# check that neighboring ZIP codes are inside NYC's 5 boroughs
neighborZIP <- c() ; neighborZIPborough <- c() ; apportRule <- c()
for (zipcode in neighbor_lst) {
    if (zipcode %in% boroughZIP$ZIP) {
        neighborZIP <- c(neighborZIP,nn)
        neighborZIPborough <- c(neighborZIPborough,boroughZIP[boroughZIP$ZIP == zipcode,2])
        apportRule <- c(apportRule,apportRule_lst[which(neighbor_lst == zipcode)])
    }
}
apportRule <- apportRule/sum(apportRule)  # re-normalize, in case some neighbors are excluded from apportionment

# create neighboring ZIP codes' obs in data-set 'protoY', if necessary
if (length(which(! neighborZIP %in% protoY$ZIP )) !=0) {
    missingZIP <- neighborZIP[which(! neighborZIP %in% protoY$ZIP )]
    
    missingBorough <- as.character(unique(boroughZIP[which(boroughZIP$ZIP==missingZIP),2]))
    protoY <- rbind(protoY,
                    cbind(ZIP=missingZIP,
                          Borough=missingBorough,
                          HousCond=rep(0,length(missingZIP)),
                          Sani=rep(0,length(missingZIP)),
                          NoiseResid=rep(0,length(missingZIP)),
                          NoiseConst=rep(0,length(missingZIP)),
                          NoiseBiz=rep(0,length(missingZIP)),
                          UrbInf=rep(0,length(missingZIP)),
                          Traffic=rep(0,length(missingZIP)),
                          NoiseTraf=rep(0,length(missingZIP)),
                          WaterSyst=rep(0,length(missingZIP)),
                          ConsumProt=rep(0,length(missingZIP)),
                          SocServ=rep(0,length(missingZIP)),
                          IAO=rep(0,length(missingZIP)),
                          EnvProt=rep(0,length(missingZIP)),
                          Violation=rep(0,length(missingZIP)),
                          Misdemeanor=rep(0,length(missingZIP)),
                          Felony=rep(0,length(missingZIP)),
                          medianInc=rep(NA,length(missingZIP)),
                          jlBenef=rep(NA,length(missingZIP))
                          )
                    )
}


# sort target ZIP code list by decreasing apportionment level
apportRuleSorted <- apportRule[order(apportRule,decreasing=T)]
# recover indices of neighboring ZIP code within NYC's 5 boroughs and in data-set
protoYneigh_idxSorted <- which(protoY$ZIP %in% neighborZIP)[order(apportRule,decreasing=T)]   # sorted


# apportion
protoYghostZIP_idx <- which(protoY$ZIP == ghostZIP)  # source ghost ZIP code index

# aa <- 0 # chk_09.03
for (zz_idx in protoYneigh_idxSorted) {
    for (ii in 3:18) {
        apportRate <- apportRuleSorted[which(protoYneigh_idxSorted == zz_idx)]
        # if (ii==10) { aa <- aa + round(apportRate * protoY[protoYghostZIP_idx,10]) }  # chk_09.03
        protoY[zz_idx,ii] <- protoY[zz_idx,ii] + round(apportRate * protoY[protoYghostZIP_idx,ii])
    }
}
protoY <- protoY[-protoYghostZIP_idx,]


# characterize missings
missingIRS <- protoY[is.na(protoY$medianInc),]

# Compute and draw missings' table, for all obs with missing ZIP
missingZIP_aggr <- aggr(protoY, 
                        numbers=TRUE,
                        bars=TRUE,
                        combined=FALSE,
                        prop=FALSE,
                        plot=TRUE,
                        axes=TRUE,
                        varheight=FALSE,
                        labels=names(protoY),
                        col=ccolors[6:8],
                        cex.axis=1,
                        ylab=c("Missing Data (count)","Missing Data Distribution") )

summary(missingZIP_aggr)



# save to disk
target_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                      "00_nyc_whole-data-set.csv")
csvSaveF(protoY,target_file)     # csv to disk

