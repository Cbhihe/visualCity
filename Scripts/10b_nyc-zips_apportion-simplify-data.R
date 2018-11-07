# #############################
## MIRI:        Analysis of NYC-311 Service Request Calls
## Script:      10b_nyc-zips_apportion-simplify-data.R
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Last edit:   
# #############################

# Based on a CVS table of common boundaries for a set of ZIP codes , this script:
#  - calculates / saves the length of each ZIP code area's perimeter
#  - determines which ZIPs are to be absorbed by which other ZIP, based on the followin criterion. 
# Criterion: 
#     for ii!=jj, if ComBor[ii,jj] >= apct & ComBor[ii,jj] >= ComBor[jj,ii], then row ZIP "ii" absorbs col ZIP "jj"
# where: apct is the percentile threshold ratio of common boundary lengths between two adjacent ZIP code areas 

# #############################

rm(list=ls(all=TRUE))

# #############################

setwd("~/Documents/Work/Academic-research/NYC311/")

options(scipen=6) # R switches to sci notation above 5 digits on plot axes
set.seed(932178)


# #############################
## Load preliminary input parameters
# #############################

# Load Year, Month, Day, ...
source(file="Scripts/01_nyc311_input-parameters.R",
       local=F,echo=F)  


# #############################
## Libraries
# #############################

library("zipcode")    # Load all valid us ZIPs  <<<<<<< OUTDATED, 
library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")      # to read shapefile (translate GPS into ZIP)
library("fastshp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")


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
## Load data file
# #############################

source_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                      "00_nyc_whole-data-set.csv")
Y <- read.csv(file=source_file,
              header=T,
              sep=",",
              quote="\"",
              dec=".",
              skip=0)

# #############################
## ZIP of interest
# #############################

bogusZIP <- c("99999")
#ZIP_lst <- as.character(Y$ZIP[! Y$ZIP %in% bogusZIP])
ZIP_lst <- as.character(Y$ZIP)

# list of ZIP codes ('ZIP_lst') must conform to available ZIP mapping database
mapZIP <- readOGR(dsn="./Data/Geolocation", layer="ZIP_CODE_040114")
ZIP_idx <- which(ZIP_lst %in% unique(mapZIP$ZIPCODE))

# list of ZIP codes in complete data set but not in ZIP mapping database
supZIP <- Y$ZIP[-c(ZIP_idx)]
(YsupZIP <- Y[which(Y$ZIP %in% supZIP),])

#ZIP_lst <- as.character(Y$ZIP[! Y$ZIP %in% c(supZIP,bogusZIP)])
ZIP_lst <- as.character(Y$ZIP[ZIP_idx])
NZIP <- length(ZIP_lst)

# #############################
## Check which neighboring ZIP boundaries coincide in more than apct (%)
## Apportion absorbed ZIP code area's observations in Y to absorbing ZIP code's observations
# #############################

apct <- 75  # expressed in %

target_file=paste0("Data/",
                   yearNbr,
                   ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                   "00_nyc_all-zip-common-borders.csv")

ComBor <- read.table(file=target_file,
                     header=T,
                     sep=",",
                     dec=".")
# ComBor contains only one entry per ZIP pair, even if a given ZIP code's area consists of
# disjoint domains.

for (ii in 1:(NZIP-1)) {   # iterate over rows, check upper triangle, avoid diagonal
    for (jj in (ii+1):NZIP) {   # iterate over columns,  check upper triangle, avoid diagonal
        if ( (ComBor[ii,jj] >= apct) | (ComBor[jj,ii] >= apct) ) {
            Ytmp <- Y[as.character(Y[,1]) %in% c(ZIP_lst[ii],ZIP_lst[jj]), ]
            Ytmp[is.na(Ytmp)] <- 0
            if (ComBor[ii,jj] >= ComBor[jj,ii]) {
                cat("ZIP code",ZIP_lst[ii],"absorbed ZIP code",ZIP_lst[jj],"\n")
                Y[as.character(Y[,1]) == ZIP_lst[ii] , c(3:18,20)] <- Ytmp[1 , c(3:18,20)] + Ytmp[2, c(3:18,20)]
                Y <- Y[as.character(Y[,1]) != ZIP_lst[jj],]
                # Note: medianInc is not apportionned for lack of reliable population density data
            } else {
                cat("ZIP code",ZIP_lst[jj]," absorbed ZIP code",ZIP_lst[ii],"\n")
                Y[as.character(Y[,1]) == ZIP_lst[jj] , c(3:18,20)] <- Ytmp[1, c(3:18,20)] + Ytmp[2 , c(3:18,20)]
                Y <- Y[as.character(Y[,1]) != ZIP_lst[ii],]
                # Note: medianInc is not apportionned for lack of reliable population density data
            }
        }
    }
}

rm(Ytmp,ii,jj)


# Results for NYC's SRCs - period April 2014:
# ZIP code 11101 is to absorb ZIP code 11109 
#    ComBor yields 84.17%
# ZIP code 11433 is to absorb ZIP code 11451
#    ComBor yields 100%

# save to disk
target_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                      "00_nyc_simple-whole-data-set.csv")
csvSaveF(Y,target_file)     # csv to disk
