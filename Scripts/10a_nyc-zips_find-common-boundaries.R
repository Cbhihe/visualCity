# #############################
## MIRI:        Analysis of NYC-311 Service Request Calls
## Author:      Cedric Bhihe
## Date:        Sept. 2018
## Script:      10a_nyc-zips_find-common-boundaries.R
## Last edit:   
# #############################


# Based on a set of ZIP code maps, this script:
#  - determines the direct neighbors of each postal code area in the provided list.
#  - calculates & saves the length of each ZIP code area's perimeter
#  - calculates the percentage of common borders between adjacent ZIP code areas
#  - saves all ZIP codes common boundary results in a square matrix ComBor[i,j] whose entries are 
#+   jointBorder(%)=(common boundary lengths)/(boundary length of j)
#+   - diagonal elements=100
#+   - for i!=j, if ComBor[i,j] >= 75% && ComBor[i,j] >= ComBor[j,i], then ZIP "i" absorbs ZIP "j"
#  - saves common boundaries results in Data/nyc311_all-zip-common-borders.csv

# #############################

rm(list=ls(all=TRUE))

# #############################

setwd("~/Documents/Work/Academic-research/NYC311/")
options(scipen=6) # R switches to sci notation above 5 digits on plot axes
ccolors=c("red","green","blue","orange","cyan","tan1","darkred","honeydew2","violetred",
          "palegreen3","peachpuff4","lavenderblush3","lightgray","lightsalmon","wheat2")
set.seed(932178)

# #############################
## Libraries
# #############################

library("ggplot2")
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
## Load preliminary input parameters
# #############################

# Load Year, Month, Day, ...
source(file="Scripts/00_nyc311_input-parameters.R",local=F,echo=F)  


# #############################
## Load data file
# #############################

source_file <- paste0(yearNbr,
                      ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                      "00_nyc_whole-data-set.csv")
Y <- read.csv(paste0("Data/",source_file),
              header=T,
              sep=",",
              quote="\"",
              dec=".",
              skip=0)

# #############################
## ZIP of interest
# #############################
# Since the ZIP (Zoning Improvement Plan) was instated by the USPS Code in 1963,
# and in order to map out a more efficient delivery network, 43 Manhattan 
# buildings earned ZIP codes exclusive to their address.  This was justified
# either because of their size or because of the great number of people in them. 


# NY State ZIP codes include:
#   ZIP "00083" is unofficial for Central Park and consists entirely of portions of ZIP 
#        codes: 10019, 10065, 10023, 10021, 10075, 10028, 10024, 10128, 10025, 10029, 10026
#   ZIP  "99999" is bogus and  recoups all unresolved missings in recorded SRCs to 311
#   ZIPs "00501" and "00544" for  Holtsville, Suffolk County  (type: unique)
#   ZIP  "00690" is for for Fishers Island, Fishers Isle, Suffolk County  (type: PO Box)

#data("zipcode") # load US zip data
#ZIP_lst <- c(unlist(zipcode[zipcode$state=="NY",1]),
#             c("00083","10004","10018","10024","10029","10032","10036","10309","10459","10463",
#               "10469","10472","11103", "11207","11210", "11221","11229","11235","11368","11379",
#               "11385","11421","11435","11694"))

ZIP_lst <- as.character(Y$ZIP[Y$ZIP !="99999"])

# #############################
## Compute common boundaries between ghost ZIP "00083" and neighboring ZIPs
# #############################

# Use zipped achive at https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
mapZIP <- readOGR(dsn="./Data/Geolocation", layer="ZIP_CODE_040114")
shp <- read.shp("Data/Geolocation/ZIP_CODE_040114.shp", format="list")

# Compute length of every ZIP code's area's perimeter
#+  -> keep in mind that a ZIP code's area may consist of disjoint surface areas
NZIP <- length(ZIP_lst)
ZIP_perim.matrix <- matrix(0,nrow=NZIP,ncol=2)
colnames(ZIP_perim.matrix) <- c("ZIP","ZIP_perim")

for (ii in 1:NZIP) {
    ZIP_idx <- which(mapZIP$ZIPCODE==ZIP_lst[ii]) 
    ZIP_perim <- 0
    # Caution: ZIP_idx may contain more than one index as a ZIP code area
    # may consist of several simply connected domains. In that case the ZIP code area's
    # perimeter is the sum of all disconnected domains' perimeters.
    for (index in ZIP_idx) {
        for (pp in 1:(length(shp[[index]]$x)-1)) {
            ZIP_perim <- ZIP_perim + sqrt((shp[[index]]$x[pp+1]-shp[[index]]$x[pp])^2 +
                                              (shp[[index]]$y[pp+1]-shp[[index]]$y[pp])^2)
        }
    }
    ZIP_perim.matrix[ii,1] <- ZIP_lst[ii]
    ZIP_perim.matrix[ii,2] <- ZIP_perim
}

rm(pp,ii,index)

# ZIP codes with zero perimeter values are in fact PO box ZIP codes. 
# Suppressing them is safe.
ZIPtoSuppress <- ZIP_perim.matrix[ZIP_perim.matrix[,2]==0,1] # identify
# Results for NYC: "10129","10163", "11202","11695" are all PO boxes.

ZIP_perim.matrix <- ZIP_perim.matrix[ZIP_perim.matrix[,2]!=0,]  # suppress 


# Compute proportion of common ZIP boundaries between all ZIPs areas pair wise
# Place in square matrix: ComBor
# ComBor contains only one entry per ZIP pair, even if a given ZIP code's area consists of
# disjoint domains.

ZIP_lst <- ZIP_perim.matrix[,1]
NZIP <- length(ZIP_lst)
ComBor <- matrix(0,nrow=NZIP,ncol=NZIP)  # common boundaries result matrix initialization
colnames(ComBor) <- ZIP_perim.matrix[1:NZIP,1]
rownames(ComBor) <- ZIP_perim.matrix[1:NZIP,1]

for (ii in 1:(NZIP-1)) {   # iterate over rows
    # ii <- ii+1
    ComBorLength <- c() # initialize vector containing common boundary lengths
    ZIP_ii <- ZIP_lst[ii]  # row ZIP
    ZIP_ii_idx <- which(mapZIP$ZIPCODE==ZIP_ii)      # row ZIP index/indices in ZIP shape-data struct
    
    for (jj in (ii+1):NZIP) {   # iterate over columns
        # jj<-jj+1
        ZIP_jj <- ZIP_lst[jj]    # column ZIP
        ZIP_jj_idx <- which(mapZIP$ZIPCODE==ZIP_jj)  # column ZIP index/indices in ZIP shape-data struct
        borderLength <- 0
        
        for (index_ii in ZIP_ii_idx) { # loop over all possible row ZIP_ii shape data struct indices in case ZIP_ii area is disjoint
            for (index_jj in ZIP_jj_idx) { # same for column ZIP_jj shape data struct indices in case ZIP_jj area is disjoint
                border_idx <- c()
                borderX <- c()
                borderY <- c()
                
                for (xx in 1:length(shp[[index_ii]]$x)) {
                    x0 <- shp[[index_ii]]$x[xx]
                    y0 <- shp[[index_ii]]$y[xx]
                    
                    for (yy in 1:(length(shp[[index_jj]]$x)-1)) {
                        
                        logictest <- ( (x0 - shp[[index_jj]]$x[yy])*(x0 - shp[[index_jj]]$x[yy+1]) <= 0 &
                                           (y0 - shp[[index_jj]]$y[yy])*(y0 - shp[[index_jj]]$y[yy+1]) <= 0   )
                        if ( logictest) {
                            cat("TRUE for",xx,"-th",as.character(mapZIP$ZIPCODE[index_ii]),"-",index_ii,"coords and", yy,"-th",as.character(mapZIP$ZIPCODE[index_jj]),"-",index_jj,"index.\n")
                            # keep xx = ZIP_ii's index for coords of point falling between or on one of two consecutive
                            # points of the boundary belonging to ZIP_jj area boundaries, areas defined by 'index_jj'
                            border_idx <- c(border_idx,xx)
                            borderX <- c(borderX,x0) # keep actual ZIP_ii area's x coords.
                            borderY <- c(borderY,y0) # idem for ZIP_ii area's y coords.
                        }
                    }
                }
                length(border_idx)
                print(border_idx)
                
                if (length(borderX) > 2) {
                    keep_idx <- seq(1,length(borderX),2)  # keep every other index in sequence
                    border_idx <- border_idx[keep_idx]
                    borderX <- borderX[keep_idx]
                    borderY <- borderY[keep_idx]
                    
                    for (bb in 2:length(border_idx)) {
                        #bb <-bb+1 # for testing only
                        if ( abs(border_idx[bb] - border_idx[bb-1]) == 1 ) {
                            # compute accumulated common boundary length for successive kept ZIP_ii boundary point index
                            # use kept border coords to conduct calculation
                            # first and last points in list of boundary points is correctly handled, because first point in list
                            # is conveniently repeated once at the end.
                            borderLength <- borderLength + sqrt( (borderX[bb]-borderX[bb-1])^2 + (borderY[bb]-borderY[bb-1])^2 )
                        }
                    }
                } # else {
                #     borderLength <- 0
                # }
            }               # end block -- loop over index_jj   
        }                   # end block -- loop over index_ii
        
        # compute proportion of ZIP_ii perimeter in common with each potential neighbor ZIP_jj
        ComBor[ii,jj] <- round(100*borderLength/as.numeric(ZIP_perim.matrix[jj,2]),2)
        ComBor[jj,ii] <- round(100*borderLength/as.numeric(ZIP_perim.matrix[ii,2]),2)
    }
}
 diag(ComBor) <- 100

# # check that proportions of common boundaries for any ZIP code area never add up to > 100%
# row_sum <- apply(ComBor,2,sum)-100   #  checks !


# #############################
## save common borders proportions (%) between ZIPs to disk
# #############################

target_file="Data/nyc311_all-zip-common-borders.csv"
# write.table(ComBor, file=target_file,
#             row.names=T, col.names=T,
#             sep=" ",
#             dec=".",
#             append=F)
csvSaveF(ComBor,target_file)



# #############################
## check which neighboring ZIP boundaries coincide in more than apct (%)
# #############################

target_file="Data/nyc311_all-zip-common-borders.csv"
ComBor <- read.table(file=target_file,
                     header=T,
                     sep=",",
                     dec=".")
apct <- 75  # expressed in %

for (ii in 1:(NZIP-1)) {   # iterate over rows, check upper triabgle, avoid diagonal
    for (jj in (ii+1):NZIP) {   # iterate over columns,  check upper triabgle, avoid diagonal
        if ( (ComBor[ii,jj] >= apct) | (ComBor[jj,ii] >= apct) ) {
            if (ComBor[ii,jj] >= ComBor[jj,ii]) {
                cat("ZIP code",ZIP_lst[ii],"(on row", ii,"), to absorb ZIP code",ZIP_lst[jj],"(on col",jj,")\n")
            } else {
                cat("ZIP code",ZIP_lst[jj],"(on row", jj,"), to absorb ZIP code",ZIP_lst[ii],"(on col",ii,")\n")
            }
        }
    }
}
# Results for NYC:
# ZIP code 11101 (on row 103 ) is to absorb ZIP code 11109 (on col 109 ) 
#    ComBor[103,109] yields 84.17%
# ZIP code 11433 (on row 194 ) is to absorb ZIP code 11451 (on col 198 )
#    ComBor[194,198] yields 100%
