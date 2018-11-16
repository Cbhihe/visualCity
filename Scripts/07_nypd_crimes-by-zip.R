# #############################
## Project:     Analysis of NYC-311 Service Requests
## Script:      07_nypd_crimes-by-zip.R
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Last edit:       
# #############################

rm(list=ls(all=TRUE))

# #############################

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
library("ggplot2")


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
                       ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                       "00_nyc-crime-map_proc03.csv")

protoY <- read.csv(source_file,
                   header=T,
                   sep=",",
                   quote="\"",
                   dec=".",
                   skip=0)


# #############################
## Reorder by unique ZIP
# #############################

ZIP_lst <- unique(unlist(protoY$ZIP))   # vector

ZIPcrime <- matrix(NA,nrow=length(ZIP_lst),ncol=5)
colnames(ZIPcrime) <- c("ZIP","Borough","Violation","Misdemeanor","Felony")
ZIPcrime[,1] <- sort(ZIP_lst,decreasing=F)
for ( zz in 1:nrow(ZIPcrime) ) {
  ZIPcrime[zz,3] <- length( which( protoY$ZIP == ZIPcrime[zz,1] 
                                   & protoY$offCat == "VIOLATION" ) )
  ZIPcrime[zz,4] <- length( which( protoY$ZIP == ZIPcrime[zz,1] 
                                   & protoY$offCat == "MISDEMEANOR" ) )
  ZIPcrime[zz,5] <- length( which( protoY$ZIP == ZIPcrime[zz,1] 
                                   & protoY$offCat == "FELONY" ) )
}


# #############################
## Add borough label to each ZIP
# #############################

source_file <- paste0("Data/nyc_borough-zip.csv")

boroughZIP <- read.csv(source_file,
                       header=T,
                       sep=",",
                       quote="\"",
                       dec=".",
                       skip=0)

boroughZIP <- as.data.frame(cbind(ZIP=as.character(sprintf("%05d",boroughZIP$ZIP)),
                                  Borough=as.character(boroughZIP[,2])
))

boroughZIP <- boroughZIP[which( boroughZIP$ZIP %in% ZIPcrime$ZIP),]

for (zz in 1:nrow(ZIPcrime)) {
    ZIPcrime[zz,2] <- ifelse( ZIPcrime[zz,1] %in% as.character(unlist(boroughZIP$ZIP)),
                             as.character(boroughZIP[which(boroughZIP[,1]== ZIPcrime[zz,1]),2]),
                             NA)
}

ZIPcrime <- ZIPcrime[which(ZIPcrime[,2] %in% c("Bronx","Brooklyn","Manhattan","Queens","Staten Island","99999")),]


# #############################
## Save processed raw csv file as `nypd_offense_modalities.csv``
# #############################

target_file <- paste0("Data/",
                          yearNbr,
                          ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                          "00_nyc_crime-by-zip.csv")
csvSaveF(ZIPcrime,target_file)     # csv to disk

