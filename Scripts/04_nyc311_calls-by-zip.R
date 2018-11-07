# #############################
## Project:     Analysis of NYC-311 Service Requests
## Script:      04_nyc311_calls-by-zip.R
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Last edit:   October 2018
# #############################

rm(list=ls(all=TRUE))

# #############################

setwd("~/Documents/Work/Academic-research/NYC311/")

set.seed(932178)
options(scipen=6) # R switches to sci notation above 5 digits on plot axes


# #############################
## Libraries
# #############################

library("ggplot2")
library("zipcode")

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
}    # exit function. Use with caution: not standard, depends on OS's internals


# #############################
## Source parameter file
# #############################

source(file="Scripts/01_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...


# #############################
## Load data
# #############################

source_file <- paste0(yearNbr,
                      ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                      "00_nyc311_proc")

protoY <- read.csv(paste0("Data/",
                          source_file,
                          "03.csv"),
                   header=T,
                   sep=",",
                   quote="\"",
                   dec=".")



# #############################
## Reorder by unique ZIP
# #############################

data("zipcode") # load zip codes for NY state, + bogus code 00083
ZIP_lst <- c(unlist(zipcode[zipcode$state=="NY",1]),
             c("99999","00083","10004","10018","10024","10029","10032","10036","10309","10459","10463","10469","10472","11103", "11207",
               "11210", "11221","11229","11235","11368","11379","11385","11421","11435","11694"))
ZIP_lst <- ZIP_lst[which( !strtrim(ZIP_lst,3) %in% c("005","063"))]

ZIP <- unique(unlist(sprintf("%05d",protoY$ZIP)))   # vector
ZIP <- ZIP[ZIP %in% ZIP_lst]
ZIPcall <- matrix(NA,nrow=length(ZIP),ncol=15)
complaintMode <- unique(as.character(unlist(protoY$Complaint)))
colnames(ZIPcall) <- c("ZIP","Borough",complaintMode)   # vector
ZIPcall[,1] <- sort(ZIP,decreasing=F)
for (zz in 1:length(ZIP)) {
    for (mm in 1:length(complaintMode)) {
        ZIPcall[zz,mm+2] <- length(which(as.character(sprintf("%05d",protoY$ZIP)) == ZIPcall[zz,1] 
                                         & protoY$Complaint == complaintMode[mm]))
    }
}

# #############################
## Add borough label to each ZIP
# #############################

source_file <- paste0("Data/nyc_borough-zip.csv")

boroughZIP <- read.csv(source_file,
                       header=T,
                       sep=",",
                       quote="\"",
                       dec=".")

boroughZIP <- as.data.frame(cbind(ZIP=as.character(sprintf("%05d",boroughZIP$ZIP)),
                                  Borough=as.character(boroughZIP[,2])
                                  ))

boroughZIP <- boroughZIP[which( boroughZIP$ZIP %in% ZIP),]

for (zz in 1:nrow(ZIPcall)) {
    ZIPcall[zz,2] <- ifelse( ZIPcall[zz,1] %in% as.character(unlist(boroughZIP$ZIP)),
                             as.character(boroughZIP[which(boroughZIP[,1]== ZIPcall[zz,1]),2]),
                             NA)
    }

ZIPcall <- ZIPcall[which(ZIPcall[,2] %in% c("Bronx","Brooklyn","Manhattan","Queens","Staten Island","99999")),]


## Below code for cases where borough label is provided but "dirty"
# boroughZIP <- boroughZIP[which( boroughZIP$ZIP %in% ZIP),]
# # ZIPs can be attributed to several boroughs by mistake
# # pick the majority attribution as the statistically correct borough label.
# borough <- c()
# for (zz in 1:length(ZIP)) {
#     borough <- boroughZIP[ which( boroughZIP$ZIP == as.numeric(ZIP[zz]) ),2 ]
#     (boroughFT <- table(borough))  # build frequency table for `borough`
#     boroughName <- names(which(boroughFT==max(boroughFT)))
#     while (! boroughName %in% c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")) {
#         boroughFT <- table(borough,exclude=boroughName)
#         if (length(boroughFT)==0){
#             boroughName=NA
#             break
#         } else {
#             boroughName <- sample(as.vector(names(which(boroughFT==max(boroughFT)))),1)
#         }
#     }
#     length(boroughName)  # check that length is 
#     ZIPcall[zz,2] <- boroughName
# }



# #############################
## Save processed file to disk
# #############################

target_file <- paste0("Data/",
                      yearNbr,
                      ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                      "00_nyc_311call-by-zip.csv")
csvSaveF(ZIPcall,target_file)     # csv to disk

