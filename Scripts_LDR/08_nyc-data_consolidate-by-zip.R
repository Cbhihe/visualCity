# #############################
## MIRI:        Analysis of NYC-311 Service Requests Call
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Script:      08_nyc-data_consolidate-by-zip.R
## Last edit:   
# #############################

rm(list=ls(all=TRUE))

# #############################

setwd("~/Documents/Work/Academic-research/visualCity/")

set.seed(932178)
options(scipen=6) # R switches to sci notation above 5 digits on plot axes


# #############################
## Source input parameters
# #############################

source(file="Scripts/01_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...


# #############################
## Libraries
# #############################

setRepositories(ind = c(1:6,8))
#library("ggplot2")


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

(source_file = as.vector(grep(paste0("^",
                                     yearNbr,
                                     ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                                     "00_nyc_.*-by-zip\\.csv$"),
                              list.files(path="Data/",full.names=FALSE),
                              ignore.case = F,
                              perl = TRUE,
                              fixed = FALSE,
                              inv=FALSE,
                              value=TRUE)))

protoY <- paste0("protoY",seq(1:length(source_file)))

for (ii in 1:length(protoY)) {
    assign(paste0("protoY",ii), 
           read.csv(paste0("Data/",source_file[ii]),
                    header=T,
                    sep=",",
                    quote="\"",
                    dec=".",
                    skip=0))
}

# #############################
## Reorder + consolidate by ZIP
# #############################

# complete protoY1
if ( length(which(! protoY2$ZIP %in% protoY1$ZIP) ) !=0) {
    missingCallZIP <- protoY2$ZIP[which(! protoY2$ZIP %in% protoY1$ZIP)]
    missingCallBorough <- as.character(protoY2[which(! protoY2$ZIP %in% protoY1$ZIP),2])
    tmp=cbind(ZIP=missingCallZIP,
              Borough=missingCallBorough,
              HousCond=rep(0,length(missingCallZIP)),
              Sani=rep(0,length(missingCallZIP)),
              NoiseResid=rep(0,length(missingCallZIP)),
              Traffic=rep(0,length(missingCallZIP)),
              NoiseConst=rep(0,length(missingCallZIP)),
              NoiseTraf=rep(0,length(missingCallZIP)),
              IAO=rep(0,length(missingCallZIP)),
              NoiseBiz=rep(0,length(missingCallZIP)),
              EnvProt=rep(0,length(missingCallZIP)),
              ConsumProt=rep(0,length(missingCallZIP)),
              UrbInf=rep(0,length(missingCallZIP)),
              SocServ=rep(0,length(missingCallZIP)),
              WaterSyst=rep(0,length(missingCallZIP))
              )
    protoY1 <- rbind(protoY1,tmp)
}; rm(tmp)
protoY1 <- protoY1[order(as.numeric(protoY1$ZIP)),]

# complete protoY2
if ( length(which(! protoY1$ZIP %in% protoY2$ZIP) ) !=0) {
    missingCallZIP <- protoY1$ZIP[which(! protoY1$ZIP %in% protoY2$ZIP)]
    missingCallBorough <- as.character(protoY1[which(! protoY1$ZIP %in% protoY2$ZIP),2])
    tmp=cbind(ZIP=missingCallZIP,
              Borough=missingCallBorough,
              Violation=rep(0,length(missingCallZIP)),
              Misdemeanor=rep(0,length(missingCallZIP)),
              Felony=rep(0,length(missingCallZIP))
             )
    protoY2 <- rbind(protoY2,tmp)
}; rm(tmp)
protoY2 <- protoY2[order(as.numeric(protoY2$ZIP)),]

# complete protoY3
protoY3 <- protoY3[protoY3$ZIP %in% protoY1$ZIP,c(1,9,10)]
missingIncZIP <-  protoY1[! protoY1$ZIP %in% protoY3$ZIP,1]
tmp <- cbind(ZIP=missingIncZIP,
             medianInc=rep(NA,length(missingIncZIP)),
             jl_benef=rep(NA,length(missingIncZIP))
             )
protoY3 <- rbind(protoY3,tmp); rm(tmp)
protoY3 <- protoY3[order(match(protoY3$ZIP,protoY1$ZIP)),]

protoY <- as.data.frame(cbind(protoY1,
                              protoY2[order(match(protoY2$ZIP,protoY1$ZIP)),c(3:5)],
                              protoY3[order(match(protoY3$ZIP,protoY1$ZIP)),c(2,3)]))
colnames(protoY) <- c(colnames(protoY1),colnames(protoY2[,c(3:5)]),"medianInc","jlBenef")

# #############################
## Save complete file as `...00-nyc_whole-data-set.csv``
# #############################

target_file <- paste0("Data/",
                          yearNbr,
                          ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                          "00_nyc_whole-data-set.csv")
csvSaveF(protoY,target_file)     # csv to disk
