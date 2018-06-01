# #############################
## MIRI Project:    Geosociological Analysis of NYC-311 Service Requests
## Author:          Cedric Bhihe, Santi Calvo
## Delivery:        2018.06.26
## Script:          10_apportion-ghost-zip_proc.R
# #############################


rm(list=ls(all=TRUE))
setwd("~/Documents/Work/Academic-research/NYC-complaints/")
options(scipen=6) # R switches to sci notation above 5 digits on plot axes
ccolors=c("red","green","blue","orange","cyan","tan1","darkred","honeydew2","violetred",
          "palegreen3","peachpuff4","lavenderblush3","lightgray","lightsalmon","wheat2")
set.seed(932178)


# #############################
## Libraries
# #############################

library("ggplot2")
# library("zipcode")    # Load all valid us ZIPs  <<<<<<< OUTDATED, 
# library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")      # to read shapefile (translate GPS into ZIP)
# library("fastshp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")


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
## Source parameter file
# #############################
source(file="Scripts/00_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...


# #############################
## Apportion ghost ZIP "00083" observations to surrounding real zips
# #############################

# load/import apportionment rules
source_file <- "nyc311_00083-neighbors-common-border.csv"

apportRule.df <- read.csv(paste0("Data/",source_file),
                   header=T,
                   sep=",",
                   quote="\"",
                   dec=".")

names(apportRule.df)
ghostZIP_idx <- which(apportRule.df$SharedBorderPrct==100)
ghostZIP <- sprintf("%05d",apportRule.df$ZIP[ghostZIP_idx])
(neighborZIP <- as.character(apportRule.df$ZIP[-ghostZIP_idx]))
(apportRule <- as.numeric(as.vector(apportRule.df$SharedBorderPrct[-ghostZIP_idx]))/100)
# sum(apportRule)  # check =1

# load csv data-set to process
source_file <- paste0("Data/",
                      yearNbr,
                      ifelse(nchar(monthNbr)==1,paste0("0",monthNbr),monthNbr),
                      "00_nyc311_proc5.csv")
protoY <- read.csv(source_file,
                   header=T,
                   sep=",",
                   quote="\"",
                   dec=".")
