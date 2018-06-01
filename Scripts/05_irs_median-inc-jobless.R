# #############################
## MIRI Project:    Geosociological Analysis of NYC-311 Service Requests
## Author:          Cedric Bhihe
## Delivery:        2018.06.26
## Script:          05_irs_median-inc-jobless.R
# #############################


rm(list=ls(all=TRUE))

setwd("~/Documents/Work/Academic-research/NYC-complaints/")

set.seed(932178)
options(scipen=6) # R switches to sci notation above 5 digits on plot axes
ccolors=c("red","green","blue","orange","cyan","tan1","darkred","honeydew2","violetred",
          "palegreen3","peachpuff4","lavenderblush3","lightgray","lightsalmon","wheat2")



# #############################
## Libraries
# #############################

library("ggplot2")
library("zipcode")    # Load all valid us ZIPs  <<<<<<< OUTDATED, 
# install.packages('e1071')
# library(e1071)   # SVM for regression and 1D data

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
## ZIP of interest
# #############################

data("zipcode") # load zip codes for NY state, + bogus code 00083
ZIP_lst <- c(unlist(zipcode[zipcode$state=="NY",1]),
             c("00083","10004","10018","10024","10029","10032","10036","10309","10459","10463","10469","10472","11103", "11207",
               "11210", "11221","11229","11235","11368","11379","11385","11421","11435","11694"))
ZIP_lst <- ZIP_lst[which( !strtrim(ZIP_lst,3) %in% c("005","063"))]

# #############################
## Load data
# #############################

source_file1 <- paste0(yearNbr,
                       ifelse(monthNbr<10,paste0("0",monthNbr),monthNbr),
                       "00_nyc311_proc")

protoX <- read.csv(paste0("Data/",
                          source_file1,
                          "03.csv"),
                   header = TRUE,
                   dec = ".",
                   sep=",",
                   check.names=TRUE)
 
ZIP <- unique(unlist(protoX$ZIP))   # vector
ZIP <- ZIP[sprintf("%05d",ZIP) %in% ZIP_lst]    # of class: vector
# class(ZIP); is.vector(ZIP)
cat("\nUnique ZIP codes in data set:",length(ZIP),"\n\n")


# #############################
## Import distr. of income per zip data from file
# #############################

protoY <- read.csv(paste0("Data/",
                          yearNbr,
                          "_zip-income_10-14_raw.csv"),
                   header= TRUE,
                   dec = ".",
                   sep="\t", 
                   check.names=TRUE)

## Build csv file w/ columns: ZIP, bin_lower_bound1,bin_lower_bound2, ...
names(protoY) <- c("ZIP","AGI_bins","Returns")   # AGI = "Adjusted Gross Income"
protoY <- protoY[!is.na(protoY$ZIP),]
protoY[as.character(protoY[,3]) =="**" , 3] <- 0
protoY$Returns <- gsub(",","",protoY$Returns)

incDist <- as.data.frame(matrix(0,nrow=length(unique(protoY$ZIP)),
                                ncol=10,
                                byrow=T)
                         )  # Income Distribution per ZIP

# arbitrary choice for maximum AGI
#binsMax <- 2000000  # upper limit of declared gross adjusted income
binsVec <- c(0,25000,50000,75000,100000,200000)

df_header <- c("ZIP")
for (bb in 1:(length(binsVec)-1)) {
  df_header <- c(df_header,paste0(as.character(binsVec[bb]),"_",as.character(binsVec[bb+1]))) 
  }
names(incDist) <- c(df_header,  # col 1:6
                    paste0(as.character(binsVec[length(binsVec)]),"_"), # col7
                    "nbrReturns",       # col 8
                    "medianInc",    # col 9, 
                    "jl_benef"
                    )

# # Prepare the abcissae for the fitted model
# obs_x <- c()
# for (ii in 2:length(binsVec)) { obs_x <- c(obs_x,round(0.5*(binsVec[ii-1]+binsVec[ii]),0)) }
# #obs_x <- c(obs_x,round(0.5*(binsVec[length(binsVec)]+binsMax),0))

cnt1 <- 1                  # protoY counter
cnt2 <- 1                  # incDist counter
while (!is.na(protoY$ZIP[cnt1])) {
  if (as.character(protoY[cnt1,2]) %in% c("Total","")) {
    incDist[cnt2,1] <- as.character(ifelse(nchar(as.character(protoY[cnt1,1])) < 5,
                                           paste0(formatC(protoY[cnt1,1],width=5,flag="0")),
                                           as.character(protoY[cnt1,1])
                                           )
                                    )
    incDist[cnt2,2] <- as.character(protoY[cnt1+1,3])
    incDist[cnt2,3] <- as.character(protoY[cnt1+2,3])
    incDist[cnt2,4] <- as.character(protoY[cnt1+3,3])
    incDist[cnt2,5] <- as.character(protoY[cnt1+4,3])
    incDist[cnt2,6] <- as.character(protoY[cnt1+5,3])
    incDist[cnt2,7] <- as.character(protoY[cnt1+6,3])
    incDist[cnt2,8] <- as.character(protoY[cnt1,3])
    
    # compute median income 
    pop50p100 <- round(as.numeric(incDist$nbrReturns[cnt2])/2,0)
    for (ii in 2:7) {
      cumulPop <- sum(as.numeric(incDist[cnt2,2:ii]))
      if (pop50p100 <= cumulPop)   break 
    }
    
    if (ii == 7) {
      incDist[cnt2,9] <- NA   # wealthy zip neighborhood; median cannot be evaluated for lack of 
                              # reliable AGI upper bound
    } else {
      ## by rule of proportion
      lowerPopBound <- ifelse(ii == 2,0,sum(as.numeric(incDist[cnt2,2:(ii-1)])))
      incDist[cnt2,9] <- as.character( round(binsVec[ii-1]+
                                               (pop50p100-lowerPopBound)/(cumulPop-lowerPopBound)*
                                               (binsVec[ii]-binsVec[ii-1])),0)
      
      # ## by distribution fitting
      # # prepare ordinates, obs_y
      # obs_y <- as.numeric(incDist[cnt2,2:(length(svm_x)+1)])
      # plot(svm_x,obs_y,type="b")
      # model <- svm (svm_x,obs_y,epsilon=0.1,gamma=145)  # default cost C=1
      # lines(svm_x,predict(model,svm_x),col="cyan")
    }
    
    # ## Gini coefficient
    # incDist[cnt2,11] <- NA  # not implemented for lack of reliable AGI upper bound
    
    cnt2 <- cnt2+1
  }
  cnt1 <- cnt1+1
}

#rm(protoY)


# ###############################
## Enrich data with nbr of tax returns showing unemployment benefits
# ###############################

source_file_2 <- paste0("Data/",
                      yearNbr,
                      "_zip-irs-exempt-unemp.csv")
protoZ <- read.csv(source_file_2,
              header = TRUE, 
              dec = ".",
              sep="\t", 
              check.names=TRUE)

protoZ <- protoZ[ protoZ$STATE == "NY" & protoZ$ZIP != 99999 , c(2,6)]
names(protoZ) <- c("ZIP","jl_benef")
for (zz in incDist$ZIP ){
  incDist$jl_benef[which(incDist$ZIP == zz)] <- protoZ$jl_benef[protoZ$ZIP == as.numeric(zz)]
}
 
# #############################
## Save to disk
# #############################

target_file <- sub("311_proc.*","_irs-by-zip.csv",source_file1)
csvSaveF(incDist,target_file)




