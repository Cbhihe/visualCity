# #############################
## Project:     Analysis of NYC-311 Service Request Calls
## Script:      13b_mca-time-evolution_common-basis.R
## Author:      Cedric Bhihe
## Created:     November 2018
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
library(nnet)           # required to use 'class.ind()'
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
                            # caution: not standard, depends on OS's internals

# #############################
## Load data
# #############################

# Parse periods of interest
yearNbr <- c()
monthNbr <- c()
#monthName <- c()
for (pp in 1:length(periodsOfInterest)) {
    periodsOfInterest[pp] <- sub("^ +","",
                                periodsOfInterest[pp],
                                perl=TRUE,
                                fixed=FALSE)  # suppress leading spaces
    periodsOfInterest[pp] <- sub(" +$","",
                                periodsOfInterest[pp],
                                perl=TRUE,
                                fixed=FALSE)  # suppress eol spaces
    posFS <- as.numeric(regexpr(pattern =' ', periodsOfInterest[pp], ignore.case = T)) # position of field separator
    
    if ( (is.na(posFS) | posFS == -1) & nchar(as.character(periodsOfInterest[pp])) == 4 ) {
        # Case: No space detected and one entire year provided; complete code       <<<<<<<<<<  TODO
        strStart <- regexpr("^[0123456789]{4}$",periodsOfInterest[pp],perl=TRUE,fixed=FALSE)
        if (strStart == 1) {
            localYearNbr <- substr(periodsOfInterest[pp],strStart, strStart+3)
            yearNbr <- c(yearNbr,as.numeric(localYearNbr))
            cat("\n\n------------\nWarning: You've selected the whole year",localYearNbr,".")
            cat("\nComplete code to specify sub-periods to compare within that year.\nAbort.\n-----------------\n")
            exit()
            }              
    } else if (posFS > 1 & posFS < 4 
               & ! suppressWarnings(is.na(as.numeric(substr(periodsOfInterest[pp],1, posFS-1)))) ) {
        # Case: Space detected at position 2 or 3. The string before is a numeral.
        localMonthNbr <- substr(periodsOfInterest[pp],1, posFS-1)
        if (as.numeric(localMonthNbr) >= 1 & as.numeric(localMonthNbr) <= 12) {
            # first field string is a numeral between 1 and 12 (incl.)
            strRmdr <- sub("^ +","",
                           substr(periodsOfInterest[pp],posFS+1,nchar(periodsOfInterest[pp])),
                           perl=TRUE,
                           fixed=FALSE)  # suppress leading spaces in string remainder
            if ( nchar(strRmdr) != 4  | ! is.numeric(strRmdr) ) {
                cat("\n\n------------\nWarning: You've specified a period of interest whose year (",strRmdr,") is not recognized.")
                cat("\n Specify periods of interest in \"Scripts/01_nyc311_input-parameters.R\" following")
                cat("\nformat recommendations.\nAbort.\n---------------\n")
                exit()
            } else {
                monthNbr <- c(monthNbr,localMonthNbr)
                yearNbr <- c(yearNbr,as.numeric(strRmdr))
                #monthName <- c(monthName,month.name[monthNbr])
            }
        } else {
            cat("\n\n------------\nWarning: You've specified a period of interest whose month (",substr(periodsOfInterest[pp],1, posFS-1),") is not recognized.")
            cat("\n Specify periods of interest in \"Scripts/01_nyc311_input-parameters.R\" following")
            cat("\nformat recommendations.\nAbort.\n---------------\n")
            exit()
        }
    } else if (posFS > 1 & posFS < 4 
               & suppressWarnings(is.na(as.numeric(substr(periodsOfInterest[pp],1, posFS-1)))) ) {
        # Case: Space detected at position 2 or 3. The string before is of type character (at least partially)
        #       Could refine test by 1/ detecting any digit, 2/ testing for character type   <<<<<<    TODO
        localMonthNbr <- match(substr(tolower(substr(periodsOfInterest[pp],1, posFS-1)),1,3),tolower(month.abb))
        if (as.numeric(localMonthNbr) >= 1 & as.numeric(localMonthNbr) <= 12) {
            strRmdr <- sub("^ +","",
                           substr(periodsOfInterest[pp],posFS+1,nchar(periodsOfInterest[pp])),
                           perl=TRUE,
                           fixed=FALSE)  # suppress leading spaces in string remainder
            if ( nchar(strRmdr) != 4  | suppressWarnings(is.na(as.numeric(strRmdr))) ) {
                cat("\n\n------------\nWarning: You've specified a period of interest whose year (",strRmdr,") is not recognized.")
                cat("\n Specify periods of interest in \"Scripts/01_nyc311_input-parameters.R\" following")
                cat("\nformat recommendations.\nAbort.\n---------------\n")
                exit()
            } else {
                monthNbr <- c(monthNbr,localMonthNbr)
                yearNbr <- c(yearNbr,as.numeric(strRmdr))
                #monthName <- c(monthName,month.name[monthNbr])
            }
        } else {
            cat("\n\n------------\nWarning: You've specified a period of interest whose month (",substr(periodsOfInterest[pp],1, posFS-1),") is not recognized.")
            cat("\n Specify periods of interest in \"Scripts/01_nyc311_input-parameters.R\" following")
            cat("\nformat recommendations.\nAbort.\n---------------\n")
            exit()
        }
    } else if (posFS >= 4 ) {
        # Case: Space detected at position 4 or greater.
        localMonthNbr <- match(substr(tolower(substr(periodsOfInterest[pp],1, posFS-1)),1,3),tolower(month.abb))
        if ( is.na(localMonthNbr) | localMonthNbr < 1 | localMonthNbr > 12 ) {
            cat("\n\n------------\nWarning: The period's month could not be correctly parsed. Specify periods")
            cat("\nof interest in \"Scripts/01_nyc311_input-parameters.R\" following format recommendations.\nAbort.\n---------------\n")
            exit()
        } else {
            strRmdr <- sub("^ +","",
                           substr(periodsOfInterest[pp],posFS+1,nchar(periodsOfInterest[pp])),
                           perl=TRUE,
                           fixed=FALSE)  # suppress leading spaces in string remainder
            if ( nchar(strRmdr) != 4  | suppressWarnings(is.na(as.numeric(strRmdr))) ) {
                # test fro strRmdr length and numeric or character type
                cat("\n\n------------\nWarning: You've specified a period of interest whose year (",strRmdr,") is not recognized.")
                cat("\nSpecify periods of interest in \"Scripts/01_nyc311_input-parameters.R\" following")
                cat("\nformat recommendations.\nAbort.\n---------------\n")
                exit()
            } else {
                monthNbr <- c(monthNbr,localMonthNbr)
                yearNbr <- c(yearNbr,as.numeric(strRmdr))
                #monthName <- c(monthName,month.name[monthNbr])
            }
        }
    } else {
        cat("\n\n------------\nWarning: The period of interest you specified is not correctly formatted or parsed.")
        cat("\nPlease check formats used in \"Scripts/01_nyc311_input-parameters.R\".\nAbort.\n---------------\n")
        exit()
    }
}

#monthNbr; yearNbr; periodOfReference  # Check parsing results

# Read / load data into list of data.frames, W
source_file <- c()
W <- list()
Boroughs <- list()

for (ff in 1:length(periodsOfInterest)) {
    
    monthNumStr <- ifelse(monthNbr[ff]<10,paste0("0",as.character(monthNbr[ff])),as.character(monthNbr[ff]))
    dfTag <- paste0("_",yearNbr[ff],monthNumStr)
    
    source_file = c(source_file,as.vector(paste0("Data/",yearNbr[ff],monthNumStr,"00_nyc_simple-whole-data-set_te-binned.csv")))
    
    # read csv data
    W[[paste0("W",dfTag)]] <- read.csv(paste0(source_file[ff]),header=T,sep=",",quote="\"",dec=".",skip=0)
    
    # 'refFlag' points to 'periodOfReference' data set
    if ( paste(month.name[monthNbr[ff]],yearNbr[ff]) == periodOfReference ) {refFlag <- ff}
    
    # assign rownames
    row.names(W[[paste0("W",dfTag)]]) <- paste0(W[[paste0("W",dfTag)]][,1],"_",yearNbr[ff],monthNumStr)
    
    # rm first column with ZIP codes
    W[[paste0("W",dfTag)]] <- W[[paste0("W",dfTag)]][,-1]
}

# assign rownames + suppress 1st column of each data.frame
# W <- lapply( W, function(x) { 
#     row.names(x) <- x[,1]
#     x <- x[,-1] } )


# ##############################
## MCA: Multiple Component Analysis
# ##############################

# ##############################
## Indicator matrix   <<<<<<<   NOT USEFUL IN THIS CONTEXT
{
    #  To encode variable modalities according to a one-hot encoding scheme:
    #+ 1st compute the total number of modalities for all _active_ categorical variables
    #+ 2nd form all modality-indicator submatrices and cbind() them in one large final indicator matrix
    
    for (ff in 1:length(periodsOfInterest)) {
        J <-0  # initialize total number of modalities (after binning each active categorical var)
        colHeader <- c()
        Nmod <- length(which(lapply(W[[ff]][,-1],is.factor)==TRUE))
        for (nn in 1:Nmod) {
            colHeader <- c( colHeader, paste0(colnames(W[[ff]])[nn+1],"_",levels(W[[ff]][,nn+1])) )
            J <- J + length(levels(W[[ff]][,nn+1]))
            Windic <- data.frame(matrix(ncol = 0, nrow = nrow(W[[ff]])))
            for (ii in 1:Nmod) { 
                subWindic <- class.ind(W[[ff]][,ii+1])
                Windic <- cbind(Windic,subWindic) 
            }
            #apply(Windic,1,sum)  # sum of rows equals nbr of active categorical variables: ok
            rownames(Windic) <- paste0(rownames(W[[ff]]),"_",yearNbr[ff])
            colHeader <- lapply(colHeader, function(x) sub("HousCond_HousCond_","HousCond_",x))
            colnames(Windic) <- colHeader
        }
        assign(paste0("Windic_",yearNbr[ff]),Windic)
    }
    rm(Windic, subWindic,colHeader)
}


# ##############################
## Match column order of each period to that of the reference period 
#  (required for the rbind() consolidation operation below) 
W[["W_201404"]] <- W[["W_201404"]][,order(match(colnames(W[["W_201404"]]),colnames(W[["W_201004"]])))]
W[["W_201804"]] <- W[["W_201804"]][,order(match(colnames(W[["W_201804"]]),colnames(W[["W_201004"]])))]
# consolidate data from all period by row, following column order 
Wtot <- rbind(W[["W_201004"]],W[["W_201404"]],W[["W_201804"]])
rm(W)

# backup composite matrix will every period appended by row
csvSaveF(cbind(ZIP=rownames(Wtot),Wtot),"Data/20100400--20180400_te-mca-data.csv")


# ##############################
## Identify special ZIP codes' indexes
#refDateTag <- paste0("_",sub("-","",substr(as.Date(paste("01",periodOfReference),format="%d %b %Y"),1,7)))
supObservations <- c()
# For 2010
zip99999_Widx <- which(rownames(Wtot) == "99999_201004")  #  bogus ZIP code
zip11430_Widx <- which(rownames(Wtot) == "11430_201004")  #  JFK, Queens
#zip10281_Widx <- which(rownames(Wtot) == "10281_201004")  #  Battery Park City, Manhattan
supObservations <- c(supObservations,zip99999_Widx,zip11430_Widx)

# For 2014
zip99999_Widx <- which(rownames(Wtot) == "99999_201404")  #  bogus ZIP code
zip11430_Widx <- which(rownames(Wtot) == "11430_201404")  #  JFK, Queens
#zip10463_Widx <- which(rownames(Wtot) == "10463_201404")  #  Riverdale, The Bronx
supObservations <- c(supObservations,zip99999_Widx,zip11430_Widx)

# For 2018
zip99999_Widx <- which(rownames(Wtot) == "99999_201804")  #  bogus ZIP code
zip11430_Widx <- which(rownames(Wtot) == "11430_201804")  #  JFK, Queens
#zip11371_Widx <- which(rownames(Wtot) == "11371_201804")  #  La Guardia, Queens
supObservations <- c(supObservations,zip99999_Widx,zip11430_Widx)

# ##############################
## Define supplementary individuals as those NOT being part of the reference period
#supObs_idx <- grep(paste0("_",sub("-","",substr(as.Date(paste("01",periodOfReference),format="%d %b %Y"),1,7))),
#                    rownames(Wtot),
#                    value=F,
#                    fixed=T,
#                    invert=T)   #  <<<<<  INVERTED SELECTION
#supObservations <- c(supObservations,supObs_idx)
Boroughs <- Wtot[-supObservations,1]   # only remove sup.ind and "99999" for reference period (i.e. active rows)
Boroughs <- factor(Boroughs)  # clean up factor levels

## Perform MCA on all data from all periods of interest, i.e. using all data points
#+ to construct the orthonormal basis which maximizes variance in optimal directions
mcaNYC311 <- MCA(Wtot[,-1],
                 ncp=17,
                 ind.sup=supObservations,
                 quanti.sup=c(17,18),
                 excl=NULL,
                 graph = TRUE,
                 level.ventil = 0,
                 axes = c(1,2),
                 row.w = NULL,
                 method="Indicator",
                 na.method="NA",
                 tab.disj=NULL
                 )

mcaCoords <- mcaNYC311$ind$coord
# summary(mcaNYC311,nbelements=12)
# dimdesc(mcaNYC311)
#dim(mcaCoords)

## Observation s' PC1-2 coordinates' temporal sequences in the order 2010 -> 2014 -> 2018
# collate all unique ZIP codes and fill data-frame with values ZIP code by ZIP code
loci <- unique(data.frame(ZIP = substr(rownames(Wtot),1,5),
                          Borough = Wtot[,1]))
# order data frame according to ascending values of ZIP codes
ZIP_lst <- loci[order(loci$ZIP,decreasing=F),]
loci <- loci[which(!ZIP_lst$ZIP %in% unique(substr(rownames(Wtot[supObservations,]),1,5))),]
# initialize / populate data-frame with collated PC1-2 data
OCTS <- as.data.frame(matrix("",ncol=(2+ncol(mcaCoords)*length(periodsOfInterest)),nrow=length(loci$ZIP)))

colHeaders <- c()

for (ff in 1:length(periodsOfInterest)) {
    monthNumStr <- ifelse(monthNbr[ff]<10,paste0("0",as.character(monthNbr[ff])),as.character(monthNbr[ff]))
    colSuffix <- paste0("_",yearNbr[ff],monthNumStr)
    colHeaders <- c(colHeaders,paste0("Dim",seq(1:ncol(mcaCoords)),colSuffix))
}
colnames(OCTS) <- c("ZIP", "Borough",colHeaders)
OCTS$ZIP <- loci$ZIP
OCTS$Borough <- loci$Borough

for (ff in 1:length(periodsOfInterest)) {
    #col <- list()
    for (colNbr in 1:ncol(mcaCoords)) {
        assign(paste0("col",colNbr), c())
    }
    monthNumStr <- ifelse(monthNbr[ff]<10,paste0("0",as.character(monthNbr[ff])),as.character(monthNbr[ff]))
    periodTag <- paste0("_",yearNbr[ff],monthNumStr)
    for (zip in as.character(OCTS$ZIP)) {
        rowNbr <- which(rownames(mcaCoords) == paste0(zip,periodTag) )
        if (length(rowNbr) != 0L) {
            # case: zip code included as observation in current time period 
            col1 <- c(col1,mcaCoords[rowNbr,1])
            col2 <- c(col2,mcaCoords[rowNbr,2])
            col3 <- c(col3,mcaCoords[rowNbr,3])
            col4 <- c(col4,mcaCoords[rowNbr,4])
            col5 <- c(col5,mcaCoords[rowNbr,5])
            col6 <- c(col6,mcaCoords[rowNbr,6])
            col7 <- c(col7,mcaCoords[rowNbr,7])
            col8 <- c(col8,mcaCoords[rowNbr,8])
            col9 <- c(col9,mcaCoords[rowNbr,9])
            col10 <- c(col10,mcaCoords[rowNbr,10])
            col11 <- c(col11,mcaCoords[rowNbr,11])
            col12 <- c(col12,mcaCoords[rowNbr,12])
            col13 <- c(col13,mcaCoords[rowNbr,13])
            col14 <- c(col14,mcaCoords[rowNbr,14])
            col15 <- c(col15,mcaCoords[rowNbr,15])
            col16 <- c(col16,mcaCoords[rowNbr,16])
            col17 <- c(col17,mcaCoords[rowNbr,17])
            # for (colNbr in 1:ncol(mcaCoords)) {
            #     assign(paste0("col",colNbr), c(paste0("col",colNbr),mcaCoords[rowNbr,colNbr]))
            # }
        } else {
            # case: zip code NOT included as observation in current time period 
            col1 <- c(col1,NA)
            col2 <- c(col2,NA)
            col3 <- c(col3,NA)
            col4 <- c(col4,NA)
            col5 <- c(col5,NA)
            col6 <- c(col6,NA)
            col7 <- c(col7,NA)
            col8 <- c(col8,NA)
            col9 <- c(col9,NA)
            col10 <- c(col10,NA)
            col11 <- c(col11,NA)
            col12 <- c(col12,NA)
            col13 <- c(col13,NA)
            col14 <- c(col14,NA)
            col15 <- c(col15,NA)
            col16 <- c(col16,NA)
            col17 <- c(col17,NA)
            # for (colNbr in 1:ncol(mcaCoords)) {
            #     assign(paste0("col",colNbr), c(paste0("col",colNbr),NA))
            # }
        }
    }
    for (colNbr in 1:ncol(mcaCoords)) {
        #assign(col[[paste0("col",colNbr)]], paste0("col",colNbr))
        #assign(OCTS[,(2+colNbr+ncol(mcaCoords)*(ff-1))], paste0("col",colNbr))
        #assign(paste0("OCTS$Dim",colNbr,periodTag), paste0("col",colNbr))
        OCTS[,(2+1+ncol(mcaCoords)*(ff-1))] <- col1
        OCTS[,(2+2+ncol(mcaCoords)*(ff-1))] <- col2
        OCTS[,(2+3+ncol(mcaCoords)*(ff-1))] <- col3
        OCTS[,(2+4+ncol(mcaCoords)*(ff-1))] <- col4
        OCTS[,(2+5+ncol(mcaCoords)*(ff-1))] <- col5
        OCTS[,(2+6+ncol(mcaCoords)*(ff-1))] <- col6
        OCTS[,(2+7+ncol(mcaCoords)*(ff-1))] <- col7
        OCTS[,(2+8+ncol(mcaCoords)*(ff-1))] <- col8
        OCTS[,(2+9+ncol(mcaCoords)*(ff-1))] <- col9
        OCTS[,(2+10+ncol(mcaCoords)*(ff-1))] <- col10
        OCTS[,(2+11+ncol(mcaCoords)*(ff-1))] <- col11
        OCTS[,(2+12+ncol(mcaCoords)*(ff-1))] <- col12
        OCTS[,(2+13+ncol(mcaCoords)*(ff-1))] <- col13
        OCTS[,(2+14+ncol(mcaCoords)*(ff-1))] <- col14
        OCTS[,(2+15+ncol(mcaCoords)*(ff-1))] <- col15
        OCTS[,(2+16+ncol(mcaCoords)*(ff-1))] <- col16
        OCTS[,(2+17+ncol(mcaCoords)*(ff-1))] <- col17
    }
}


## OCTS's coordinates' missings
# Impute missing values or suppress lines when only one coordinate pair is present on row.
colSeq <- seq(3,2+ncol(mcaCoords)+ncol(mcaCoords)*(length(periodsOfInterest)-1),ncol(mcaCoords)) # sequence of colums to test
abs_idx <- which(apply(OCTS[,colSeq],1,is.na))  # row wise cumulative cell index

# Suppress almost empty rows 
# ('almost empty' means only one set of coords is present on row)
rowsToSuppress <- c()
for (index in abs_idx) {
    currentRow <- floor(index/length(periodsOfInterest)) + ifelse(index %% length(periodsOfInterest) != 0,1,0)
    positionInRow <- ifelse(index %% length(periodsOfInterest) == 0,
                            length(periodsOfInterest),
                            index %% length(periodsOfInterest))
    currentRowSum <- sum(OCTS[currentRow,colSeq],na.rm=T)
    if (currentRowSum %in% as.numeric(OCTS[currentRow,colSeq])) {
        # case: current row sum = sum of one of the row components means that component is the only non zero one.
        rowsToSuppress <- c(rowsToSuppress,currentRow)
    } else {
        if (positionInRow > 1) {
            closestNonZero <- max(which(OCTS[currentRow,seq(min(colSeq),colSeq[positionInRow-1],2)] != 0))
            closestNonZero <- max(which(OCTS[currentRow,seq(min(colSeq),colSeq[positionInRow-1],ncol(mcaCoords))] != 0))
        } else {
            closestNonZero <- 1+min(which(OCTS[currentRow,seq(colSeq[positionInRow+1],max(colSeq),ncol(mcaCoords))] != 0
                                        & !is.na(OCTS[currentRow,seq(colSeq[positionInRow+1],max(colSeq),ncol(mcaCoords))])))
        }
        OCTS[currentRow,colSeq[positionInRow]] <- OCTS[currentRow,colSeq[closestNonZero]]
        OCTS[currentRow,colSeq[positionInRow]+1] <- OCTS[currentRow,colSeq[closestNonZero]+1]
        OCTS[currentRow,colSeq[positionInRow]+2] <- OCTS[currentRow,colSeq[closestNonZero]+2]
        OCTS[currentRow,colSeq[positionInRow]+3] <- OCTS[currentRow,colSeq[closestNonZero]+3]
        OCTS[currentRow,colSeq[positionInRow]+4] <- OCTS[currentRow,colSeq[closestNonZero]+4]
        OCTS[currentRow,colSeq[positionInRow]+5] <- OCTS[currentRow,colSeq[closestNonZero]+5]
        OCTS[currentRow,colSeq[positionInRow]+6] <- OCTS[currentRow,colSeq[closestNonZero]+6]
        OCTS[currentRow,colSeq[positionInRow]+7] <- OCTS[currentRow,colSeq[closestNonZero]+7]
        OCTS[currentRow,colSeq[positionInRow]+8] <- OCTS[currentRow,colSeq[closestNonZero]+8]
        OCTS[currentRow,colSeq[positionInRow]+9] <- OCTS[currentRow,colSeq[closestNonZero]+9]
        OCTS[currentRow,colSeq[positionInRow]+10] <- OCTS[currentRow,colSeq[closestNonZero]+10]
        OCTS[currentRow,colSeq[positionInRow]+11] <- OCTS[currentRow,colSeq[closestNonZero]+11]
        OCTS[currentRow,colSeq[positionInRow]+12] <- OCTS[currentRow,colSeq[closestNonZero]+12]
        OCTS[currentRow,colSeq[positionInRow]+13] <- OCTS[currentRow,colSeq[closestNonZero]+13]
        OCTS[currentRow,colSeq[positionInRow]+14] <- OCTS[currentRow,colSeq[closestNonZero]+14]
        OCTS[currentRow,colSeq[positionInRow]+15] <- OCTS[currentRow,colSeq[closestNonZero]+15]
        OCTS[currentRow,colSeq[positionInRow]+16] <- OCTS[currentRow,colSeq[closestNonZero]+16]
    }
}
OCTS <- OCTS[-unique(rowsToSuppress),]  # suppress rows

# ##############################
## MCA plots
{
    # > levels(Boroughs)
    # "Bronx"         "Brooklyn"      "Manhattan"     "Queens"        "Staten Island"
    selectBorough <- 5
    
    plot.MCA(mcaNYC311,
             #mcaNYC311$ind$coord[,1],mcaNYC311$ind$coord[,2],
             choix="ind",
             autoLab="auto",
             cex=0.9,
             #col.ind = ccolors[as.numeric(factor(Boroughs[-supObservations]))], 
             col.ind = ccolors[as.numeric(Boroughs)], 
             col.var = c(rep("lightsalmon3",52),rep("purple",12)),
             col.ind.sup = "red", col.quanti.sup = "orchid3",
             #label = c("var","ind.sup","quanti.sup"),
             label = c("var"),
             title = paste0("MCA - Dynamic biplot for NYC 311 + NYPD 911 data",
                            '\n(Periods: ',periodOfReference,' -> 2014 -> 2018)'))
    legend("bottomright",
           legend=levels(Boroughs),
           cex=0.8,
           pt.cex=1.3,
           pch=16,
           col=ccolors[1:length(levels(Boroughs))])
    
    # April 2010 -> April 2014
    arrows(OCTS_pc12[which(OCTS_pc12$Borough == levels(Boroughs)[selectBorough]),3], # x0
           OCTS_pc12[which(OCTS_pc12$Borough == levels(Boroughs)[selectBorough]),4], # y0
           OCTS_pc12[which(OCTS_pc12$Borough == levels(Boroughs)[selectBorough]),5], # x1
           OCTS_pc12[which(OCTS_pc12$Borough == levels(Boroughs)[selectBorough]),6], # y1
           length = 0.12, # arrow head length in inches (0.08"~ 2mm)
           angle = 20,    #  angle between each arrow head line and arrow shaft
           code=2,        # arrow head is at destination point
           col= ccolors[selectBorough],        # color-coded according to borough 
           lty=1,         # 1=solid, 2=dashed, 3=dotted
           lwd=1)       # lwd may default to one on certain devices 
    
    # April 2014 -> April 2018
    arrows(OCTS_pc12[which(OCTS_pc12$Borough == levels(Boroughs)[selectBorough]),5], # x0
           OCTS_pc12[which(OCTS_pc12$Borough == levels(Boroughs)[selectBorough]),6], # y0
           OCTS_pc12[which(OCTS_pc12$Borough == levels(Boroughs)[selectBorough]),7], # x1
           OCTS_pc12[which(OCTS_pc12$Borough == levels(Boroughs)[selectBorough]),8], # y1
           length = 0.12, # arrow head length in inches (0.08"~ 2mm)
           angle = 20,    #  angle between each arrow head line and arrow shaft
           code=2,        # arrow head is at destination point
           col= ccolors[selectBorough],        # color-coded according to borough 
           lty=1,         # 1=solid, 2=dashed, 3=dotted
           lwd=1)       # lwd may default to one on certain devices 
    
    
    mcaPlot2 <- fviz_mca_biplot(mcaNYC311, 
                                axes = c(1, 2),
                                geom.ind = c("point","text"), 
                                geom.var = c("point","text"),
                                repel = TRUE, 
                                pointsize = 1.3,
                                label = "var",
                                invisible = "none", 
                                #habillage = factor(Boroughs[-supObservations]), 
                                habillage = Boroughs, 
                                addEllipses = FALSE,
                                palette = ccolors[1:length(levels(Boroughs))],
                                arrows = c(FALSE, FALSE),
                                map = "asymmetric",
                                #col.var = c(rep("lightsalmon3",52),rep("purple",12)),
                                title = paste0("MCA - Dynamic biplot for NYC 311 + NYPD 911 data",
                                               '\n(Periods: April 2010 -> 2014 -> 2018)'))
    mcaPlot2 
    
    mcaPlot3a <- plot.MCA(mcaNYC311,
                          #mcaNYC311$ind$coord[,1],mcaNYC311$ind$coord[,2],
                          title=paste0("MCA - Variable projection in PC1-2 for NYC 311 + NYPD 911 data",
                                       "\n(Periods: April 2010 -> 2014 -> 2018)"),
                          choix="var",
                          autoLab="yes",
                          cex=0.9,
                          col.ind = ccolors[as.numeric(Boroughs)], 
                          col.var = c(rep("lightsalmon3",13),rep("purple",3), c("blue","blue")),
                          col.ind.sup = "red", col.quanti.sup = "orchid3",
                          label = c("var","ind.sup","quanti.sup"))
    mcaPlot3a + abline(a=c(0,0),b=0.65,lty=2,lwd=2,col="gray")
    
    mcaPlot3b <- plot.MCA(mcaNYC311,
                          #mcaNYC311$ind$coord[,1],mcaNYC311$ind$coord[,2],
                          title=paste0("MCA - Dynamic bi-plot projection in PC1-2 for NYC 311 + NYPD 911 data",
                                       "\n(Periods: April 2010 -> 2014 -> 2018)"),
                          choix="ind",
                          autoLab="auto",
                          cex=0.9,
                          col.ind = ccolors[as.numeric(Boroughs)], 
                          col.var = c(rep("lightsalmon3",52),rep("purple",12), c("blue","blue")),
                          col.ind.sup = "red", col.quanti.sup = "orchid3",
                          label = c("var"),
                          new.plot=T)
    mcaPlot3b  + legend("bottomright",
                        legend=levels(Boroughs),
                        cex=0.8,
                        pt.cex=1.3,
                        pch=16,
                        col=ccolors[1:length(levels(Boroughs))])
    
    mcaPlot4 <- fviz_mca_var(mcaNYC311,
                             title=paste0("a) Variables' projection in PC1-2 (all)",
                                          "\nMCA on NYC 311 + NYPD 911 data",
                                          "\n(April 2010 -> 2014 -> 2018)"),
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
                             #jitter = list(what = "label", width = NULL, height = NULL)   # deprecated, use "repel=T" instead
                             repel=TRUE,
                             legend=NULL)
    #mcaPlot4 + points(mcaNYC311$var$coord[c(53:64),1],mcaNYC311$var$coord[c(53:64),2],pch=17,pointsize=2,col="red")         
    mcaPlot4
    
    mcaPlot5 <- fviz_mca_var(mcaNYC311,
                             title=paste0("b) Variables' projection in PC1-2 (cos2>0.2)",
                                          "\nMCA on NYC 311 + NYPD 911 data",
                                          "\n(April 2010 -> 2014 -> 2018)"),
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
                             title=paste0("a) Individuals' projection in PC1-2\n(MCA for ",
                                          "April 2010 -> 2014 -> 2018",
                                          "\nNYC 311 + NYPD 911 data)"),
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
                             title=paste0("b) Individuals' projection in PC1-2, by NYC borough\n(MCA for ",
                                          "April 2010 -> 2014 -> 2018",
                                          "\nNYC 311 + NYPD 911 data)"),
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
                             title=paste0("Crime-clustered individuals' projection in PC1-2\n(MCA for ",
                                          "April 2010 -> 2014 -> 2018",
                                          "\nNYC 311 + NYPD 911 data)"),
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
}


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

# ##############################

# ##############################
## Heat maps for individual ZIP code area changes between observation periods 
## Mahalanobis & Euclidian distances for individual accross observation periods 
# ##############################

covMatrix <- cov(mcaCoords)
MahaDist1_2 <- c()
MahaDist2_3 <- c()
MahaDist1_3 <- c()
for ( zip in as.character(OCTS$ZIP) ) {
    indiv1 <- OCTS[which(as.character(OCTS$ZIP) == zip),c(3:19)]
    indiv2 <- OCTS[which(as.character(OCTS$ZIP) == zip),c(20:36)]
    indiv3 <- OCTS[which(as.character(OCTS$ZIP) == zip),c(37:53)]
    
    MahaDist1_2 <- c(MahaDist1_2,as.numeric(indiv1-indiv2) %*% as.numeric(solve(covMatrix) %*% t(indiv1-indiv2)))
    MahaDist2_3 <- c(MahaDist2_3,as.numeric(indiv2-indiv3) %*% as.numeric(solve(covMatrix) %*% t(indiv2-indiv3)))
    MahaDist1_3 <- c(MahaDist1_3,as.numeric(indiv1-indiv3) %*% as.numeric(solve(covMatrix) %*% t(indiv1-indiv3)))
    
    # for (ff in 1:length(periodsOfInterest) ) {
    #     refDateTag <- paste0("_",sub("-","",substr(as.Date(paste("01",periodsOfInterest[ff]),format="%d %b %Y"),1,7)))
    #     assign(paste0("coords",ff),c(zip,as.character(loci$Borough[which(zip==loci$ZIP)]),mcaCoords[which(rownames(mcaCoords)== paste0(zip,refDateTag)),]) )
    # }
}
MahaDist <- cbind(ZIP=as.character(OCTS$ZIP),
                  Borough=as.character(OCTS$Borough),
                  MahaD12=round(MahaDist1_2,2),
                  MahaD23=round(MahaDist2_3,2),
                  MahaD13=round(MahaDist1_3,2))

target_file <- "Data/20100400--20180400_te-mahalanobis-dist_mca-data.csv"
csvSaveF(MahaDist,target_file)

# ##############################
## Topographical mapping

source_file <- "Data/20100400--20180400_te-mahalanobis-dist_mca-data.csv"
MahaDist <- read.csv(source_file,header=T,sep=",",quote="\"",dec=".",skip=0)

library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")      # to read shapefile (translate GPS into ZIP)
library("fastshp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")
#library("colorspace")

#pal <- choose_palette()

# Use zipped achive at https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
mapZIP <- readOGR(dsn="./Data/Geolocation", layer="ZIP_CODE_040114")
shp <- read.shp("Data/Geolocation/ZIP_CODE_040114.shp", format="list")

# ZIP_lst <- MahaDist$ZIP

# retrieve mapZIP's ZIP indexes and their borough to be mapped
mapZIPidx <- which(mapZIP$ZIPCODE %in% ZIP_lst$ZIP)

mapZIPborough <- c()
for (ii in mapZIPidx) {
    mapZIPborough <- c(mapZIPborough, 
                       as.character(ZIP_lst$Borough[ which( as.character(ZIP_lst$ZIP) == mapZIP$ZIPCODE[ii] )]))
}
mapZIPborough <- as.factor(as.character(as.factor(mapZIPborough)))

# Compute & plot overall area bounding box
xMinBox <- c() ; yMinBox <- c() ; xMaxBox <- c() ; yMaxBox <- c();
for (index in mapZIPidx) {
    xMinBox <- c(xMinBox,shp[[index]]$box[1])
    yMinBox <- c(yMinBox,shp[[index]]$box[2])
    xMaxBox <- c(xMaxBox,shp[[index]]$box[3])
    yMaxBox <- c(yMaxBox,shp[[index]]$box[4])
}

xMin <- min(xMinBox); yMin <- min(yMinBox)
xMax <- max(xMaxBox); yMax <- max(yMaxBox)

## Colors for heat map
#   hot  ---- orangered3
#   cold ---- blue
scaleNbr <- ncol(MahaDist) - 2
distCol <- matrix("",ncol=scaleNbr, nrow=nrow(MahaDist))

maxHeat <- max(MahaDist[,seq(3,(2+scaleNbr))])  # corresponds to heat.colors(256)[256]
minHeat <- min(MahaDist[,seq(3,(2+scaleNbr))])  # corresponds to heat.colors(256)[1]
for (cc in 1:scaleNbr) {
    for (rr in 1:nrow(MahaDist)) {
        # color scales need 101 hues so the fringe cases of minHeat and maxHeat can be accomodated
        # Color scale (heat): white, yellow, orange, red 
        #distCol[rr,cc] <- heat.colors(101,alpha=0.5)[floor(100*((MahaDist[rr,(2+cc)]-minHeat)/(maxHeat-minHeat)+0.01))]
        # Color scale (truncated rainbow): red, orange, yellow, green, cyan, blue 
        distCol[rr,cc] <- rainbow(101,s=1,v=1,start=0,end=4/6,alpha=0.5)[floor(100*((maxHeat-MahaDist[rr,(2+cc)])/(maxHeat-minHeat)+0.01))]
    }
}
distCol <- as.data.frame(distCol,row.names=MahaDist$ZIP)

# draw initial ghost ZIP perimeter

timeInterval <- 3

if (timeInterval == 1) {
    plottitle <- paste0("NYC 5-borough ZIP semantics change\n(April 2010 / 2014 ","heat map)")
} else if (timeInterval == 2) {
    plottitle <- paste0("NYC 5-borough ZIP semantics change\n(April 2014 / 2018 ","heat map)")
} else {
    plottitle <- paste0("NYC 5-borough ZIP semantics change\n(April 2010 / 2018 ","heat map)")
}

#Assign screen design, i.e. graphs per page
par(mfrow=c(1,1))
#Assign empty space around graph(s)
par(mar=c(5,4,4,8)+0.1);
# Draw legend outside axes' scales
par(xpd = T) # all plotting clipped to figure region

plot(x=c((xMin+xMax)/2),y=c((yMin+yMax)/2),
     xlim=c(xMin,xMax),ylim=c(yMin,yMax),
     axes=FALSE,
     type="p",pch=".",col="darkgreen",xlab="",ylab="", main=plottitle)

axis(1,at=c(),labels=F,col.axis="black",tck=0)
axis(2,labels=F,col.axis="black",tck=0)


for ( ii in 1:length(mapZIPidx) ) {
    index <- mapZIPidx[ii]
    #lines(shp[[index]]$x, shp[[index]]$y,type="l",col="skyblue",xlab="x",ylab="y") # draw ZIP perimeter
    lines(shp[[index]]$x, shp[[index]]$y,type="l",col=ccolors[mapZIPborough[ii]],xlab="x",ylab="y") # draw ZIP perimeter
    
    # lines(c(shp[[index]]$box[1],shp[[index]]$box[1],shp[[index]]$box[3],shp[[index]]$box[3],shp[[index]]$box[1]),
    #       c(shp[[index]]$box[2],shp[[index]]$box[4],shp[[index]]$box[4],shp[[index]]$box[2],shp[[index]]$box[2]),
    #       type="l", lwd=1,col="red") # draw ZIP box
    
    ##put zip code string at the center of each zip area bounding box
    localZIP <- mapZIP$ZIPCODE[index]
    
    if (as.character(localZIP) %in% MahaDist$ZIP) {
        points(x=(xMinBox[ii]+xMaxBox[ii])/2,y=(yMinBox[ii]+yMaxBox[ii])/2,
               type = "p",
               col = as.character(distCol[which(rownames(distCol) == as.character(localZIP)),timeInterval]),
               # bg = as.character(zipQuadrant[which(labels(zipQuadrant) == as.character(localZIP))]),
               pch = 16,
               # cex = 3 * as.numeric(zipWeight[which(labels(zipWeight) == as.character(localZIP))]),
               cex = 3 #,
               # alpha=I(1/5)
         )
    }
}

legend("topleft",legend=levels(mapZIPborough),cex=.8,
       lwd=1,col=ccolors[1:length(levels(mapZIPborough))])

legend(x="right", 
    inset=c(-0.15,0), # move legend by 10% of graphics x-width
    #adj=0,
    title="Change metric,\nd² (Mahalanobis)",
    legend=as.character(seq(ceiling(maxHeat),floor(minHeat),floor((ceiling(minHeat)-maxHeat)/11))),
    bty="n",
    cex=.8,
    #pch=c(22,0,0),
    fill=rainbow(12,s=1,v=1,start=0,end=4/6)
    #beside = TRUE)
)
