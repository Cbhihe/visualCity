# #############################
## Project:     Analysis of NYC-311 Service Request Calls
## Script:      12_mca-time-evolution.R
## Author:      Cedric Bhihe
## Created:     November 2018
## Last edit:    2018
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
#library(nnet)           # to fit neural network
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
## Check parsing results
#monthNbr; yearNbr; periodOfReference

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


# ###########################
## MCA: Multiple Component Analysis
# ###########################

# -----------
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
# -----------

## Match column order of each period to that of the reference period 
#  (required for the rbind() consolidation operation below) 
W[["W_201404"]] <- W[["W_201404"]][,order(match(colnames(W[["W_201404"]]),colnames(W[["W_201004"]])))]
W[["W_201804"]] <- W[["W_201804"]][,order(match(colnames(W[["W_201804"]]),colnames(W[["W_201004"]])))]
# consolidate data from all period by row, following column order 
Wtot <- rbind(W[["W_201004"]],W[["W_201404"]],W[["W_201804"]])
rm(W)

# backup composite matrix will every period appended by row
csvSaveF(cbind(ZIP=rownames(Wtot),Wtot),"Data/20100400--20180400_te-mca-data.csv")

## Identify special ZIP codes' indexes
refDateTag <- paste0("_",sub("-","",substr(as.Date(paste("01",periodOfReference),format="%d %b %Y"),1,7)))
#  bogus ZIP code number for period of reference
zip99999_Widx <- which(rownames(Wtot) == paste0("99999",refDateTag))

supObservations <- c()
if (periodOfReference == "April 2010") {
    zip11430_Widx <- which(rownames(Wtot) == "11430_201004")  #  JFK, Queens 
    zip10281_Widx <- which(rownames(Wtot) == "10281_201004")  #  Battery Park City, Manhattan
    supObservations <- c(zip10281_Widx, zip11430_Widx)
} else if (periodOfReference == "April 2014") {
    zip11430_Widx <- which(rownames(Wtot) == "11430_201404")  #  JFK, Queens 
    zip10463_Widx <- which(rownames(Wtot) == "10463_201404")  #  Riverdale, The Bronx
    supObservations <- c(zip10463_Widx, zip11430_Widx)
} else if (periodOfReference == "April 2018") {
    zip11430_Widx <- which(rownames(Wtot) == "11430_201804")  #  JFK, Queens 
    zip11371_Widx <- which(rownames(Wtot) == "11371_201804")  #  La Guardia, Queens
    supObservations <- c(zip11371_Widx, zip11430_Widx)
} else {
    cat("\n\n---------------\nUNKNOWN YEAR\n Abort\n--------------------\n\n")
    exit()
}

## Define supplementary individuals as those NOT being part of the reference period
supObs_idx <- grep(paste0("_",sub("-","",substr(as.Date(paste("01",periodOfReference),format="%d %b %Y"),1,7))),
                   rownames(Wtot),
                   value=F,
                   fixed=T,
                   invert=T)
supObservations <- c(supObservations,supObs_idx)
Boroughs <- Wtot[-c(zip99999_Widx, supObservations),1]   # only remove sup.ind and "99999" for reference period (i.e. active rows)
Boroughs <- factor(Boroughs)  # clean up factor levels

## Perform MCA
mcaNYC311 <- MCA(Wtot[,-1],
                 ncp=5,
                 ind.sup=c(zip99999_Widx,supObservations),
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

mcaCoords <- rbind(mcaNYC311$ind$coord[,c(1,2)],mcaNYC311$ind.sup$coord[,c(1,2)])
summary(mcaNYC311,nbelements=12)
dimdesc(mcaNYC311)

## Observation s' PC1-2 coordinates' temporal sequences in the order 2010 -> 2014 -> 2018

# collate all unique ZIP codes and fill data-frame with values ZIP code by ZIP code
loci <- unique(data.frame(ZIP = substr(rownames(Wtot),1,5),
                          Borough = Wtot[,1]))
# order data frame according to ascending values of ZIP codes
loci <- loci[order(loci$ZIP,decreasing=F),]

# initialize / populate data-frame with collated PC1-2 data
OCTS_pc12 <- as.data.frame(matrix("",ncol=(2+2*length(periodsOfInterest)),nrow=length(loci$ZIP)))
colHeaders <- c()
for (ff in 1:length(periodsOfInterest)) {
    monthNumStr <- ifelse(monthNbr[ff]<10,paste0("0",as.character(monthNbr[ff])),as.character(monthNbr[ff]))
    colSuffix <- paste0("_",yearNbr[ff],monthNumStr)
    colHeaders <- c(colHeaders,paste0("Dim1",colSuffix),paste0("Dim2",colSuffix))
}
colnames(OCTS_pc12) <- c("ZIP", "Borough",colHeaders)
OCTS_pc12$ZIP <- loci$ZIP
OCTS_pc12$Borough <- loci$Borough

for (ff in 1:length(periodsOfInterest)) {
    col1 <- c()
    col2 <- c()
    monthNumStr <- ifelse(monthNbr[ff]<10,paste0("0",as.character(monthNbr[ff])),as.character(monthNbr[ff]))
    periodTag <- paste0("_",yearNbr[ff],monthNumStr)
    for (zip in as.character(OCTS_pc12$ZIP)) {
        rowNbr <- which(rownames(mcaCoords) == paste0(zip,periodTag) )
        if (length(rowNbr) != 0L) {
            # case: zip code included as observation in current time period 
            col1 <- c(col1,mcaCoords[rowNbr,1])
            col2 <- c(col2,mcaCoords[rowNbr,2])
        } else {
            # case: zip code NOT included as observation in current time period 
            col1 <- c(col1,NA)
            col2 <- c(col2,NA)
        }
    }
    OCTS_pc12[,(1+2*ff)] <- col1
    OCTS_pc12[,(2*(ff+1))] <- col2
}

# complete OCTS_pc12 by imputing proximate values to missings 
# <<<<<<<<<<<  TODO
rm(loci, col1,col2, rowNbr)



## MCA plots
mcaPlot1 <- plot.MCA(mcaNYC311,
                    #mcaNYC311$ind$coord[,1],mcaNYC311$ind$coord[,2],
                    choix="ind",
                    autoLab="yes",
                    cex=0.9,
                    #col.ind = ccolors[as.numeric(factor(Boroughs[-supObservations]))], 
                    col.ind = ccolors[as.numeric(Boroughs)], 
                    col.var = c(rep("lightsalmon3",52),rep("purple",12)),
                    col.ind.sup = "red", col.quanti.sup = "orchid3",
                    #label = c("var","ind.sup","quanti.sup"),
                    label = c("var"),
                    title = paste0("MCA - Dynamic biplot for NYC 311 + NYPD 911 data",
                                   '\n(Reference period: ',periodOfReference,' -> 2014 -> 2018)'))
mcaPlot1 + legend("bottomright",
                  legend=levels(Boroughs),
                  cex=0.8,
                  pt.cex=1.3,
                  pch=16,
                  col=ccolors[1:length(levels(Boroughs))])
arrows(x0, y0, 
       x1, y1, 
       length = 0.08, # arrow head length in inches (0.08"~ 2mm)
       angle = 20,    #  angle between each arrow head line and arrow shaft
       code=2,        # arrow head is at destination point
       col= 1,        # color-coded according to borough 
       lty=3,         # 1=solid, 2=dashed, 3=dotted
       lwd=0.7)       # lwd may default to one on certain devices 


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
                               '\n(Reference period: ',periodOfReference,' -> 2014 -> 2018)'))
mcaPlot2 

mcaPlot3a <- plot.MCA(mcaNYC311,
                    #mcaNYC311$ind$coord[,1],mcaNYC311$ind$coord[,2],
                    title=paste0("Variable projection in PC1-2\n(MCA on NYC 311 + NYPD 911 data for ",
                                periodOfReference,")"),
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
                     title=paste0("Dynamic bi-plot projection in PC1-2\n(MCA on NYC 311 + NYPD 911 data for ",
                                  periodOfReference," -> 2014 -> 2018)"),
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
                         title=paste0("a) Variables' projection in PC1-2 (all)\n(MCA on ",
                                      periodOfReference," NYC 311 + NYPD 911 data)"),
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
