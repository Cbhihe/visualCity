# #############################
## MIRI:        Analysis of NYC-311 Service Request Calls
## Author:      Cedric Bhihe
## Date:        Summer 2018
## Script:      03_apportion-ghost-zip_prep.R
# #############################


rm(list=ls(all=TRUE))
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
## Source parameter file
# #############################
source(file="Scripts/00_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...


# #############################
## ZIP of interest
# #############################
# Since the ZIP (Zoning Improvement Plan) was instated by the USPS Code in 1963,
# in order to map out a more efficient delivery network, 43 Manhattan buildings
# have earned codes exclusive to their address, either because of their great 
# size or the number of people who occupy them. 

# For this work the ZIP code list of interest must include exactly 5 digits codes
# with leading "0" if necessary; e.g. 
# ZIP_lst <- as.character(c("00501","10001":"14999"))

data("zipcode") # load US zip data
ZIP_lst <- c(unlist(zipcode[zipcode$state=="NY",1]),
             c("00083","10004","10018","10024","10029","10032","10036","10309","10459","10463",
               "10469","10472","11103", "11207","11210", "11221","11229","11235","11368","11379",
               "11385","11421","11435","11694"))
# NY State ZIP codes include:
#   ZIP "00083" is unofficial for Central Park and consists of small of big portions of ZIP codes:
#        10019, 10065, 10023, 10021, 10075, 10028, 10024, 10128, 10025, 10029, 10026
#   "00501" and "00544" for  Holtsville, Suffolk County  (type: unique)
#   "00690" for Fishers Island, Fishers Isle, Suffolk County  (type: PO Box)


# #############################
## Compute common boundaries between ghost ZIP "00083" and neighboring ZIPs
# #############################

# Use zipped achive at https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
mapZIP <- readOGR(dsn="./Data/Geolocation", layer="ZIP_CODE_040114")
shp <- read.shp("Data/Geolocation/ZIP_CODE_040114.shp", format="list")

# Compute  proportion of common ZIP boundaries between ZIP "00083" (Central Park) and neighboring ZIPs.
# Neighboring ZIPs are: 10019, 10065, 10023, 10021, 10075, 10028, 10024, 10128, 10025, 10029, 10026
neighborZIP <- c("10019","10022","10065","10023","10021","10075","10028","10024","10128","10025","10029","10026")
ghostZIP <- "00083"

ghostZIP_idx <- which(mapZIP$ZIPCODE == ghostZIP)  # index for ZIP "00083" is 55 
neighborZIP_idx <- c()
for (nn in 1:length(neighborZIP)) neighborZIP_idx <- c(neighborZIP_idx, which(mapZIP$ZIPCODE == neighborZIP[nn]))

# Compute & plot overall area bounding box
xMinBox <- c() ; yMinBox <- c() ; xMaxBox <- c() ; yMaxBox <- c();
for (index in c(ghostZIP_idx,neighborZIP_idx)) {
    xMinBox <- c(xMinBox,shp[[index]]$box[1])
    yMinBox <- c(yMinBox,shp[[index]]$box[2])
    xMaxBox <- c(xMaxBox,shp[[index]]$box[3])
    yMaxBox <- c(yMaxBox,shp[[index]]$box[4])
}

xMin <- min(xMinBox); yMin <- min(yMinBox)
xMax <- max(xMaxBox); yMax <- max(yMaxBox)
plottitle <- "NYC ZIP codes neighboring with \"00083\""

# draw initial ghost ZIP perimeter
plot(shp[[ghostZIP_idx]]$x,shp[[ghostZIP_idx]]$y,
     xlim=c(xMin,xMax),ylim=c(yMin,yMax),
     type="l",col="darkgreen",xlab="plane x",ylab="plane y", main=plottitle)

for (ii in 1:length(c(ghostZIP_idx,neighborZIP_idx))) {
    index <- c(ghostZIP_idx,neighborZIP_idx)[ii]
    
    # skip ii=1 to not re-draw initial ghost ZIP perimeter
    if (ii != 1) { 
        lines(shp[[index]]$x, shp[[index]]$y,type="l",col="blue",xlab="x",ylab="y") # draw ZIP perimeter
    }  
    lines(c(shp[[index]]$box[1],shp[[index]]$box[1],shp[[index]]$box[3],shp[[index]]$box[3],shp[[index]]$box[1]),
          c(shp[[index]]$box[2],shp[[index]]$box[4],shp[[index]]$box[4],shp[[index]]$box[2],shp[[index]]$box[2]),
          type="l", lwd=1,col=ifelse(ii==1,"green","red")) # draw ZIP box
    
    # put zip code string at the center of each zip area bounding box 
    localZIP <- ifelse(nchar(c(ghostZIP,neighborZIP)[ii]) <5,
                       paste0(formatC(as.numeric(c(ghostZIP,neighborZIP)[ii]),width=5,flag="0")),
                       as.character(c(ghostZIP,neighborZIP)[ii]))
    text(x=(xMinBox[ii]+xMaxBox[ii])/2,
         y=(yMinBox[ii]+yMaxBox[ii])/2,
         adj=0.5,labels=localZIP,
         col=ifelse(ii==1,"darkgreen","blue"))
}

# compute overall ghost ZIP perimeter
ghostZIP_perim <- 0
for (pp in 1:(length(shp[[ghostZIP_idx]]$x)-1)) {
    ghostZIP_perim <- ghostZIP_perim + sqrt((shp[[ghostZIP_idx]]$x[pp+1]-shp[[ghostZIP_idx]]$x[pp])^2 +
                                                (shp[[ghostZIP_idx]]$y[pp+1]-shp[[ghostZIP_idx]]$y[pp])^2)
}

# compute ZIP code area's boundaries common to ghost zip and neighboring zip codes' areas
commonBorderLength <- c() # initialize vector containing common boundary lengths
neighborZIP_lprct <- c()  # initialize proportion of ghostZIP perimeter in common with each neighbor

for (index in neighborZIP_idx) {
    border_idx <- c()
    borderX <- c()
    borderY <- c()
    
    for (xx in 1:length(shp[[ghostZIP_idx]]$x)) {
        #xx <- xx+1 # for testing only
        x0 <- shp[[ghostZIP_idx]]$x[xx]
        y0 <- shp[[ghostZIP_idx]]$y[xx]
        
        for (ii in 1:(length(shp[[index]]$x)-1)) {
            logictest <- ( (x0 - shp[[index]]$x[ii])*(x0 - shp[[index]]$x[ii+1]) <= 0 &
                               (y0 - shp[[index]]$y[ii])*(y0 - shp[[index]]$y[ii+1]) <= 0 
            )
            if ( logictest) {
                # cat("TRUE for",xx,"-th ghostZIP coords and", ii,"-th",as.character(mapZIP$ZIPCODE[index]),"index.\n")
                border_idx <- c(border_idx,xx) # keep xx = ghostZIp's index for coords of point falling between or on 
                                               # 1 of 2 consecutive points of the boundary belonging to neighbor-ZIP
                                               # area defined by 'index'
                borderX <- c(borderX,x0) # also keep actual ghostZIP coords.
                borderY <- c(borderY,y0) # idem
            }
        }
    }
    
    borderLength <- 0
    keep_idx <- seq(1,length(borderX),2)  # keep every other index in sequence
    border_idx <- border_idx[keep_idx]
    borderX <- borderX[keep_idx] 
    borderY <- borderY[keep_idx]
    
    for (bb in 2:length(border_idx)) {
        #bb <-bb+1 # for testing only
        if ( abs(border_idx[bb] - border_idx[bb-1]) == 1 ) {
            # compute accumulated common boundary length for successive kept ghostZIP boundary point index
            # use kept border coords to conduct calculation
            # first and last points in list of boundary points is correctly handled, because first point in list
            # is conveniently repeated once at the end.
            borderLength <- borderLength + sqrt( (borderX[bb]-borderX[bb-1])^2 + (borderY[bb]-borderY[bb-1])^2 )
        } #else if ( border_idx[bb] %in% c(1,length(shp[[ghostZIP_idx]]$x)) & 
        #             border_idx[bb-1] %in% c(1,length(shp[[ghostZIP_idx]]$x))) {
        #     borderLength <- borderLength+sqrt( (shp[[ghostZIP_idx]]$x[1]-shp[[ghostZIP_idx]]$x[length(shp[[ghostZIP_idx]]$x)])^2 
        #                     + (shp[[ghostZIP_idx]]$y[1]-shp[[ghostZIP_idx]]$y[length(shp[[ghostZIP_idx]]$x)])^2)
        # }
    }
    # compute proportion of ghostZIP perimeter in common with each neighbor
    commonBorderLength <- c(commonBorderLength,borderLength)
    neighborZIP_lprct <- c(neighborZIP_lprct, round(100*borderLength/ghostZIP_perim,2)) 
}


# #############################
## save apportionment rules to disk
# #############################

commonBorder_df <- as.data.frame(cbind(ZIP=c(as.character(neighborZIP),"00083"),
                                    SharedBorderLength=c(commonBorderLength,ghostZIP_perim),
                                    SharedBorderPrct=c(neighborZIP_lprct,100)))
# sum(as.numeric(as.vector(commonBorder_df$SharedBorderPrct[1:12])))   #  check proportions add up to 100%
target_file="Data/nyc311_00083-neighbors-common-border.csv"
csvSaveF(commonBorder_df,target_file)