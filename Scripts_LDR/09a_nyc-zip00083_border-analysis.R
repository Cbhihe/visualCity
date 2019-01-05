# #############################
## Project:     Analysis of NYC-311 Service Request Calls
## Script:      09a_nyc-zip00083_border-analysis.R
## Author:      Cedric Bhihe
## Date:        January 2019
## Last edit:   
# #############################

rm(list=ls(all=TRUE))

# #############################
## Environment and env. var.
# #############################

setwd("~/Documents/Work/Academic-research/visualCity/")

options(scipen=6) # R switches to sci notation above 5 digits on plot axes
set.seed(932178)


# #############################
## Libraries
# #############################

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
}    # save cvs to file on disk


# #############################
## Compute ghost "00083" zip's common borders with real neighboring zips 
# #############################

## Use zipped achive at https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
mapZIP <- readOGR(dsn="Data/Geolocation", layer="ZIP_CODE_040114")
shp <- read.shp("Data/Geolocation/ZIP_CODE_040114.shp", format="list")

## Compute  proportion of common ZIP boundaries between ghost ZIP "00083" (Central Park) and neighboring ZIPs.
# Neighboring ZIPs are: 10019, 10065, 10023, 10021, 10075, 10028, 10024, 10128, 10025, 10029, 10026
neighborZIP <- c("10019","10022","10065","10023","10021","10075","10028","10024","10128","10025","10029","10026")
ghostZIP <- "00083"

ghostZIP_idx <- which(mapZIP$ZIPCODE==ghostZIP)  # index for ZIP "00083" is 55 
neighborZIP_idx <- c()
for (nn in 1:length(neighborZIP)) neighborZIP_idx <- c(neighborZIP_idx, which(mapZIP$ZIPCODE == neighborZIP[nn]))

# Compute & plot overall area bounding box
# Keep all neighboring zip codes' boxes
xMinBox <- c() ; yMinBox <- c() ; xMaxBox <- c() ; yMaxBox <- c();
for (index in c(ghostZIP_idx,neighborZIP_idx)) {
  xMinBox <- c(xMinBox,shp[[index]]$box[1])
  yMinBox <- c(yMinBox,shp[[index]]$box[2])
  xMaxBox <- c(xMaxBox,shp[[index]]$box[3])
  yMaxBox <- c(yMaxBox,shp[[index]]$box[4])
}

xMin <- min(xMinBox); yMin <- min(yMinBox)
xMax <- max(xMaxBox); yMax <- max(yMaxBox)

# draw initial ghost ZIP perimeter
plottitle <- "NYC ZIP codes neighboring with \"00083\""
plot(shp[[ghostZIP_idx]]$x, shp[[ghostZIP_idx]]$y,
     xlim=c(xMin,xMax), ylim=c(yMin,yMax),
     xlab="plane x",ylab="plane y", 
     type="l",
     lwd=3,
     col="darkgreen",
     main=plottitle)

# draw initial overall bounding box
lines(c(xMin,xMax,xMax,xMin,xMin),
      c(yMin,yMin,yMax,yMax,yMin),
      type="l", lty=3,lwd=0.7,col="tan")

for (ii in 1:length(c(ghostZIP_idx,neighborZIP_idx))) {
  index <- c(ghostZIP_idx,neighborZIP_idx)[ii]
  # skip ii=1 to not re-draw initial ghost ZIP perimeter
  if (ii != 1) { 
    lines(shp[[index]]$x, shp[[index]]$y,type="l",col="blue",xlab="x",ylab="y") # draw ZIP perimeter
  }  
  lines(c(shp[[index]]$box[1],shp[[index]]$box[1],shp[[index]]$box[3],shp[[index]]$box[3],shp[[index]]$box[1]),
        c(shp[[index]]$box[2],shp[[index]]$box[4],shp[[index]]$box[4],shp[[index]]$box[2],shp[[index]]$box[2]),
        type="l", lwd=1,col=ifelse(ii==1,"green","red")) # draw ZIP box
  
  # put zip code string at the center of each zip area bouding box 
  localZIP <- ifelse(nchar(c(ghostZIP,neighborZIP)[ii]) <5,
                     paste0(formatC(as.numeric(c(ghostZIP,neighborZIP)[ii]),width=5,flag="0")),
                     as.character(c(ghostZIP,neighborZIP)[ii]))
  text(x=(xMinBox[ii]+xMaxBox[ii])/2,
       y=(yMinBox[ii]+yMaxBox[ii])/2,
       adj=0.5,labels=localZIP,
       col=ifelse(ii==1,"darkgreen","blue"))
}

## compute overall ghost ZIP perimeter
ghostZIP_perim <- 0
for (pp in 1:(length(shp[[ghostZIP_idx]]$x)-1)) {
  ghostZIP_perim <- ghostZIP_perim + sqrt((shp[[ghostZIP_idx]]$x[pp+1]-shp[[ghostZIP_idx]]$x[pp])^2 +
                                            (shp[[ghostZIP_idx]]$y[pp+1]-shp[[ghostZIP_idx]]$y[pp])^2)
}

## compute ZIP codes' area's boundaries common to ghost zip and neighboring zip codes' areas
border_len <- c(ghostZIP_perim)
border_lenprop <- c(100)  # initialize proportion of ghostZIP perimeter in common with each neighbor

for (index in neighborZIP_idx) {
  border_idx <- c()
  borderX <- c()
  borderY <- c()
  
  # for (xx in 1:length(ghostZIPx_ordered)) {
  #   #xx <- xx+1
  #   x0 <- ghostZIPx_ordered[xx]
  #   y0 <- ghostZIPy_ordered[xx]
  for ( xx in 1:length(shp[[ghostZIP_idx]]$x) ) {
    x0 <- shp[[ghostZIP_idx]]$x[xx]
    y0 <- shp[[ghostZIP_idx]]$y[xx]
    
    for (ii in 1:(length(shp[[index]]$x)-1)) {
      logictest <- ( (x0 - shp[[index]]$x[ii])*(x0 - shp[[index]]$x[ii+1]) < 0 & 
                         (y0 - shp[[index]]$y[ii])*(y0 - shp[[index]]$y[ii+1]) < 0 ) |
        ( x0 == shp[[index]]$x[ii] & y0 == shp[[index]]$y[ii] ) 
      
      if ( logictest) {
        # cat("TRUE for",xx,"-th ghostZIP coords and", ii,"-th",as.character(mapZIP$ZIPCODE[index]),"index.\n")
        border_idx <- c(border_idx,xx)  # keep neighboring ZIP's shape component's index 
        borderX <- c(borderX,x0)  # keep ghost ZIP's coordinates for loci found to be on a neighboring ZIP's boundary
        borderY <- c(borderY,y0)
      }
    }
  }
  
  borderLength <- 0
  for (bb in 2:length(border_idx)) {
    if ( abs(border_idx[bb]-border_idx[bb-1]) == 1 ) {
      borderLength <- borderLength + sqrt( (borderX[bb]-borderX[bb-1])^2 + (borderY[bb]-borderY[bb-1])^2 )
    } #else if ( border_idx[bb] %in% c(1,length(shp[[ghostZIP_idx]]$x)) & 
    #             border_idx[bb-1] %in% c(1,length(shp[[ghostZIP_idx]]$x))) {
    #   borderLength <- borderLength+sqrt( (shp[[ghostZIP_idx]]$x[1]-shp[[ghostZIP_idx]]$x[length(shp[[ghostZIP_idx]]$x)])^2 
    #                                      +
    #                                      (shp[[ghostZIP_idx]]$y[1]-shp[[ghostZIP_idx]]$y[length(shp[[ghostZIP_idx]]$x)])^2)
    #   
    # }
  }
  # compute proportion of ghostZIP perimeter in common with each neighbor
  border_len <- c(border_len,borderLength)
  border_lenprop <- c(border_lenprop, 100*borderLength/ghostZIP_perim) 
}

## check that neighbor zip code areas' common border length proportions sum to 1
sum(border_lenprop[2:length(border_lenprop)])/100


## Save to disk for future use
border_data <-as.data.frame(cbind(ZIP=as.character(c(ghostZIP,neighborZIP)),
                                  border_len=border_len,
                                  len_prop=border_lenprop))
target_file <- paste0("Data/nyc311-zip00083-neighbors-common-border.csv") 
csvSaveF(border_data,target_file)     # csv to disk
