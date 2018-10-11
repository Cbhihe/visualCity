# #############################
##  Project:    Analysis of NYC 311 Service Requests 
##  Script:     06_nypd_data-prep.R
##  Author:     Cedric Bhihe
##  Delivery:   January 2019
## Last edit:   
# #############################

rm(list=ls(all=TRUE))

# #############################

setwd("~/Documents/Work/Academic-research/NYC311/")

set.seed(932178)
options(scipen=6) # R switches to sci notation above 5 digits on plot axes
ccolors=c("red","green","blue","orange","cyan","tan1","darkred","honeydew2","violetred",
          "palegreen3","peachpuff4","lavenderblush3","lightgray","lightsalmon","wheat2")

datestamp <- format(Sys.time(),"%Y%m%d-%H%M%S"); 


# #############################
## Input parameters
# #############################
source(file="Scripts/01_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...

periodStart <- as.Date(paste0(yearNbr,"-",
                              ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),"-",
                              ifelse(dayNbr<10,paste0("0",as.character(dayNbr)),as.character(dayNbr))))

daysInMonth <- as.numeric(difftime(addMonthF(periodStart,1),periodStart))

periodEnd <- as.Date(paste0(yearNbr,"-",
                            ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),"-",
                            daysInMonth))


# #############################
## Libraries and repos
# #############################

setRepositories(ind = c(1:6,8))

library("zipcode", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")
library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")      # to read shapefile (translate GPS into ZIP)
#library("maptools", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")
library("fastshp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")


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

addMonthF <- function(date,n) {
  seq(date,by=paste(n,"months"), length = 2)[2]
}      # time calculations

whichBoxF <- function(x0,y0,shape) {

  if ( !is.list(shape) | !is.numeric(x0) | !is.numeric(y0) ) {
    cat("Arg type error in function whichBoxF().\nArgs should be of type \"numeric\",\"numeric\",\"shape\".\n\n")
    stop
  } else {
    
    index_lst <- c()
    # #############
    ## MALFORMED POLYGONS 
    # #############
    # take out last 303-276 = 27 points out of polygon # 178, to make it a closed domain
    shape[[178]]$x <- shape[[178]]$x[1:276]
    shape[[178]]$y <- shape[[178]]$y[1:276]  
    # #############
    
    for ( bb in 1:length(shape) ){
      # Note: shape[[bb]]$box   is vector c(Xmin,Ymin,Xmax,Ymax)
      # if inside the box, keep ZIP code index `bb`
      if ( x0 >= shape[[bb]]$box[1] & x0 <= shape[[bb]]$box[3] &
           y0 >= shape[[bb]]$box[2] & y0 <= shape[[bb]]$box[4] ) { index_lst <- c(index_lst,bb) }
    }
    
    if (length(index_lst) == 0) {
      # cat("... no ZIP found\n")
      index <- NA
    } else if (length(index_lst)==1) {
      # cat("... only one ZIP found\n")
      index <- index_lst[1]
    } else {
      bb_IN <- c()
      for (bb in index_lst) {

        # scan area over perimeter's x and y coordinates simultaneously
        # polygon's perimeter identified by index `bb` 
        # cflagx, pflagx -- current and previous flags for x scan
        # cflagy, pflagy -- current and previous flags for y scan
        xintercepts <- c(); yintercepts <- c()
        pflagx <- 0; cflagx <- 0
        pflagy <- 0; cflagy <- 0
        
        for (ii in 1:length(shape[[bb]]$x)) {
          if (ii != 1) { pflagx <- cflagx ;  pflagy <- cflagy }
          cflagx <- ifelse( x0 <= shape[[bb]]$x[ii], +1, -1 )
          cflagy <- ifelse( y0 <= shape[[bb]]$y[ii], +1, -1 )
          
          if (ii == 1) { pflagx <- cflagx; pflagy <- cflagy }
          
          if (cflagx * pflagx < 0) {
            # change of flagx's sign
            # retrieve previous point's coordinates (xp,yp)
            xp <- shape[[bb]]$x[ii-1]; yp <- shape[[bb]]$y[ii-1]
            # retrieve current point's coordinates (xc,yc)
            xc <- shape[[bb]]$x[ii]  ; yc <- shape[[bb]]$y[ii]
            # compute polygon's boundary's y intercept
            if( x0 != xc ) {
              y <- (yc*(x0-xp) +yp*(xc-x0))/(xc-xp)
              yintercepts <- c(yintercepts,y)
            } else {
              # in case the POI (x0,y0) belongs to ZIP area boundary
              y <- yc*(x0-xp)/(xc-xp)
              yintercepts <- c(yintercepts,y-1,y+1)
            }
          }
          
          if (cflagy * pflagy < 0) {
            # change of flagy's sign
            # retrieve previous point's coordinates (xp,yp)
            xp <- shape[[bb]]$x[ii-1]; yp <- shape[[bb]]$y[ii-1]
            # retrieve current point's coordinates (xc,yc)
            xc <- shape[[bb]]$x[ii]  ; yc <- shape[[bb]]$y[ii]
            # compute polygon's boundary's y intercept
            if (y0 != yc) {
              x <- (xc*(y0-yp) +xp*(yc-y0))/(yc-yp)
              xintercepts <- c(xintercepts,x)
            } else {
              # in case the POI (x0,y0) belongs to ZIP area boundary
              x <- xc*(y0-yp)/(yc-yp)
              xintercepts <- c(xintercepts,x-1,x+1)
            }
          }
          
        }
        # nbr of intercepts always even or nil for closed perimeters
        stopifnot(length(xintercepts) %% 2 == 0 & length(yintercepts) %% 2 == 0)
        # check whether (x0,y0) is IN or OUT (redundant on x and y for consistency)
        # (x0,y0) is IN if:
        #    nbr of x-intercepts < x0    AND
        #    nbr of x-intercepts > x0    AND
        #    nbr of y-intercepts < y0    AND
        #    nbr of y-intercepts > y0
        # are odd
        if (length(xintercepts[xintercepts < x0]) %% 2 == 1 &
            length(yintercepts[yintercepts < y0]) %% 2 == 1) bb_IN <- c(bb_IN,bb)
      }
      
      if( length(bb_IN) == 0) {
        index <- NA
      } else if ( length(bb_IN) == 1 ) {
        index <- bb
      } else {
        # several boxes and corresponding ZIP codes contain the POI (x0,y0)
        # compute smallest box and pick coresponding index
        boxareas <- c()
        for (bb in bb_IN) {
          boxareas <- c(boxareas,(shape[[bb]]$box[3] - shape[[bb]]$box[1])*(shape[[bb]]$box[4] - shape[[bb]]$box[2]))
        }
        index <- bb_IN[ which (boxareas == min(boxareas)) ]
      }
    }
      
      
      # Algorithm below works but does not discriminate well
      #   alpha <- c()  # scanned angles
      #   Dist2 <- c()  # scanned radii squared  (computed from (x0,y0) to scanned perimeter)
      #   for (bb in index_lst) {
      #     
      #     for (ii in length(shape[[bb]]$x)) {
      #       alpha <- c(alpha,round(acos((shape[[bb]]$x[ii]-x0)/sqrt((shape[[bb]]$x[ii]-x0)^2+(shape[[bb]]$y[ii]-y0)^2))/pi,5))
      #       Dist2 <- c( Dist2 , (shape[[bb]]$x[ii]-x0)^2+(shape[[bb]]$y[ii]-y0)^2 )  }
      #     
      #     aRange <- c(aRange,max(alpha)-min(alpha))  # keep alpha ranges
      #     scannedDist2 <- c(scannedDist2, mean(Dist2))
      #   }
      #   
      #   locRes <- list( ZIPindex = index_lst, alphaRange = aRange , meanScanRadius = scannedDist2)
      # 
      #   aRange_max <- max(locRes$alphaRange)
      #   meanScanRadius_min <- min(locRes$meanScanRadius)
      #   small_list_index_lst <- which( (aRange_max - locRes$alphaRange ) / aRange_max <= 0.02 )  # meta-index or "index of index"
      #   if (length(small_list_index_lst) > 1 ) {
      #     index <- locRes$ZIPindex[small_list_index_lst[ which(locRes$meanScanRadius[small_list_index_lst] == min(locRes$meanScanRadius[small_list_index_lst]) ) ]]
      #   } else { 
      #     index <- index_lst[1] 
      #   }
      # returns index in `mapZIP$ZIPCODE` list
  }
}

# #############################
## ZIP of interest
# #############################
#  list must include exactly 5 digits ZIP codes with leading "0" is necessary
# ZIP code "00083" corresponds to Central Park
# ZIP code "99999" is bogus and for illustrative purposes in the PCA / MCA analysis

data("zipcode") # load OUTDATED zip codes for NY state  
ZIP_lst <- c(unlist(zipcode[zipcode$state=="NY",1]),
             c("99999","00083","10004","10018","10024","10029","10032","10036","10309","10459","10463","10469","10472","11103", "11207",
               "11210", "11221","11229","11235","11368","11379","11385","11421","11435","11694"))
ZIP_lst <- ZIP_lst[which( !strtrim(ZIP_lst,3) %in% c("005","063"))]


# #############################
## Import *.cvs data
# #############################
source_file <- paste0(yearNbr,
                      ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                      "00_nyc-crime-map_raw")

protoY_raw <- read.csv(paste0("Data/",source_file,".csv"),
                       header=T,
                       sep=",",
                       quote="\"",
                       dec=".")


# #############################
## Clean-up 1: General categorical variables
# #############################

## Choose relevant columns to keep + reorganize
protoY <- cbind(ZIP=vector(mode="character",length=nrow(protoY_raw)),
                Date=as.Date(protoY_raw[,2],"%m/%d/%Y"),
                protoY_raw[,-c(1:11,13:19,22,23)])

names(protoY) <- c("ZIP","Date","offCat","planeX","planeY","GPS")

# Keep only relevant year,month
# protoY <- protoY[protoY$Date %in% periodStart:periodEnd,]   # ok for df ?
protoY <- protoY[as.Date(protoY$Date) %in% periodStart:periodEnd,]
# names(protoY)  # list columns kept so far

## Save processed raw csv file as `*_proc01.csv``
proc_file <-sub("raw","proc",source_file)
target_file <- paste0("Data/",proc_file,"01.csv") 
csvSaveF(protoY,target_file)     # csv to disk


# #############################
## Clean-up 2: Crime categories and missings
# #############################

class(protoY$offCat)                     # returns "factor"
(offCat_lst <- unique(protoY$offCat))
cat("\n\nInfraction categories:",length(offCat_lst),"\n\n")

GPS_lst <- as.character(protoY$GPS)
GPS_lst[which(GPS_lst=="")] <- NA
protoY <- cbind(protoY[,c(1:5)],GPS=GPS_lst)

protoYmissings <- protoY[ (is.na(protoY$planeX) | is.na(protoY$planeY)) & is.na(protoY$GPS), ]


# #############################
## imput ZIP code "99999" to missings
# #############################
protoY_bak <- protoY
impZIP <- as.vector(protoY$ZIP)
impZIP[ which( (is.na(protoY$planeX) | is.na(protoY$planeY)) & is.na(protoY$GPS)) ] <- "99999"
protoY <- as.data.frame(cbind(ZIP=impZIP,protoY[,2:6]))


# #############################
## For each crime modality, list all possible corresponding description and offenses
# #############################
c1 <- c();c2 <- c();c3 <- c()
c1 <- as.character(unique(unlist(protoY_raw$PD_DESC[protoY_raw$LAW_CAT_CD == as.character(offCat_lst)[1]])))
c2 <- as.character(unique(unlist(protoY_raw$PD_DESC[protoY_raw$LAW_CAT_CD == as.character(offCat_lst)[2]])))
c3 <- as.character(unique(unlist(protoY_raw$PD_DESC[protoY_raw$LAW_CAT_CD == as.character(offCat_lst)[3]])))
maxLength <- max(length(c1),length(c2),length(c3))
if (length(c1) < maxLength)  c1 <- c(c1,rep_len("NA",length.out=maxLength-length(c1)))
c1[c1==""] <- "NA"
if (length(c2) < maxLength)  c2 <- c(c2,rep_len("NA",length.out=maxLength-length(c2)))
c2[c2==""] <- "NA"
if (length(c3) < maxLength)  c3 <- c(c3,rep_len("NA",length.out=maxLength-length(c3)))
c3[c3==""] <- "NA"
offConcepts <- as.data.frame(cbind(c1,c2,c3))
names(offConcepts) <- as.character(offCat_lst)
# offConcepts <- offConcepts[,order(names(offConcepts))] # order offConcepts columns according to colnames

## Save processed raw csv file as `nypd_offense_modalities.csv``
target_file <- paste0("Data/",
                          yearNbr,
                          ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                          "00_nyc_crime-modalities.csv")
csvSaveF(missings,target_file)     # csv to disk



# #############################
## Translation of GPS or cartesian coordinates in ZIP codes
# #############################

# use zipped achive at https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u
mapZIP <- readOGR(dsn="./Data/Geolocation", layer="ZIP_CODE_040114")
shp <- read.shp("Data/Geolocation/ZIP_CODE_040114.shp", format="list")

# Plot deficient polygon nbr 178
index <- 178  # corresponds to ZIP 11433
as.character(mapZIP$ZIPCODE[index])  # "11433"
# plot ZIP perimeter and corresponding box center point
x0 <- (shp[[index]]$box[1] + shp[[index]]$box[3])/2  # center point's abcissa
y0 <- (shp[[index]]$box[2]+shp[[index]]$box[4])/2  # center point's ordinate

par(mfrow=c(1,1))
plot(shp[[index]]$x[1:276], shp[[index]]$y[1:276],type="l",col="blue",xlab="x",ylab="y")
points(x0,y0,type="p",cex=1,col="green")
lines(c(shp[[index]]$box[1],shp[[index]]$box[1],shp[[index]]$box[3],shp[[index]]$box[3],shp[[index]]$box[1]),
      c(shp[[index]]$box[2],shp[[index]]$box[4],shp[[index]]$box[4],shp[[index]]$box[2],shp[[index]]$box[2]),
      type="l", lwd=1,col="red")
# plot(mapZIP@polygons[[index]]@Polygons[[1]]@coords[,1],
#      mapZIP@polygons[[index]]@Polygons[[1]]@coords[,2],
#      type="l",col="blue",xlab="x",ylab="y")
par(mfrow=c(1,1))



Nrec <- length(which(as.character(protoY$ZIP) != "99999"))  # nbr of non-missings in protoY
protoYmissings <- protoY[which(as.character(protoY$ZIP) == "99999"),]
protoY <- protoY[which(as.character(protoY$ZIP) != "99999"),]
Nzip <- dim(mapZIP)[1]
queryZIP <- as.character(mapZIP@data$ZIPCODE)         # list of geolocated ZIPs
foundZIP <- c()

for (rr in 1:Nrec) {
  x0 <- protoY$planeX[rr] ; y0 <- protoY$planeY[rr]   # NYPD coords of crime
  ZIPindex <- whichBoxF(x0,y0,shp)                    # find `queryZIP`'s index for ZIP containing (x0,y0)
  cat("NA for obs' index:\n")
  nn <- 0
  if (is.na(ZIPindex))  n <- n+1; cat(nn,") ",rr,"\n")
  # accumulate ZIPs and normalize ZIP length to 5 characters
  foundZIP <- c(foundZIP, 
                ifelse(is.na(ZIPindex),
                       "99999",
                       ifelse(nchar(queryZIP[ZIPindex]) < 5,
                              paste0(formatC(as.numeric(queryZIP[ZIPindex]),width=5,flag="0")),
                              as.character(queryZIP[ZIPindex])
                              )
                       )
                )
}

# #############################
## Consolidate protoY + save to disk
# #############################

protoY <- cbind(foundZIP,protoY[,-1])
names(protoYmissings) <- names(protoY)
protoY <- rbind(protoY,protoYmissings)
names(protoY) <- c("ZIP","Date","offCat","planeX","planeY","GPS")
target_file <- paste0("Data/",proc_file,"02.csv") 
csvSaveF(protoY,target_file)     # csv to disk


## Suppress superfluous columns + save to disk
target_file <- paste0("Data/",proc_file,"03.csv")
csvSaveF(protoY[,c(1,3)],target_file)


# ############################################
## Analyze missings
# ############################################

twoWayTable=rbind(as.vector(table(protoY[which(as.character(protoY$ZIP) != "99999"),3])),
                  as.vector(table(protoY[which(as.character(protoY$ZIP) == "99999"),3])))
row.names(twoWayTable) <- c("non-missings","missings")
colnames(twoWayTable) <- as.character(levels(protoY$offCat))

# # Check that there is independence between row and colum modalities
chisq.test(twoWayTable)

# # Conclusion: (for April 2014 NYPD crime data)
# #   chi-sq=86.671, for DF=2 and highly significant p-value <2.2 e-16, is 
# #   outside the [0.05, 7.4] interval from (chi-sq tables) for two-sided
# #   distributions at 5% overall risk. 
# #   We can reject the null hypothesis at the rist 5% of erring. 
# #   There is significant association of row and column variables.
# #   We are introducing some bias by suppressing missings.
# 
# # Conclusion: (for April 2015 NYPD crime data)
# #   chi-sq=96.292, for DF=2 and highly significant p-value <2.2 e-16, is 
# #   outside the [0.05, 7.4] interval from (chi-sq tables) for two-sided
# #   distributions at 5% overall risk. 
# #   We can reject the null hypothesis at the rist 5% of erring. 
# #   There is significant association of row and column variables.
# #   We are introducing some bias by suppressing missings.
