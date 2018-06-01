# #############################
## MIRI Project:    Geosociological Analysis of NYC-311 Service Requests
## Author:          Cedric Bhihe, Santi Calvo
## Delivery:        2018.06.26
## Script:          02_nyc311_missing-impute.R
# #############################


rm(list=ls(all=TRUE))

setwd("~/Documents/Work/Academic-research/NYC-complaints/")
set.seed(932178)

options(scipen=6) # R switches to sci notation above 5 digits on plot axes
ccolors=c("red","green","blue","orange","cyan","tan1","darkred","honeydew2","violetred",
          "palegreen3","peachpuff4","lavenderblush3","lightgray","lightsalmon","wheat2")

datestamp <- format(Sys.time(),"%Y%m%d-%H%M%S"); 

# #############################
## Libraries
# #############################
library("VIM")        # Visualization and Imputation of Missing values
                      # methods aggr(),barmiss(), histmiss() for missing/imputed values
library("ggplot2")
library("zipcode")    # Load all valid us ZIPs  <<<<<<< OUTDATED, 
                      # see section "ZIP codes of interest" below
# library("rgdal", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")      # to read shapefile (translate GPS into ZIP)
# library("fastshp", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")



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

data("zipcode") # load zip codes for NY state, + bogus code 00083
ZIP_lst <- c(unlist(zipcode[zipcode$state=="NY",1]),
             c("00083","10004","10018","10024","10029","10032","10036","10309","10459","10463",
               "10469","10472","11103", "11207","11210", "11221","11229","11235","11368","11379",
               "11385","11421","11435","11694"))
ZIP_lst <- ZIP_lst[ which( ! strtrim(ZIP_lst,3) %in% c("005","063") ) ]

# NY State ZIP codes include:
#   ZIP "00083" is unofficial for Central Park and consists of small of big portions of ZIP codes:
#        10019, 10065, 10023, 10021, 10075, 10028, 10024, 10128, 10025, 10029, 10026
#   "00501" and "00544" for  Holtsville, Suffolk County  (type: unique)
#   "00690" for Fishers Island, Fishers Isle, Suffolk County  (type: PO Box)


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

urlF <- function(address,latlng,return.call="json",sensor="false",key=my_APIkey) {
  ## URL encoding function 
  root_url <- "https://maps.googleapis.com/maps/api/geocode/"
  if ( latlng != "" ) {
    u <- paste0(root_url, return.call, "?latlng=", latlng, "&sensor=", sensor,"&key=",my_APIkey)
  } else {
    address <- tolower(address)
    u <- paste0(root_url, return.call, "?address=", address, "&sensor=", sensor,"&key=",my_APIkey)
  }
  return(URLencode(u))   # in package "utils", to percent-encode characters in URLs.
}

geoCodeF <- function(address,
                     latlng,
                     verbose,
                     Nquery,
                     bounds=NULL,
                     key = my_APIkey,
                     language = "en",
                     region = NULL,
                     components = NULL,
                     simplify = F,
                     curl_proxy = NULL) {

  ## Google Maps API query function
  #' @param verbose logical (boolean), defaults to 'FALSE'
  #' @param address \code{string}. Street address to pass to geocode, in format
  #   used by the national postal service of the country concerned... 'address'
  #   can also consist of two intersecting streets, e.g.:
  #   intersect_1 <- "EAST 15 STREET"
  #   intersect_2 <- "2 AVENUE"
  #   address <- paste0(intersect_1," + ",intersect_2,", New York,NY, USA")
  #' @param latlng \code{list}. Contains 1 o more pairs of lat/lon coordinates
  #   latlng <- list(c(lat1,lon1),c(lat2,lon2),c(lat3,lon3), ...)  
  #' @param bounds list of two, each element is a vector of lat/lon coordinates
  #   representing the south-west and north-east bounding box, e.g.:
  #   bounds <- list(c(34.172684,-118.604794),c(34.236144,-118.500938))
  #' @param language \code{string}. Specifies the language in which to return the results.
  #   See the list of supported languages:
  #   \url{https://developers.google.com/maps/faq#using-google-maps-apis}. If no
  #   language is supplied, the service will attempt to use the language of the domain
  #   from which the request was sent
  #' @param region \code{string}. Specifies the region code, specified as a ccTLD
  #   ("top-level domain"). For details, see:
  #   \url{https://developers.google.com/maps/documentation/directions/intro#RegionBiasing}
  #' @param key \code{string}. A valid Google Developers Geocode API key
  #' @param components \code{data.frame} of two columns, component and value. Restricts
  #   the results to a specific area. One or more of "route","locality","administrative_area",
  #   "postal_code","country", e.g.:
  #   components <- data.frame(component = c("postal_code", "country"),value = c("3000", "AU"))
  #' @param simplify \code{logical} - TRUE indicates the returned JSON will be coerced into a list. 
  #                                 - FALSE indicates the returend JSON will be returned as a string
  #' @param curl_proxy a curl proxy object
  #' @return Either list or JSON string of the geocoded address
  
  if (! exists("verbose", mode="logical") ) verbose=F   # default
  if(verbose) cat("Address:",address,"\nLat-Long:",latlng,"\n")
  
  if ( latlng != "" ) {
    u <- urlF(address="",latlng)
    doc <- getURL(u)
    Nquery <- Nquery +1
    ## Doc -- "location_type" :
    # "ROOFTOP" indicates that the returned result is a precise geocode for which we have location
    # information accurate down to street address precision.
    # "RANGE_INTERPOLATED" indicates that the returned result reflects an approximation (usually on
    # a road) interpolated between two precise points (such as intersections). Interpolated results 
    # are generally returned when rooftop geocodes are unavailable for a street address.
    # "GEOMETRIC_CENTER" indicates that the returned result is the geometric center of a result such
    # as a polyline (for example, a street) or polygon (region).
    # "APPROXIMATE" indicates that the returned result is approximate.
    Sys.sleep(APIsleep)
  } else if ( address != "" ) {
    u <- urlF(address,latlng="")
    doc <- getURL(u)
    Nquery <- Nquery +1
    Sys.sleep(APIsleep)
  } else {
    u <- ""
    doc <- ""
    stop("\n-------------------------\nEither a valid address or GPS data is required.\n-------------------------\n")
  }
  
  x <- fromJSON(doc,simplify = FALSE)  
  ## simplify = c(TRUE,FALSE,Strict,StrictCharacter,StrictLogical,StrictNumeric)
  # 'Strict' means 'StrictCharacter+StrictLogical+StrictNumeric'
  # Any pair-wise combination of (StrictCharacter,StrictLogical,StrictNumeric) with the '+' 
  # operator is possible.
  
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    
    sep_loc <- as.numeric(gregexpr(pattern =', usa$',formatted_address,ignore.case=T))
    res_zip <- regexpr("^[0123456789]{5}$",substr(formatted_address,sep_loc-5,sep_loc-1),perl=TRUE,fixed=FALSE)
    
    if (res_zip != "-1") {
      zip <- as.character(grep("^[0123456789]{5}$",substr(formatted_address,sep_loc-5,sep_loc-1),
                               perl=TRUE,
                               fixed=FALSE,
                               inv=FALSE,
                               value=TRUE))
      fail_token <- 0
    } else {
      zip <-""
      fail_token=1
    }
    return(c(zip, as.character(lat), as.character(lng), fail_token,as.numeric(Nquery)))
    
  } else { fail_token=1; return(c("","","",fail_token,as.numeric(Nquery))) }
}

# Translation of GPS or cartesian coordinates in ZIP codes
whichBoxF <- function(x0,y0,shape) {
  
  if ( !is.list(shape) | !is.numeric(x0) | !is.numeric(y0) ) {
    cat("Arg type error in function whichBoxF().\nArgs should be of type \"numeric\",\"numeric\",\"shape\".\n\n")
    stop
  } else {
    
    index_lst <- c()
    # #############
    ## MALFORMED POLYGONS 
    # #############
    shp[[178]]$x <- shp[[178]]$x[1:276]; shp[[178]]$y <- shp[[178]]$y[1:276]
    # #############
    
    for ( bb in 1:length(shape) ){
      # Note: shp[[bb]]$box   is vector c(Xmin,Ymin,Xmax,Ymax)
      # if inside the box, keep ZIP code index `bb`
      if ( x0 >= shp[[bb]]$box[1] & 
           y0 >= shp[[bb]]$box[2] & 
           x0 <= shp[[bb]]$box[3] & 
           y0 <= shp[[bb]]$box[4] ) index_lst <- c(index_lst,bb) }
    
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
          # for (ii in 1:200) {
          #   ii <- ii+1
          if (ii != 1) { pflagx = cflagx ;  pflagy = cflagy }
          cflagx <- ifelse( x0 <= shape[[bb]]$x[ii], +1, -1 )
          cflagy <- ifelse( y0 <= shape[[bb]]$y[ii], +1, -1 )
          
          if (ii == 1) { pflagx = cflagx; pflagy = cflagy }
          
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
        # are all odd
        if (length(xintercepts[xintercepts < x0]) %% 2 == 1 &
            length(yintercepts[yintercepts < y0]) %% 2 == 1) bb_IN <- c(bb_IN,bb)
      }
      
      if( length( bb_IN) == 0) {
        index <- NA
      } else if ( length( bb_IN) == 1 ) {
        index <- bb
      } else {
        # compute smallest box and pick coresponding index
        boxareas <- c()
        for (bb in bb_IN) {
          boxareas <- c(boxareas,( shp[[bb]]$box[3] - shp[[bb]]$box[1] ) * ( shp[[bb]]$box[4] - shp[[bb]]$box[2]) )
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
}    # query for cartesian coordinates to get correponding NYC ZIP codes


# #############################
## Source parameter file
# #############################
source(file="Scripts/00_nyc311_input-parameters.R",
       local=F,echo=F)  # Year, Month, Day, ...


# #############################
## Import / inspect pre-processed csv data
# #############################

## Missings are essentially records of NYC 311 calls w/o ZIP, 
## and either no address, planeXY or GPS coordinates

# Load '*proc02.csv' file

source_file <- paste0(yearNbr,
                      ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                      "00_nyc311_proc")

protoY <- read.csv(paste0("Data/",source_file,"02.csv"),
                   header=T,
                   sep=",",
                   quote="\"",
                   dec=".")

plottitle <- paste0("NYC 311 Calls (",format(ISOdate(yearNbr,monthNbr,1),"%B")," ",yearNbr,")")
subtitle="(Whole data set)"
modplot <- barplot(sort(table(protoY$Complaint),decreasing = T),
                   horiz=F,
                   beside=T,
                   legend.text=F,
                   axes=T,
                   las=2,
                   cex.lab=1,
                   ylab="Nbr of service requests (calls)",
                   #log="y",
                   cex.axis=0.8,
                   cex=1,
                   col=ccolors[5],
                   main=plottitle,
                   xaxt="n")
text(length(unlist(labels(table(protoY$Complaint)))),
     max(as.numeric(unlist(table(protoY$Complaint))))/2,
     pos=1,
     subtitle)
labs <- as.character(unlist(labels(sort(table(protoY$Complaint),decreasing = T))))
text(cex=1,x=modplot+0.5, y=- max (table(protoY$Complaint))/20, labs, xpd=TRUE, srt=45, pos=2)

# #############################
## Analysis of missings
# #############################

dim(protoY)
class(protoY)  # "data.frame"
summary(protoY[,c(1,2,3,12)])


nrow(protoY[protoY$ZIP == "" | is.na(protoY$ZIP),]) # (In April 2014) 3206 obs w/ missing ZIPs
                                                    # (In April 2015) 4231 obs w/ missing ZIPs
nrow(protoY[protoY$GPS == "" | is.na(protoY$GPS),]) # (In April 2014) 7777 obs w/ missing GPS coords
                                                    # (In April 2015) 9116 obs w/ missing GPS coords
nrow(protoY[protoY$ZIP == "" & protoY$GPS == "",])  # (In April 2014) 3158 obs w/ missing ZIP and GPS coords
                                                    # (In April 2015) 4192 obs w/ missing ZIP and GPS coords
nrow(protoY[protoY$ZIP == "" 
            & protoY$Address == ""
            & protoY$GPS == "",])                   # (In April 2014) 2979 obs w/ missing ZIP, Address, GPS coords
                                                    # (In April 2015) 3581 obs w/ missing ZIP, Address, GPS coords

# To apply method 'aggr()' transform missing in NAs
protoYmissing <- as.matrix(protoY) 
protoYmissing[as.character(protoY$ZIP) == "",1] <- NA
protoYmissing[protoY$Address == "",4] <- NA
protoYmissing[protoY$Xstreet_1 == "",5] <- NA
protoYmissing[protoY$Xstreet_2 == "",6] <- NA
protoYmissing[protoY$Intersect_1 == "",7] <- NA
protoYmissing[protoY$Intersect_2 == "",8] <- NA
protoYmissing[protoY$planeX == "",10] <- NA
protoYmissing[protoY$planeY == "",11] <- NA
protoYmissing[protoY$GPS == "",12] <- NA

protoYmissing <- as.data.frame(protoYmissing)
names(protoYmissing)
missingZIP <- protoYmissing[is.na(protoYmissing$ZIP),c(4,5,7,10,12)]

# Compute and draw missings' table, for all obs with missing ZIP
missingZIP_aggr <- aggr(missingZIP, 
                        numbers=TRUE,
                        bars=TRUE,
                        combined=FALSE,
                        prop=FALSE,
                        plot=TRUE,
                        axes=TRUE,
                        varheight=FALSE,
                        col=ccolors[6:8], 
                        labels=names(protoYmissing[,c(4,5,7,10,12)]),
                        cex.axis=1,
                        ylab=c("Missing Data (count)","Missing Data Distribution") )

summary(missingZIP_aggr)
# Observe that (in April 2014):
#   2740 observations or 85.46% of all obs missing a ZIP code have no other geographic locator.
# Observe that (in April 2015):
#   3069 observations or 3% of tot. obs and 72.54% of all obs missing a ZIP code have no other geographic locator.

#   Such obs are useless and should be either eliminated or kept in a "missingZIP" fake obs.
#   Prior to doing so, one should inspect the corresponding complaint modality distribution.
#   Compare the modality distribution of 'protoYmissingZIP' 2740 complaints frequency wise with those of the whole data set


# missingZIP_aggr$combinations
# missingZIP_aggr$count
# missingZIP_aggr$percent
# missingZIP_aggr$missings
missingZIP_aggr$tabcomb
missingZIP_aggr$tabcomb[10,]  # view the "all-missing" section,  i.e. missing all, but for object + date of service request call 
mod_allmissing <- protoYmissing$Complaint[is.na(protoYmissing$ZIP)
                                      & is.na(protoYmissing$Address)
                                      & (is.na(protoYmissing$Xstreet_1) | is.na(protoYmissing$Xstreet_2))
                                      & (is.na(protoYmissing$Intersect_1) | is.na(protoYmissing$Intersect_2))
                                      & (is.na(protoYmissing$planeX) | is.na(protoYmissing$planeY))
                                      & is.na(protoYmissing$GPS)]
sort(table(mod_allmissing),decreasing = T)
sum(as.numeric(table(mod_allmissing)))  # Nbr of obs, missing every geographic locator (e.g. 2740, in April 2014)


# #############################
## Plot distributions
# #############################

# labels in decreasing order of modality occurence in 'allmissing' (=all-missing section)
mod_labels <- as.character(unlist(labels(sort(table(mod_allmissing),decreasing = T))))
Nlab = length(mod_labels)

# build dataframe with distribution of:
#   - "all" complaints, with modality labels and identifying factor "whole_data"
#   - "all-missing" complaints, with modality labels and identifying factor "all-missing_data"
vec1 <- as.numeric(unlist(sort(table(protoYmissing$Complaint),decreasing = T)))
names(vec1) <- as.character(unlist(labels(sort(table(protoYmissing$Complaint),decreasing = T))))
vec2 <- as.numeric(unlist(sort(table(mod_allmissing),decreasing = T))) 
names(vec2) <- as.character(unlist(labels(sort(table(mod_allmissing),decreasing = T))))
vec2 <- vec2[order(match(names(vec2),names(vec1)))]  # sort vec2 according to order of vec1's colnames

plotdata <- data.frame(Modalities=c(names(vec1),names(vec2)),
                       Service_calls_count=c(vec1,vec2),
                       Dataset=c(rep("Whole",Nlab),rep("Loc-missings",Nlab))
                       )
# To order barplot per decreasing mod. count, either:
plotdata$Modalities <- factor(plotdata$Modalities,level=names(vec1))
plottitle <- paste0("NYC 311 Calls \(",format(ISOdate(yearNbr,monthNbr,1),"%B")," ",yearNbr,"\)")
ggplot(data=plotdata,mapping=aes(x=Modalities,y=Service_calls_count,fill=Dataset)) +
  ggtitle(plottitle) + 
  geom_bar(position="dodge",stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c(2,4))

# or, in the case of a single y-data source:

# ggplot(data=plotdata,
#        mapping=aes(x=reorder(Modalitie,-Service_calls_count),
#                    y=Service_calls_count,
#                    fill=Dataset)) + 
#   geom_bar(position="dodge",stat="identity") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_fill_manual(values=c(2,4))

## Conclusion:
# As expected distributions are not identical and suppressing obs with zero loc info 
# will introduce a bias in terms of the "SocServ" modality. 
# We also decide to keep all 311 service calls, which miss location info, under the
# fabricated ZIP code "99999".

# The other affected modality is "IAO".
cat("Fraction of modality \'SocServ\':",
    round(vec2[names(vec2)=="SocServ"]/vec1[names(vec1)=="SocServ"]*100,2),
    "%")
cat("Fraction of modality \'IAO\':",
    round(vec2[names(vec2)=="IAO"]/vec1[names(vec1)=="IAO"]*100,2),
    "%\n\n")

## Keep obs where all loc info is missing under fabricated ZIP code "99999"
allLocMissing_idx <- which(is.na(protoYmissing$ZIP)
                                          & is.na(protoYmissing$Address)
                                          & is.na(protoYmissing$Xstreet_1)
                                          & is.na(protoYmissing$Intersect_1)
                                          & is.na(protoYmissing$planeX)
                                          & is.na(protoYmissing$GPS))
totZIP <- as.character(protoY$ZIP)
totZIP[allLocMissing_idx] <- "99999"
protoY <- as.data.frame(cbind(ZIP=totZIP,protoY[,c(2:12)]))

## Cleanup
rm(missingZIP,missingZIP_aggr,protoYmissing, totZIP,
   mod_allmissing,mod_labels,allLocMissing_idx,Nlab,
   plottitle, subtitle,vec1,vec2,modplot,labs)



# #############################
## Fill missing ZIPs and GPS coordinates (via Google Maps queries)
# #############################

## Build list of indices for obs whose ZIPs either are missing or do not conform to the 
# standard ZIPs for NY state, per data-frame 'zipcode'. 
# See https://gist.github.com/dryan/7486408 for list of valid/invalid ZIP codes in the USA

ZIP <- as.character(unlist(protoY$ZIP))
# if "-" exists, then trim from "-" to end of string, else keep ZIP unchanged
length(which(as.numeric(gregexpr(pattern ='-',ZIP))>0))  # nbr of such cases
ZIP <- ifelse(as.numeric(gregexpr(pattern ='-',ZIP))>0,
              substr(ZIP,1,as.numeric(gregexpr(pattern ='-',ZIP))-1),
              ZIP)

length(which(ZIP=="N/A"))
ZIP[which(ZIP=="N/A")] <- ""

ZIP <- ifelse(nchar(ZIP)==0 | nchar(ZIP)==5,
              ZIP,
              ifelse(nchar(ZIP) >5,
                     NA,
                     paste0(formatC(as.numeric(ZIP),width=5,flag="0")))
              )

# List of problematic ZIPs' indices
checkZIP_idx <- which( ! ZIP %in% c(ZIP_lst,"99999"))
# As an alternative, one can also exclude from above list, obs w/ cartesian coords planeX and planeY. 
# In the presence of Cartesian coords a ZIP code can be found using the Cartesian coords DB for NYC. 
#checkZIP_idx <- which( !(ZIP %in% c(ZIP_lst,"99999")) & (is.na(protoY$planeX) | is.na(protoY$planeY) )) 
                       
# Build complete list of GPS coordinates
GPS_lst <- as.character(unlist(protoY$GPS))

imputeTo99999_idx<- c()
GPS_lat <- c()
GPS_lon <- c()
ii <- 0

token_GPS <- 0
token_address <- 0
token_Xstreet <- 0
token_intersect <- 0

APIsleep <- 0.1 # maximum query rate accepted by Google Maps is 50 queries/s/user or 1 query/0.02s/user

cat("\nProcessed ZIP count:\n")

if (! geolocalF) {
  
  # Minimal https querying method to Google Maps' servers with package ggmap's method revgeocode()
  # Rough limit is 1 query / second
  
  library("ggmap")  
  
  while (ii < length(checkZIP_idx)) {
    ii <- ii+1
    zz <- checkZIP_idx[ii]          # ZIP index value in list
    cat(paste0(ii,"-th processed.\n"))
    gM_out <- NA
    Nfails <- 0
    
    if (GPS_lst[zz] != ""
        & token_GPS != 1) {
      sep_loc <- as.numeric(gregexpr(pattern =',',GPS_lst[zz]))
      while (is.na(gM_out[1]) & Nfails <= 20) {
        Nfails <- Nfails +1
        gM_out=revgeocode(c(as.numeric(substr(GPS_lst[zz],sep_loc+1,nchar(GPS_lst[zz])-1)),
                            as.numeric(substr(GPS_lst[zz],2,sep_loc-1))),
                          output="address",
                          override_limit=T)
        Sys.sleep(APIsleep)
      }
      if (! is.na(gM_out[1])) {
        sep_loc <- as.numeric(regexpr(pattern =', usa$',gM_out,ignore.case=T))
        res_zip <- regexpr("^[0123456789]{5}$",substr(gM_out,sep_loc-5,sep_loc-1),perl=TRUE,fixed=FALSE)
        if (res_zip != "-1") {
          ZIP[zz] <- as.character(grep("^[0123456789]{5}$",substr(gM_out,sep_loc-5,sep_loc-1),
                                       perl=TRUE,
                                       fixed=FALSE,
                                       inv=FALSE,
                                       value=TRUE))
          token_address <- 0
          token_Xstreet <- 0
          token_intersect <- 0
          
        } else {
          ZIP[zz] <- ""
          token_GPS <- 1
          ii <- ii -1
        }
      }
      
      
    } else if (as.character(protoY$Address[zz]) != "" 
               & token_address != 1) {
      while (is.na(gM_out[1]) & Nfails <= 20) {
        Nfails <- Nfails +1
        gM_out <- geocode(paste0(as.character(protoY$Address[zz]),",New York,USA"),
                          output="latlona",
                          override_limit=T)
        Sys.sleep(APIsleep)
      }
      if (! is.na(gM_out[1])) {
        sep_loc <- as.numeric(regexpr(pattern =', usa$',gM_out$address,ignore.case=T))
        res_zip <- regexpr("^[0123456789]{5}$",substr(gM_out,sep_loc-5,sep_loc-1),perl=TRUE,fixed=FALSE)
        if (res_zip != "-1") {
          ZIP[zz] <- as.character(grep("^[0123456789]{5}$",substr(gM_out,sep_loc-5,sep_loc-1),
                                       perl=TRUE,
                                       fixed=FALSE,
                                       inv=FALSE,
                                       value=TRUE))
          GPS_lst[zz] <- paste0("(",gM_out$lat," ",gM_out$lon,")")
          token_GPS <- 0
          token_Xstreet <- 0
          token_intersect <- 0
          
        } else {
          ZIP[zz] <- ""
          GPS_lst[zz] <- ""
          token_address <- 1
          ii <- ii -1}   
      }
      
    } else if (as.character(protoY$Xstreet_1[zz]) != "" 
               & as.character(protoY$Xstreet_2[zz]) != ""
               & token_Xstreet != 1) {
      token_Xstreet  <- 0
      while (is.na(gM_out[1]) & Nfails <= 20) {
        Nfails <- Nfails +1
        gM_out <- geocode(paste0(as.character(protoY$Xstreet_1[zz]),
                                 " + ",
                                 as.character(protoY$Xstreet_2[zz]),
                                 ",New York,USA"),
                          output="latlona",
                          override_limit=T)
        Sys.sleep(APIsleep)
      }
      if (! is.na(gM_out[1])) {
        sep_loc <- as.numeric(gregexpr(pattern =', usa$',gM_out$address,ignore.case=T))
        res_zip <- regexpr("^[0123456789]{5}$",substr(gM_out,sep_loc-5,sep_loc-1),perl=TRUE,fixed=FALSE)
        if (res_zip != "-1") {
          ZIP[zz] <- as.character(grep("^[0123456789]{5}$",substr(gM_out,sep_loc-5,sep_loc-1),
                                       perl=TRUE,
                                       fixed=FALSE,
                                       inv=FALSE,
                                       value=TRUE))
          GPS_lst[zz] <- paste0("(",gM_out$lat," ",gM_out$lon,")")
          token_GPS <- 0
          token_address <- 0
          token_intersect <- 0
          
        } else {
          ZIP[zz] <- ""
          GPS_lst[zz] <- ""
          token_Xstreet <- 1
          ii <- ii -1}     
      }
      
    } else if (as.character(protoY$Intersect_1[zz]) != "" 
               & as.character(protoY$Intersect_2[zz]) != ""
               & token_intersect != 1) {
      while (is.na(gM_out[1]) & Nfails <= 20) {
        Nfails <- Nfails +1
        gM_out <- geocode(paste0(as.character(protoY$Intersect_1[zz]),
                                 " + ",
                                 as.character(protoY$Intersect_2[zz]),
                                 ",New York,USA"),
                          output="latlona",
                          override_limit=T)
        Sys.sleep(APIsleep)
      }
      if (! is.na(gM_out[1])) {
        sep_loc <- as.numeric(gregexpr(pattern =', usa$',gM_out$address,ignore.case=T))
        res_zip <- regexpr("^[0123456789]{5}$",substr(gM_out,sep_loc-5,sep_loc-1),perl=TRUE,fixed=FALSE)
        if (res_zip != "-1") {
          ZIP[zz] <- as.character(grep("^[0123456789]{5}$",substr(gM_out,sep_loc-5,sep_loc-1),
                                       perl=TRUE,
                                       fixed=FALSE,
                                       inv=FALSE,
                                       value=TRUE))
          GPS_lst[zz] <- paste0("(",gM_out$lat," ",gM_out$lon,")")
          token_GPS <- 0
          token_address <- 0
          token_Xstreet <- 0
          
        } else {
          ZIP[zz] <- ""
          GPS_lst[zz] <- ""
          token_intersect <- 1
          ii <- ii -1}
      }
      
    } else {
      imputeTo99999_idx <- c(imputeTo99999_idx, zz)
      token_GPS <- 0
      token_address <- 0
      token_Xstreet <- 0
      token_intersect <- 0
    }
  }
  
} else {
  # Google Maps' API's free querying limits are:
  #   - 2500/day and 50 queries/s/user
  # with billing enabled:
  #    USD 0.50 / 1000 additional requests, up to 100,000/day max
  
  library(RCurl)      # to download content from web (getURL(), ...)
  library(RJSONIO)    # to parse and manipulate Json (fromJSON(), ...)
  library(plyr)       # to plit/apply/combine
  
  if(is.null(my_APIkey)) { stop("\n     -------------------------\n
                                Valid Google Developers API key required.
                                \n      -------------------------\n") }
  Nquery <- 0
  
  while (ii < length(checkZIP_idx)) {
    ii <- ii+1
    zz <- checkZIP_idx[ii]          # ZIP index value in list
    cat(paste(ii,"processed.\n"))
    if (as.numeric(Nquery) >= 0.98 * max_query){ break }
    
    if (GPS_lst[zz] != ""
        & token_GPS != 1) {
      sep_loc <- as.numeric(gregexpr(pattern =',',GPS_lst[zz]))
      GPS_coord <- paste0(substr(GPS_lst[zz],2,sep_loc-1),",",substr(GPS_lst[zz],sep_loc+1,nchar(GPS_lst[zz])-1))
      gM_out=geoCodeF(address="",latlng=GPS_coord,verbose=F,as.numeric(Nquery))
      # 'geoCodeF()' returns c(zip, lat, lng, fail_token, Nquery) 
      # with mode of c(gM_out) "character"
      # 'Sys.sleep(APIsleep)' already built in 'geoCodeF()'
      # failure to return zip when fail_token gM_out[4] = 1, success when = 0
      Nquery <- as.numeric(gM_out[5])  # keep track of queries
      
      if (gM_out[4] != "1") {
        ZIP[zz] <- gM_out[1]
        token_address <- 0
        token_Xstreet <- 0
        token_intersect <- 0
      } else {
        token_GPS <- 1
        ZIP[zz] <- ""
        ii <- ii -1
      }
      
    } else if (as.character(protoY$Address[zz]) != "" 
               & token_address != 1) {
      gM_out=geoCodeF(address,latlng="",verbose=F,as.numeric(Nquery))
      Nquery <- gM_out[5]
      
      if (gM_out[4] != "1") {
        ZIP[zz] <- gM_out[1]
        GPS_lst[zz] <- paste0("(",gM_out[2],", ",gM_out[3],")")
        Nquery <- as.numeric(gM_out[5])
        token_GPS <- 0
        token_Xstreet <- 0
        token_intersect <- 0
      } else {
        token_address <- 1
        ZIP[zz] <- ""
        ii <- ii -1
      }
      
    } else if ( as.character(protoY$Xstreet_1[zz]) != "" 
                & as.character(protoY$Xstreet_2[zz]) != ""
                & token_Xstreet != 1  ) {
      street_A <- as.character(protoY$Xstreet_1[zz])
      street_B <- as.character(protoY$Xstreet_2[zz])
      address <- paste0(street_A," + ",street_B,", New York,NY, USA")
      gM_out=geoCodeF(address,latlng="",verbose=F,as.numeric(Nquery))
      Nquery <- as.numeric(gM_out[5])
      
      if (gM_out[4] != "1") {
        ZIP[zz] <- gM_out[1]
        GPS_lst[zz] <- paste0("(",gM_out[2],", ",gM_out[3],")")
        token_GPS <- 0
        token_address<- 0
        token_intersect <- 0
      } else {
        token_Xstreet <- 1
        ZIP[zz] <- ""
        ii <- ii -1
      }
      
    } else if (as.character(protoY$Intersect_1[zz]) != "" 
               & as.character(protoY$Intersect_2[zz]) != ""
               & token_intersect != 1) {
      street_A <- as.character(protoY$Intersect_1[zz])
      street_B <- as.character(protoY$Intersect_2[zz])
      address <- paste0(street_A," + ",street_B,", New York,NY, USA")
      gM_out=geoCodeF(address,latlng="",verbose=F,as.numeric(Nquery))
      Nquery <- as.numeric(gM_out[5])
      
      if (gM_out[4] != "1") {
        ZIP[zz] <- gM_out[1]
        GPS_lst[zz] <- paste0("(",gM_out[2],", ",gM_out[3],")")
        token_GPS <- 0
        token_address<- 0
        token_Xstreet <- 0
      } else {
        token_intersect <- 1
        ZIP[zz] <- ""
        ii <- ii -1
      } 
    } else {
      imputeTo99999_idx <- c(imputeTo99999_idx, zz)
      token_GPS <- 0
      token_address <- 0
      token_Xstreet <- 0
      token_intersect <- 0
    }
    
  }
  
  cat("\n\nTotal nbr of https API\'s queries reached:",Nquery,"\n\n")
  
  }

# Re-build 'protoY'
ZIP[imputeTo99999_idx] <- "99999"
protoY <- cbind(ZIP=unlist(ZIP,use.name=F),protoY[,c(2:12)],GPS=unlist(GPS_lst,use.name=F))
length(which(! protoY$ZIP %in% c(ZIP_lst,"99999"))) 
protoY <- protoY[which(protoY$ZIP %in% c(ZIP_lst,"99999")),]
cat("Nbr of unique ZIPs in dataset:",length(unique(as.character(unlist(protoY$ZIP)))))
# Observe:
  # 247 in April-2014 dataset
  # 236 in April-2015 dataset



# #############################
# Save processed csv file as `*_proc03.csv`
# #############################
proc_file <- source_file
target_file <- paste0("Data/",proc_file,"03.csv") 
csvSaveF(protoY,target_file)     # csv to disk
