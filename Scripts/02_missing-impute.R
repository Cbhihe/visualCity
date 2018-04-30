##  MIRI:     MVA
##  Project:  Geographical Analysis of NYC 311 Service Requests (April 2014 vs April 2015)  
##  Authors:  Cedric Bhihe <cedric.bhihe@gmail.com>
##            Santi Calvo <s.calvo93@gmail.com>  
##  Delivery: 2018.06.26

# #############################
##  Script name: missing-impute_project_mva.R
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
library("VIM")        # Visualization and Imputation of Missing values
                      # ;ethods aggr(),barmiss(), histmiss() for missing/imputed values
library("ggplot2")
library("zipcode")    # Load all valid us ZIPs  <<<<<<<   OUTDATED


# #############################
## Google Maps specific API         (set 'geolocalF'  to TRUE to use)
# #############################
geolocalF=TRUE   # Use Google Maps API
# geoloacF="F     # Do not use Google Maps API
my_APIkey <- "_______________"  # Google Maps API's private key
max_query <- 2500               # API's limit for nbr of free https queries/day = 2500


# #############################
## ZIP of interest
# #############################
#  list must include exactly 5 digits ZIP codes with leading "0" is necessary
#  e.g. zip_lst <- as.character(c("08001","10001":"14999"))

data("zipcode") # load US zip data
ZIP_lst <- c(unlist(zipcode[zipcode$state=="NY",1]),
             c("10004","10018","10024","10029","10032","10036","10309","10459","10463","10469","10472","11103", "11207",
               "11210", "11221","11229","11235","11368","11379","11385","11421","11435","11694"))
# ZIP_lst <- as.character(c("00501","00544","06390","10001":"14999"))


# #############################
## Analysis period
# #############################
yearNbr <- 2015
monthNbr <- 4
dayNbr <- 1


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

plottitle <- paste0("NYC 311 Calls (April ",yearNbr,")")
subtitle="(Whole data set)"
barplot(sort(table(protoY$Complaint),decreasing = T),
        horiz=F,
        beside=T,
        legend.text=F,
        axes=T,
        las=2,
        cex.lab=1,
        ylab="Nbr of service requests (calls)",
        #log="y",
        cex.axis=0.8,
        cex=0.9,
        col=ccolors[5],
        main=plottitle)
text(length(unlist(labels(table(protoY$Complaint)))),
     max(as.numeric(unlist(table(protoY$Complaint)))),
     pos=1,
     subtitle)

# #############################
## Analysis of missings
# #############################

dim(protoY)
class(protoY)
summary(protoY[,c(1,2,3,12)])


nrow(protoY[protoY$ZIP == "" | is.na(protoY$ZIP),]) # (In April 2014) 3206 obs w/ missing ZIPs (no NA)
nrow(protoY[protoY$GPS == "" | is.na(protoY$GPS),]) # (In April 2014) 7777 obs w/ missing GPS coords (no NA)
nrow(protoY[protoY$ZIP == "" & protoY$GPS == "",])  # (In April 2014) 3158 obs w/ missing ZIP and GPS coords
nrow(protoY[protoY$ZIP == "" & protoY$Address == "" & protoY$GPS == "",]) 
        # (In April 2014) 2979 records, where ZIP, Address and GPS coords are simultaneously missing

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

#   Such obs are useless and should be eliminated.
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
                                      & is.na(protoYmissing$Xstreet_1)
                                      & is.na(protoYmissing$Intersect_1)
                                      & is.na(protoYmissing$planeX)
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
                       Dataset=c(rep("Whole",Nlab),rep("Loc-missing",Nlab))
                       )
# To order barplot per decreasing mod. count, either:
plotdata$Modalities <- factor(plotdata$Modalities,level=names(vec1))
ggplot(data=plotdata,mapping=aes(x=Modalities,y=Service_calls_count,fill=Dataset)) + 
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
# The other affected modality is"Misc". Being non-descript it is of lesser import.
cat("\nSuppressed fraction of modality \'SocServ\':",
    round(vec2[names(vec2)=="SocServ"]/vec1[names(vec1)=="SocServ"]*100,2),
    "%")
cat("Suppressed fraction of modality \'Misc\':",
    round(vec2[names(vec2)=="Misc"]/vec1[names(vec1)=="Misc"]*100,2),
    "%\n\n")

## Remove obs where all loc info is missing
allmissing_indices <- which(is.na(protoYmissing$ZIP)
                                          & is.na(protoYmissing$Address)
                                          & is.na(protoYmissing$Xstreet_1)
                                          & is.na(protoYmissing$Intersect_1)
                                          & is.na(protoYmissing$planeX)
                                          & is.na(protoYmissing$GPS))
protoY <- protoY[-allmissing_indices,]

## Cleanup
rm(missingZIP,missingZIP_aggr,protoYmissing, 
   mod_allmissing,mod_labels, 
   plottitle, subtitle,vec1,vec2)



# #############################
## Fill missing ZIPs and GPS coordinates (via Google Maps queries)
# #############################

## Build list of indices for obs whose ZIPs either are missing or do not conform to the 
# standard ZIPs for NY state, per data-frame 'zipcode'. 
# See https://gist.github.com/dryan/7486408 for list of valid/invalid ZIP codes in the USA

ZIP <- as.character(unlist(protoY$ZIP))
# if "-" exists, then trim from "-" to end of string, else keep ZIP unchanged
ZIP <- ifelse(as.numeric(gregexpr(pattern ='-',ZIP))>0,
              substr(ZIP,1,as.numeric(gregexpr(pattern ='-',ZIP))-1),
              ZIP)

ZIP[which(ZIP=="N/A")] <- ""

ZIP <- ifelse(nchar(ZIP)==0 | nchar(ZIP)==5,
              ZIP,
              ifelse(nchar(ZIP) >5,
                     NA,
                     paste0(formatC(as.numeric(ZIP),width=5,flag="0")))
              )

checkZIP_indices <- which( ! ZIP %in% ZIP_lst ) # list of problematic ZIPs' indices


# Build complete list of GPS coordinates
GPS_lst <- as.character(unlist(protoY$GPS))

discardZIP_indices<- c()
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
  
  while (ii < length(checkZIP_indices)) {
    ii <- ii+1
    zz <- checkZIP_indices[ii]          # ZIP index value in list
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
      discardZIP_indices <- c(discardZIP_indices, zz)
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
  
  if(is.null(my_APIkey)) { stop("\n-------------------------\n
                                Valid Google Developers API key required.
                                \n-------------------------\n") }
  Nquery <- 0
  
  while (ii < length(checkZIP_indices)) {
    ii <- ii+1
    zz <- checkZIP_indices[ii]          # ZIP index value in list
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
      discardZIP_indices <- c(discardZIP_indices, zz)
      token_GPS <- 0
      token_address <- 0
      token_Xstreet <- 0
      token_intersect <- 0
    }
    
  }
  
  # ##############  
  cat("\n\nTotal nbr of https API\'s queries reached:",Nquery,"\n\n")
  
  }

# Re-build 'protoY'
protoY <- cbind(ZIP=unlist(ZIP,use.name=F),protoY[,c(2:12)],GPS=unlist(GPS_lst,use.name=F))
protoY <- protoY[-discardZIP_indices,]
#protoY <- protoY[nchar(ZIP) <= 5,]
Y <- protoY[which(protoY$ZIP %in% ZIP_lst),]  
# & substr(protoY$ZIP,2,5) != "0000" # additional test  necessary ?
cat("Nbr of unique ZIPs in dataset:",length(unique(as.character(unlist(Y$ZIP)))))
# Observe:
  # 2__ in April-2014 dataset
  # 234 in April-2015 dataset

# #############################
# Save processed csv file as `*_proc03.csv`
# #############################
proc_filename <- source_file
target_filename <- paste0("Data/",proc_filename,"03.csv") 
csvSaveF(protoY,target_filename)     # csv to disk

