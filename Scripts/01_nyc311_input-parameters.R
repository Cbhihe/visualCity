# #############################
## Project:     Analysis of NYC-311 Service Requests
## Script:      01_nyc311_input-parameters.R
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Last edit:   November 2018
# #############################


ccolors=c("red","green","blue","orange","cyan","tan1","darkred","honeydew2","violetred",
          "palegreen3","peachpuff4","lavenderblush3","lightgray","lightsalmon","wheat2")


# #############################
## Google Maps specific API flag        (set 'geolocalF'  to TRUE to use)
# #############################
# Define 'geolocalF' parameter in Google's geocode API calling script

if (! exists("geolocalF", mode="logical") ) geolocalF=FALSE   # default to FALSE when not defined

if (geolocalF) {
    home <- Sys.getenv("HOME")
    source(paste0(home,"/.apikeys/google-geocode.apikey"))
    # max_query <- 2500 # API's limit for nbr of free https queries/day = 2500
                        # Does not apply anymore
    APIsleep <- 0.02 # maximum query rate accepted by Google Maps is 50 queries/s/user or 1 query/0.02s/user
}



# #############################
## Input parameters
# #############################

## Analysis period
MONTHLST <- c("january","february","march","april","may", "june","july","august","september","october","november","december")
MONTHLSTabb <- substr(month_lst,1,3)

#timeWindow <-"monthly"         # sliding time window not implemented
yearNbr <- 2018
monthNbr <- 4
dayNbr <- 1

# Specify periods of interest for time evolution (dynamic analysis) studies.
dynAnalysisF <- TRUE   # value: TRUE or 1 for temporal evolution analysis; otherwise FALSE or 0
periodsOfInterest <- c("April 2010 ","April 2014","April 2018")
periodOfReference <- "April 2010"  # period of reference for time evolution studies.

# Formats should be:
#           'month year'    (MMM YYYY) 
# and/or    'month year'    (MM YYYY)
# exor      'year'          (YYYY)
# Do not mix those formats, e.g. as in 'c("April 2014","May 2018","2017")'
# Leading and trailing spaces are ignored. 


# #############################
## Misc
# #############################

datestamp <- format(Sys.time(),"%Y%m%d-%H%M%S")
