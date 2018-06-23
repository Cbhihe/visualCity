# #############################
## MIRI Project:    Geosociological Analysis of NYC-311 Service Requests
## Author:          Cedric Bhihe
## Delivery:        2018.06.26
## Script:          00_nyc311_input-parameters.R
# #############################


# #############################
## Google Maps specific API flag        (set 'geolocalF'  to TRUE to use)
# #############################
# geolocalF=TRUE   # Use Google Maps API
geolocalF=FALSE  # Do not use Google Maps API
if (geolocalF) {
    source("/home/ckb/.apikeys/googlemaps.apikey")
    max_query <- 2500               # API's limit for nbr of free https queries/day = 2500
}

# #############################
## Input parameters
# #############################

## Analysis period
#timeWindow <-"monthly"         # sliding time window not implemented yet
yearNbr <- 2014
monthNbr <- 4
dayNbr <- 1

# #############################
## Miscellanea
# #############################
set.seed(932178)

options(scipen=6) # R switches to sci notation above 5 digits on plot axes
ccolors=c("red","green","blue","orange","cyan","tan1","darkred","honeydew2","violetred",
          "palegreen3","peachpuff4","lavenderblush3","lightgray","lightsalmon","wheat2")

datestamp <- format(Sys.time(),"%Y%m%d-%H%M%S")
