# #############################
## Project:     Analysis of NYC-311 Service Requests
## Script:      01_nyc311_input-parameters.R
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Last edit:   September 2018
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
## 
# #############################

datestamp <- format(Sys.time(),"%Y%m%d-%H%M%S")
