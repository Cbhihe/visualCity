# #############################
## Project:     Analysis of NYC-311 Service Requests
## Script:      01_nyc311_input-parameters.R
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Last edit:   September 2018
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
#timeWindow <-"monthly"         # sliding time window not implemented yet
yearNbr <- 2010
monthNbr <- 4
dayNbr <- 1

# #############################
## 
# #############################

datestamp <- format(Sys.time(),"%Y%m%d-%H%M%S")
