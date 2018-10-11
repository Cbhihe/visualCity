# #############################
## Project:     Analysis of NYC-311 Service Requests
## Script:      02_nyc311_data-prep.R
## Author:      Cedric Bhihe
## Delivery:    January 2019
## Last edit:   
# #############################

rm(list=ls(all=TRUE))

# #############################

setwd("~/Documents/Work/Academic-research/NYC311/")

options(scipen=6) # R switches to sci notation above 5 digits on plot axes
set.seed(932178)
ccolors=c("blue","red","green","orange","cyan","tan1","darkred","honeydew2","violetred",
          "palegreen3","peachpuff4","lavenderblush3","lightgray","lightsalmon","wheat2")

datestamp <- format(Sys.time(),"%Y%m%d-%H%M%S"); 


# ############################################
## Repos and libraries
# ############################################

setRepositories(ind = c(1:6,8))
#setRepositories()   # to specify repo on the fly:
#chooseCRANmirror()  # in case package(s) cannot be downloaded from default repo

#library(xlsx, lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")


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


# #############################
## Source parameter file
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
## Import *.cvs data
# #############################
source_file <- paste0(yearNbr,
                   ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                   "00_nyc311_raw")

protoY_raw <- read.csv(paste0("Data/",source_file,".csv"),
                   header=T,
                   sep=",",
                   quote="\"",
                   dec=".")


# #############################
## Clean-up 1: General categorical variables
# #############################

#agencyAcronym <-unique(protoY[,4:5])

protoY <- cbind(protoY_raw[,9],          # choose relevant columns to keep + reorganize
                as.Date(protoY_raw[,2],"%m/%d/%Y"),
                protoY_raw[,-c(1:5,8:9,11,16,18:24,27:37)])

colnames(protoY)[c(1:13)] <- c("ZIP",
                               "Date",
                               "Complaint",
                               "Descriptor",
                               "Address",
                               "Xstreet_1", "Xstreet_2",
                               "Intersect_1", "Intersect_2",
                               "City",
                               "planeX", "planeY",
                               "GPS")

# Keep only relevant year,month
# protoY <- protoY[protoY$Date %in% periodStart:periodEnd,]   # ok for df ?
protoY <- protoY[as.Date(protoY$Date) %in% periodStart:periodEnd,]
# names(protoY)       # list columns kept so far

# Save processed raw csv file as `*_proc01.csv``
proc_file <-sub("raw","proc",source_file)
target_file <- paste0("Data/",proc_file,"01.csv") 
csvSaveF(protoY,target_file)         # commit csv to disk


# #############################
## Clean-up 2: Complaints
# #############################
class(protoY$Complaint)         # returns "factor"
# extract "Complaint" and "Descriptor" to speed up string replacement
Ycplt_full <- as.data.frame(protoY[,c("Complaint","Descriptor")])
# make it matrix/ with column vectors/lists for easier handling
Ycplt <- Ycplt_full[,1]
complaint_lst <- unique(Ycplt)
cat("\n\nService request modalities:",length(complaint_lst),"\n\n")   # unique complaint types in service requests 
#(sort(complaint.lst,decreasing = F))


## How to manipulate
cto <- table(protoY$Complaint)           # cto = "Complaint Type Occurences", as 'factors'
# cto[names(cto)=="Water System"]        # match complaint to specific type
# names(cto[cto <= 5])                   # list complaint types which occur 5 or fewer times
# which(names(cto)=="Water System")      # extract list index of complaint type
# cto[names(cto)=="Water System"]        # extract nbr of occurences with 'name'
# cto[[205]]                             # extract nbr of occurences at index 205
# cto[names(cto)=="Water System"][[1]]   # extract nbr of occurences for specific complaint
sink_file <- paste0("Report/",proc_file,"01_modalities.csv")
sink(file=sink_file,append=F,split=F)
cat("Service Requests\' NYC 311 Calls\' unprocessed",
    length(complaint_lst),
    "modalities.\n")
cat("Period:",as.character(periodStart),"-",as.character(periodEnd),"\n\n")
raw_mod <- as.matrix(cbind(row.names(cto[order(cto, decreasing = T)]),cto[order(cto, decreasing = T)]))
cat("Modality","Obs_count","\n",sep=",") 
apply(raw_mod,1,function(x) cat(x[1],x[2],"\n",sep=","))  # list complaint types in decreasing order of frequency
sink()

## Manipulate with df
# cto.df <- as.data.frame(cto)
# attach(cto.df)   # place data frame in position 2 on search path 
# sort(cto,decreasing=T)


# Dimensional reduction in variables of type COMPLAINT
nRowsY <- length(protoY$Complaint)
maxNrows=90000
nDiv <- (floor(nRowsY/maxNrows))+1  
# processing intervals' upper bounds
uBound_lst <- lapply(seq(1:nDiv)*maxNrows,function(x) min(nRowsY,x)) 
#Ycplt_acc <- matrix("",ncol=1,nrow=nrow(Y))
Ycplt_acc <- c()
  
cat("Processing complaints:\n")
for (dd in 1:nDiv) {
  lBound=(dd-1)*maxNrows+1
  uBound=uBound_lst[[dd]]
  cat(" * data interval",dd,":",lBound,"-",uBound,"\n")
  # Replace "positive complaints" with "Kudos"
  Ycplt_p <- lapply( Ycplt[lBound:uBound],function(x) sub("ADOPT-A-BASKET|TAXI COMPLIM.*",
                                                      "IAO",x,ignore.case=T,perl=T) )  # originally categorized as "Kudos"
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("GENERAL.*|RANGEHOOD|^(.* )*HEAT.*|PAINT.*|FLOORING.*|WATER LEAK|.*PLUMBING.*|BOILER.*|ASBESTOS|INDOOR.*|ELEVATOR.*|FIRE ALARM.*|ELECTRIC.*|DOOR.*|OUTSIDE BUILDING|APPLIANCE|BUILDING CONDITION|BUILDING\\/USE|MOLD|WINDOW GUARD|AIR QUALITY",
                                        "HousCond",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("UNSANITARY.*|SANITATION.*|.*SMELL.*|.*GRAFFITI.*|.*DIRTY.*|.*RECYCLING.*|MISSED COLLECTION.*|STANDING WATER|DRINKING WATER|WATER QUALITY|PUBLIC TOILE.*|.*LITTER.*|PLANT|VACANT LOT|SWEEPING.*|FOOD ESTABLISHMENT",
                                        "Sani",x,ignore.case=T,perl=T) ) # Sanitation
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("RODENT.*",
                                             "Sani",x,ignore.case=T,perl=T) )   # Originally categorized as "Rodent"
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^HAZARDOUS MATER.*|.*SNAD.*|HAZMAT.*|POISON IVY|ILLEGAL ANIMAL.*|^(.* )+TREE.*|GAS STATION DISCHARGE LINES|INDUSTRIAL WASTE|LEAD|WATER CONSERVATION|RADIOACTIVE.*|ANIMAL ABUSE|X-RAY.*|DPR INTERNAL|ANIMAL IN A PARK",
                                        "EnvProt",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^SEWER.*|^WATER SYSTEM",
                                        "WaterSyst",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^NOISE - COMM.*",
                                        "NoiseBiz",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^NOISE - RESID.*|^NOISE - PARK.*|^NOISE - HOUSE OF WORSHIP",
                                        "NoiseResid",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^NOISE - VEH.*|^NOISE - STREET.*|COLLECTION TRUCK NOISE|NOISE.*HELICOPTER",
                                        "NoiseTraf",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^NOISE - CONSTRUCT.*",
                                        "NoiseConst",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^NOISE$",
                                        "NoiseXX",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^BLOCKED DRI.*|^ILLEGAL PARK.*|DERELICT VEH.*|DERELICT BIC.*|TRAFFIC",
                                        "Traffic",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^CONSUMER.*|^TAXI.*|^FOOD.*|BEACH/POOL\\/SAUNA.*|LEGAL SERVICE.* PROVIDER.*|TRANSPORTATION PROVIDER.*|CALORIE LABELING|FALSE ADVERT.*|TRANS FAT.*|BOTTLED WATER|FOR HIRE VEHICLE.*",
                                        "ConsumProt",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("HOME DELIVERED MEAL.*|HOUSING OPTIONS|BENEFIT CARD REPLACEMENT|ALZHEIMER'S CARE|HOMELESS.*|SENIOR CENTER.*|HOME CARE PROVIDER.*|BEREAVEMENT SUPPORT.*|DHS.*|.*MEALS HOME.*|HOUSING - LOW.*|SCRIE|ELDER.*|HEAP ASSISTANCE",
                                        "SocServ",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("HARBORING BEES.*|SCAFFOLD.*|BEST\\/SITE.*|OPEN FLAME.*|ILLEGAL FIRE.*|LIFEGUARD|FIRE SAFETY DIRECTOR.*|.*SPIT.*|SAFETY|SNOW|.*SPRINKLER.*",
                                        "IAO",x,ignore.case=T,perl=T) )  # originally categorized in "Safety"
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("STREET COND.*|STALLED SITES|MAINTENANCE OR FACILITY|EMERGENCY RESPONSE.*|.*SIDEWALK COND.*|BRIDGE COND.*|BUS STOP.*|HIGHWAY.*|BIKE RACK.*|STREET LIGHT.*|^BROKEN MUNI.*|BROKEN PARKING.*|CURB.*|STREET SIGN.*|MUNICIPAL PARKING FACILITY|TRAFFIC SIGNAL.*|PUBLIC PAYPHONE.*|SCHOOL MAINTENANCE|NON-RESIDENTIAL HEAT|TUNNEL COND.*",
                                        "UrbInf",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("PUBLIC ASSEMBLY.*|SQUEEGEE|CASE MANAGEMENT.*|INVESTIGATIONS AND DISCIPLINE.*|TANNING|.*CONSTRUCTION.*|DOF LITERATURE REQUEST|INVESTIGATIONS AND DISCIPLINE.*|DOF PROPERTY.*|PANHANDLING|VENDING|F?ATF|DISORDERLY.*|DRINKING.*|SMOKING.*|VIOLATION.*|URINATING.*|ANIMAL FACILITY - NO.*|.*PLACARD.*|UNLEASHED.*|NON-EMERGENCY POLICE.*|BIKE\\/ROLLER\\/SKATE CHRONIC|CRANES AND DERRICKS|TATTOOING|POSTING ADVERT.*",
                                        "IAO",x,ignore.case=T,perl=T) )  # Inspect, Audit and Order
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("DCA \\/ DOH NEW LICENSE.*|OTHER ENFORCE.*|FERRY.*|DOF PARKING -.*|SRDE|PARKING CARD|FOUND PROPERTY|MISCELLA.*",
                                        "IAO",x,ignore.case=T,perl=T) )   # originally categorized in "Misc"
  
  Ycplt_acc <- c(Ycplt_acc,Ycplt_p)
  }

cat("Processing of service requests ended.\n")

Y <- cbind(protoY[,c(1,2)],Complaint=unlist(Ycplt_acc,use.name=F),protoY[,c(5:13)])

Y_wk <- as.matrix(Y)
# Additional dimensional reduction based on Y$Complaint and Y$Descriptor
# List unique combinations of type of complaint and corresponding `Y$Descriptor` values
unique(Ycplt_full[Y$Complaint=="NoiseXX",2])  # for NOISE

list_1 <- which(Y$Complaint=="NoiseXX")  # row indices where NOISE occurs in Y$Complaint

list_2 <- grep("CONSTRUC|JACK HAMMER",toupper(Ycplt_full$Descriptor))  # row indices where CONSTRUC occurs in Y$Descriptor
Y_wk[as.vector(intersect(list_1,list_2)),3] <- "NoiseConst"
cat(length(intersect(list_1,list_2)),"changes.\n")

list_2 <- grep("MANUFACTURING|ALARM|ICE CREAM TRUCK",toupper(Ycplt_full$Descriptor))
Y_wk[as.vector(intersect(list_1,list_2)),3] <- "NoiseBiz"
cat(length(intersect(list_1,list_2)),"changes.\n")

list_2 <- grep("BARKING|MOOING|LAUGHING|TRUMPETING|BRAYING|SHOUT|SCREAM|LAWN CARE|LOUD MUSIC|AIR CONDITION|PRIVATE CARTING|OTHER ANIMAL",toupper(Ycplt_full$Descriptor))
Y_wk[as.vector(intersect(list_1,list_2)),3] <- "NoiseResid"
cat(length(intersect(list_1,list_2)),"changes.\n")

list_2 <- grep("BOAT|OTHER NOISE",toupper(Ycplt_full$Descriptor))
Y_wk[as.vector(intersect(list_1,list_2)),3] <- "NoiseTraf"
cat(length(intersect(list_1,list_2)),"changes.\n")

# Y$Complaint <- vapply(Y$Complaint,paste,collapse=", ",character(1L))  # flatten list - attempt 1
# x <- vapply(Y$Complaint, length, 1L)                                  # flatten list - attempt 2
# Y<- Y[rep(rownames(Y), x), ]
# Y$Complaint <- unlist(Y$Complaint, use.names=F)
# Yp <- apply(Y,2,as.character)                                         # flatten list - attempt 3
target_file <- paste0("Data/",proc_file,"02.csv") 
csvSaveF(Y_wk,target_file)                                          # csv to disk

rm(Ycplt_full,Ycplt_acc,Ycplt_p,protoY,protoY_raw)                      # cleanup
