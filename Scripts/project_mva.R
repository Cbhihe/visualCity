##  MIRI:     MVA
##  Project:  Geographical Analysis of NYC 311 Service Requests (April 2014 vs April 2015)  
##  Authors:  Cedric Bhihe <cedric.bhihe@gmail.com>
##            Santi Calvo <s.calvo93@gmail.com>  
##  Delivery: 2018.06.26

# #############################
##  Script name: project_mva.R
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
library("VIM")  # for visualization of missing and imputed values
library(qdapTools)  # for easy manipulation of lists combined with df


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
## Import *.cvs data
# #############################
yearNbr <- 2015
monthNbr <- 4
dayNbr <- 1  

## NOTE:
# Time window fixed to one month. Neither an arbitrary sliding time 
# window nor any other functionality is implemented.  
# timeWin <-"monthly"         # sliding time window not implemented yet

periodStart <- as.Date(paste0(yearNbr,"-",
                              ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),"-",
                              ifelse(dayNbr<10,paste0("0",as.character(dayNbr)),as.character(dayNbr))))

daysInMonth <- as.numeric(difftime(addMonthF(periodStart,1),periodStart))

periodEnd <- as.Date(paste0(yearNbr,"-",
                            ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),"-",
                            daysInMonth))

source_file <- paste0(yearNbr,
                   ifelse(monthNbr<10,paste0("0",as.character(monthNbr)),as.character(monthNbr)),
                   "00_nyc311_raw")

protoY_raw <- read.csv(paste0("Data/",source_file,".csv"),
                   header=T,
                   sep=",",
                   quote="\"",
                   dec=".")

#agency_acronym <-unique(protoX[,4:5])


# #############################
## Clean-up 1: Choose relevant year,month, columns to keep + reorganize
# #############################
protoY <- cbind(protoY_raw[,9],
           as.Date(protoY_raw[,2],"%m/%d/%Y"),
           protoY_raw[,-c(1:5,8:9,11,16,18:24,27:37)])

colnames(protoY)[c(1:13)] <- c("ZIP","Date","Complaint","Descriptor","Address",
                                           "Xstreet_1","Xstreet_2","Intersect_1","Intersect_2",
                                           "City","planeX","planeY","GPS")
Y <- protoY[protoY$Date %in% periodStart:periodEnd,]   # ok for df
# names(Y)  # list columns kept so far

# Save processed raw csv file as `*_proc.csv``
proc_filename <-sub("raw","proc",source_file)
target_filename <- paste0("Data/",proc_filename,"01.csv") 
csvSaveF(Y,target_filename)     # csv to disk
rm(protoY_raw)


# #############################
## Clean-up 2: Complaints
# #############################
class(Y$Complaint)                     # returns "factor)
# extract "Complaint" and "Descriptor" to speed up string replacement
Ycplt_full <- as.data.frame(Y[,c("Complaint","Descriptor")])
# make it matrix/ with column vectors/lists for easier handling
Ycplt <- Ycplt_full[,1]
complaint_lst <- unique(Ycplt)
length(complaint_lst)                  # unique complaint types in service requests 
#(sort(complaint.lst,decreasing = F))


## How to manipulate
cto <- table(Y$Complaint)                # cto: "Complaint Type Occurences" as 'factors'
# cto[names(cto)=="Water System"]        # match complaint to specific type
# names(cto[cto <= 5])                   # list complaint types which occur 5 or fewer times
# which(names(cto)=="Water System")      # extract list index of complaint type
# cto[names(cto)=="Water System"]        # extract nbr of occurences with 'name'
# cto[[205]]                             # extract nbr of occurences at index 205
# cto[names(cto)=="Water System"][[1]]   # extract nbr of occurences for specific complaint
sort(cto, decreasing = T) # list complaint types in decreasing order of frequency

## Manipulate with df
# cto.df <- as.data.frame(cto)
# sort(cto,decreasing=T)


# Dimensional reduction in variables of type COMPLAINT
nRowsY <- length(Y$Complaint)
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
                                                      "Kudos",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("GENERAL.*|RANGEHOOD|HEAT.*|PAINT.*|FLOORING.*|WATER LEAK|.*PLUMBING.*|BOILER.*|ASBESTOS|INDOOR.*|ELEVATOR.*|FIRE ALARM.*|ELECTRIC.*|DOOR.*|OUTSIDE BUILDING|APPLIANCE|BUILDING CONDITION|BUILDING\\/USE|MOLD|WINDOW GUARD|AIR QUALITY",
                                        "HousCond",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("UNSANITARY.*|SANITATION.*|.*SMELL.*|.*GRAFFITI.*|.*DIRTY.*|.*RECYCLING.*|MISSED COLLECTION.*|STANDING WATER|DRINKING WATER|WATER QUALITY|PUBLIC TOILE.*|.*LITTER.*|PLANT|VACANT LOT|SWEEPING.*|FOOD ESTABLISHMENT",
                                        "Sani",x,ignore.case=T,perl=T) ) # Sanitation
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("RODENT.*",
                                        "Rodent",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^HAZARDOUS MATER.*|.*SNAD.*|HAZMAT.*|POISON IVY|ILLEGAL ANIMAL.*|^(.* )+TREE.*|GAS STATION DISCHARGE LINES|INDUSTRIAL WASTE|LEAD|WATER CONSERVATION|RADIOACTIVE.*|ANIMAL ABUSE|X-RAY.*|DPR INTERNAL|ANIMAL IN A PARK",
                                        "EnvProt",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^SEWER.*|^WATER SYSTEM",
                                        "WaterSyst",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^NOISE - COMM.*",
                                        "NoiseBiz",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^NOISE - RESID.*|^NOISE - PARK.*|^NOISE - HOUSE OF WORSHIP",
                                        "NoiseResid",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^NOISE - VEH.*|^NOISE - STREET.*|COLLECTION TRUCK NOISE|NOISE HELICOPTER",
                                        "NoiseTraf",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^NOISE - CONSTRUCT.*",
                                        "NoiseConst",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^NOISE$",
                                        "NoiseXX",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^BLOCKED DRI.*|^ILLEGAL PARK.*|DERELICT VEH.*|DERELICT BIC.*|TRAFFIC",
                                        "Traffic",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("^CONSUMER.*|^TAXI.*|^FOOD.*|BEACH/POOL\\/SAUNA.*|LEGAL SERVICE PROVIDER.*|TRANSPORTATION PROVIDER.*|CALORIE LABELING|FALSE ADVERT.*|TRANS FAT.*|BOTTLED WATER|FOR HIRE VEHICLE.*",
                                        "ConsumProt",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("HOME DELIVERED MEAL.*|BENEFIT CARD REPLACEMENT|ALZHEIMER'S CARE|HOMELESS.*|SENIOR CENTER.*|HOME CARE PROVIDER.*|BEREAVEMENT SUPPORT.*|DHS.*|.*MEALS HOME.*|HOUSING - LOW.*|SCRIE|ELDER.*|HEAP ASSISTANCE",
                                        "SocServ",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("HARBORING BEES.*|SCAFFOLD.*|BEST\\/SITE.*|OPEN FLAME.*|ILLEGAL FIRE.*|LIFEGUARD|FIRE SAFETY DIRECTOR.*|.*SPIT.*|SAFETY|SNOW|.*SPRINKLER.*",
                                        "Safety",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("STREET COND.*|STALLED SITES|MAINTENANCE OR FACILITY|EMERGENCY RESPONSE.*|.*SIDEWALK COND.*|BRIDGE COND.*|BUS STOP.*|HIGHWAY.*|BIKE RACK.*|STREET LIGHT.*|^BROKEN MUNI.*|BROKEN PARKING.*|CURB.*|STREET SIGN.*|MUNICIPAL PARKING FACILITY|TRAFFIC SIGNAL.*|PUBLIC PAYPHONE.*|SCHOOL MAINTENANCE|NON-RESIDENTIAL HEAT|TUNNEL COND.*",
                                        "UrbInf",x,ignore.case=T,perl=T) )
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("PUBLIC ASSEMBLY.*|CASE MANAGEMENT.*|INVESTIGATIONS AND DISCIPLINE.*|TANNING|.*CONSTRUCTION.*|DOF LITERATURE REQUEST|INVESTIGATIONS AND DISCIPLINE.*|DOF PROPERTY.*|PANHANDLING|VENDING|ATF|DISORDERLY.*|DRINKING.*|SMOKING.*|VIOLATION.*|URINATING.*|ANIMAL FACILITY - NO.*|UNLEASHED.*|NON-EMERGENCY POLICE.*|BIKE\\/ROLLER\\/SKATE CHRONIC|CRANES AND DERRICKS|TATTOOING|POSTING ADVERT.*",
                                        "IAO",x,ignore.case=T,perl=T) )  # Inspect, Audit and Order
  Ycplt_p <- lapply( Ycplt_p,function(x) sub("DCA \\/ DOH NEW LICENSE.*|OTHER ENFORCE.*|FERRY.*|DOF PARKING -.*|SRDE|PARKING CARD|FOUND PROPERTY|MISCELLA.*",
                                        "Misc",x,ignore.case=T,perl=T) )
  
  Ycplt_acc <- c(Ycplt_acc,Ycplt_p)
  }

cat("Complaint processing ended.\n")

Yp <- Y
Y <- cbind(Y[,c(1,2)],t(Ycplt_acc),Y[,c(5:13)])
colnames(Y)[3] <- "Complaint"
Yp <- Y


# Additional dim reduction based on Y$Complaint and Y$Descriptor
# List unique combinations of type of complaint and corresponding `Y$Descriptor` values
unique(Ycplt_full[Y$Complaint=="NoiseXX",2])  # for NOISE

list_1 <- as.numeric(rownames(Y[Y$Complaint=="NoiseXX",]))  # row indices where NOISE occurs in Y$Complaint

list_2 <- grep("CONSTRUC|JACK HAMMER",toupper(Ycplt_full$Descriptor))  # row indices where CONSTRUC occurs in Y$Descriptor
Y[as.vector(intersect(list_1,list_2)),3] <- "NoiseConst"

list_2 <- grep("MANUFACTURING|ALARM|ICE CREAM TRUCK",toupper(Ycplt_full$Descriptor))
Y[as.vector(intersect(list_1,list_2)),3] <- "NoiseBiz"

list_2 <- grep("BARKING|LAWN CARE|LOUD MUSIC|AIR CONDITION|PRIVATE CARTING|OTHER ANIMAL",toupper(Ycplt_full$Descriptor))
Y[as.vector(intersect(list_1,list_2)),3] <- "NoiseResid"

list_2 <- grep("BOAT(ENGINE|OTHER NOISE",toupper(Ycplt_full$Descriptor))
Y[as.vector(intersect(list_1,list_2)),3] <- "NoiseTraf"

# Y$Complaint <- vapply(Y$Complaint,paste,collapse=", ",character(1L))  # flatten list - attempt 1
# x <- vapply(Y$Complaint, length, 1L)                                  # flatten list - attempt 2
# Y<- Y[rep(rownames(Y), x), ]
# Y$Complaint <- unlist(Y$Complaint, use.names=F)
Yp <- apply(Y,2, as.character)                                         # flatten list - attempt 3
target_filename <- paste0("Data/",proc_filename,"01.csv") 
csvSaveF(Yp,target_filename)     # csv to disk

rm(Ycplt_full,Ycplt_acc,Ycplt_p,protoY,protoY_raw)

# #############################
## Missings' analysis
# #############################

# Missings are essentially records of 311 calls w/o ZIP, address, nor planeXY coordinates
Y_aggr <- aggr(Ypp[,c(2:4,10:12)], 
               numbers=TRUE,
               bars=TRUE,
               combined=FALSE,
               prop=FALSE,
               plot=TRUE,
               axes=TRUE,
               col=ccolors[6:7], 
               labels=names(Ypp[,c(2:4,10:12)]),
               cex.axis=1,
               ylab=c("Missing Data (count)","Missing Data Distribution")
               )
summary(Y_aggr)
# Y_aggr$combinations
# Y_aggr$count
# Y_aggr$percent
# Y_aggr$missings
# Y_aggr$tabcomb

# Replace emty cells with NA 
# Map out missing ZIP and complaint type
# Decide whether eliminating entire ZIP row is the right option.
