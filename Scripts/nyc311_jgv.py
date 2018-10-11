# -*- coding: utf-8 -*-
"""
Created on Wed March 14 09:00:00 2018
@author: Jorge García Vidal
"""

import numpy as np
#import math as mat
#from scipy import signal
import matplotlib.pyplot as plt
import matplotlib as mpl
#import pylab as pl
import csv
from pylab import rcParams
from math import radians, cos, sin, asin, sqrt


np.set_printoptions(threshold=np.inf)



def read_csv_file(file_name, c_lonlat, c_type, included_types,count_rows, SHUFFLED=True):
    """
    Reads csv file and returns A SHUFFLED c_lonlat, a list of lon lat, c_type 
    with a number that corresponds to the TYPE
    if it is one of the included_types. Only reads the number count_rows of 
    rows the file If count_rows==-1, reads the whole file
    """


    c_preshuffle_lonlat=[]
    c_preshuffle_type=[]
    count=0
    with open(file_name, 'rb') as csvfile:
        spamreader = csv.reader(csvfile, delimiter=',')
        for row in spamreader:
            # count_rows== -1 => read the entire file
            if(count_rows==-1 or count<count_rows):
 
                if( (row[TYPE] in included_types) and (row[LON]!='') and (row[LAT]!='') ):
                    c_preshuffle_lonlat.append([float(row[LON]),float(row[LAT])])
                    c_preshuffle_type.append(included_types.index(row[TYPE]))
#                    print ', '.join(row)
            count=count+1

    indexes=[]
    
    
    for i in range(len(c_preshuffle_lonlat)): indexes.append(i)
    
    #we randomly shuffle to avoid btach writting of complains that could
    #affect the stochastic gradient algorithm
    if SHUFFLED==True: np.random.shuffle(indexes)
    
    for i in range(len(c_preshuffle_lonlat)):
        c_lonlat.append(c_preshuffle_lonlat[indexes[i]])
        c_type.append(c_preshuffle_type[indexes[i]])


    return len(c_lonlat)

def neigh_nyc(lon,lat):
    col="grey"
    if lon<-74.048689 and lat < 40.649837:
        col="blue" #Staten Island
    elif lon>-73.934230 and lat > 40.813085:
        col="red" #Bronx
    elif lon >-74.041000 and lat < 40.701931:
        col="green" # Brooklyn
    elif lon > -73.934230 and lat > 40.702931:
        col="orange" # Queens
    else:
        col="black" #Manhattan
    
    return col
    
    
def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])

    # haversine formula 
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    r_earth = 6371000 # Radius of earth in meters. Use 3956 for miles
    return c * r_earth

def square_dist_lonlat(rr,ss):
    return haversine(rr[0],rr[1],ss[0],rr[1]),haversine(rr[0],rr[1],rr[0],ss[1])
    
def dist(rr,ss):
    return np.sqrt( (rr[0]-ss[0])**2+(rr[1]-ss[1])**2)
    
def softmax(lu,lv,li,lj):
    """
    Compute softmax => exp( u[i]v[j])/ SUM_t exp( u[t]v[j])
    """
    
    denom=0.0
    for tt in range(V):
        if tt!=lj:
            dot=np.dot(lu[tt],lv[lj])
            denom+=np.exp(dot)
    dot=np.dot(lu[li],lv[lj])
    sm=np.exp(dot)/denom
 
    return sm


def neighbors_count_2(ln_count,ln_exp,ln_indicator,lCity,lGrid,types, lt,lR):
    """
    ln_counts Returns the counts of neighbors per type for complain t in a 
    circle of radius = lR
    ln_exp rsturns counts with exponential decrease according to distance/lR
    ln:_indicator is 1 if counts >0, 0 if counts==0
    """
    center_location=[lGrid[lt][0],lGrid[lt][1]]
    #print t,n
    for ls in range(len(lCity)):
#        if C[i][2]!=C[t][2]:
            location=[lCity[ls][0],lCity[ls][1]]
            distx,disty=square_dist_lonlat(center_location,location)
            #print distance
            if np.abs(distx) < lR and np.abs(disty)<lR:
                ln_count[types[ls]]+=1.0
                if ln_indicator[types[ls]]==0 : ln_indicator[types[ls]]=1.0
                ln_exp[types[ls]]+=np.exp(-np.sqrt(distx**2+disty**2)/lR)
    #print n
    return


    
def plot_projections(array,TITLE="projections"):

    size=10
    for i in range(len(array)):
        #plt.xlim(-0.001,0.001)
        #plt.ylim(-0.001,0.001)
        plt.axhline(y=0)
        plt.axvline(x=0)
        plt.title(TITLE)
        plt.scatter(array[i,0],array[i,1],color=color_grid_point[i],s=size)
        plt.text(array[i,0],array[i,1],str(i),color=color_grid_point[i])
    
    return



def plot_city_color(C,types,color_scale):
    rcParams['figure.figsize'] = 10, 10
    limits=0.3
    NYC_LON=-73.91; NYC_LAT=40.7    
    for i in xrange(len(C)):
        plt.xlim(-limits+NYC_LON,limits+NYC_LON)
        plt.ylim(-limits+NYC_LAT,limits+NYC_LAT)
#        plt.text(C[i][0],C[i][1],str(types[i]),color=color_scale[types[i]])
        plt.text(C[i][0],C[i][1],"+",color=color_scale[types[i]%len(color_scale)])
    plt.show()

    return

def plot_city(C,types):
 
    limits=0.3
    NYC_LON=-73.91; NYC_LAT=40.7    
    for i in xrange(len(C)):
        plt.xlim(-limits+NYC_LON,limits+NYC_LON)
        plt.ylim(-limits+NYC_LAT,limits+NYC_LAT)
        #plt.text(C[i][0],C[i][1],str(types[i]),color="black")
        plt.text(C[i][0],C[i][1],'.',color="black")
 

    return
    
def plot_grid(G,neigh=[]):


    plt.xlim(UP_W_CORNER_LON,LO_E_CORNER_LON)
    plt.ylim(LO_E_CORNER_LAT,UP_W_CORNER_LAT)
    for ii in range(len(G)):
        sum_term=0.0
        for jj in range(V):
            sum_term+=neigh[ii][jj]
        if len(neigh)>0 and sum_term>0:
            plt.text(G[ii][0],G[ii][1],str(ii),color=color_grid_point[ii])
        else:
            plt.text(G[ii][0],G[ii][1],'+',color=color_grid_point[ii])


    return

def plot_array(zvals,str_title="--"):
    
    cmap1 = mpl.colors.LinearSegmentedColormap.from_list('my_colormap',['pink','blue'], 256)
    img1 = plt.imshow(zvals,interpolation='nearest',cmap = cmap1, origin='lower')
    plt.title(str_title)
    plt.colorbar(img1,cmap=cmap1)
    
    return

def plot_heatmap_counts(xlon,xlat,xcounts,str_title="--"):
    rcParams['figure.figsize'] = 12.5, 10

    cmap1 = mpl.colors.LinearSegmentedColormap.from_list('my_colormap',['pink','blue'], 256)
    #plt.subplot(211)
    plt.scatter(xlon, xlat, c=xcounts,cmap=cmap1)
    plt.title(str_title)
    plt.colorbar(cmap=cmap1)
    #plt.subplot(212)
    #plt.scatter(xlat, xlon, s=xcounts)
    return



def max_array (lu):
    normu=np.zeros(shape=(V,1))
    for li in range(V):
        for lj in range(DIMENSION):
            normu[li]+=lu[li][lj]**2
    maxnorm=np.amax(normu)
    return maxnorm**0.5
    
#############################
#%%

#This is a SQUARE...
UP_W_CORNER_LON=-74.2729;UP_W_CORNER_LAT=40.9833
LO_E_CORNER_LON=-73.6975;LO_E_CORNER_LAT=40.4079
N_GRID=20
STEP=(LO_E_CORNER_LON-UP_W_CORNER_LON)/float(N_GRID)


grid_lonlat=[]
color_grid_point=[]
for i in range(N_GRID):
    for j in range(N_GRID):
        lon_grid=UP_W_CORNER_LON+ STEP*i
        lat_grid=LO_E_CORNER_LAT+ STEP*j
        grid_lonlat.append([lon_grid,lat_grid])
        color_grid_point.append(neigh_nyc(lon_grid,lat_grid))


#%%

#############################

DIMENSION=2
COUNT=0;INDICATOR=1;EXP_COUNT=2;EXP_INDICATOR=3          
NUMROWS=10000 #rows we read from the csv file. -1 means "all"
R=haversine(UP_W_CORNER_LON, UP_W_CORNER_LAT, UP_W_CORNER_LON, UP_W_CORNER_LAT+STEP/2.0) #radius in meters 
COUNT_TYPE=COUNT

Blue=0;Orange=1;Green=2;Black=3;Magenta=4;Brown=5;Seagreen=6;Khaki=7;Drakcyan=8;Crimson=9
Color=["blue","orange","green","black","magenta","brown","seagreen","khaki","darkcyan","crimson"]

file_name= "C:\\Users\\jorge\\Documents\\res\\CUSP\\Pois2Vec\\Top_25_April_2017.csv"
TYPE=5; LAT=50; LON=51

City_lonlat=[]
City_types=[]

included_types= ['Rodent', 'Noise - Residential', 'ELECTRIC', 
'Street Light Condition', 'Noise - Commercial', 'Blocked Driveway', 
'Noise', 'Traffic Signal Condition', 'Sanitation Condition', 
'Dirty Conditions', 'Taxi Complaint', 'Street Condition', 
'Water System', 'Noise - Vehicle', 'PLUMBING', 'Elevator', 
'Noise - Street/Sidewalk', 'Food Establishment', 'Derelict Vehicle', 
'Derelict Vehicles', 'Missed Collection (All Materials)', 
'Damaged Tree', 'Graffiti', 'APPLIANCE', 'Broken Muni Meter', 
'General Construction/Plumbing', 'Noise - Park', 'Consumer Complaint', 
'Derelict Bicycle', 'Plumbing', 'Electrical', 'Taxi Report', 
'Noise - House of Worship', 'Collection Truck Noise', 
'Taxi Compliment', 'ELEVATOR', 'Noise - Helicopter']

V=len(included_types) # number of types

#Read is SHUFFLED
read_csv_file(file_name,City_lonlat,City_types, included_types,NUMROWS,True)

#%%


neigh_list=[] #Here we store neighbors of each type per grid by COUNTS
neigh_list_exp=[] #Here we store neighbors of each type per grid by EXPONENTIAL DECAY
num_neigh=0.0
for tt in range(len(grid_lonlat)):
    n=[0]*V
    n_exp=[0]*V
    n_indicator=[0]*V

    neighbors_count_2(n, n_exp,n_indicator, City_lonlat,grid_lonlat,City_types,tt,R)
    if COUNT_TYPE == COUNT: neigh_list.append(n)
    elif COUNT_TYPE == INDICATOR:neigh_list.append(n_indicator)
    elif COUNT_TYPE == EXP_COUNT: neigh_list_exp.append(n_exp)

    for i in range(V):
        num_neigh+=n[i]


print "num_neighbors", num_neigh    

#%%

term_doc=np.zeros(shape=(len(neigh_list),V))


for i in range(len(neigh_list)):
    sum_term=0.0
    for j in range(V):
        sum_term+=neigh_list[i][j]
    #print i, sum_term
    if sum_term>0 :
        for j in range(V): 
            term_doc[i][j]=float(neigh_list[i][j])/float(sum_term)

doc_freq=np.zeros(shape=(V,1))
for i in range(len(neigh_list)):
    for j in range(V):
        if neigh_list[i][j]>0:
            doc_freq[j]+=1
            
inv_doc_freq=np.zeros(shape=(V,V))
for j in range(V):
    inv_doc_freq[j][j]=np.log( N_GRID*N_GRID/(1.0+doc_freq[j]) )
    

tc_idc=np.dot(term_doc,inv_doc_freq)




#%%
DIM=2
u, s, vh = np.linalg.svd(np.transpose(tc_idc), full_matrices=False)

for i in range(DIM,len(s)):
    s[i]=0.0

sd=np.diag(s)

tmp=np.zeros(shape=(V,len(neigh_list)))

tmp=np.dot(np.dot(u,sd),vh)

tmp2=np.zeros(shape=(2,V))
tmp2[0][0]=1.0
tmp2[1][1]=1.0

tmp3=np.dot(tmp2,tmp)
proj_tc_idc=np.transpose(tmp3)

#%%
#%%
rcParams['figure.figsize'] = 15, 15
plot_grid(grid_lonlat,neigh_list)
plt.show()
#%%






    
#v=np.transpose(vh)
#vxy=np.zeros(shape=(2,V))
#vxy[0]=s[0]*v[0]
#vxy[1]=s[1]*v[1]
#rcParams['figure.figsize'] = 15, 15
#proj_tc_idc=np.dot(tc_idc,np.transpose(vxy))
rcParams['figure.figsize'] = 15, 15
plot_projections(proj_tc_idc,"projections grid")
plt.show()

rcParams['figure.figsize'] = 15, 7
f, axarr = plt.subplots(1, 2)
plt.subplot(1,2,1)
plot_grid(grid_lonlat,neigh_list)
plt.subplot(1,2,2)
plot_projections(proj_tc_idc,"projections grid")
plt.show()






