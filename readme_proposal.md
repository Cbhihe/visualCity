##          A Machine Learning Project Proposal
####               by Cedric Bhihe, 2018.03.02


***Urban Service requests calls to 311 in NYC:***

Each year, starting in 2011, between 70,000 and 120,000 calls to 311 are received by NYC's 
public office and logged with a slew of metadata attributes. Calls are registered with 50+ 
attributes, describing time of the year, exact geolocation pertaining to the complaint, etc.

Calls are heretofore described equivalently as either “service requests” or “complaints”.


Data availability: Data is available in annual batches from 2011 to 2015 (both included). 
Data at various stages of processing is included here for your perusal. 
It is also available at:
https://nycopendata.socrata.com/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9


Data usage: All data is public, and regulated by the terms of use of data and information 
generally available at: http://www1.nyc.gov.  The specific terms and conditions of use for data made available through this repo are included 
here under _nyc.gov_webpage-data-terms-of-use.txt_ and also available from http://www1.nyc.gov/home/terms-of-use.page.

Every dataset entry is labeled.  A rapid inspection shows that "NA" (non-assigned) values exist but in small proportion, making it possible to carry out routine imputation or suppression without incurring in blatant issues of data bias.


In very broad, non-exhaustive terms, it would be interesting to study:
- Classification
- Clustering of like- or similar service requests
- Geo-correlative study (GPS data for each service request is available) for instance with SAT scores 
to explore the relationship between quantity, type and variety of complaints and educational 
level or income.
- Density estimation (mechanisms which generate the data)
- Predictibility of complaints, e.g.:
	- use 2014 as training set and estimate trends in 2015
	- detect micro trends that announce correlated complaints, taking seasonality into account 
	and wealth level of district, where the service call originated.

In addition, although not necessary for clustering, categorical data can be easily transformed 
(pre-processing) in times series over sliding windows of varying widths.
