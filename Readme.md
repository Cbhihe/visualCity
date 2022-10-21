## Analysis of NYC’s
- **Service Request Calls to 311**
- **Crime Report Calls to 911**

The project deals with the data extraction and exploratory analysis of calls to 311 and 911 in the 5 borough area of New York City.  It is based on data publicly available in NYC’s [open data trove](https://opendata.cityofnewyork.us/).

We enrich that data with IRS data on median (declared) income and jobless rate, police data on crime and hopefully (one day) with educational achievement data.

We reveal correlations and generally speaking associations between types of calls, organized according to various categories and spatially structured by zip codes.  This is more precise than the census tract scale, the which normally includes many ZIP codes.  Our approach is not only to extract correlations but also to try to detect evolution patterns and from that to enter the area of temporal analysis.  Applications and uses are plenty: socially, for business and for city administrators alike.

The full processing pipeline for data mining is provided.  Several examples of API implementation are also provided in particular, but not only, to query Google map based on direction and/or GPS coordinates.<BR>
Note: Since July 2018, Google has stopped granting a previously rate-limited but free access to its geo-mapping data, via its geo-location API.  For that reason users will need to be equipped with a pay per use API key (obtained by opening an proper account on Google’s platform) to be able to use the API implementation provided here.  As an alternative OpenStreetMap can be tapped but no API implementation is provided for it here.

In very broad, non-exhaustive terms, we considered:
- Classification
- Clustering
- Geo-correlative study (GPS data for each complaint is available)
- Density estimation (mechanisms which generate the data)
- Predictibility of complaints, e.g.:
	- use 2013 as training set and estimate trends in 2014
	- detect micro trends that announce correlated complaints, taking seasonality into account and wealth level of the district where the complaint originates.

Techniques used are CA, MCA, PCA, Hierarchical Clustering and kNN.  A Random Forest based approach may be added at a future date.

- Linear Dimensional Reduction (LDR) techniques used are CA, SVD, PCA MCA are presented here.
- Only one non-Linear methods (NLDR), Hierarchical Clustering with k-Means consolidation, is provided.
- In time, three learning techniques are to be applied to the data set:
	1) t-SNE for visualization (non-parametric)
	2) UMAP for visualization (parametric)
	3) Random Forest for prediction (parametric)

All the code included here is extensively commented:
- R: for data extraction and exploration
- Python: for machine learning processing
