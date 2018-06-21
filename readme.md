## Analysis of NYC’s Service Request Calls to 311

- The project deals with the data ETL and exploratory analysis of calls to 311 in the 5 borough area of New York City.  It is based on data publicly available in NYC’s [open data trove](https://opendata.cityofnewyork.us/).   We enrich that data with IRS data on median (declared) income and jobless rate, police data on crime and hopefully (one day) with educational achievement data. 
- We reveal correlations and generally speaking associations between types of calls, organized according to various categories and spatially structured by zip codes.  This is more precise than the census tract scale, the which normally includes many ZIP codes.  Our approach is not only to extract correlations but also to try to detect evolution patterns and from that to enter the area of predicions.  Applications are plenty: socially, for business and for city administrators alike.
- The full ETL pipeline for data mining is provided.  Several examples of API implementation are also provided in particular, but not only, to query Google map based on direction and/or GPS coordinates.
- Techniques used are CA, MCA, PCA, Hierarchical Clustering and kNN.  A Random Forest approach to prediction may be added at a future date.
- All the code included here is extensively commented:
     - R: for ETL and data exploration
     - Python: for machine learning processing 

