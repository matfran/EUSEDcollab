# EUSEDcollab
The code associated with the EUSEDcollab sediment yield data repository: https://doi.org/10.6084/m9.figshare.22117559

This Github repository contains the following code:

1) A Jupyter notebook containing the code to perform some simple analyses of the EUSEDcollab data repository. This also contains descriptive information of each step to guide users less familiar with the processing steps. The code should be run in a local environment containing the neccessary python libraries. 

2) A Jupyter notebook ('EUSEDcollab_functions') containing some analysis and plotting functions for the dataset.

3) A jupyter notebook showing a simple example using Google Earth Engine (GEE) through the Python API to extract values at the EUSEDcollab monitoring station points. This is intended to demonstrate how further auxillary data sources (e.g. the wealth of data stored in GEE) can be used to facilitate research works using the catchment data.

4) The R code to perform the quality control procedure on the each catchment time series.

To run the tutorial notebook using the default folder pathways, place all scripts in a singular root folder e.g. 'your_path/EUSEDcollab_analysis', then download and add the EUSEDcollab data repository into this folder e.g. 'your_path/EUSEDcollab_analysis/EUSEDcollab_data_repository'.
