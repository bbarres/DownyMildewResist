[![DOI](https://zenodo.org/badge/113194524.svg)](https://zenodo.org/badge/latestdoi/113194524)
# Investigation on the sensitivity of *Plasmopara viticola* to amisulbrom and ametoctradin in French vineyards using bioassays and molecular tools
*This repository contains the R code used for the data analyses and production of the figures of the related article*  

## Context


## The data
You can find the dataset and the geographical data used to produce the analyses and the figures of this manuscript in the *data* folder. Three files contain geographical data: 
+ commu.RData
+ departe.RData
+ regions.RData

These geographical data were obtained using the data from the [IGN website](http://professionnels.ign.fr/adminexpress). The version of the data used is the "Edition Novembre 2017"
The fourth dataset contains the code for the analysis of the frequency of the AOX-related resistance in France: 
+ R_mildiouQI2.txt

## The R scripts
+ **load_mildew_data.R:** the script to load the different datasets in the environment
+ **map_ameto.R:** the script to produce the maps of the ametoctradin resistant populations in France
+ **map_amisul.R:** the script to produce the maps of the ametoctradin resistant populations in France
+ **map_AOX.R:** the script to produce the maps of theAOX-related resistant populations in France
+ **AOX_evolution.R:** the script to analyse the evolution of the frequency of the AOX-related resistance in France and to produce the Figure

