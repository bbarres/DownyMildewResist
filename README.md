[![DOI](https://zenodo.org/badge/113194524.svg)](https://zenodo.org/badge/latestdoi/113194524)
# Investigation on the sensitivity of *Plasmopara viticola* to amisulbrom and ametoctradin in French vineyards using bioassays and molecular tools
*This repository contains the R code used for the data analyses and production of the figures of the related article*  

## Context


## The data
You can find the dataset and the geographical data used to produce the analyses and the figures of this manuscript in the *data* folder. Three files contain geographical data: 
+ commu.RData
+ departe.RData
+ regions.RData

|  commu.RData  |  departe.RData  |  regions.RData  |
| --------------- |  --------------- |   --------------- |
|  ![alt text](https://xqowha.db.files.1drv.com/y4mrSE2QIRtvj8BAk6zej2_8WTCjO1WxNNqvJJn189PQZtrYHC-jBiwaqwbCY5rTQunn8hvVMFWArvEz_8XPwfpqcqn9y08ohuijpqmVQfsBmPd_5mKWMw6hiuynOoJF4Gh3V5NsyuwQ8QOyAkjNRZgy9wNdSAeFfPyrXPhJIRLP1L5f-jSsrovM02sbUEs7wXjdmbpXBuTEAVryX-JeXUBjg?width=256&height=255&cropmode=none)  |  ![alt text](https://xqovha.db.files.1drv.com/y4mJeGFQBu2P37nly83ADc3vqKAMHynSiSIw5B9N0Gyuz4Uv5CWWm8dTYGOBRiRRtYrZaqpzVj-m7tjXDr9FoXnSte7Magct34bm_lID3VC3JZMXmumYeheFN15YjlDRQPmrUFX8DklJ4MAx5YCThctP2A-3WC-gHxZLV2uy5LmfFn9ZrUzztvcZt1BF6iaGf54hzV6_Ztlup5D9_6ifNC10Q?width=256&height=254&cropmode=none) | ![alt text](https://xgouha.db.files.1drv.com/y4mYTK7YsDgwIlYw6rz3j08Wd43zp1ZsXssjSbMkGL-3L4YW7ysn8MqmxecalhwJwvNu_jSRveyFkUTiUXs0FBa5SqpCfF7Gb-AP9jEfn4g3oTqBTQ90UXTe6sqXagD8p0V6m6L0RIW5eRrjxA6wQIbSQ_7dWRAtGCQlKkmLbjjFpH4p2Iw82Vfh_0ydNxFtiJhtt9v3KC-_wq7RLTWSlBi-w?width=256&height=253&cropmode=none) |




These geographical data were obtained using the data from the [IGN website](http://professionnels.ign.fr/adminexpress). The version of the data used is the "Edition Novembre 2017". 

The fourth dataset contains the code for the analysis of the frequency of the AOX-related resistance in France: 
+ R_mildiouQI2.txt
  + *pop_ID*:

## The R scripts
+ **load_mildew_data.R:** the script to load the different datasets in the environment
+ **map_ameto.R:** the script to produce the maps of the ametoctradin resistant populations in France
+ **map_amisul.R:** the script to produce the maps of the ametoctradin resistant populations in France
+ **map_AOX.R:** the script to produce the maps of theAOX-related resistant populations in France
+ **AOX_evolution.R:** the script to analyse the evolution of the frequency of the AOX-related resistance in France and to produce the Figure

