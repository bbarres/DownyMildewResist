[![DOI](https://zenodo.org/badge/113194524.svg)](https://zenodo.org/badge/latestdoi/113194524)
# Supporting data and code for: Investigation on the sensitivity of *Plasmopara viticola* to amisulbrom and ametoctradin in French vineyards using bioassays and molecular tools
*This repository contains the R code used for the data analyses and production of the figures of the related article*  

![alt text](https://xwozha.db.files.1drv.com/y4mSuQULu970Jf5GnBDqavtyLhJmOPeVlKoA_UQoxH6HL-ywl0eMmxNb7C3_xA9cEiBSke1YD65qrKmw0lqQER1sfw3CdlPEwtYegKz9xDQpTbt5K0SUGSJDxYywFIMK1ZNS2pNNdMvWuL1wRJGXmgXnvZBXFA6sV-gf_wEfAJoe7BMVkBN6sF_j5Bmwur9NUocTggzp9k25bAgKSzvra9srA?width=1584&height=588&cropmode=none)

## Context
Complex III inhibitors are fungicides that are extensively used in french vineyards. Among different pathogens, they are targetting *Plasmopara viticola*, which is responsible for grapewine downy mildew. The evolution of fungicide resistance is one of the adverse effect of a regular and repeated use of an active substance against a pest. In the [study](https://onlinelibrary.wiley.com/doi/abs/10.1002/ps.5461) related to this repository, we investigated the evolution of resistance to ametoctradin and amisulbrom in French populations of *P. viticola*. This repository describe the datasets used in the study and provide the R scripts used to make the statistical analysis and to produce the figures that are found in the manuscript. 

## Datasets
You can find the dataset and the geographical data used to produce the analyses and the figures of this manuscript in the *data* folder. Two files contain geographical data: 
+ departe.RData
+ regions.RData

|  departe.RData  |  regions.RData  |
|  -------------  |  -------------  |
|  ![alt text](https://xqovha.db.files.1drv.com/y4mJeGFQBu2P37nly83ADc3vqKAMHynSiSIw5B9N0Gyuz4Uv5CWWm8dTYGOBRiRRtYrZaqpzVj-m7tjXDr9FoXnSte7Magct34bm_lID3VC3JZMXmumYeheFN15YjlDRQPmrUFX8DklJ4MAx5YCThctP2A-3WC-gHxZLV2uy5LmfFn9ZrUzztvcZt1BF6iaGf54hzV6_Ztlup5D9_6ifNC10Q?width=256&height=254&cropmode=none) | ![alt text](https://xgouha.db.files.1drv.com/y4mYTK7YsDgwIlYw6rz3j08Wd43zp1ZsXssjSbMkGL-3L4YW7ysn8MqmxecalhwJwvNu_jSRveyFkUTiUXs0FBa5SqpCfF7Gb-AP9jEfn4g3oTqBTQ90UXTe6sqXagD8p0V6m6L0RIW5eRrjxA6wQIbSQ_7dWRAtGCQlKkmLbjjFpH4p2Iw82Vfh_0ydNxFtiJhtt9v3KC-_wq7RLTWSlBi-w?width=256&height=253&cropmode=none) |


These geographical data were obtained using the data from the [IGN website](http://professionnels.ign.fr/adminexpress). The version of the data used is the "Edition Novembre 2017". 

The third dataset contains the results of the different bioassays and biomolecular test conducted on the sampled populations in France, as well as some geographical information on these populations: 
+ R_mildiouQI2.txt
  + *sample_ID*: sample identifier
  + *year*: sampling year
  + *sampling_date*: sampling date
  + *departement*: departement code to which the sampled commune belong
  + *AMISULBROM_SSSHAM*: result of the discriminant dose bioassay with amisulbrom whithout SHAM (NA=not tested, 0=sensitive, 1=resistant)
  + *AMISULBROM_SHAM*: result of the discriminant dose bioassay with amisulbrom with SHAM (NA=not tested, 0=sensitive, 1=resistant)
  + *S34L*: result of the biomecular test that detect the mutation responsible for the S34L substitution (NA=not tested, 0=no resistant allele, 1=resistant allele)
  + *AOX*: AOX-related resistance status deducted from the comparison of the columns AMISULBROM_SSSHAM and AMISULBROM_SHAM (NA=not tested, 0=not AOX-related resistant, 1=AOX-related resistant)
  + *AMETOC*: Ametoctradin resistance status resulting from the discriminant dose bioassay with ametoctradin with SHAM (NA=not tested, 0=sensitive, 1=target site resistant only for the first bioassay, 2=target site resistant for the two bioassays)
  + *AMISUL*: Amisulbrom resistance status deducted from a combination of the information of the columns AMISULBROM_SSSHAM and AMISULBROM_SHAM (NA=not tested, 0=sensitive, 1=target site resistant)

The fourth dataset contains the results of the dose-response bioassays conducted on populations between 2012 and 2015, with and without the addition of 80 mg/L of SHAM to inhibit the alternative oxydase pathway
+ Amisul_base.txt
  + *sample_ID*: sample identifier
  + *region*: sampling region
  + *raw_MIC*: the value of the "raw" MIC. This is the first value for which no sporulation for the population was observed on leaf discs. These values can be different from the ones obtained using regression. 
  + *year*: sampling year
  + *sham*: yes/no - is there addition of 80 mg/l of SHAM for the bioassay
  + *0	0.01	0.1	1	10	100*: each column give the percentage of sporulation observed at the different concentration of amisulbrom. The 1 mg/l value was used as the discriminant dose for the tested populations. 

## R scripts
+ **load_mildew_data.R:** the script to load the different datasets in the environment
+ **AOX_evolution.R:** the script to analyse the evolution of the frequency of the population containing AOX-related resistance strains in France and to produce the Figure 1
+ **map_AOX.R:** this script produce the maps of the populations containing AOX-related resistant strains in France. This is the Figure 2
+ **map_ameto.R:** the script to produce the maps of populations containing  ametoctradin resistant strains in France using either bioassay or biomolecular tools. This is the Figure 3 in the manuscript
+ **map_amisul.R:** the script to produce the maps of the populations containing amisulbrom resistant strains in France. This plot was not included in the manuscript
+ **MIC_amisul.R:** the script used to produce the supplementary material S1 analyses and figures. It mainly consists in the computation of the MIC values of amisulbrom dose-response curves and the analyses of their distribution. 

## Citation
You can cite the related study as follow: 
+ Fontaine S., Remuson F., Caddoux L. and Barrès B. [Investigation on the sensitivity of *Plasmopara viticola* to amisulbrom and ametoctradin in French vineyards using bioassays and molecular tools. *Pest Management Science*, 75:2115-2123, 2019. doi:10.1002/ps.5461.](https://onlinelibrary.wiley.com/doi/abs/10.1002/ps.5461)

If you want to use (some of) the code found on this page or if you want to cite this repository: 
+ Benoit Barrès. [Supporting data and code for: Investigation on the sensitivity of *Plasmopara viticola* to amisulbrom and ametoctradin in French vineyards using bioassays and molecular tools. Zenodo.](https://zenodo.org/badge/latestdoi/113194524)
