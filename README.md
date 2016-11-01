# Measuring and modeling Russian coverage of climate change
  
### Summary
This page provides the data and code necessary to replicate the statistical analysis in the following paper: 

	"Measuring and modeling Russian newspaper coverage of climate change." Constantine Boussalis, 
	Travis G. Coan, and Marianna Marianna Poberezhskaya. Global Environmental Change, 2016.

### Dependencies

The scripts located in the src directory were tested in R version 3.2.3 (2015-12-10) and requires the following packages:

* [plyr](https://cran.r-project.org/web/packages/plyr/index.html)  (tested on version 1.8.3)
* [zoo](https://cran.r-project.org/web/packages/zoo/index.html) (tested on version 1.7-12)
* [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) (tested on verison 2.1.0)
* [ggthemes](https://cran.fhcrc.org/web/packages/ggthemes/index.html) (tested on version 3.0.3)
* [coda](https://cran.r-project.org/web/packages/coda/index.html) (tested on version 0.18-1)
* [brms](https://cran.r-project.org/web/packages/brms/index.html) (tested on version 0.9.0)

Note that the R scripts are Linux-centric, so you will need to modify the paths to the directories if using a Windows system.

### Usage

The following two scripts (see src/) provide the necessary code to replicate the statistical analysis:

* replication.R (provides code for replication of Figure 1, 2, and 3)
* utility.R     (a set of function to help generate Figure 1, 2, and 3)

Please note that you will need to set the working directory (and modify paths if on Windows) in order to successfully execute the script.