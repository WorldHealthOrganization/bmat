bmat
================

## Table of contents

[Summary](#summary)  
[Required software](#reqs)  
[Installation](#install)  
[Data inputs](#data)  
[Use cases](#use)  

## <a name="summary"></a>

## Summary

The bmat package includes global and one-country implementations of the
BMis and BMat model for the estimation of the maternal mortality rate
(MMR) defined as the number of maternal deaths in a population per
100,000 live births

## <a name="reqs"></a>

## Requirements

For usage, an R installation (4.2.2), a JAGS installation (4.x), and a
working developer environment such as an Rtools installation (4.2) are
required. R and Rtools can be downloaded on CRAN. JAGS is a program for
the analysis of Bayesian models using Markov Chain Monte Carlo (MCMC)
(Plummer, 2017). JAGS is written in C++ and is portable to all major
operating systems. JAGS is available for download
[here](https://sourceforge.net/projects/mcmc-jags/).

Developer environment options. Windows: Install Rtools. Mac: Install
Xcode from the Mac App Store. Linux: Install a compiler and various
development libraries (details vary across different flavors of Linux).

## <a name="install"></a>

## Installation

1.  Obtain the version of bmat you wish to use. For the latest version
    clone the master branch of this repository. For versions of bmat
    which are associated with release dates visit the release dates and
    tags pages on github.
2.  Create project from existing directory in R studio. Use the folder
    obtained from step 1 as the existing directory.
3.  Enter devtools::install() in Rstudio console while having the
    project open in Rstudio.
4.  When prompted to install required packages, enter “yes” in the
    console.

Package dependencies are listed in the package DESCRIPTION file and will
be automatically installed upon installing the main package.

Versions of bmat which are associated with release dates may also be
installed from binary using the base R function install.package().

Note: Developers may wish to forgo installation and use the
devtools::load_all() function to actively make changes to package code
without iterative installation.

## <a name="data"></a>

## Data inputs

The data for the use cases can be found here
[bmat/data-raw/](https://github.com/WorldHealthOrganization/bmat/tree/master/data-raw).
A meta file describing the data for use cases can be found here.
[bmat/data-raw/meta_data_bmat_gather.xlsx](https://github.com/WorldHealthOrganization/bmat/blob/master/data-raw/meta_data_bmat_gather.xlsx).
For more detailed information about the data used please refer to the
WHO report, Trends in maternal mortality: 2000 to 2020. Geneva: WHO,
UNICEF, UNFPA and The World Bank; 2023.

## <a name="use"></a>

## Use cases

#### Case 1. One country run

To produce one country estimates which are based on the latest fixed
global estimates, refer to the following vignette
[bmat/vignettes/0_fit_one_country.R](https://github.com/WorldHealthOrganization/bmat/blob/master/vignettes/0_fit_one_country.R)

#### Case 2. Full estimation process

For the full process of producing estimates for all countries from start
to finish, run the following vignettes in order. Vignettes can be found
here
[bmat/vignettes/](https://github.com/WorldHealthOrganization/bmat/tree/master/vignettes).
The second and third use case are computationally intensive when using
settings that are beyond that of a test run. This will generally require
server computation.

1_process_data.R  
2_fit_models.R  
3_aggregates.R  
4_coutnry_profile_loop.R  
