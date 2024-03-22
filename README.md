# Downscale and harmonize MAgPIE output using LUH2 as reference data

R package **mrdownscale**, version **0.16.0**

[![CRAN status](https://www.r-pkg.org/badges/version/mrdownscale)](https://cran.r-project.org/package=mrdownscale)  [![R build status](https://github.com/pik-piam/mrdownscale/workflows/check/badge.svg)](https://github.com/pik-piam/mrdownscale/actions) [![codecov](https://codecov.io/gh/pik-piam/mrdownscale/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrdownscale) 

## Purpose and Functionality

Downscale and harmonize MAgPIE output using LUH2 as reference
    data.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrdownscale")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Pascal Sauer <pascal.sauer@pik-potsdam.de>.

## Citation

To cite package **mrdownscale** in publications use:

Sauer P, Dietrich J (2024). _mrdownscale: Downscale and harmonize MAgPIE output using LUH2 as reference data_. R package version 0.16.0, <https://github.com/pik-piam/mrdownscale>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrdownscale: Downscale and harmonize MAgPIE output using LUH2 as reference data},
  author = {Pascal Sauer and Jan Philipp Dietrich},
  year = {2024},
  note = {R package version 0.16.0},
  url = {https://github.com/pik-piam/mrdownscale},
}
```
