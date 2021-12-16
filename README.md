# Simulation studies for {skewsamp}

This repository contains the code for simulation studies done to verify
the correct implementation of the R package [{skewsamp}](https://github.com/jobrachem/skewsamp). 

Due to size limitations, the data are not contained in this repository.
Instead, you can find them on the Open Science Framework, alongside the
final report (in *german*).

-   <https://osf.io/z5vtf/> (Project)
-   <https://osf.io/yb5xm/> (Report)

To reproduce the analyses, download the data from the OSF and 
place them in `chak/data` and `cundill/data`, respectively.

The simulations revealed that the GLM-based approach (Cundill &
Alexander, 2015) works robustly. The nonparametric NECDF approach is
dependent on pilot data and can provide significant underestimations of
the required sample sizes. Please consult the report linked above for
further details.
