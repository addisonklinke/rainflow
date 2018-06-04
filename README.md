# rainflow

## Overview

rainflow is an R package designed to extract cyclic features from compressed time series. 
It offers a variety of compression algorithms and follows standard ATSM E1049-85 for calculating the mean and amplitude for each cycle.

## Authors

Addison G. Klinke  
Email: agk38@case.edu

## Installation

rainflow depends on R >= 3.3.3  
From R, run the following commands:  

```r
# rainflow is not yet on CRAN, so you will need the devtools package to install from GitHub
install.packages("devtools")

# Then install rainflow itself
devtools::install_github("addisonklinke/rainflow")
```

## Contributing

When contributing to this repository, please first discuss the change you wish to make via issue, email, or any other method with the owners of this repository before making a change. 

## License

This project is licensed under the MIT License - see the LICENSE.md file for details

## Acknowledgments

This project began during my master's thesis at the Case Western Reserve University under professors Laura S. Bruckman and Roger H. French. 
Funding provided by the Solar Durability and Lifetime Extension Research Center (Ohio Third Frontier, Wright Project Program Award Tech 12-004) and the Department of Energy’s SunShot PREDICTS2 program (DE-EE0007143).
