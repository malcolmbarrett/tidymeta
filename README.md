
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidymeta

Tidy and plot meta-analyses from popular meta-analytic tools in R.
Currently in early development.

## Installation

You can install the development version of tidymeta from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("malcolmbarrett/tidymeta")
```

# Example

``` r
library(tidymeta)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
#> 
#> Attaching package: 'ggplot2'
#> The following object is masked from 'package:dplyr':
#> 
#>     vars

iud_cxca %>% 
  group_by(group) %>% 
  meta_analysis(yi = lnes, sei = selnes, slab = study_name, 
                exponentiate = TRUE) %>% 
  sub2summary(group) %>% 
  forest_plot(group = group) +
    scale_x_log()
#> Warning in bind_rows_(x, .id): binding factor and character vector,
#> coercing into character vector
#> Warning in bind_rows_(x, .id): binding character and factor vector,
#> coercing into character vector
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />
