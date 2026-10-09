# Accessibility of asymptomatic test sites and where to put new ones, Liverpool 2020

**Outputs.** Green MA. Thinking spatially to communicate and evaluate the roll-out of ‘mass’ testing in Liverpool, 2020. *People, Place and Policy* 2021;15(1):54-56. https://doi.org/10.3351/ppp.2021.9589727428. Also the Royal Geographical Society geovisualisation *COVID-19 and access to testing in Liverpool* (https://rgs.org/geography/geovisualisation/covid-19-and-access-to-testing-in-liverpool) and the site-coverage analysis that informed Liverpool City Council during the community testing pilot, reported in the *Liverpool Covid-SMART Community Testing Pilot Evaluation* (University of Liverpool for DHSC, 2021).

**What it does.** Estimates 15-minute walking access to asymptomatic test sites (HERE routing API isolines), maps coverage against neighbourhood deprivation and uptake, and runs location-allocation models to identify where additional sites would most improve coverage.

**Source.** Copied from https://github.com/markagreen/mapping_test_accessibility (commit `3c5770f2`, 23 March 2021), MIT licence, copyright (c) 2021 Mark A Green; licence text in `LICENSE`. Unchanged apart from removing the original README files, whose content is reproduced below.

## Scripts (`scripts/`)

| Script | Purpose |
|---|---|
| `calculate_access.R` | Estimates accessibility of point (site) locations |
| `geoviz_access_map.R` | The featured map in the RGS geovisualisation |
| `ppp_article.R` | The featured graphic in the People, Place and Policy article |
| `locating_new_sites.Rmd` | Estimating where new asymptomatic test sites should be located in Liverpool |
| `location_allocation_model.R` | Location-allocation modelling for optimal site coverage |
| `possible_LCC_sites.Rmd` | Location-allocation modelling of candidate Liverpool City Council sites |

The HERE API key is left blank (`set_key("")`) in three scripts; a free key can be requested from HERE.

## Data (`Data/`)

`ltc.csv`: the list of asymptomatic test sites (name, longitude, latitude) during the pilot period. Postcode and population data used by the scripts are not shared and can be obtained from UK Borders or equivalent sources.

## R packages

RColorBrewer, data.table, dplyr, ggmap, ggplot2, hereR, leaflet, maxcovr, osmdata, patchwork, purrr, sf, tidyverse.
