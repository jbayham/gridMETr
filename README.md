# gridMETr
A suite of utilities to download and process weather data from gridMET (Abatzoglou, 2013).

The basic operations involve choosing and downloading the raw data.  

## Quick Start

Use the `gridmetr_download()` to download the data to a folder called data/_variable_name.

```
gridmetr_download(variables = c("pdsi","erc"),
                  years = seq.int(2000,2002))
```




### ToDo

- Determine why future (parallel) fails within the download function

- Determine steps to convert to package

- Provide some visualization utilities





# References

Abatzoglou, J. T. (2013), Development of gridded surface meteorological data for ecological applications and modelling. Int. J. Climatol., 33: 121–131.
