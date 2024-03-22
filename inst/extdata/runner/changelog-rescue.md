# Land Use Data for RESCUE - Changelog

## 2024-03-22

- fixed wood harvest area (unit) conversion bug
- removed NAs
- renamed longitude/latitude to lon/lat
- added bounds variables for lon/lat/time
- added more metadata, incl. harmonization period

## 2023-12-08

- dataset for 1995-2100 in 5-year timesteps for all REMIND-MAgPIE scenarios used for the emissions
- added metadata to nc files
- management.nc: added fulwd, rndwd
- transitions.nc: reporting all LUH variables incl. wood harvest variables
- all transitions are gross transitions
- transitions in 2100 are net-zero gross transitions (to be used for extended (2100-2300) scenarios)
- cover all LUH cells (e.g. Greenland was missing before)

## 2023-10-11

- dataset for 1995-2100 in 5-year timesteps for one scenario
- states.nc: reporting all LUH variables except secma, secmb
- management.nc: reporting all LUH variables except fulwd, rndwd, combf, flood, fharv_c3per, fharv_c4per
- additionally reporting 2nd gen biofuel (crpbf2_c3per, crpbf2_c4per) and managed forests (manaf) in management.nc
