**Scripts and data to generate models and figures for "Revisiting the Ocean’s Metabolic Balance: Correcting Biases in Bottle Incubations Reveals a More Autotrophic Ocean".**

**/data summary:**\

# Field_sampling.csv
Field measurements conducted in the marginal seas of the Northwest Pacific, including metabolic state and environmental parameters, were collected at stations during five cruises between 2018 and 2023.
- **Cruise**: Cruise time
- **Date**: Sampling date
- **Time**: Sampling time
- **Lon**: Sampling longitude
- **Lat**: Sampling latitude
- **Depth**: Sampling depth (unit: m)
- **Temperature**: Temperature (unit: °C)
- **Salinity**: Salinity
- **Chla**: Chlorophyll-a concentration (unit: mg m⁻³)
- **DO**: Dissolved oxygen concentration (unit: µmol L⁻¹)
- **Si**: Silicate concentration (unit: µmol L⁻¹); missing values indicate concentrations below the instrument's detection limit
- **NO3**: Nitrate concentration (unit: µmol L⁻¹); missing values indicate concentrations below the instrument's detection limit
- **PO4**: Phosphate concentration (unit: µmol L⁻¹); missing values indicate concentrations below the instrument's detection limit
- **NCP**: Net community production (unit: mmol O₂ m⁻³ d⁻¹)
- **GPP**: Gross primary production (unit: mmol O₂ m⁻³ d⁻¹)
- **CR**: Community respiration (unit: mmol O₂ m⁻³ d⁻¹)
- **BA_t0**: Initial bacterial abundance in bottle incubations (in situ bacterial abundance) (unit: cells mL⁻¹)
- **BA_t1**: Bacterial abundance after 24-hour incubation (unit: cells mL⁻¹)
- **BP_t0**: Initial bacterial production in bottle incubations (in situ bacterial production) (unit: mg C m⁻³ d⁻¹)
- **BP_t1**: Bacterial production after 24-hour incubation (unit: mg C m⁻³ d⁻¹)
- **BR_total_filtered**: Measurement of bacterial respiration rate using pre-filtered incubations (unit: mmol O₂ m⁻³ d⁻¹)
- **BA_t0_filtered**: Initial bacterial abundance in pre-filtered bottle incubations (in situ bacterial abundance) (unit: cells mL⁻¹)
- **BA_t1_filtered**: Bacterial abundance after 24-hour incubation in pre-filtered bottles (unit: cells mL⁻¹)
- **BP_t0_filtered**: Initial bacterial production in pre-filtered bottle incubations (in situ bacterial production) (unit: mg C m⁻³ d⁻¹)
- **BP_t1_filtered**: Bacterial production after 24-hour incubation in pre-filtered bottles (unit: mg C m⁻³ d⁻¹)

# high-frequency_sampling.csv
Time-series observations of bacterial abundance and bacterial production during bottle incubations at three selected stations.
- **Station**: Station ID
- **Lon**: Station longitude
- **Lat**: Station latitude
- **Time**: Observation at the i-th hour during the bottle incubation (unit: hour)
- **BA_mean**: Mean bacterial abundance measured at the i-th hour (unit: 10⁵ cells mL⁻¹)
- **BA_sd**: Standard deviation of bacterial abundance measured at the i-th hour
- **BP_mean**: Mean bacterial production measured at the i-th hour (unit: mg C m⁻³ d⁻¹)
- **BP_sd**: Standard deviation of bacterial production measured at the i-th hour

# light-dark_bottle_dataset.csv
Global light-dark bottle incubation dataset, collected from previously reported literature.
- **Date**: Sampling date
- **Lon**: Sampling longitude
- **Lat**: Sampling latitude
- **Depth**: Sampling depth (unit: m)
- **GPP**: Gross primary production (unit: mmol O₂ m⁻³ d⁻¹)
- **CR**: Community respiration (unit: mmol O₂ m⁻³ d⁻¹)
- **NCP**: Net community production (unit: mmol O₂ m⁻³ d⁻¹)
- **Reference**: Data contributor
- **Source**: Data source

# euphotic-zone_integrated_NCP_grid.csv
1° gridded climatology dataset of marine metabolic rates and environmental factors.
- **Lon**: Grid longitude
- **Lat**: Grid latitude
- **Month**: Month
- **MLD**: Climatology of mixed layer depth for the grid (unit: m), from HYCOM
- **Temp_WOA**: Climatology of sea surface temperature for the grid (unit: °C), from World Ocean Atlas 2023
- **Sal_WOA**: Climatology of sea surface salinity for the grid, from World Ocean Atlas 2023
- **DO_WOA**: Climatology of sea surface dissolved oxygen concentration for the grid (unit: µmol kg⁻¹), from World Ocean Atlas 2023
- **Si_WOA**: Climatology of sea surface silicate concentration for the grid (unit: µmol kg⁻¹), from World Ocean Atlas 2023
- **NO3_WOA**: Climatology of sea surface nitrate concentration for the grid (unit: µmol kg⁻¹), from World Ocean Atlas 2023
- **PO4_WOA**: Climatology of sea surface phosphate concentration for the grid (unit: µmol kg⁻¹), from World Ocean Atlas 2023
- **Chla**: Climatology of sea surface chlorophyll-a concentration for the grid (unit: mg m⁻³), from satellite observations
- **GPP**: Climatology of gross primary production integrated over the euphotic zone for the grid (unit: mmol O₂ m⁻² d⁻¹), calculated from light-dark bottle dataset
- **CR**: Climatology of community respiration integrated over the euphotic zone for the grid (unit: mmol O₂ m⁻² d⁻¹), calculated from light-dark bottle dataset
- **NCP**: Climatology of net community production integrated over the euphotic zone for the grid (unit: mmol O₂ m⁻² d⁻¹), calculated from light-dark bottle dataset
- **NCP_corr**: Climatology of net community production integrated over the euphotic zone for the grid (unit: mmol O₂ m⁻² d⁻¹), calculated from the light-dark bottle dataset after correcting for bacterial respiration rate

# WOA_RS_grid.csv
1° gridded climatology dataset of the global ocean.
- **Lon**: Grid longitude
- **Lat**: Grid latitude
- **Month**: Month
- **MLD**: Climatology of mixed layer depth for the grid (unit: m), from HYCOM
- **Temp_WOA**: Climatology of sea surface temperature for the grid (unit: °C), from World Ocean Atlas 2023
- **Sal_WOA**: Climatology of sea surface salinity for the grid, from World Ocean Atlas 2023
- **DO_WOA**: Climatology of sea surface dissolved oxygen concentration for the grid (unit: µmol kg⁻¹), from World Ocean Atlas 2023
- **Si_WOA**: Climatology of sea surface silicate concentration for the grid (unit: µmol kg⁻¹), from World Ocean Atlas 2023
- **NO3_WOA**: Climatology of sea surface nitrate concentration for the grid (unit: µmol kg⁻¹), from World Ocean Atlas 2023
- **PO4_WOA**: Climatology of sea surface phosphate concentration for the grid (unit: µmol kg⁻¹), from World Ocean Atlas 2023
- **SST**: Climatology of sea surface temperature for the grid (unit: °C), from satellite observations
- **Chla**: Climatology of sea surface chlorophyll-a concentration for the grid (unit: mg m⁻³), from satellite observations
- **VGPM**: Climatology of net primary production integrated over the euphotic zone for the grid (unit: mg C m⁻² d⁻¹), from the vertically generalized production model
- **CbPM**: Climatology of net primary production integrated over the euphotic zone for the grid (unit: mg C m⁻² d⁻¹), from the carbon-based primary productivity model
- **GPP**: Climatology of gross primary production integrated over the euphotic zone for the grid (unit: mmol O₂ m⁻² d⁻¹), from Huang et al. (2021)

# inverse_biogeochemical_model.csv
1° gridded dataset of global ocean total organic carbon export rates, derived from Wang et al. (2023).
- **Lon**: Grid longitude
- **Lat**: Grid latitude
- **TOCflux**: Rate of total organic carbon export (particulate organic carbon and dissolved organic carbon) at the euphotic zone (unit: mg C m⁻² d⁻¹)


**/output summary:**\
corrected_BR.csv:
Field measurements from the marginal seas of the Northwest Pacific during five cruises between 2018 and 2023, showing corrected bacterial respiration and net community production at sampling stations.
- **BGE**: Bacterial Growth Efficiency
- **BR_total**: Total bacterial respiration rate during 24-hour bottle incubations, including in situ bacterial respiration and the overestimated bacterial respiration due to the bottle incubation effect (unit: mg C m⁻³ d⁻¹)
- **BR_insitu**: Corrected in situ bacterial respiration (unit: mg C m⁻³ d⁻¹)
- **BRbias**: Overestimated bacterial respiration due to the bottle effect (unit: mg C m⁻³ d⁻¹)
- **BRbias_CR**: Proportion of overestimated bacterial respiration relative to community respiration
- **NCP_corr**: Net community production after correcting for bacterial respiration

global_NCP_grid_EOF.csv:
1° gridded climatology dataset of global ocean's net community production, based on global predictions made using a machine learning model derived from the light-dark bottle dataset.
- **Lon**: Grid longitude
- **Lat**: Grid latitude
- **Month**: Month (1-12 represents each month; 13 represents the annual average)
- **NCP**: Global NCP predictions before bacterial respiration correction (unit: mg C m⁻² d⁻¹)
- **NCP_corr**: Global NCP predictions after bacterial respiration correction (unit: mg C m⁻² d⁻¹)


**/scripts summary:**\
figurexx.R - generates figure xx\
tablexx.R - generates table xx\


