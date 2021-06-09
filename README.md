---
output:
  html_document: default
  pdf_document: default
---
# Content description and contact information

This repository contains all R-scripts and respective model output data needed to reproduce the methods, results and figures of the manuscript 
*Browsing herbivores improve the state and functioning of savannas – a model assessment of alternative land use strategies* by Katja Irob, Niels Blaum, Selina Baldauf, Leon Kerger, Ben Strohbach, Angelina Kanduvarisa, Dirk Lohmann and Britta Tietjen. 
For any questions regarding content in this repository please contact <a href="mailto:irob.k@fu-berlin.de">Katja Irob</a>.

## R-Scripts 
R-Scripts to reproduce all figures and statistical procedures are provided in the folder *Analysis* and the corresponding data in the *Data* folder. Scripts for the evaluation of the sensitivity analysis output and plant species parameterisation are in the folder *Sensitivity*, corresponding sensitivity output data in the folder *Input*. If you want to reproduce the results please download the repository and run the code with R (most conveniently in R-Studio).
For any questions regarding the R-Scripts please contact <a href="mailto:irob.k@fu-berlin.de">Katja Irob</a>.

| Script        | Description   | 
| ------------- |-------------| 
| Analysis/Coverplots.R | Script to visualise and analyse the meta-PFT cover time series for the land use scenarios used in the main part of the manuscript | 
| Analysis/Coverplots_appendix.R| Script to create the meta-PFT cover time series for all other land use scenarios presented in the appendix | 
| Analysis/Composition_plots.R| Script to visualise and analyse the strategy-type composition of the last 20 years of simulation for each scenario used in the main part of the manuscript|
| Analysis/Composition_appendix.R| Script to visualise the strategy-type composition of the last 20 years of simulation for additional scenarios presented in the appendix |
| Analysis/Richness_evenness.R| Generation of richness and evenness values, visualisation and analysation of the data of the last 20 years of simulation for each scenario used in the main part of the manuscript|
| Analysis/Richness_evenness_appendix.R| Generation of richness and evenness values and visualisation of the data of the last 20 years of simulation for additional scenarios presented in the appendix |
| Analysis/EFA.R| Exploratory factor analysis to determine clusters depending on plant strategies. Additional calculation of functional dispersion (FDis) and visualisation of PFT clusters including individual abundance and distance to centroid for the main scenarios |
| Analysis/Waterplots.R | Calculation of T/ET, visualisation of T/ET and soil moisture and statistical evaluation for the main scenarios |
| Analysis/Waterplots_appendix.R | Calculation of T/ET, visualisation of T/ET and soil moisture for all additional scenarios |
| Sensitivity/Parameterisation_sensitivity_browse.R | Merging sensitivity output for all parameters and climate repetitions, determining parameter value at desired cover change based on linear regression analysis, visualisation of output for browsing scenarios | 
| Sensitivity/Parameterisation_sensitivity_graze.R | Merging sensitivity output for all parameters and climate repetitions, determining parameter value at desired cover change based on linear regression analysis, visualisation of output for grazing scenarios | 

## Folder /Data

### Folder /Results + /Appendix

This folder contains model output files for grazing and browsing scenarios for the stocking rates 20 ha/LSU and 40 ha/LSU. The filename e.g. *"EH_100years_yearly_EH_SR20browse_climrep-1.txt"* refers to the site "Etosha Heights" (EH), 100 years simulation time and yearly output, the stocking rate (SR) 20 ha/LSU of browsers and the climate repetition 1 (of 30). /Appendix additionally contains the result  files  for the stocking rates 10,  20, 30 and 50. The files contain the complete model output and are condensed to the variables of interest using the R scripts in */Analysis*. Only the output variables used in this study are explained below. 


| File        | Description   | 
| ------------- |-------------| 
|EH_100years_yearly_EH_SR20/40browse/graze_climrep-x.txt | EcoHyD simulation output for 100 years based on environmental conditions at Etosha Heights, Namibia. <br> **Year**: year from 0-99 <br> **meanGcover_x**: mean perennial grass cover of strategy types 0-9 [-] <br> **meanScover_x**: mean shrub cover of strategy types 0-11 [-] <br> **meanAcover_x**: mean annual grass cover of base-type [-] <br> **meanGtotalcover**: total perennial grass cover [-] <br> **meanStotalcover**: total shrub cover [-] <br> **meanAtotalcover**: total annual grass cover [-] <br> **Annualevaporation**: total annual evaporation [Vol%] <br> **AnnualtranspirationL1**: total annual plant transpiration of layer 1 [Vol%] <br> **ML1**: soil moisture in layer 1 during rainy season [Vol%] |

# Folder /Model

Model source files  can be found in */Source*. To reproduce examples, download repository and keep folder structure. Output files are stored in  */Results*.

## Folder /Parameters

| File        | Description   | 
| ------------- |-------------|
| elevation_30_EH.txt |  Digital  elevation map in 30 by 30 grid based on study site at Etosha Heights. [m] |
|  modelparameters_control.txt  | General configuration parameters for model EcoHyD. <br> **Site**: name of site [-]		<br> **Latitude**: 	latitude of the site 	[°S] <br> **simYears**: total simulation time	[years]		<br> **xsize**: x length of the grid (number of x-cells)	[-]	<br> **ysize**: y length of the grid (number of y-cells)	[-] <br> **cellsize**: 	size of a single cell		[m]	<br> **vegTimeStep**: 	number of days after which vegetation is updated	[days] |
| modelscenarios.txt  | Scenarios, climate repetitions and repetitions are determined here. 
Input files will be chosen and output files will be coded based on IDs and repetitions you indicate here.|
| soilparameters_EH.txt | Soil parameters for loamy sand, calibrated for Etosha Heights. |  
| vegetationsparameters_*ScenarioName*.txt | General vegetation  parameters and specific parameters for the PFTs shrubs, perennial and annual grasses. Sub-types are generated here. | 

##  Folder /Weather

Climate input data for EcoHyD. Precipitation and temperature time series generated in NamRain based on TRMM series.

| File        | Description   | 
| ------------- |-------------|
| EH_100years_control_climrep-*1-30*.txt |  Climate files at hourly resolution in Y (year), M (month), D (day), H (hour)  format. <br> **Rain**: Hourly precipitation amount  [mm] <br> **Temp**: Hourly temperature  [°C] |




## Folder /Sensitivity

### Folder /Input

The folders *Browse* and *Graze* hold the single result  files  of  the  sensitivity analysis for grazing and browsing respectively for both perennial grasses  and shrubs. The parameterisation is based  on this  output. 

| File        | Description   | 
| ------------- |-------------|
|/Browse/perennials_sensoutput_**ParameterName**_**Date**.txt |  Cover  change  of  perennial grasses in response  to parameter value changes in browsing scenario for the parameters: biomass production (GR), mortality (MR), palatability (GP), defense (GL), competitive strength for water (UR), water needed for establishment (ES) and resistance to drought (WP).|
|/Browse/shrubss_sensoutput_**ParameterName**_**Date**.txt | Cover  change  of  shrubs in response  to parameter value changes in browsing scenario for the parameters: biomass production (GR), mortality (MR), palatability (GP), defense (GL), competitive strength for water (UR), water needed for establishment (ES) and resistance to drought (WP). |
|/Graze/perennials_sensoutput_**ParameterName**_**Date**.txt |  Cover  change  of  perennial grasses in response  to parameter value changes in grazing scenario for the parameters: biomass production (GR), mortality (MR), palatability (GP), defense (GL), competitive strength for water (UR), water needed for establishment (ES) and resistance to drought (WP).|
|/Graze/shrubss_sensoutput_**ParameterName**_**Date**.txt | Cover  change  of  shrubs in response  to parameter value changes in grazing scenario for the parameters: biomass production (GR), mortality (MR), palatability (GP), defense (GL), competitive strength for water (UR), water needed for establishment (ES) and resistance to drought (WP). |

## Folder /Parms

| File        | Description   | 
| ------------- |-------------|
| Sens_output_parameter_browsing_na.txt |  Standard parameter values of the base-types of perennial  grasses and shrubs used as reference for  parameterisation of strategy types in browsing scenarios.  Included are the parameters biomass production (GR), mortality (MR), palatability (GP), defense (GL), competitive strength for water (UR), water needed for establishment (ES) and resistance to drought (WP).|
| Sens_output_parameter_grazing_nc.txt |  Standard parameter values of the base-types of perennial  grasses and shrubs used as reference for  parameterisation of strategy types in grazing scenarios.  Included are the parameters biomass production (GR), mortality (MR), palatability (GP), defense (GL), competitive strength for water (UR), water needed for establishment (ES) and resistance to drought (WP).|




