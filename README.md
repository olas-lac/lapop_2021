## LAPOP 2021

This repository contains the information necessary for generating the indicators used by the OLAS. The repository contains two scripts and two folders, inputs and outputs.

### Inputs
- exchange_rates_wb.xls - World bank exchange rates for converting monetary indicators
- water_lapop.Rda - Full LAPOP 2021 dataset
- lapop_2021_income_table.csv - Lookup table for income amounts for each value in the LAPOP dataset

### Scripts
This repository contains two scripts, the r script for processing the raw data and generating indicators and the STATA script used to imputate incomes from the 
ranges included in the LAPOP survey.

- olas_lapop_2021.R
- income_generation_2021.do

Users should run the code from olas_lapop_2021.R up to line 96. Then, users should add a folder in their project called "stata_income_generation" for houseing the income data generated by the .do file, and then run income_generation_2021.do. 
Once users confirm that the stata_income_generation folder contains a file for each country, users should run lines 100 - to the end of the olas_lapop_2021.R script.

### Outputs

The script outputs the final indicator dataset in a few formats (wide and long). If users are interested in the data without wanting to run the scripts, 
the long version is available in this repository.

