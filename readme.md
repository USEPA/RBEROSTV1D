# RBEROSTV1D

## Brief Project Description

This project expands the RBEROST[1] framework for implementation in the Puget Sound Basin from a static to a dynamic version. It extends the framework from reliance on long-term average loadings to seasonal x 20-yr annual loading estimates derived from the USGS Dynamic SPARROW model for the Puget Sound [2].  The River Basin Export Reduction Optimization Support Tool (RBEROST) uses data on best management practice (BMP) efficiencies, costs, and baseline nutrient loadings to create an optimization problem that will meet numerous loading targets in the watershed for the lowest financial cost.

The main branch of this github repository includes the data and code files necessary to run RBEROSTv1D, raw data and code used to format data for use in RBEROST.

## Getting Started with Package Management

This project now uses renv for reproducible package management. This ensures all users have the same package versions as the developers.

## First-Time Setup (New Users)
Open the R project file: RBEROSTV2D.Rproj
Run the setup script: renv::status() to check the state of your package library or run renv::restore() to install all required packages.
Wait for packages to install (this may take several minutes)

## Running dynamic RBEROST
There are 4 steps to running RBEROST.

# Assembling and formatting data. 
Code files that begin with 00_ are files that format a certain type of data for use in the RBEROST model. Data for the case study in the Long Island Sound River basin are provided, and RBEROST expects data to be formatted the same as these example files.
# Single watershed versus batch mode.
Two RMarkdown files are provided to support optimizations.  To run optimizations for a single watershed, click on RunRBEROST-Pacific-Dynamic.Rmd to start.  To run optimizations for multiple watersheds, click on RunRBEROST-Pacific-Dynamic_Batch.Rmd to start.
# Writing the AMPL scripts. 
The mathematical optimization for RBEROST is written in AMPL. This step is accomplished by running the "Run Preprocessor" section in RunRBEROST-Pacific-Dynamic.Rmd. There are several items users can edit before running this code, including the file path where the data are located, the file path where output files should be written to, the planning horizon (default is 15 years), the expected interest rate (default is 3%), whether or not to consider only "actionable" load sources (subject to management practices represented in RBEROST) in meeting targets, and whether or not to include uncertainty analysis. If users choose to include uncertainty analysis, they additionally may choose the number of scenarios they wish to view, and the step change between these scenarios. The step change is the percent margin of error that will be added incrementally between each scenario. Running this code chunk will source the R script 01_Optimization_Preprocessing_Pacific_Seasonal_V6_[PROD].R, which takes the data files, provided as csv files, and creates the AMPL scripts that describe the mathematical problem. The 3 scripts are the command script, the data script, and the model script. To define the problem, the user should edit the two 01_UserSpecs files to define (1) the BMPs they wish to implement, the extent of their implementation, BMP costs, and design depths and (2) the locations, type and target reduction of nutrient loading targets within the watershed. Defaults for many of these options are given, but the user may override any parameter they wish. If the user chooses to include uncertainty in their analysis, RBEROST will then source 01_Optimization_Preprocessing_Uncertainty_Pacific_Seasonal_V6_[PROD].R, and three additional AMPL files will be created with the tags "_uncertainty".  
# Submitting the AMPL scripts for optimization. 
RBEROST uses a free online server, NEOS[3], to solve the AMPL optimization problem. The tool currently uses the CPLEX solver. The code chunk "Submit AMPL files to NEOS Server" will submit the AMPL files generated in the preprocessing step to the NEOS server for optimization and return a results text file.  Alternatively, users can manually submit the AMPL files to the NEOS server - see documentation for details. 
# Viewing NEOS results. 
The results of the NEOS optimization can be viewed by running the code chunk "Run Postprocessor" in RunRBEROST-Pacific-Dynamic.Rmd. This code will launch an RShiny app by sourcing the User Interface and Server files provided in the "R" folder. Within the RShiny app, users can dynamically choose the NEOS results they wish to view. After providing necessary results .txt file and several of the input .csv files, the user will be able to preview the uploaded files, and see a summary report of total cost and BMP implementation. The user may also download more details of BMP implementation in each NHD+ subcatchment as csv files.

The project is organized into 4 folders.

R: The R folder contains all code files. 00_*.R files process data for use in the optimization, 01_Optimization_Preprocessing_Pacific_Seasonal_V6_[PROD].R runs the preprocessor and creates AMPL script files, 01_Optimization_Preprocessing_Uncertainty_Pacific_Seasonal_V6_[PROD].R runs the preprocessor with the uncertainty analysis module included, and creates AMPL script files, and 01_Optimization_Preprocessing_gateway.R selects which script to run based on user inputs to RunRBEROST-Pacific-Dynamic.Rmd. 02_Optimization_RunShiny.R sources Optimization_ServerFile.R, Optimization_UI_Postprocessor.R, and Optimization_UserInterfaceFile.R files to run the Shiny app that summarizes and reports the results. Optimization_HelperFunctions.R writes functions that are used elsewhere in the tool - this file is sourced in multiple other R scripts.

Preprocessing: The Inputs folder contains all of the csv files necessary to run the preprocessor, including the 01_UserSpecs_BMPs.csv and 01_UserSpecs_loadingtargets.csv files. The Outputs folder contains the written AMPL scripts and results files.

Data: The Data folder includes original data sources that are processed and formatted to produce the Preprocessing input files used by RBEROST.
Figures: The Figures folder includes an R markdown file and resulting output that produces map figures for publications.

SensitivityAnalysis: This folder includes programs to run sensitivity analyses.

Questions about code can be directed to: Naomi Detenbeck detenbeck.naomi@epa.gov or Craig Connolly connolly.craig@epa.gov

Questions about the project can be directed to: Naomi Detenbeck detenbeck.naomi@epa.gov  or Craig Connolly connolly.craig@epa.gov

[1] https://github.com/USEPA/RBEROSTv2s [2] https://apps.usgs.gov/sparrow/sparrow-puget-sound/ [3] https://neos-server.org/neos/index.html

### Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
