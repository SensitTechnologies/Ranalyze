# Ranalyze
R code to analyze data in CSV files

This project was created to analyze data collected via [Sensit Test Suite](https://github.com/SensitTechnologies/TestSuite).  This data exists in multiple comma-separated-value (CSV) files.  It's also a good introduction to the R language, since it was created by folks who were themselves new to R and its capabilities.

## Prerequisites
Install [R Studio](https://rstudio.com/), and clone this repository.

## Usage
To do anything useful, you'll need some CSV files.
1. Open Data AnalysisProject.Rproj.  This will open R Studio.
2. In the Source Window (top left), open SensorAnalysisL1.R, SensorAnalysisL2.R, SensorAnalysisL3.R.
3. In the Source Window (top left), click "Source" for each of the three files listed above to add their functions to the Global Environment (top right).
4. In the Console (bottom left), type *loadLibraries()*.  This prepares the R environment by downloadinglibraries you need.
4. In the Workspace Window (bottom right), navigate to the directory with CSV files you wish to analyze.
5. In the Workspace Window (bottom right), click *More --> Set As Working Directory*.
6. In the Console (bottom left), type *findTestDevices()*.
7. In the Console (bottom left), type *analyzeTest()*.  Plots should be generated.
8. In the Plots Window (bottom right), click the left and right arrows to navigate between the various generated plots.
