PatientLevelPredictionUI
======================

Introduction
============
An UI built on top of the R package for building patient level predictive models using data in Common Data Model format.

Features
========
- Takes a cohort and outcome of interest as input.
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Uses a large set of covariates including for example all drugs, diagnoses, procedures, as well as age, comorbidity indexes, etc.
- Large scale regularized regression to fit the predictive models.
- Includes function for evaluating predictive models
- Supported outcome models are logistic, Poisson, and survival (time to event). TO ADD.

Technology
==========
PatientLevelPredictionUI is a Shiny application running on R.

System Requirements
===================
Requires R (version 3.2.2 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in PatientLevelPrediction require Java.

Dependencies
============
 * PatientLevelPrediction
 * Cyclops
 * DatabaseConnector
 * SqlRender

Getting Started
===============
1. On Windows, make sure [RTools](http://cran.r-project.org/bin/windows/Rtools/) is installed.
2. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
3. In R, use the following commands to download and install PatientLevelPredictionUI:

  ```r
  install.packages("shiny")
  install.packages("devtools")
  library(devtools)
  install_github("ohdsi/SqlRender") 
  install_github("ohdsi/DatabaseConnector") 
  install_github("ohdsi/Cyclops") 
  install_github("ohdsi/PatientLevelPrediction") 
  ```

Getting Involved
================
To Add
 
License
=======
PatientLevelPredictionUI is licensed under Apache License 2.0

Development
===========
PatientLevelPredictionUI is being developed in R Studio.

Alpha

# Acknowledgements
- To Add