<div align="right">
  <img src="./figs/logo.png" alt=" " width="150">
</div>

# A Topology of Voter Turnout in the United Kingdom

This repository contains my extended research project, submitted to the University of Manchester for the degree of Master of Science (MSc) in Data Science. The study’s objective is to generate a multidimensional map of the political and socioeconomic landscape of electoral participation in the UK. Using Topological Data Analysis Ball Mapper (TDABM) algorithm (Dłotko, 2019), I cluster the 650 parliamentary constituencies in a multidimensional space based on their observed characteristics at the 2010, 2015, 2017, and 2019 UK General Elections.

The repository is structured as follows:

- _pre-processing.R_: Pre-processing steps for data cleaning and unification
- _modelling.R_: Baseline OLS models and spatial econometric models
- _visualisations.R_: Exploratory spatial analysis
- _tda.ipynb_: Application of TDA to map constituencies in a multidimensional space
- _tdaDesc.ipynb_: Descriptive analysis of topologies
- _tdaResiduals.ipynb_: Analysis of residuals patterns in the topologies
- _tdaResidualMapping.ipynb_: Descriptive analysis of balls with low model performance


 ![alt text](https://github.com/Alexanderbenit7/turnout-uk/blob/master/figs/full_topologies70_continuous.jpg?raw=true)
