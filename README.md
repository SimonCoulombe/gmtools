# Gmtools

## Motivation

Traditional Insurance Risk Modeling uses a very specific tool set. Risk modeler are therefore are highly specialized; meaning that producing models with these tools quickly yields high quality models. This sets something of a high bar which new tools and techniques must clear to gain acceptance.

This package is aimed at providing various accelerators to help risk modelers prove the value of Machine Learning techniques as applied to insurance modelling. Insurance Risk modelling has well established best practices and therefore we need to make sure that any new ML techniques either use existing or have equivalent functionality. 

## Capabilities

This section outlines the capabilities that are required when buiding risk models - we also give a high level overview of the functions within the package where they meet these capabilities:

#### Fast Model Fitting

- Typically we use GLMs
- GLMs fit quite fast anyway, they fit really fast in emblem
- This allows an iterative approach making model building intuitive
- Any machine learning approaches should handle large datasets

- There are three main modelling packages utilised throughout this project; XGBoost, LightGBM and h2o. They're all extremely fast and can scale to extremely large datasets with appropriate compute resources.
- For data manipulation we make use of the data.table package which allows fast data manipulation for large datasets.

The following functions are useful for high speed model fitting:

1. train_xgboost: a wrapper for the xgboost package allows highly parralelized computation of GBDTs. This package will scale to extremely large datasets.
2. train_lightgbm: a wrapper for the lightgbm pacakge, again it allows for highly parrallelized compuation of GBDTs - the speed of modelling fitting is even faster than xgboost.
3. fit_h2o_mdl: a wrapper for the fitting of models using the h2o package - this package is based on a java implementation allowing massively parralelised computation of GLMs, GBMs, RandomForests & feed forward neural networks.

#### Graphical Interfaces

- Current state of the art techniques produce a vast array of graphics that allow an intutive understanding of the model and the underlying data
- Any new tools should make the production of these familiar graphics as automated as possible (and improve on them where necessary)
- These interfaces are also very easy for beginners to use; therefore techniques that require a lot of high level coding are unable to gain traction.

- Building a graphical interface  

#### Tools for explaining the model

- The traditional GLMs used in the insurance industry are white-box, all the information for how the model calculates a prediction is easily available. You could even calculate the results by hand if you were so inclined.
- Good tools for interpreting the models behaviour are essential if a tool is going to gain traction

#### Generic Machine Learning Data Manipulation Tools

- we need help with encoding etc
- currently we do it all in one concerted effort



