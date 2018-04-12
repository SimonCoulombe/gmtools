# Gmtools

## Motivation

Traditional Insurance Risk Modeling uses a very specific tool set. Risk modelers are therefore are highly specialized; meaning that producing models with these tools quickly yields high quality models. This sets something of a high bar which new tools and techniques must clear to gain acceptance.

This package is aimed at providing various accelerators to help risk modelers prove the value of Machine Learning techniques as applied to insurance modeling. Insurance Risk modeling has well established best practices and therefore we need to make sure that any new ML techniques either use existing or have equivalent functionality. 

## Capabilities

This section outlines the capabilities that are required when building risk models - this can be used to better understand the nature of the functions included within the package.

#### Fast Model Fitting

Typically risk models are based around GLMs which have a number of strengths namely;

1. They are quick to fit, especially when using some of the standard proprietary packages
2. The quickness of fitting allows for an iterative (and somewhat intuitive) approach
3. They can handle relatively large datasets without issue (60M records and up)

Any prospective techniques should therefore be able to scale well to large datasets and do so in a timely manner. With these constraints in mind the following algorithms are heavily used in this package:

- XGBoost: scales well to large dataset and can be enabled for GPU computation which further increases speed
- LightGbm: demonstratively faster than XGBoost in most settings, it can also be compiled with GPU support
- H2O: comes with a wide range of fully parallelized algorithms making it easy to work with on large datasets.
- For data manipulation we make use of the data.table package which allows fast data manipulation for large datasets.

#### Graphical Interfaces

The existing tools often present results graphically , this means that the packages are extremely easy to use for the novice. The obvious trade off in the ability to automate and scriptability not withstanding. 

For new machine learning tools to gain acceptance they should be capable of producing similar graphics in order to allow for a "gentle" introduction. However for Machine Learning techniques to shine there must be a heavy emphasis on coding skills.

#### Tools for explaining the model

The traditional linear model and its generalized counter part are often portrayed as highly interpretable. Whilst we don't believe that they're as interpretable as often claimed they do have the handy feature of directly observable coefficients. Senior stakeholders will never be comfortable signing of Machine Learning Models unless the modeler can build some confidence that he or she properly understands the behavior of the model.

To that end a number of functions are included in this package which make model explanation easier.

#### Generic Machine Learning Data Manipulation Tools

The basic Machine Learning encoding strategies are not well understood by risk modelers - this is for no other reason than that they have never been required to understand them until now. To reduce the time spent on this essential (and sometimes complex) activity we've included a number of methods that make the most well known types of encoding highly accessible.





