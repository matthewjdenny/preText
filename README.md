# preText -- Master: [![Travis-CI Build Status](https://travis-ci.org/matthewjdenny/preText.svg?branch=master)](https://travis-ci.org/matthewjdenny/preText)  
An R package to assess the consequences of text preprocessing decisions.

 **[[getting started with preText vignette]](http://www.mjdenny.com/getting_started_with_preText.html)**.

The paper detailing the procedure can be found at the link below:

* Matthew J. Denny, and Arthur Spirling (2016). "Assessing the Consequences of Text Preprocessing Decisions". [[ssrn.com/abstract=2849145]](https://ssrn.com/abstract=2849145)

## Installation
We are currently working on getting a version of the package up on CRAN. As soon
as it is available, we will update the installation instructions.

If you want to get the latest version from GitHub, start by checking out the 
**Requirements for using C++ code with R** section in the following 
tutorial: [Using C++ and R code Together with Rcpp](http://www.mjdenny.com/Rcpp_Intro.html). 
You will likely need to install either `Xcode` or `Rtools` depending on whether 
you are using a Mac or Windows machine before you can install the preText package 
via GitHub, since it makes use of C++ code.

	install.packages("devtools")
   
Now we can install from Github using the following line:

	devtools::install_github("matthewjdenny/preText")

Once the `GERGM` package is installed, you may access its functionality as you 
would any other package by calling:

	library(preText)

If all went well, you should be able to replicate the steps in the `vignette("getting_started")`.

## Basic Usage

The basic functionality of this package is detailed in a vignette, which is  **[[available here]](http://www.mjdenny.com/getting_started_with_preText.html)**. Beyond this basic functionality the package includes a number of additional utility and analysis functions for exploring and comparing multiple document--term matrices. 

## Bug Reporting

**PLEASE REPORT ANY BUGS OR ERRORS TO <mdenny@psu.edu>**. 
