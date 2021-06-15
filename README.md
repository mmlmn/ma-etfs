# MA strategies on Polish ETFs

This code can be used to replicate the results of a study of MA strategies on Polish-listed ETFs.

To start, visit [Examples.R](Examples.R), where you will find explanations on how to generate MA strategies and analyse them.

All results are available in .Rmd and pdf files in [extras](./data/extras)

# Technical notes

To run the code, you need the required R packages as well as a C++ compiler.
In order to install the required packages, simply visit Examples.R and run the first code chunk labeled 'Environment setup'. It will check if the required libraries are already installed and load them, at the same time installing the missing ones.
For C++ compiler, Clang or GCC should do. On Windows, you may try [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

If you have trouble compiling the Rcpp chunk, consider putting the project in a directory path without spaces.
