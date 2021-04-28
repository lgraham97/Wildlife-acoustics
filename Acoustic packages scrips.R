#Lorna Graham 
#Wildlife acoustic data 

#Getting started####
install.packages("devtools")
devtools::install_github("https://github.com/DenaJGibbon/behaviouR", force = TRUE)
#Warning message appeared, used force=TRUE to force download and needed to select [1] in the consol to update 

library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)

#creating Rscript in project to store commands for this practical example 