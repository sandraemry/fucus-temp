## May 22, 2018 - Going to try and load ibutton data and clean it
 #load necessary packages
library(tidyverse)

#remove anything in global environment
rm(list = ls())

# create list of all file paths for csv files in the ibutton folder
cell_files <- 