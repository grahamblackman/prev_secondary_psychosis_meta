########################################################################################
#                                                                                      #
# meta_sec_psy_master.R from Blackman et al                                            #
#                                                                                      # 
# Secondary causes of psychosis: A systematic review and meta-analysis                 # 
#                                                                                      #
#                                                                                      #
# Master script                                                                        #
#                                                                                      #
# Code base and data: https://github.com/grahamblackman/prev_secondary_psychosis_meta  #
#                                                                                      #
########################################################################################


# Prerequisites:
#
# * set the working directory to the directory of the scripts and datafile
# * You will also need to have a folder in this directory entitled 'output'
#
# This script loads the relevant libraries, defines key functions, and executes the
# subscripts.
#
# The following line will clear all objects from memory. Useful for a 'fresh start' when
# debugging but not necessary for the script to run
#
# rm(list = ls()) 

# The following line clears all the plots from RStudio if this script is being run inside it
# Useful for similar 'fresh start' reasons
# Throws up an error if there are no plots to clear but not a problem for the script
#
# dev.off(dev.list()["RStudioGD"])

# For the following write_to_file variable (used by the visualistion scripts)
#  set to 0 to display (e.g. RStudio)
#  set to 1 to write figures to ./output/ directory (must already exist in the working directory)
write_to_file = 1

# Load packages

library("meta") # for meta analysis
library("metafor") # for meta analysis
library("readxl")# for reading excel documents
library("skimr") # for summary
library("tidyverse") # multiple packages, inc ggplots2
library("lubridate") # dealing with dates
library("MASS") # Functions and datasets to support "Modern Applied Statistics with S
library("janitor") # cleaning data
library("lme4") # linear model 4, required for meta analysis
library("expss") #Tables, Labels and Some Useful Functions from Spreadsheets (eg excel and SPSS' Statistics)
library("scales") # descriptions 
library("forester") # visualised multiple meta analytic estimates onto a single forest plot  

# Ensure names map to explicit package functions and are not overwritten

escalc <- metafor::escalc
rma <- metafor::rma
transf.ipft.hm <- metafor::transf.ipft.hm
forest <- metafor::forest
funnel <- metafor::funnel
count_if <- expss::count_if

# Load data

datafile_path = paste("secondary_psych_data.xlsx")

data <- read_xlsx(datafile_path, sheet = "data")

# Execute subscripts
source("meta_sec_psy_pro.R") 
source("meta_sec_psy_desc.R", echo = TRUE) 
source("meta_sec_psy_prev.R", echo = TRUE)  
source("meta_sec_psy_prev_subtype.R", echo = TRUE) 
source("meta_sec_psy_prev_investigation.R", echo = TRUE) 
source("meta_sec_psy_vis_file.R")
source("meta_sec_psy_vis_subtypes_file.R")  
source("meta_sec_psy_vis_investigations_file.R")  

# Display R version and package versions

version

print_package_version <- function(package_name) {
  sprintf("Package version for %s is %s", package_name, packageVersion(package_name)) # for meta analysis
}

print_package_version("meta")
print_package_version("metafor")
print_package_version("readxl")
print_package_version("skimr")
print_package_version("tidyverse")
print_package_version("lubridate")
print_package_version("MASS")
print_package_version("janitor")
print_package_version("devtools")
print_package_version("lme4")
print_package_version("expss")
print_package_version("scales") 
print_package_version("forester") 
