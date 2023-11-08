# Project:   gspcr
# Objective: Script to re-compile pre-compiled vignettes
# Author:    Edoardo Costantini
# Created:   2023-11-07
# Modified:  2023-11-08
# Notes:     see https://www.kloppenborg.ca/2021/06/long-running-vignettes/

# # Store current wd
old_wd <- getwd()

# # Move to the vignette forlder to simplify figure/ management
setwd("vignettes/")

# Compile the vignettes that takes a lot of time
knitr::knit("main-features.Rmd.orig", output = "main-features.Rmd")

# Revert to original working directory
setwd(old_wd)

# Compile the vignettes
devtools::build_rmd("vignettes/main-features.Rmd")
