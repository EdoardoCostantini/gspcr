# Project:   gspcr
# Objective: Script to re-compile pre-compiled vignettes
# Author:    Edoardo Costantini
# Created:   2023-11-07
# Modified:  2023-11-08
# Notes:     see https://www.kloppenborg.ca/2021/06/long-running-vignettes/

# Store current wd
old_wd <- getwd()

# Define vignette names
vignettes <- c(
    "vignette-1-example-analysis",
    "vignette-2-gspcr-arguments",
    "vignette-3-alternatives"
)

# Decide which vignette to compile
vignette_target <- vignettes#[1]

# Move to the vignette forlder to simplify figure/ management
setwd("vignettes/")

# Compile the vignettes that takes a lot of time
lapply(vignette_target, function(X){
    knitr::knit(
        input = paste0(X, ".Rmd.orig"),
        output = paste0(X, ".Rmd")
    )
})

# Revert to original working directory
setwd(old_wd)

# Compile the vignettes
lapply(vignette_target, function(X) {
    devtools::build_rmd(paste0("vignettes/", X, ".Rmd"))
})