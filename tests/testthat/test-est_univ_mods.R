# Project:   gspcr
# Objective: Test est_univ_mods.R function
# Author:    Edoardo Costantini
# Created:   2023-04-13
# Modified:  2023-04-13
# Notes: 

# Test: continuous dv, continuous preds ----------------------------------------

est_univ_mods(
    dv = GSPCRexdata$y$cont,
    ivs = GSPCRexdata$X,
    fam = "gaussian"
)

# Test: binary dv, continuous preds ----------------------------------------

est_univ_mods(
    dv = GSPCRexdata$y$bin,
    ivs = GSPCRexdata$X,
    fam = "binomial"
)

# Test: continuous dv, continuous preds ----------------------------------------

est_univ_mods(
    dv = GSPCRexdata$y$ord,
    ivs = GSPCRexdata$X,
    fam = "cumulative"
)

# Test: continuous dv, continuous preds ----------------------------------------

est_univ_mods(
    dv = GSPCRexdata$y$cat,
    ivs = GSPCRexdata$X,
    fam = "baseline"
)

# Test: poisson dv, continuous preds ----------------------------------------

est_univ_mods(
    dv = GSPCRexdata$y$pois,
    ivs = GSPCRexdata$X,
    fam = "poisson"
)