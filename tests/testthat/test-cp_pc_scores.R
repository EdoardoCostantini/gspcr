# Project:   gspcr
# Objective: Test the cp_pc_scores function
# Author:    Edoardo Costantini
# Created:   2023-05-30
# Modified:  2023-05-30
# Notes:

# Test: function works on numeric data -----------------------------------------

# Use the function with training = validation data
pc_scores <- cp_pc_scores(
    X_train = mtcars[1:20, -1],
    X_valid = mtcars[-c(1:20), -1],
    Q = 3
)

# Test: function works on mixed data -------------------------------------------

# Sample training data for iris data 
train <- sample(1:nrow(iris), nrow(iris) * 2/3)
test <- -train

# Use the function with training = validation data
pc_scores <- cp_pc_scores(
    X_train = iris[train, ],
    X_valid = iris[test, ],
    Q = 3
)
