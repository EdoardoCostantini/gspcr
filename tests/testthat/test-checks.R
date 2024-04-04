# Project:   gspcr
# Objective: Test checking functions
# Author:    Edoardo Costantini
# Created:   2023-11-17
# Modified:  2023-11-17
# Notes: 

# Test check_npcs_range -----------------------------------------------------------

# Returns message when when too many pcs are demanded
expect_warning(
    check_npcs_range(
        npcs_range = c(1, 3, 7),
        ivs = iris
    ),
    regexp = NULL
)

# Returns the right correction when too many pcs are demanded
suppressWarnings(
    expect_equal(
        1:ncol(iris),
        check_npcs_range(
            npcs_range = 1:10,
            ivs = iris
        )
    )
)

# Test check_factors -----------------------------------------------------------

# Consider a dataset with a factor with empty levels
df_empty <- cbind(
    iris[1:100, ],
    Species2 = iris[51:150, "Species"], # Only two levels
    Species3 = iris[c(1:30, 50:80, 100:138), "Species"] # All three levels
)

# Returns message when when empty categories are there
expect_warning(
    df_empty_checked <- check_factors(ivs = df_empty),
    regexp = NULL
)

# Categories are dropped
expect_equal(
    nlevels(df_empty_checked$Species),
    2
)

# Values stayed the same
expect_equal(
    sapply(df_empty[, -c(1:4)], as.character),
    sapply(df_empty_checked[, -c(1:4)], as.character)
)