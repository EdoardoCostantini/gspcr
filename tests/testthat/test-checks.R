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
        npcs_range = 1:10,
        ivs = iris
    ),
    regexp = NULL
)

# Returns the right correction when too many pcs are demanded
suppressWarnings(
    expect_equal(
        1:ncol(ivs),
        check_npcs_range(
            npcs_range = 1:10,
            ivs = iris
        )
    )
)

