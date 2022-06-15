#-------------------------------------------------------------------------------
# Copyright (c) 2022 Bioinformatic Research Group in Epidemiology (BRGE)
#
# This program and the accompanying materials are made available under the terms
# of the MIT Licence
#-------------------------------------------------------------------------------

#
# Set up
#

context("asDateDS::arg::setup")

#
# Tests
#

context("asDateDS::arg::character bad date")
test_that("simple asDateDS, character date, bad date", {
    x      <- c('1000-1000-1000')
    format <- NULL
    origin <- NULL

    expect_error(expect_warning(asDateDS("x", format, origin), "All formats failed to parse. No formats found.", fixed = TRUE), "Could not convert selected column to [Date]", fixed = TRUE)
})

context("asDateDS::arg::bool date, no format, no origin")
test_that("simple asDateDS, bool date, no format, no origin", {
    x      <- c(FALSE, TRUE)
    format <- NULL
    origin <- NULL

    expect_error(asDateDS("x", format, origin), "Selected column is not of accepted class [numeric/integer/character]", fixed = TRUE)
})

context("asDateDS::arg::bool date, no format, default origin")
test_that("simple asDateDS, bool date, no format, default origin", {
    x      <- c(FALSE, TRUE)
    format <- NULL
    origin <- lubridate::origin

    expect_error(asDateDS("x", format, origin), "Selected column is not of accepted class [numeric/integer/character]", fixed = TRUE)
})

context("asDateDS::arg::bool date, format, default origin")
test_that("simple asDateDS, bool date, format, default origin", {
    x      <- c(FALSE, TRUE)
    format <- '%Y-%m-%d'
    origin <- lubridate::origin

    expect_error(asDateDS("x", format, origin), "Selected column is not of accepted class [numeric/integer/character]", fixed = TRUE)
})

#
# Done
#

context("asDateDS::arg::shutdown")

context("asDateDS::arg::done")
