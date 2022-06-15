#-------------------------------------------------------------------------------
# Copyright (c) 2022 Bioinformatic Research Group in Epidemiology (BRGE)
#
# This program and the accompanying materials are made available under the terms
# of the MIT Licence
#-------------------------------------------------------------------------------

#
# Set up
#

context("subsetByDateDS::smk::setup")

#
# Tests
#

context("subsetByDateDS::smk::empty sunset")
test_that("simple subsetByDateDS, empty sunset", {
    x            <- data.frame(A = c(as.Date("1991-12-10"), as.Date("1992-12-11"), as.Date("1993-12-12"), as.Date("1994-12-13"),
                                     as.Date("2004-10-10"), as.Date("2003-10-11"), as.Date("2002-10-12"), as.Date("2001-10-13")))
    date_column  <- "A"
    day          <- NULL
    month        <- NULL
    year         <- NULL
    range        <- NULL

    res <- subsetByDateDS("x", date_column, day, month, year, range)

    expect_true(all("numeric" %in% class(res)))
    expect_length(res, 8)
    expect_equal(res[1], 1)
    expect_equal(res[2], 1)
    expect_equal(res[3], 1)
    expect_equal(res[4], 1)
    expect_equal(res[5], 1)
    expect_equal(res[6], 1)
    expect_equal(res[7], 1)
    expect_equal(res[8], 1)
})

context("subsetByDateDS::smk::lower range")
test_that("simple subsetByDateDS, lower range", {
    x            <- data.frame(A = c(as.Date("1991-12-10"), as.Date("1992-12-11"), as.Date("1993-12-12"), as.Date("1994-12-13"),
                                     as.Date("2004-10-10"), as.Date("2003-10-11"), as.Date("2002-10-12"), as.Date("2001-10-13")))
    date_column  <- "A"
    day          <- NULL
    month        <- NULL
    year         <- NULL
    range        <- c(as.Date("1991-12-10"), as.Date("1994-12-13"))

    res <- subsetByDateDS("x", date_column, day, month, year, range)

    expect_true(all("numeric" %in% class(res)))
    expect_length(res, 8)
    expect_equal(res[1], 1)
    expect_equal(res[2], 1)
    expect_equal(res[3], 1)
    expect_equal(res[4], 1)
    expect_equal(res[5], 0)
    expect_equal(res[6], 0)
    expect_equal(res[7], 0)
    expect_equal(res[8], 0)
})

context("subsetByDateDS::smk::upper range")
test_that("simple subsetByDateDS, upper range", {
    x            <- data.frame(A = c(as.Date("1991-12-10"), as.Date("1992-12-11"), as.Date("1993-12-12"), as.Date("1994-12-13"),
                                     as.Date("2004-10-10"), as.Date("2003-10-11"), as.Date("2002-10-12"), as.Date("2001-10-13")))
    date_column  <- "A"
    day          <- NULL
    month        <- NULL
    year         <- NULL
    range        <- c(as.Date("2001-10-13"), as.Date("2004-10-10"))

    res <- subsetByDateDS("x", date_column, day, month, year, range)

    expect_true(all("numeric" %in% class(res)))
    expect_length(res, 8)
    expect_equal(res[1], 0)
    expect_equal(res[2], 0)
    expect_equal(res[3], 0)
    expect_equal(res[4], 0)
    expect_equal(res[5], 1)
    expect_equal(res[6], 1)
    expect_equal(res[7], 1)
    expect_equal(res[8], 1)
})

context("subsetByDateDS::smk::middle range")
test_that("simple subsetByDateDS, middle range", {
    x            <- data.frame(A = c(as.Date("1991-12-10"), as.Date("1992-12-11"), as.Date("1993-12-12"), as.Date("1994-12-13"),
                                     as.Date("2004-10-10"), as.Date("2003-10-11"), as.Date("2002-10-12"), as.Date("2001-10-13")))
    date_column  <- "A"
    day          <- NULL
    month        <- NULL
    year         <- NULL
    range        <- c(as.Date("1993-12-12"), as.Date("2002-10-12"))

    res <- subsetByDateDS("x", date_column, day, month, year, range)

    expect_true(all("numeric" %in% class(res)))
    expect_length(res, 8)
    expect_equal(res[1], 0)
    expect_equal(res[2], 0)
    expect_equal(res[3], 1)
    expect_equal(res[4], 1)
    expect_equal(res[5], 0)
    expect_equal(res[6], 0)
    expect_equal(res[7], 1)
    expect_equal(res[8], 1)
})

context("subsetByDateDS::smk::dmy 1 range")
test_that("simple subsetByDateDS, dmy 1 range", {
    x            <- data.frame(A = c(as.Date("1991-12-10"), as.Date("1992-12-11"), as.Date("1993-12-12"), as.Date("1994-12-13"),
                                     as.Date("2004-10-10"), as.Date("2003-10-11"), as.Date("2002-10-12"), as.Date("2001-10-13")))
    date_column  <- "A"
    day          <- 13
    month        <- 12
    year         <- 1994
    range        <- NULL

    res <- subsetByDateDS("x", date_column, day, month, year, range)

    expect_true(all("numeric" %in% class(res)))
    expect_length(res, 8)
    expect_equal(res[1], 0)
    expect_equal(res[2], 0)
    expect_equal(res[3], 0)
    expect_equal(res[4], 1)
    expect_equal(res[5], 0)
    expect_equal(res[6], 0)
    expect_equal(res[7], 0)
    expect_equal(res[8], 0)
})

context("subsetByDateDS::smk::dmy 2 range")
test_that("simple subsetByDateDS, dmy 2 range", {
    x            <- data.frame(A = c(as.Date("1991-12-10"), as.Date("1992-12-11"), as.Date("1993-12-12"), as.Date("1994-12-13"),
                                     as.Date("2004-10-10"), as.Date("2003-10-11"), as.Date("2002-10-12"), as.Date("2001-10-13")))
    date_column  <- "A"
    day          <- 10
    month        <- 10
    year         <- 2004
    range        <- NULL

    res <- subsetByDateDS("x", date_column, day, month, year, range)

    expect_true(all("numeric" %in% class(res)))
    expect_length(res, 8)
    expect_equal(res[1], 0)
    expect_equal(res[2], 0)
    expect_equal(res[3], 0)
    expect_equal(res[4], 0)
    expect_equal(res[5], 1)
    expect_equal(res[6], 0)
    expect_equal(res[7], 0)
    expect_equal(res[8], 0)
})


context("subsetByDateDS::smk::dmy multi")
test_that("simple subsetByDateDS, dmy multi", {
    x            <- data.frame(A = c(as.Date("1991-12-10"), as.Date("2004-10-10"), as.Date("1993-12-12"), as.Date("1994-12-13"),
                                     as.Date("2004-10-10"), as.Date("2003-10-11"), as.Date("2002-10-12"), as.Date("2004-10-10")))
    date_column  <- "A"
    day          <- 10
    month        <- 10
    year         <- 2004
    range        <- NULL

    res <- subsetByDateDS("x", date_column, day, month, year, range)

    expect_true(all("numeric" %in% class(res)))
    expect_length(res, 8)
    expect_equal(res[1], 0)
    expect_equal(res[2], 1)
    expect_equal(res[3], 0)
    expect_equal(res[4], 0)
    expect_equal(res[5], 1)
    expect_equal(res[6], 0)
    expect_equal(res[7], 0)
    expect_equal(res[8], 1)
})

#
# Done
#

context("subsetByDateDS::smk::shutdown")

context("subsetByDateDS::smk::done")
