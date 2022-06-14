#-------------------------------------------------------------------------------
# Copyright (c) 2022 Bioinformatic Research Group in Epidemiology (BRGE)
#
# This program and the accompanying materials are made available under the terms
# of the MIT Licence
#-------------------------------------------------------------------------------

#
# Set up
#

context("asDateDS::smk::setup")

#
# Tests
#

context("asDateDS::smk::character date, no format, no origin")
test_that("simple asDateDS, character date, no format, no origin", {
    x      <- c('1991-12-10', '1992-12-11', '1993-12-12', '1994-12-13', '2004-10-10', '2003-10-11', '2002-10-12', '2001-10-13')
    format <- NULL
    origin <- NULL

    res <- asDateDS("x", format, origin)

    expect_true(all("Date" %in% class(res)))
    expect_length(res, 8)
    expect_equal(as.character(res[1]), "1991-12-10")
    expect_equal(as.character(res[2]), "1992-12-11")
    expect_equal(as.character(res[3]), "1993-12-12")
    expect_equal(as.character(res[4]), "1994-12-13")
    expect_equal(as.character(res[5]), "2004-10-10")
    expect_equal(as.character(res[6]), "2003-10-11")
    expect_equal(as.character(res[7]), "2002-10-12")
    expect_equal(as.character(res[8]), "2001-10-13")
})

context("asDateDS::smk::character date, no format, default origin")
test_that("simple asDateDS, character date, no format, default origin", {
    x      <- c('1991-12-10', '1992-12-11', '1993-12-12', '1994-12-13', '2004-10-10', '2003-10-11', '2002-10-12', '2001-10-13')
    format <- NULL
    origin <- lubridate::origin

    res <- asDateDS("x", format, origin)

    expect_true(all("Date" %in% class(res)))
    expect_length(res, 8)
    expect_equal(as.character(res[1]), "1991-12-10")
    expect_equal(as.character(res[2]), "1992-12-11")
    expect_equal(as.character(res[3]), "1993-12-12")
    expect_equal(as.character(res[4]), "1994-12-13")
    expect_equal(as.character(res[5]), "2004-10-10")
    expect_equal(as.character(res[6]), "2003-10-11")
    expect_equal(as.character(res[7]), "2002-10-12")
    expect_equal(as.character(res[8]), "2001-10-13")
})

context("asDateDS::smk::character date, format, default origin")
test_that("simple asDateDS, character date, format, default origin", {
    x      <- c('1991-12-10', '1992-12-11', '1993-12-12', '1994-12-13', '2004-10-10', '2003-10-11', '2002-10-12', '2001-10-13')
    format <- '%Y-%m-%d'
    origin <- lubridate::origin

    res <- asDateDS("x", format, origin)

    expect_true(all("Date" %in% class(res)))
    expect_length(res, 8)
    expect_equal(as.character(res[1]), "1991-12-10")
    expect_equal(as.character(res[2]), "1992-12-11")
    expect_equal(as.character(res[3]), "1993-12-12")
    expect_equal(as.character(res[4]), "1994-12-13")
    expect_equal(as.character(res[5]), "2004-10-10")
    expect_equal(as.character(res[6]), "2003-10-11")
    expect_equal(as.character(res[7]), "2002-10-12")
    expect_equal(as.character(res[8]), "2001-10-13")
})

context("asDateDS::smk::numeric date, no format, no origin")
test_that("simple asDateDS, no format, no origin", {
    x      <- c(8013.0, 8380.0, 8746.0, 9112.0, 12701.0, 12336.0, 11972.0, 11608.0)
    format <- NULL
    origin <- NULL

    res <- asDateDS("x", format, origin)

    expect_true(all("Date" %in% class(res)))
    expect_length(res, 8)
    expect_equal(as.character(res[1]), "1991-12-10")
    expect_equal(as.character(res[2]), "1992-12-11")
    expect_equal(as.character(res[3]), "1993-12-12")
    expect_equal(as.character(res[4]), "1994-12-13")
    expect_equal(as.character(res[5]), "2004-10-10")
    expect_equal(as.character(res[6]), "2003-10-11")
    expect_equal(as.character(res[7]), "2002-10-12")
    expect_equal(as.character(res[8]), "2001-10-13")
})

context("asDateDS::smk::numeric date, no format, default origin")
test_that("simple asDateDS, no format, default origin", {
    x      <- c(8013.0, 8380.0, 8746.0, 9112.0, 12701.0, 12336.0, 11972.0, 11608.0)
    format <- NULL
    origin <- lubridate::origin

    res <- asDateDS("x", format, origin)

    expect_true(all("Date" %in% class(res)))
    expect_length(res, 8)
    expect_equal(as.character(res[1]), "1991-12-10")
    expect_equal(as.character(res[2]), "1992-12-11")
    expect_equal(as.character(res[3]), "1993-12-12")
    expect_equal(as.character(res[4]), "1994-12-13")
    expect_equal(as.character(res[5]), "2004-10-10")
    expect_equal(as.character(res[6]), "2003-10-11")
    expect_equal(as.character(res[7]), "2002-10-12")
    expect_equal(as.character(res[8]), "2001-10-13")
})

context("asDateDS::smk::numeric date, format, default origin")
test_that("simple asDateDS, format, default origin", {
    x      <- c(8013.0, 8380.0, 8746.0, 9112.0, 12701.0, 12336.0, 11972.0, 11608.0)
    format <- '%Y-%m-%d'
    origin <- lubridate::origin

    res <- asDateDS("x", format, origin)

    expect_true(all("Date" %in% class(res)))
    expect_length(res, 8)
    expect_equal(as.character(res[1]), "1991-12-10")
    expect_equal(as.character(res[2]), "1992-12-11")
    expect_equal(as.character(res[3]), "1993-12-12")
    expect_equal(as.character(res[4]), "1994-12-13")
    expect_equal(as.character(res[5]), "2004-10-10")
    expect_equal(as.character(res[6]), "2003-10-11")
    expect_equal(as.character(res[7]), "2002-10-12")
    expect_equal(as.character(res[8]), "2001-10-13")
})

context("asDateDS::smk::integer date, no format, no origin")
test_that("simple asDateDS, no format, no origin", {
    x      <- c(8013, 8380, 8746, 9112, 12701, 12336, 11972, 11608)
    format <- NULL
    origin <- NULL

    res <- asDateDS("x", format, origin)

    expect_true(all("Date" %in% class(res)))
    expect_length(res, 8)
    expect_equal(as.character(res[1]), "1991-12-10")
    expect_equal(as.character(res[2]), "1992-12-11")
    expect_equal(as.character(res[3]), "1993-12-12")
    expect_equal(as.character(res[4]), "1994-12-13")
    expect_equal(as.character(res[5]), "2004-10-10")
    expect_equal(as.character(res[6]), "2003-10-11")
    expect_equal(as.character(res[7]), "2002-10-12")
    expect_equal(as.character(res[8]), "2001-10-13")
})

context("asDateDS::smk::integer date, no format, default origin")
test_that("simple asDateDS, no format, default origin", {
    x      <- c(8013, 8380, 8746, 9112, 12701, 12336, 11972, 11608)
    format <- NULL
    origin <- lubridate::origin

    res <- asDateDS("x", format, origin)

    expect_true(all("Date" %in% class(res)))
    expect_length(res, 8)
    expect_equal(as.character(res[1]), "1991-12-10")
    expect_equal(as.character(res[2]), "1992-12-11")
    expect_equal(as.character(res[3]), "1993-12-12")
    expect_equal(as.character(res[4]), "1994-12-13")
    expect_equal(as.character(res[5]), "2004-10-10")
    expect_equal(as.character(res[6]), "2003-10-11")
    expect_equal(as.character(res[7]), "2002-10-12")
    expect_equal(as.character(res[8]), "2001-10-13")
})

context("asDateDS::smk::integer date, format, default origin")
test_that("simple asDateDS, format, default origin", {
    x      <- c(8013, 8380, 8746, 9112, 12701, 12336, 11972, 11608)
    format <- '%Y-%m-%d'
    origin <- lubridate::origin

    res <- asDateDS("x", format, origin)

    expect_true(all("Date" %in% class(res)))
    expect_length(res, 8)
    expect_equal(as.character(res[1]), "1991-12-10")
    expect_equal(as.character(res[2]), "1992-12-11")
    expect_equal(as.character(res[3]), "1993-12-12")
    expect_equal(as.character(res[4]), "1994-12-13")
    expect_equal(as.character(res[5]), "2004-10-10")
    expect_equal(as.character(res[6]), "2003-10-11")
    expect_equal(as.character(res[7]), "2002-10-12")
    expect_equal(as.character(res[8]), "2001-10-13")
})

#
# Done
#

context("asDateDS::smk::shutdown")

context("asDateDS::smk::done")
