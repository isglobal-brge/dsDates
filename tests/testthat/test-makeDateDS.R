#
# Set up
#

context("makeDateDS::smk::setup")

#
# Tests
#

context("makeDateDS::smk::year only, Date format")
test_that("makeDateDS with year only, Date format", {
    year_vec <- c(1990, 1991, 1992, 1993)
    
    res <- makeDateDS("year_vec", NULL, NULL, NULL, NULL, NULL, "Date", "test_date")
    
    expect_true(all("Date" %in% class(res)))
    expect_length(res, 4)
    expect_equal(as.character(res[1]), "1990-01-01")
    expect_equal(as.character(res[2]), "1991-01-01")
    expect_equal(as.character(res[3]), "1992-01-01")
    expect_equal(as.character(res[4]), "1993-01-01")
})

context("makeDateDS::smk::year and month, Date format")
test_that("makeDateDS with year and month, Date format", {
    year_vec <- c(1990, 1991, 1992)
    month_vec <- c(6, 7, 8)
    
    res <- makeDateDS("year_vec", "month_vec", NULL, NULL, NULL, NULL, "Date", "test_date")
    
    expect_true(all("Date" %in% class(res)))
    expect_length(res, 3)
    expect_equal(as.character(res[1]), "1990-06-01")
    expect_equal(as.character(res[2]), "1991-07-01")
    expect_equal(as.character(res[3]), "1992-08-01")
})

context("makeDateDS::smk::year, month, day, Date format")
test_that("makeDateDS with year, month, day, Date format", {
    year_vec <- c(1990, 1991, 1992)
    month_vec <- c(6, 7, 8)
    day_vec <- c(15, 20, 25)
    
    res <- makeDateDS("year_vec", "month_vec", "day_vec", NULL, NULL, NULL, "Date", "test_date")
    
    expect_true(all("Date" %in% class(res)))
    expect_length(res, 3)
    expect_equal(as.character(res[1]), "1990-06-15")
    expect_equal(as.character(res[2]), "1991-07-20")
    expect_equal(as.character(res[3]), "1992-08-25")
})

context("makeDateDS::smk::NA in year, Date format")
test_that("makeDateDS with NA in year, Date format", {
    year_vec <- c(1990, NA, 1992)
    month_vec <- c(6, 7, 8)
    day_vec <- c(15, 20, 25)
    
    res <- makeDateDS("year_vec", "month_vec", "day_vec", NULL, NULL, NULL, "Date", "test_date")
    
    expect_true(all("Date" %in% class(res)))
    expect_length(res, 3)
    expect_equal(as.character(res[1]), "1990-06-15")
    expect_true(is.na(res[2]))
    expect_equal(as.character(res[3]), "1992-08-25")
})

context("makeDateDS::smk::NA in month/day, Date format")
test_that("makeDateDS with NA in month/day, Date format", {
    year_vec <- c(1990, 1991, 1992)
    month_vec <- c(6, NA, 8)
    day_vec <- c(15, 20, NA)
    
    res <- makeDateDS("year_vec", "month_vec", "day_vec", NULL, NULL, NULL, "Date", "test_date")
    
    expect_true(all("Date" %in% class(res)))
    expect_length(res, 3)
    expect_equal(as.character(res[1]), "1990-06-15")
    expect_equal(as.character(res[2]), "1991-01-20")  # month NA defaults to 1
    expect_equal(as.character(res[3]), "1992-08-01")  # day NA defaults to 1
})

context("makeDateDS::smk::POSIXct format with year, month, day")
test_that("makeDateDS POSIXct format with year, month, day", {
    year_vec <- c(1990, 1991, 1992)
    month_vec <- c(6, 7, 8)
    day_vec <- c(15, 20, 25)
    
    res <- makeDateDS("year_vec", "month_vec", "day_vec", NULL, NULL, NULL, "POSIXct", "test_date")
    
    expect_true(all("POSIXct" %in% class(res)))
    expect_length(res, 3)
    expect_equal(format(res[1], "%Y-%m-%d %H:%M:%S"), "1990-06-15 00:00:00")
    expect_equal(format(res[2], "%Y-%m-%d %H:%M:%S"), "1991-07-20 00:00:00")
    expect_equal(format(res[3], "%Y-%m-%d %H:%M:%S"), "1992-08-25 00:00:00")
})

context("makeDateDS::smk::POSIXct format with hour, minute, second")
test_that("makeDateDS POSIXct format with hour, minute, second", {
    year_vec <- c(1990, 1991, 1992)
    month_vec <- c(6, 7, 8)
    day_vec <- c(15, 20, 25)
    hour_vec <- c(10, 14, 18)
    minute_vec <- c(30, 45, 15)
    second_vec <- c(0, 30, 45)
    
    res <- makeDateDS("year_vec", "month_vec", "day_vec", "hour_vec", "minute_vec", "second_vec", "POSIXct", "test_date")
    
    expect_true(all("POSIXct" %in% class(res)))
    expect_length(res, 3)
    expect_equal(format(res[1], "%Y-%m-%d %H:%M:%S"), "1990-06-15 10:30:00")
    expect_equal(format(res[2], "%Y-%m-%d %H:%M:%S"), "1991-07-20 14:45:30")
    expect_equal(format(res[3], "%Y-%m-%d %H:%M:%S"), "1992-08-25 18:15:45")
})

context("makeDateDS::smk::POSIXct format with NA in time components")
test_that("makeDateDS POSIXct format with NA in time components", {
    year_vec <- c(1990, 1991, 1992)
    month_vec <- c(6, 7, 8)
    day_vec <- c(15, 20, 25)
    hour_vec <- c(10, NA, 18)
    minute_vec <- c(30, 45, NA)
    
    res <- makeDateDS("year_vec", "month_vec", "day_vec", "hour_vec", "minute_vec", NULL, "POSIXct", "test_date")
    
    expect_true(all("POSIXct" %in% class(res)))
    expect_length(res, 3)
    expect_equal(format(res[1], "%Y-%m-%d %H:%M:%S"), "1990-06-15 10:30:00")
    expect_equal(format(res[2], "%Y-%m-%d %H:%M:%S"), "1991-07-20 00:45:00")  # hour NA defaults to 0
    expect_equal(format(res[3], "%Y-%m-%d %H:%M:%S"), "1992-08-25 18:00:00")  # minute NA defaults to 0
})

context("makeDateDS::smk::table column access")
test_that("makeDateDS with table column access", {
    person_table <- data.frame(
        year_of_birth = c(1990, 1991, 1992),
        month_of_birth = c(6, 7, 8),
        day_of_birth = c(15, 20, 25)
    )
    
    res <- makeDateDS("person_table$year_of_birth", "person_table$month_of_birth", "person_table$day_of_birth", NULL, NULL, NULL, "Date", "test_date")
    
    expect_true(all("Date" %in% class(res)))
    expect_length(res, 3)
    expect_equal(as.character(res[1]), "1990-06-15")
    expect_equal(as.character(res[2]), "1991-07-20")
    expect_equal(as.character(res[3]), "1992-08-25")
})

context("makeDateDS::smk::integer vectors")
test_that("makeDateDS with integer vectors", {
    year_vec <- c(1990L, 1991L, 1992L)
    month_vec <- c(6L, 7L, 8L)
    day_vec <- c(15L, 20L, 25L)
    
    res <- makeDateDS("year_vec", "month_vec", "day_vec", NULL, NULL, NULL, "Date", "test_date")
    
    expect_true(all("Date" %in% class(res)))
    expect_length(res, 3)
    expect_equal(as.character(res[1]), "1990-06-15")
    expect_equal(as.character(res[2]), "1991-07-20")
    expect_equal(as.character(res[3]), "1992-08-25")
})

context("makeDateDS::arg::invalid year type")
test_that("makeDateDS with invalid year type", {
    year_vec <- c("1990", "1991", "1992")
    
    expect_error(makeDateDS("year_vec", NULL, NULL, NULL, NULL, NULL, "Date", "test_date"), 
                 "Year column 'year_vec' must be of class \\[numeric/integer\\], but got", fixed = FALSE)
})

context("makeDateDS::arg::invalid month type")
test_that("makeDateDS with invalid month type", {
    year_vec <- c(1990, 1991, 1992)
    month_vec <- c("6", "7", "8")
    
    expect_error(makeDateDS("year_vec", "month_vec", NULL, NULL, NULL, NULL, "Date", "test_date"), 
                 "Month column 'month_vec' must be of class \\[numeric/integer\\], but got", fixed = FALSE)
})

context("makeDateDS::arg::length mismatch")
test_that("makeDateDS with length mismatch", {
    year_vec <- c(1990, 1991, 1992)
    month_vec <- c(6, 7)  # Wrong length
    
    expect_error(makeDateDS("year_vec", "month_vec", NULL, NULL, NULL, NULL, "Date", "test_date"), 
                 "Month vector must have the same length as year vector", fixed = TRUE)
})

context("makeDateDS::arg::all NA year")
test_that("makeDateDS with all NA year", {
    year_vec <- c(NA_real_, NA_real_, NA_real_)
    
    expect_error(makeDateDS("year_vec", NULL, NULL, NULL, NULL, NULL, "Date", "test_date"), 
                 "Could not create dates from provided columns", fixed = TRUE)
})

context("makeDateDS::arg::non-existent column")
test_that("makeDateDS with non-existent year column", {
    # Create a table but don't create the column we're looking for
    person_table <- data.frame(
        person_id = 1:3,
        other_column = c(1, 2, 3)
    )
    
    expect_error(makeDateDS("person_table$year_of_birth", NULL, NULL, NULL, NULL, NULL, "Date", "test_date"), 
                 "Cannot access 'person_table\\$year_of_birth'", fixed = FALSE)
})

context("makeDateDS::arg::non-existent month column")
test_that("makeDateDS with non-existent month column", {
    person_table <- data.frame(
        person_id = 1:3,
        year_of_birth = c(1990, 1991, 1992)
        # month_of_birth does not exist
    )
    
    expect_error(makeDateDS("person_table$year_of_birth", "person_table$month_of_birth", NULL, NULL, NULL, NULL, "Date", "test_date"), 
                 "Cannot access 'person_table\\$month_of_birth'", fixed = FALSE)
})

context("makeDateDS::arg::non-existent day column")
test_that("makeDateDS with non-existent day column", {
    person_table <- data.frame(
        person_id = 1:3,
        year_of_birth = c(1990, 1991, 1992),
        month_of_birth = c(6, 7, 8)
        # day_of_birth does not exist
    )
    
    expect_error(makeDateDS("person_table$year_of_birth", "person_table$month_of_birth", "person_table$day_of_birth", NULL, NULL, NULL, "Date", "test_date"), 
                 "Cannot access 'person_table\\$day_of_birth'", fixed = FALSE)
})

context("makeDateDS::arg::non-existent object")
test_that("makeDateDS with non-existent object", {
    expect_error(makeDateDS("nonexistent_object", NULL, NULL, NULL, NULL, NULL, "Date", "test_date"), 
                 "Cannot access 'nonexistent_object'", fixed = FALSE)
})

context("makeDateDS::smk::NA handling - mixed NA values")
test_that("makeDateDS handles mixed NA values correctly", {
    year_vec <- c(1990, NA, 1992, 1993)
    month_vec <- c(6, 7, NA, 8)
    day_vec <- c(15, 20, 25, NA)
    
    res <- makeDateDS("year_vec", "month_vec", "day_vec", NULL, NULL, NULL, "Date", "test_date")
    
    expect_true(all("Date" %in% class(res)))
    expect_length(res, 4)
    expect_equal(as.character(res[1]), "1990-06-15")  # All values present
    expect_true(is.na(res[2]))  # Year is NA
    expect_equal(as.character(res[3]), "1992-01-25")  # Month NA defaults to 1
    expect_equal(as.character(res[4]), "1993-08-01")  # Day NA defaults to 1
})

context("makeDateDS::smk::NA handling - all optional components NA")
test_that("makeDateDS handles all optional components NA", {
    year_vec <- c(1990, 1991, 1992)
    month_vec <- c(NA_real_, NA_real_, NA_real_)
    day_vec <- c(NA_real_, NA_real_, NA_real_)
    
    res <- makeDateDS("year_vec", "month_vec", "day_vec", NULL, NULL, NULL, "Date", "test_date")
    
    expect_true(all("Date" %in% class(res)))
    expect_length(res, 3)
    # All should default to month=1, day=1
    expect_equal(as.character(res[1]), "1990-01-01")
    expect_equal(as.character(res[2]), "1991-01-01")
    expect_equal(as.character(res[3]), "1992-01-01")
})

context("makeDateDS::smk::NA handling - POSIXct with NA time components")
test_that("makeDateDS handles NA in time components for POSIXct", {
    year_vec <- c(2020, 2021, 2022)
    month_vec <- c(1, 6, 12)
    day_vec <- c(15, 20, 25)
    hour_vec <- c(10, NA, 18)
    minute_vec <- c(NA, 45, 15)
    
    res <- makeDateDS("year_vec", "month_vec", "day_vec", "hour_vec", "minute_vec", NULL, "POSIXct", "test_date")
    
    expect_true(all("POSIXct" %in% class(res)))
    expect_length(res, 3)
    expect_equal(format(res[1], "%Y-%m-%d %H:%M:%S"), "2020-01-15 10:00:00")  # minute NA defaults to 0
    expect_equal(format(res[2], "%Y-%m-%d %H:%M:%S"), "2021-06-20 00:45:00")  # hour NA defaults to 0
    expect_equal(format(res[3], "%Y-%m-%d %H:%M:%S"), "2022-12-25 18:15:00")
})

#
# Done
#

context("makeDateDS::smk::shutdown")

context("makeDateDS::smk::done")

