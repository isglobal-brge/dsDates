#' Create Date or POSIXct from Year/Month/Day Columns
#'
#' @title Make Date from Separate Year/Month/Day Columns
#' @description 
#' Creates a Date or POSIXct object from separate numeric year, month, and day columns.
#' Handles missing values gracefully by using defaults (month/day = 1, hour/minute/second = 0).
#' This function is useful for OMOP CDM data where birth dates are stored as separate
#' year_of_birth, month_of_birth, day_of_birth columns.
#'
#' @param year.name Character string providing the name of the year column/vector (mandatory).
#' @param month.name Character string providing the name of the month column/vector (optional, defaults to NULL → uses 1).
#' @param day.name Character string providing the name of the day column/vector (optional, defaults to NULL → uses 1).
#' @param hour.name Character string providing the name of the hour column/vector (optional, defaults to NULL → uses 0).
#' @param minute.name Character string providing the name of the minute column/vector (optional, defaults to NULL → uses 0).
#' @param second.name Character string providing the name of the second column/vector (optional, defaults to NULL → uses 0).
#' @param format Character string specifying output format: "Date" (default) or "POSIXct".
#' @param newobj Character string providing the name for the output object.
#'
#' @return A Date or POSIXct vector with the combined date values.
#'
#' @details
#' The function handles NA values as follows:
#' - If year is NA, the result is NA
#' - If month or day are NA but year exists, defaults are used (month=1, day=1)
#' - If hour/minute/second are NA but date components exist, defaults are used (0)
#'
#' @examples
#' \dontrun{
#' # Year only (month/day default to 1)
#' year_vec <- c(1990, 1991, 1992)
#' makeDateDS("year_vec", NULL, NULL, NULL, NULL, NULL, "Date", "birth_date")
#'
#' # Year and month (day defaults to 1)
#' year_vec <- c(1990, 1991, 1992)
#' month_vec <- c(6, 7, 8)
#' makeDateDS("year_vec", "month_vec", NULL, NULL, NULL, NULL, "Date", "birth_date")
#'
#' # Full date with NA handling
#' year_vec <- c(1990, NA, 1992)
#' month_vec <- c(6, 7, NA)
#' day_vec <- c(15, 20, 25)
#' makeDateDS("year_vec", "month_vec", "day_vec", NULL, NULL, NULL, "Date", "birth_date")
#' }
#'
#' @export
makeDateDS <- function(year.name, month.name, day.name, hour.name, minute.name, second.name, format, newobj){
  # Extract year vector (mandatory)
  year <- eval(parse(text=year.name), envir = parent.frame())
  
  # Validate year is numeric/integer
  if(!inherits(year, c("numeric", "integer"))){
    stop("Year column must be of class [numeric/integer]")
  }
  
  # Check if all years are NA
  if(all(is.na(year))){
    stop("Could not create dates from provided columns")
  }
  
  # Extract optional components with defaults
  if(is.null(month.name)){
    month <- rep(1L, length(year))
  } else {
    month <- eval(parse(text=month.name), envir = parent.frame())
    if(!inherits(month, c("numeric", "integer"))){
      stop("Month column must be of class [numeric/integer]")
    }
    # Ensure same length as year BEFORE any operations
    if(length(month) != length(year)){
      stop("Month vector must have the same length as year vector")
    }
    # Handle NA values: use default (1) if NA but year exists
    month[is.na(month) & !is.na(year)] <- 1L
  }
  
  if(is.null(day.name)){
    day <- rep(1L, length(year))
  } else {
    day <- eval(parse(text=day.name), envir = parent.frame())
    if(!inherits(day, c("numeric", "integer"))){
      stop("Day column must be of class [numeric/integer]")
    }
    # Ensure same length as year BEFORE any operations
    if(length(day) != length(year)){
      stop("Day vector must have the same length as year vector")
    }
    # Handle NA values: use default (1) if NA but year exists
    day[is.na(day) & !is.na(year)] <- 1L
  }
  
  # Handle time components for POSIXct format
  if(format == "POSIXct"){
    if(is.null(hour.name)){
      hour <- rep(0L, length(year))
    } else {
      hour <- eval(parse(text=hour.name), envir = parent.frame())
      if(!inherits(hour, c("numeric", "integer"))){
        stop("Hour column must be of class [numeric/integer]")
      }
      if(length(hour) != length(year)){
        stop("Hour vector must have the same length as year vector")
      }
      hour[is.na(hour) & !is.na(year)] <- 0L
    }
    
    if(is.null(minute.name)){
      minute <- rep(0L, length(year))
    } else {
      minute <- eval(parse(text=minute.name), envir = parent.frame())
      if(!inherits(minute, c("numeric", "integer"))){
        stop("Minute column must be of class [numeric/integer]")
      }
      if(length(minute) != length(year)){
        stop("Minute vector must have the same length as year vector")
      }
      minute[is.na(minute) & !is.na(year)] <- 0L
    }
    
    if(is.null(second.name)){
      second <- rep(0L, length(year))
    } else {
      second <- eval(parse(text=second.name), envir = parent.frame())
      if(!inherits(second, c("numeric", "integer"))){
        stop("Second column must be of class [numeric/integer]")
      }
      if(length(second) != length(year)){
        stop("Second vector must have the same length as year vector")
      }
      second[is.na(second) & !is.na(year)] <- 0L
    }
    
    # Create POSIXct datetime
    output <- lubridate::make_datetime(year, month, day, hour, minute, second)
  } else {
    # Create Date (default)
    output <- lubridate::make_date(year, month, day)
  }
  
  # Validate output
  if(all(is.na(output))){
    stop("Could not create dates from provided columns")
  }
  
  return(output)
}

