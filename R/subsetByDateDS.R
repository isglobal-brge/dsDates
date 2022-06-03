#' Title
#'
#' @param day
#' @param month
#' @param year
#' @param range
#'
#' @return
#' @export
#'
#' @examples
subsetByDateDS <- function(x.name, date_column, day, month, year, range){

  library(lubridate)
  x <- eval(parse(text=x.name), envir = parent.frame())[date_column]
  if(!is.null(range)){
    range <- lubridate::interval(lubridate::ymd(range[1]), lubridate::ymd(range[2]))
    matches_final <- x[,1] %within% range
    return(as.numeric(matches_final))
  } else {
    day_matches <- lubridate::day(x[,1]) %in% day
    month_matches  <- lubridate::month(x[,1]) %in% month
    year_matches  <- lubridate::year(x[,1]) %in% year
    matches <- cbind(day_matches, month_matches, year_matches)[,c(!is.null(day), !is.null(month), !is.null(year))]
    matches_final <- apply(data.frame(matches), 1, function(x){
      all(x)
    })
    return(as.numeric(matches_final))
  }
}
