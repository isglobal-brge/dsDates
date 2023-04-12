#' Title
#'
#' @param x
#' @param format
#' @param origin
#'
#' @return
#' @export
#'
#' @examples
asDateDS <- function(x, format, origin){
  if(!is.null(format)){
    format <- unserialize(wkb::hex2raw(format))
  }  
  x <- eval(parse(text=x), envir = parent.frame())
  if(inherits(x, "character")){
    output <- lubridate::as_date(x, format = format)
  } else if (inherits(x, c("numeric", "integer"))){
    if(is.null(origin)){
      output <- lubridate::as_date(x)
    } else {
      output <- lubridate::as_date(x, origin)
    }
  } else {
    stop("Selected column is not of accepted class [numeric/integer/character]")
  }
  if(all(is.na(output))){
    stop("Could not convert selected column to [Date]")
  } else {
    return(output)
  }
}
