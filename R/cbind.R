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
cbind <- function(x, y, name){
  table <- base::cbind(x,y)
  colnames(table)[ncol(table)] <- name
  return(table)
}
