#' @export
DupRm <- function(strings.vec){
   str_dupRm.vec <- gsub("([[:graph:]])\\1+", "\\1", strings.vec)
   return(str_dupRm.vec)
}
