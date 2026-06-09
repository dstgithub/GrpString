#' @export
PatternInfo <-
function(patterns, strings, rev = FALSE){

   ## 1. functions of statistics of patterns

   # 1.1 initialize df
   pattern_info.df <- data.frame(matrix(NA, ncol = length(patterns), nrow = length(strings)))

   # 1.2 statistics of patterns 
   # get the start position of each pattern in each string;
   # if a pattern does not exist in one string, return '-1'.

   # function of statistics of patterns - forward order
   stat_pattern <- function(patterns, strings){
      sapply(patterns, function(p) {
                sapply(strings, function(s) {
                       regexpr(p, s)[1]
                })

       })
   }

   # function of statistics of patterns - reverse order
   stat_pattern_rev <- function(patterns, strings){
      sapply(patterns, function(p) {
                sapply(strings, function(s) {
                       rev(gregexpr(p, s)[[1]])[1]
                })

       })
   }
  
  # 1.3 apply above functions

  pattern_info_f.df <- stat_pattern(patterns, strings)
  pattern_info_r.df <- stat_pattern_rev(patterns, strings)

  colnames(pattern_info_f.df) <- patterns
  colnames(pattern_info_r.df) <- patterns

  # include string lengths

  length.vec <- nchar(strings)

  pattern_info_fwd.df <- cbind(length.vec, pattern_info_f.df)
  colnames(pattern_info_fwd.df)[1] <- "length"

  pattern_info_rev.df <- cbind(length.vec, pattern_info_r.df)
  colnames(pattern_info_rev.df)[1] <- "length"

 # 2. output df
  if (rev == FALSE){
    return(pattern_info_fwd.df)
  } else {
    return(pattern_info_rev.df)
  }

}
