#' @export
UniPatterns <-
function(grp1_pattern, grp2_pattern, grp1_string, grp2_string){
   
   ## 1. Names of output files

   # 1.1 Get names of each group from function arguments

   # grp_names <- as.character(substitute(grp1_pattern(grp2_pattern, grp1_string, grp2_string)))

   grp1p <- as.character(substitute(grp1_pattern))
   grp2p <- as.character(substitute(grp2_pattern))

   grp_pnames.vec <- c(grp1p, grp2p, grp1p, grp2p)
   grp_pnames.vecx <- c(grp2p, grp1p, grp2p, grp1p) # for output file

   grp1s <- as.character(substitute(grp1_string))
   grp2s <- as.character(substitute(grp2_string))

   grp_snames.vec <- c(grp1s, grp2s, grp2s, grp1s)


   # 1.2 Generate names of columns as well as output files
   # 1.2.1 name of unique-patterns-only file

   unique_pattern_only_colname <- paste("onlyIn", "_", grp_snames.vec[1:2], sep="")
   unique_pattern_only_fname <- paste("uni_", grp_pnames.vec[1], "-", grp_pnames.vec[2], ".txt", sep="")

   # 1.2.2 names of unique-patterns-information file
   # in the order of: patterns vs. strings - grp1 in grp1; grp2 in grp2; grp1 in grp2; grp2 in grp1;

   unique_pattern_info_fnames <- paste(grp_pnames.vec, "-vs-", grp_pnames.vecx, "_in_", grp_snames.vec, ".txt", sep="")


   ## 2. Unique patterns only

   # 2.1 Get unique patterns only
   pattern_grp1_only <- grp1_pattern[!(grp1_pattern %in% grp2_pattern)]
   pattern_grp2_only <- grp2_pattern[!(grp2_pattern %in% grp1_pattern)]

   # 2.2 Export unique patterns only file

   # 2.2.1 function to combine two vectors with different lengths
  
   combine_2_vec <- function(vec1, vec2){
      vec1name <- as.character(substitute(vec1))
      vec2name <- as.character(substitute(vec2))

      len1 <- length(vec1)
      len2 <- length(vec2)
      if (len1 == len2){
         vec12.df <- data.frame(vec1, vec2)
      } else if (len1 < len2){
           vec12.df <- data.frame(c(vec1, rep("_", len2-len1)),vec2)
      } else {
           vec12.df <- data.frame(vec1, c(vec2, rep("_", len1-len2)))
      }
      colnames(vec12.df) <- c(vec1name, vec2name)
      return(vec12.df)
   }

   # 2.2.2 get and write unique patterns only, in 2 columns, for the 2 string groups, respectively

   pattern_uniqu.df <- combine_2_vec(pattern_grp1_only, pattern_grp2_only)
   colnames(pattern_uniqu.df) <- unique_pattern_only_colname

   return(pattern_uniqu.df)

}
