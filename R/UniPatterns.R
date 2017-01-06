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

   utils::write.table(pattern_uniqu.df, sep="\t", col.names=T, file=unique_pattern_only_fname)


   ## 3. Unique patterns information

   # 3.1 initialize df for pattern summary

   # 3.1.1 list of unique patterns
   upattern.list <- list(pattern_grp1_only, pattern_grp2_only, pattern_grp1_only, pattern_grp2_only)

   # 3.1.2 each column is for a unique pattern

   num_col_grp.vec <- sapply(upattern.list, length)

   # 3.1.3 list of original strings
   string.list <- list(grp1_string, grp2_string, grp2_string, grp1_string)

   # 3.1.4 each row is an original string
   num_row_grp.vec <- sapply(string.list, length)

   # 3.1.5 length of each string
   len_s.list <- lapply(string.list, nchar)

   # 3.1.6 initialize df
   pattern_info.df.list <- lapply(1:4, function(i) data.frame(matrix(NA, ncol = num_col_grp.vec[i], num_row_grp.vec[i])))

   # 3.1.7 initialize df that also stores string lengths and upattern numbers
   pattern_info.df1.list <- lapply(1:4, function(i) data.frame(matrix(NA, ncol = (num_col_grp.vec[i]+2), num_row_grp.vec[i])))

   # 3.1.8 initialize df that stores rownames as a column
   pattern_info.df3.list <- lapply(1:4, function(i) data.frame(matrix(NA, ncol = (num_col_grp.vec[i]+3), num_row_grp.vec[i])))

   # 3.2 statistics of patterns 
   # get the start position of each pattern in each string;
   # if a pattern does not exist in one string, return '-1'.

   # 3.2.1 function of statistics of patterns
   stat_pattern <- function(patterns, strings){
      sapply(patterns, function(p) {
                sapply(strings, function(s) {
                       regexpr(p, s)[1]
                })

       })
   }
 
   # 3.2.2 apply function
   # pattern_info.df.list contains 4 df, each of which indicates the starting postions of each grp1/2 unique pattern in grp1/2 strings

   pattern_info.df.list <- lapply(1:4, function(i) stat_pattern(upattern.list[[i]], string.list[[i]]))


   # 3.2.3 num_p.list contains 4 vec, each of which indicates the sum numbers of each grp1/2 string containing grp1/2 unique patterns 
   num_p.list <- lapply(1:4, function(i){
                      apply(pattern_info.df.list[[i]], 1, function(x) sum(x!=-1))
                 })

   # 3.2.4 combine columns
   pattern_info.df1.list <- lapply(1:4, function(i){
                              data.frame(len_s.list[[i]], num_p.list[[i]], pattern_info.df.list[[i]], check.names=F)
                           })

   # 3.2.5 add the rownames as a proper column
   pattern_info.df3.list <- lapply(1:4, function(i){
                              cbind(rownames(pattern_info.df1.list[[i]]), pattern_info.df1.list[[i]])
                            #  cbind(row.names = rownames(pattern_info.df1.list[[i]]), pattern_info.df1.list[[i]])
                            #  rownames(pattern_info.df.list[i]) <- NULL
                           })

   # 3.2.6 rename the 1st and the last two column names
   for (i in 1:4){
      colnames(pattern_info.df3.list[[i]])[1] <- grp_snames.vec[i]
      colnames(pattern_info.df3.list[[i]])[2:3] <- c("Length", "numPattern")
   }

   # 3.2.7 output 4 unique pattern information files
   lapply(1:4, function(i){
           utils::write.table(pattern_info.df3.list[[i]], file=unique_pattern_info_fnames[i], sep="\t", row.names=F, col.names=T)
   })


   ## 4. Unique patterns summary

   # 4.1 number of strings that have at least one pattern
   num_str_w_pattern.vec = sapply(num_p.list, function(x) sum(!x==0))

   # 4.2 ratio
   ratio_str_w_pattern.vec <- sapply(1:4, function(i) num_str_w_pattern.vec[i]/num_row_grp.vec[i])

   # 4.3 print
   # 'invisible' hides NULLs returned
   invisible(lapply(1:4, function(i){
      tell.list = sprintf("For patterns %s in strings %s, the number of total strings is: %g
                  The number of strings that have at least one unique pattern is: %g 
                  The ratio of strings that have at least one unique pattern is:  %.3f \n\n",
                  grp_pnames.vec[i], grp_snames.vec[i], num_row_grp.vec[i], num_str_w_pattern.vec[i], ratio_str_w_pattern.vec[i])
      cat(tell.list)
   }))

}
