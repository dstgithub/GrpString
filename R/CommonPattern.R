#' @export
CommonPattern <-
function(strings.vec, low = 5, high = 25, interval = 5, eveChar.df){

   ##### 1. Prepare file names

   # 1.1 get the name of the string vector
   strings.vec_name <- deparse(substitute(strings.vec))




   ##### 2. Store string information
   # column1: strings; column2:lengths of strings; column3:number of all substrings

   # 2.0 number of strings in the original vector
   numStrings <- length(strings.vec)

   # 2.1 Initialize data frame

   string_info.m <- matrix(ncol = 3, nrow = numStrings)
   string_info.df <- data.frame(string_info.m, stringsAsFactors = FALSE)

   # 2.2 Get and store string information
   # 2.2.1 stings and string lengths
 
   string_info.df[,1] <- strings.vec
   string_info.df[,2] <- nchar(string_info.df[,1])

   maxNchar = max(string_info.df[,2]) # maximum length

   # 2.2.2 numbers of all substrings (length >= 3)
   string_info.df[,3] <- 0
   string_info.df[,3] <- (1 + string_info.df[,2] -2) * (string_info.df[,2] -2) /2

   # 2.1.3 maxium number of substrings or rows to store substrings
   numrow_substr <- max(string_info.df[,3])


   ##### 3. Get and store substrings

   # 3.1 initialize data frame to store all substrings
   # To store all substrings. Each column is for a set of substrings of a string.
   # note numrow_substr is the maxium number of substrings or rows, i.e.,
   # only one or a few columns will use up numrow; most will be fewer

   subStr_all.m <- matrix(ncol = numStrings, nrow = numrow_substr)
   subStr_all.df <- data.frame(subStr_all.m, stringsAsFactors = FALSE)
 
   # 3.2 Get all substrings (length >= 3) for each string 

   for (m in 1:numStrings){
      k <- 1
      for (i in 1:(string_info.df[m,2]-2)){ # start char position of a string
         for (j in (i+2):string_info.df[m,2]){ # end char position of a string
           subStr_all.df[k, m] <- substring(string_info.df[m,1], i, j)
           k <- k+1
         }
      }
   }


   ##### 4. Statistics and sort

   # 4.1 Find pattern (substring) frequencies
   # organize all substrings in col1 and the corresponding Freq in col2 using 'table'

   subStr_all_result.df <- as.data.frame(table(unlist(subStr_all.df)))

   names(subStr_all_result.df) <- c("Pattern", "Freq_total")

   # in case substring starting w/ '0'.
   subStr_all_result.df$Pattern <- as.character(subStr_all_result.df$Pattern)

   # 4.2 Ratio of number of each substring to number of original full strings

   pattRatio <- subStr_all_result.df$Freq_total/numStrings

   # 4.3 function to write as percent format

   percent <- function(x, digits = 2, format = "f", ...){
                    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
              }

   # 4.4 more columns

   # Percentage in % and digit formats, respectively
   subStr_all_result.df$Percent_total <- percent(pattRatio) # column of ratio in percent
   subStr_all_result.df$pattRatio <- pattRatio            # column of ratio in digits
   subStr_all_result.df$pattRatio <- format(subStr_all_result.df$pattRatio, digits = 2, nsmall = 2)

   # column of substring length 
   subStr_all_result.df$Length <- nchar(subStr_all_result.df$Pattern)

   # 4.5 Sort substring length - from longest to shortest

   subStr_all_result.df <- subStr_all_result.df[with(subStr_all_result.df, order(-subStr_all_result.df$Length)),]

   # 4.6 total number of all substrings

   numPatternTotal <- length(subStr_all_result.df$Length)

   ##### 5. Another consideration: same substrings is only counted once within one string ###
   # That is, each string can have a certain substring ONLY ONCE.
   # e.g., in string 'abcdabc', 'abc' is only counted once.
   # Thus, it has the same substrings as in the 'all' file, with possibly different frequencies

   # 5.1 Get and store substrings
   # 5.1.1 initialize data frame to store all substrings

   subStr_once.m <- matrix(ncol = numStrings, nrow = numrow_substr)
   subStr_once.df <- data.frame(subStr_once.m, stringsAsFactors = FALSE)
 
   # 5.1.2 Get all substrings (length >= 3) for each string 

   subStr_once.list <- apply(subStr_all.df, 2, function(x) unique(x))

   # number of unique substrings in each string and the max number
   n_subStr_once <- sapply(subStr_once.list, length)
   max.n_subStr_once <- max(n_subStr_once)

   subStr_once.m <- matrix(ncol = numStrings, nrow = max.n_subStr_once)
   subStr_once.df <- data.frame(subStr_once.m, stringsAsFactors = FALSE)
 
   subStr_once.df <- sapply(subStr_once.list, "[", i = 1:max.n_subStr_once)


   # 5.2 Find frequencies for 'once' substrings

   subStr_once_result.df <- as.data.frame(table(unlist(subStr_once.df)))

   # 5.3 First two columns of the 'once' data frame

   names(subStr_once_result.df) <- c("Patterns1", "Freq_str")
   subStr_once_result.df$Patterns1 <- as.character(subStr_once_result.df$Patterns1)

   # 5.4 Ratio of number of each substring to number of original full strings

   pattRatio1= subStr_once_result.df$Freq_str/numStrings

   # 5.5 more columns
   subStr_once_result.df$Percent_str <- percent(pattRatio1) # column of ratio in percent
   subStr_once_result.df$pattRatio1 <- pattRatio1            # column of ratio in digits
   subStr_once_result.df$pattRatio1 <- format(subStr_once_result.df$pattRatio1, digits = 2, nsmall = 2)

   # column of substring length # same as all substrings
    subStr_once_result.df$Length1 <- nchar(subStr_once_result.df$Patterns1)

   # 5.6 sort substring length - from longest to shortest

   subStr_once_result.df <- subStr_once_result.df[with(subStr_once_result.df,
                                      order(-subStr_once_result.df$Length1)),]


   ##### 6. Combine all and once to all.

   # 6.1 Duplicate the 'all' df

   subStr_result_out.df <- subStr_all_result.df

   # 6.2 Get freq and percent info from the 'once' df

   subStr_result_out.df$Freq_str <- subStr_once_result.df$Freq_str
   subStr_result_out.df$Percent_str <- subStr_once_result.df$Percent_str

   # 6.2.1 Sort substring length (long to short), then frequency (high to low), then remove the ratio column 

   subStr_result_out.df <- subStr_result_out.df[with(subStr_result_out.df,
           order(-subStr_result_out.df$Length, -subStr_result_out.df$Freq_total)),]
   subStr_result_outx.df <- subStr_result_out.df[, -4]

   # 6.2.2 Write the raw result to .txt as output file. 

   out.raw.name <- paste0(strings.vec_name, "_out", ".txt")
   utils::write.table(subStr_result_outx.df, sep = "\t",
                      row.names=F, col.names = TRUE, file = out.raw.name)


   # 6.3 Case 1: use only frequencies >= 2, then remove the ratio column (no conversion to token)

   subStr_result_out_f2.df <- subset(subStr_result_out.df, subStr_result_out.df$Freq_total > 1)[, -4]

   # 6.3.1 name of output file

   out.f2.name <- paste0(strings.vec_name, "_f2up", ".txt")

   # 6.3.2 Write .txt as output file. Note no conversion happens here because patterns with freq>=2 can be a lot.

   utils::write.table(subStr_result_out_f2.df, sep = "\t",
                      row.names=F, col.names = TRUE, file = out.f2.name)


   # 6.4 Case 2+: use pattern Ratio >= low_cutoff, ... 15%, 20%, 25%,...high_cutoff..., respectively
   #     default percentages: low_cutoff = 5, high_cutoff = 25, inter_cutoff = 5

   # 6.4.1 cutoff sequence: from ow_cutoff to high_cutoff

   cutoff.seq <- seq(low, high, by = interval)

   # count numbers in the range

   cutoff.seq_num <- length(cutoff.seq)

   # percentage to percent in digits

   cutoff.seq.n <- 0.01 * cutoff.seq

   # form of 2 digits for every number

   cutoff.seq.c <- sprintf("%02d", cutoff.seq)

   # 6.4.2 names of output files

   out.file.names <- paste0(strings.vec_name, "_", cutoff.seq.c, "up", ".txt")

   # 6.4.3 Get list containing dfs with different %, then remove the ratio column

   subStr_result_cutoff.list <- lapply(cutoff.seq.n, function(x){
                                    subset(subStr_result_out.df, subStr_result_out.df$pattRatio >= x)[, -4]
                                })

   ##### 7. Optional: convert characters back to event names

   # Test whether conversion is needed or not

   if(missing(eveChar.df)){

      # 7.1 Directly write output files with different percentages

      lapply(1:cutoff.seq_num, function(i){
               utils::write.table(subStr_result_cutoff.list[[i]], sep = "\t", row.names = FALSE, col.names = TRUE,
                      file = out.file.names[i])
      })
   } else{

      # 7.2 convert character back to event name     
      # 7.2.1 Split all patterns (substrings)

      subStr_result_cutoff.sp.list <- lapply(1:cutoff.seq_num, function(i){
                                              strsplit(subStr_result_cutoff.list[[i]]$Pattern, split = "")
                                      }) 


      # 7.2.2 replace characters with event names, then combine each set of event names to a string

      subStr_result_cutoff.t.list <- lapply(subStr_result_cutoff.sp.list, function(x){ 
                                            lapply(1:length(x), function(i){
                                               paste(plyr::mapvalues(x[[i]], eveChar.df[,2], eveChar.df[,1],
                                                     warn_missing = FALSE), collapse = ",")
                                            })
                                         })


      # 7.2.3 Add the converted strings as the last column of the output df

      subStr_result_cutoff.tr.list <- lapply(1:cutoff.seq_num, function(i){
                                           cbind(subStr_result_cutoff.list[[i]],
                                             unlist(subStr_result_cutoff.t.list[[i]]))
                                })                                                

      # 7.2.4 Change the name of the newly added column 
      for (i in 1:cutoff.seq_num){
         names(subStr_result_cutoff.tr.list[[i]])[7] = "Event_name"
      }

      # 7.3.5 Write output files with different percentages, with an additional patterns (in token) column

      lapply(1:cutoff.seq_num, function(i){
               utils::write.table(subStr_result_cutoff.tr.list[[i]], sep = "\t", row.names = FALSE, col.names = TRUE,
                      file = out.file.names[i])
      })
    } # end of else


   ##### 8. End of function reminder

   cat('      Files with different percentages of common patterns are exported.
      Patterns occur at least twice and all possible substrings are exported in separate files.\n')
   sprintf("Number of original strings: %d; Total number of substrings (patterns): %d.", numStrings, numPatternTotal)

}
