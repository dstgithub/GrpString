#' @export
CommonPatt <-
function(strings.vec, low = 10){

   
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
   subStr_all.df <- data.frame(subStr_all.m, stringsAsFactors=FALSE)
 
   # 3.2 Get all substrings (length>=3) for each string 

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

   subStr_all_result.df <- subStr_all_result.df[with(subStr_all_result.df,
                                     order(-subStr_all_result.df$Length)),]
 
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

   names(subStr_once_result.df) <- c("Patterns1", "Freq1")
   subStr_once_result.df$Patterns1 <- as.character(subStr_once_result.df$Patterns1)

   # 5.4 Ratio of number of each substring to number of original full strings

   pattRatio1 <- subStr_once_result.df$Freq1/numStrings

   # 5.5 more columns
   subStr_once_result.df$Percent1 <- percent(pattRatio1) # column of ratio in percent
   subStr_once_result.df$pattRatio1 <- pattRatio1            # column of ratio in digits
   subStr_once_result.df$pattRatio1 <- format(subStr_once_result.df$pattRatio1, digits=2, nsmall=2)

   # column of substring length # same as all substrings
    subStr_once_result.df$Length1 <- nchar(subStr_once_result.df$Patterns1)

   # 5.6 sort substring length - from longest to shortest

   subStr_once_result.df <- subStr_once_result.df[with(subStr_once_result.df,
                                      order(-subStr_once_result.df$Length1)),]


   ##### 6. Combine all and once to all.

   # 6.1 Copy the 'all' df

   subStr_result_out.df <- subStr_all_result.df

   # 6.2 Get freq and percent info from the 'once' df

   subStr_result_out.df$Freq_str <- subStr_once_result.df$Freq1
   subStr_result_out.df$Percent_str <- subStr_once_result.df$Percent1

   # 6.2.1 Sort substring length (long to short), then frequency (high to low)

   subStr_result_out.df <- subStr_result_out.df[with(subStr_result_out.df,
         order(-subStr_result_out.df$Length, -subStr_result_out.df$Freq_total)),]


   # 6.3 use only pattRatio >=low, then remove the ratio column (no conversion to token)

   subStr_result_out_f2.df <- subset(subStr_result_out.df, subStr_result_out.df$pattRatio >= (low/100))[, -4]

   return(subStr_result_out_f2.df)

}
