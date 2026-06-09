#' @export
EveStr <-
function(eveName.df, eveName.vec, char.vec){

   ##### 1. number of eveNames in each row; empty cells are not counted

   eveName.df_num <- apply(eveName.df, 1, function(x) length(x[x != ""]))


   ##### 2. replace eveNames to characters

   # 2.1 replace   
   eveName_to_char.df <- apply(eveName.df, 2, function(x){
                 plyr::mapvalues(x, eveName.vec, char.vec, warn_missing = FALSE)
   })

   # 2.2 remove NA
   eveName_to_char.df1 <- apply(eveName_to_char.df, 2, function(x){
                 plyr::mapvalues(x, NA, "", warn_missing = FALSE)  
   })


   ##### 3. combine characters to a string in each row

   # 3.1 combine chars to a string in each row and store in a dataframe
   char_to_string.vec <- apply(eveName_to_char.df1, 1, function(x) paste(x, collapse = ""))

   # 3.2 check the sum of the lengths of strings, to see whether it matches the number in 1. 
   #     This does not apply to eveNames of single characters converting to char
   string_lengths <- nchar(char_to_string.vec)

   string_lengths_sum <- sum(string_lengths)

   eveName.sp_num_sum <- sum(eveName.df_num)

   return(char_to_string.vec)

   if (string_lengths_sum > eveName.sp_num_sum){
      stop('One or more event names are not replaced by characters. Please check the output datafame and your char file')
   } else {
      cat('All the event Names have been replaced to chars and the strings are in the datafame.\n')
   }
}
