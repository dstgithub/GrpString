#' @export
EveString <-
function(eveName.file, eveName.vec, char.vec){

   ##### 1. Prepare file names

   # 1.1 sub-function to get the last n character of a string
   strEnd <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
   }

   # 1.2 find the Appendix of the eveName file name, which should be either '.txt' or '.csv'
   eveNameFile_nameA <- strEnd(eveName.file, 4)

   # 1.3 check the file appendix, and get the corresponding seperating method
   if (eveNameFile_nameA == ".txt"){
      sep_method <- "\t"
   } else if (eveNameFile_nameA == ".csv"){
      sep_method <- ","
   } else{
     stop('There is something wrong with the (name of the) eveName file. Please check you file.') 
   }

   # 1.4 find the Main part of the eveName file name
   eveNameFile_nameM <- unlist(strsplit(eveName.file, eveNameFile_nameA))
 
   # 1.5 generate the name of the output file of strings, ending with '_string.txt'
   stringFile_name <- paste(eveNameFile_nameM, "str.txt", sep = "_")

   # If not all eveNames are converted, a temporary file is outputed
   stringTemp_name <- paste(eveNameFile_nameM, "temp.txt", sep = "_")


   ##### 2. Read files 

   # 2.1 read the eveName file line by line
   eveName_lines <- readLines(eveName.file)

   # 2.2 split line of action, get individual eveName
   # different sep_method for txt or csv, see 1.
   eveName.sp <- strsplit(as.character(eveName_lines), sep_method) 

   # 2.2.1 number of eveNames in each row; empty cells are not counted
   eveName.sp_num <- sapply(eveName.sp, function(x) length(x[x != ""]))

   # 2.3 bind data to one data frame
   eveName.sp1 <- lapply(eveName.sp, function(x) as.data.frame(t(x)))
   eveName.df1 <- plyr::rbind.fill(eveName.sp1)


   ##### 3. replace eveNames to chars

   # 3.1 replace - eveNames to chars   
   eveName_to_char.df1 <- apply(eveName.df1, 2, function(x){
                plyr::mapvalues(x, eveName.vec, char.vec, warn_missing=FALSE)
   })

   # 3.2 remove NA
   eveName_to_char.df2 <- apply(eveName_to_char.df1, 2, function(x){
                plyr::mapvalues(x, NA, "", warn_missing=FALSE)
   })  


   ##### 4. combine chars to a string in each row

   # 4.1 combine chars to a string in each row and store in a vector
   char_to_string.vec <- apply(eveName_to_char.df2, 1, function(x) paste(x, collapse = ""))

   # 4.2 check the sum of the lengths of strings, to see whether it matches the number in 2.2.1 
   #     This does not apply to eveNames of single characters converting to char
   string_lengths <- nchar(char_to_string.vec)

   string_lengths_sum <- sum(string_lengths)

   eveName.sp_num_sum <- sum(eveName.sp_num)

   if (string_lengths_sum > eveName.sp_num_sum){
      return(char_to_string.vec)
      stop('Warning: One or more event names are not replaced by characters. Please check the output vector and your conversion key.')
   } else {
      return(char_to_string.vec)
      cat('All the event names have been replaced to characters and the strings are in the output vecotr.\n')
   }
}
