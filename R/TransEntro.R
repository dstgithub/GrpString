#' @export
TransEntro <- function(strings.vec){

   ##### 0. get all unique characters of all strings

   # 0.1 remove strings with less than 2 characters 
   stringsx.pos <- which(nchar(strings.vec) >= 2)
   stringsx.vec <- strings.vec[stringsx.pos]
   num_strings <- length(stringsx.vec)

   # 0.2 make sure stringsx.vec is characters
   stringsx.vec <- as.character(stringsx.vec)

   # 0.3 split string into single characters
   str.sp.list <- lapply(stringsx.vec, function(x) strsplit(x, ''))

   # 0.4 vector of unique characters from above, sorted
   str.spu <- sort(unique(unlist(str.sp.list)))

   # 0.5 number of unique characters
   num.str.spu <- length(str.spu)

   # 0.6 number sequence from 1 to num.str.spu
   str.spun <- 1:num.str.spu


   ###### Level 2 function: all transitions in a string ######

   TransLoca <- function(str){

    ##### 1. split str to all transitions, in a vector

      ### Level 3 function: split a string to all transitions ###

      Split2v <- function(s){ # vector version
          len <- nchar(s)-1
          ss.vec <- vector(length = len) 
          for (i in 1:len){
            ss.vec[i] <- (substring(s,i,i+1)) 
          }
          return(ss.vec)
      } # end of level 3 function Split2v

      ### 1.2 apply level 3 split2v function  
      str.trans <- Split2v(str)

      # 1.2 number of each transition, in a vector
      # name of each element in vector is transition
      trans.table <- table(str.trans)

      # 1.3 number of all transitions
      # it is equal to sum(trans.table)
      num.trans <- nchar(str) - 1


      ##### 2. convert each of the transitions to a pair of numbers

      ### Level 3 function: convert transition to a pair of numbers ###
      #   each pair represents a row number and a column number for a df

      TransPos <- function(single.trans){
         # 2.1.1 transition itself
         trans <- names(single.trans)

         # 2.1.2 split transition to two characters, store in vector
         trans.char <- unlist(strsplit(trans, ''))

         # 2.1.3 replace characters to digits
         # digits in str.spun correspond to characters in str.spu
         trans.loca <- as.numeric(plyr::mapvalues(trans.char, str.spu, str.spun,
                                  warn_missing = FALSE))

         return(trans.loca)
      } # end of level 3 function TransPos

      ### 2.2 apply function TransLoca to all transitions in the string

      transLoca.list <- lapply(1:length(trans.table), function(i) TransPos(trans.table[i]))


      ##### 3. assign numbers of transitions to data frame
      #   if a transition does not occur, the number is 0

      # 3.1 initialize df
      trans.df <- data.frame(matrix(0, ncol = num.str.spu, nrow = num.str.spu))


      # 3.2  assign numbers of transitions
      # it seems that the for loop is the only choice(?)
      for(i in 1:length(trans.table)){
           trans.df[transLoca.list[[i]][1], transLoca.list[[i]][2]] <- trans.table[i]
      }

      # 3.3 second characters (i.e., 'To') of transitions
      colnames(trans.df) <- str.spu 

      return(trans.df)

   } # end of level 2 function TransLoca


   ##### 4. numbers of transitions in all strings
 
   ### 4.1 apply level 2 function TransLoca to get a list of transition matrix
   trans.df.list <- lapply(stringsx.vec, TransLoca)

   # 4.2 add all the df in the list together
   # do.call in previous versions can only be used for 2 dfs in a list
   trans.df.sum.df <- Reduce('+', trans.df.list)
  
 
   ##### 5. transition number and frequency (ratio) of each transition in all strings

   # 5.1 numbers of all transitions
   trans_num.vec <- trans.df.sum.df[trans.df.sum.df > 0]

   # 5.2 frequencies of all transitions.
   #     They are called "normalized" transitions in function TransMx.                                 
   trans_num_norm.vec <- trans_num.vec / sum(trans_num.vec) 


   ##### 6. Entropy of transitions for a group of strings

   entropy <- -sum(trans_num_norm.vec * log2(trans_num_norm.vec))

   return(entropy)

}
