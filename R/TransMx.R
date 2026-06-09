#' @export
TransMx <- function(strings.vec){

   ##### Prepare: get the name of the input string vector
       strings.vec_name <- deparse(substitute(strings.vec))
 
   ##### 0. get all unique characters of all strings

   # 0.1 remove strings with fewer than 2 characters 
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
  
   # 4.3 first characters (i.e., 'From') of transitions
   trans.df.sum1.df <- noquote(cbind(str.spu, trans.df.sum.df))
   colnames(trans.df.sum1.df)[1] <- "From/To"

   ### 4.4 get normalized numbers of transitions

   # 4.4.1 grand total number of all transitions in all strings
   trans.total <- sum(trans.df.sum.df)  

   # 4.4.2 normalized transition numbers in all strings
   trans.df.sum_norm.df <- trans.df.sum.df / trans.total

   # 4.4.3 round to 4 decimals if not 0
   trans.df.sum_norm.df <- apply(trans.df.sum_norm.df, 1:2, function(x){
                                    ifelse (x > 0, round(x, digits = 4), "0")
                              })
 
   # 4.4.4 first characters (i.e., 'From') of transitions    
   trans.df.sum_norm1.df <- noquote(cbind(str.spu, trans.df.sum_norm.df))
   colnames(trans.df.sum_norm1.df)[1] <- "From/To"

 
   ##### 5. transition numbers of each transition in all strings

   # 5.1 numbers of all transitions
   trans_num.vec <- trans.df.sum.df[trans.df.sum.df > 0]

   # 5.2 store in a df                                  
   trans_num.df <- data.frame(matrix(0, ncol = 2, nrow = length(trans_num.vec)))
   trans_num.df[,2] <- trans_num.vec

   # 5.3 positions of transitions in trans.df.sum.df
   trans_pos.mx <- which(trans.df.sum.df > 0, arr.ind=T)

   # 5.4 convert positions represented by digits to characters
   trans_pos.lett.mx <- plyr::mapvalues(trans_pos.mx, str.spun, str.spu, warn_missing = FALSE)
 
   # 5.5 paste characters to form transitions
   trans_pos.2lett.vec <- apply(trans_pos.lett.mx, 1, function(x) paste(x, collapse = ""))

   # 5.6 store in the df
   trans_num.df[,1] <- trans_pos.2lett.vec

   # 5.7 sort by descent order and then assign col names and remove (current row names)
   trans_num.df <- trans_num.df[with(trans_num.df, order(-trans_num.df[,2])),]
   colnames(trans_num.df) <- c("transition", "number_of_transition")
   rownames(trans_num.df) <- NULL

   ##### 6. put the three df above in a list

   trans.out.df.list <- list(Transition_Matrix = trans.df.sum1.df,
                              Transition_Normalized_Matrix = trans.df.sum_norm1.df,
                              Transition_Organized = trans_num.df)
        

   ##### return the list containing the 3 df

    return(trans.out.df.list)
}
