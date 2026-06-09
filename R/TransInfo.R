#' @export
TransInfo <-
function(strings.vec, type1 = "letters", type2 = "digits"){

  # remove strings with less than 2 characters 
  stringsx.pos <- which(nchar(strings.vec) >= 2)
  stringsx.vec <- strings.vec[stringsx.pos]

  # make sure stringsx.vec is characters
  stringsx.vec <- as.character(stringsx.vec)
  
   # number of strings

   numStr <- length(stringsx.vec)


   # default transition types

    if (type1 == "letters"){
       type1 <- "^[[:alpha:]]*$"
    }

    if (type2 == "digits"){
       type2 <- "^[[:digit:]]*$"
    }


   ### function to split all strings to substrings with length of 2, store in a list

   Split2 <- function(s){
       ss.list <- list()
       len <- nchar(s)-1
       for (i in 1:len){
         ss.list[i] <- list(substring(s,i,i+1)) 
       }
       return(ss.list)
   }
  

   ### functions to determine string type

    Type1Act <- function(ss){
         return (ss[grepl(type1, ss)])
    }

    Type2Act <- function(ss){
         return (ss[grepl(type2, ss)])
    }

    MixedAct <- function(ss){
         all.lett <- grepl(type1, ss)
         all.numb <- grepl(type2, ss)
         return (ss[!(all.lett | all.numb)])
    }


   ### split each string to substrings with length of 2

   strings.ss.list <- lapply(stringsx.vec, Split2)


   ### all 1st type of transitions (default all letters)

   type1.list <- lapply(strings.ss.list, Type1Act)


   ### all 2nd type of transitions (default all numbers)

   type2.list <- lapply(strings.ss.list, Type2Act)


   ### all mixed type of transitions (default a letter and a number)

   act_mixed.list <- lapply(strings.ss.list, MixedAct)


   ### number of each type

   transition_number <- vector(length = 3)

   transition_number[1] <- length(unlist(type1.list))
   transition_number[2] <- length(unlist(type2.list))
   transition_number[3] <- length(unlist(act_mixed.list))

   transition_type <- c("type1", "type2", "mixed")

   transitions.df <- data.frame(transition_type, transition_number)
 

   return(transitions.df)
}
