#' @export
StrDif <-
function(grp1_string, grp2_string, num_perm = 1000, o.x = 0.01, o.y = 0, p.x = 0.015, p.y = 0){

   # number of strings in each group
  
   num_subj1 <- length(grp1_string)
   num_subj2 <- length(grp2_string)
   num_subj_all <- num_subj1 + num_subj2

   # combination of strings from 2 groups

   grps_string <- c(grp1_string, grp2_string)

   ### function to find longer string

   longerStr <- function(s1, s2){
      l1<- nchar(s1)
      l2 <- nchar(s2)
      if (l1 > l2){
         return (l1)
      } else {
         return (l2)
      }
   }

   #### within
   ### function of within: ld

   ld.within <- function(s){
      # number of strings
      len <- length(s)

      # list to hold ld 

      ld.within.list <- vector(mode="list", length=len)
      longLen.list <- vector(mode="list", length=len)

      ld.within.list <- sapply(1:(len-1), function(i){
                           utils::adist(s[i],s[(i+1):len])
                        })

      ld.within.vec <- unlist(ld.within.list)

      return(ld.within.vec)
   }

   ### function of within: length of longer string between 2

   longLen.within <- function(s){
      # number of strings
      len <- length(s)

      # list to hold the length of longer string between 2

      longLen.within.list <- vector(mode = "list", length = len)

      longLen.within.list <- sapply(1:(len-1), function(i){
                         mapply(longerStr, s1 = s[i], s2 = s[(i+1):len])
                  })

      longLen.within.vec <- unlist(longLen.within.list)
      return(longLen.within.vec)
   }

   ### function of mean normalized ld - within

   ave_ld.within_norm <- function(s1, s2){

      ## apply within functions to two groups of strings

      ld.within.vec1 <- ld.within(s1)
      longLen.within.vec1 <- longLen.within(s1)

      ld.within.vec2 <- ld.within(s2)
      longLen.within.vec2 <- longLen.within(s2)

      ## combine two groups

      ld.within.vec.com <- c(ld.within.vec1, ld.within.vec2)
      longLen.within.vec.com <- c(longLen.within.vec1, longLen.within.vec2)

      ## get mean of normalized ld

      ld.within.vec_norm.vec <- ld.within.vec.com/longLen.within.vec.com

      ld.within_norm <- mean(ld.within.vec_norm.vec)

       return(ld.within_norm)
   }
      
   ## original normalized ld: within

   ld.within_norm.ori <- ave_ld.within_norm(grp1_string, grp2_string)
   

   #### between
   ### function of mean normalized ld - between

   ave_ld.between_norm <- function(s1, s2){

      # number of strings
      len1 <- length(s1)
      len2 <- length(s2)

      # matrix to hold ld and the length of longer string between 2

      ld.between.m <- matrix(ncol = len2, nrow = len1)
      longLen.m <- matrix(ncol = len2, nrow = len1)

      ld.between.m <- utils::adist(s1, s2)

      longLen.m <-sapply(1:len2, function(i) {
            mapply(longerStr, s1 = s2[i], s2 = s1)
      })

      ld.between_norm.m <- ld.between.m / longLen.m

      ld.between_norm <- mean(ld.between_norm.m)

      return(ld.between_norm)
   }

   ## original normalized ld: between

   ld.between_norm.ori <- ave_ld.between_norm(grp1_string, grp2_string)


   #### difference of ld
   ### function of difference of ld between mean normalized ld (between) and mean normalized ld (within)

   dif_ld_norm <- function(s1, s2){

      ld_b_norm <- ave_ld.between_norm(s1, s2)
      ld_w_norm <- ave_ld.within_norm(s1, s2)

      dif_ld_norm <- ld_b_norm - ld_w_norm

      return(dif_ld_norm)
   }

   ## original difference of normalized ld

   dif_ld_norm.ori <- dif_ld_norm(grp1_string, grp2_string)


   #### Permutation

   ### number of permutation
   # It should be the same if using num_subj2
   actual_perm <- choose(num_subj_all, num_subj1)

   if(actual_perm < num_perm){
       num_perm <- actual_perm
   }

   ### use a matrix to hold all permutation strings, each column is a set of all strings

   # strings_perm.m <- matrix(NULL, nrow = num_subj_all, ncol = num_perm) 
   strings_perm.m <- matrix(nrow = num_subj_all, ncol = num_perm) 

   # permute all strings num_perm times

   strings_perm.m <- replicate(num_perm, sample(grps_string, num_subj_all, replace = FALSE))
 
   # Force the first permutation (1st column in the matrix) to be the original:

   strings_perm.m[,1] <- grps_string  

   # divide the matrix to groups 1 and 2

   grp1_strings_perm.m <- strings_perm.m[1:num_subj1,]
   grp2_strings_perm.m <- strings_perm.m[(num_subj1+1):num_subj_all,]

   # output 1000 mean dif ld, apply function dif_ld_norm:

     dif_ld_norm.vec <- vector(length = num_perm)

     dif_ld_norm.vec <- sapply(1:num_perm, function(k){
                            dif_ld_norm(grp1_strings_perm.m[,k], grp2_strings_perm.m[,k])
     })


   pvalue <- round(mean(dif_ld_norm.vec >= dif_ld_norm.ori), 5)

   p_out <- format(pvalue, nsmall = 5)

   ### legend and text postions based on number of permutation
   
   if (o.y == 0){
     o.y <- 2 * num_perm * 0.11
   }

   if  (p.y == 0){
     p.y <- o.y - 50
   }


   ### Graph

   graphics::hist(dif_ld_norm.vec,col = "grey",
        main = "Distribution of differences in average distance under H0 being true")
   graphics::box()
   graphics::abline(v = dif_ld_norm.ori, lwd=3, col=4)

   ### legend ('Observed difference') and text (p value)

   graphics::legend(o.x, o.y, c("Observed difference"), lwd = 3, col = 4, lty = 1, seg.len = 1)
 
   graphics::text(p.x, p.y, col="red", paste("pvalue = ", p_out))
 

      tell.list <- sprintf("\n\n For the initial two groups of strings,
   the average normalized between-group Levenstein Distance is:  %.5f  
   the average normalized within-group Levenstein Distance is:  %.5f  
   the difference in the average normalized Levenstein Distance between between-group and within-group is:  %.5f.
   The p value of the permutation test is:   %.5f \n\n",
   ld.between_norm.ori, ld.within_norm.ori, dif_ld_norm.ori, pvalue)
   cat(tell.list)


   ### return the vector of ld differences

   return(dif_ld_norm.vec)

}
