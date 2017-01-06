#' @export
HistDif <-
function(dif.vec, obsDif, pvalue, o.x = 0.01, o.y = 0, p.x = 0.015, p.y = 0){

   num_perm <- length(dif.vec)

   p_out <- format(pvalue, nsmall = 5)

   ### legend and text postions based on number of permutation
   
   if (o.y == 0){
     o.y <- 2 * num_perm * 0.11
   }

   if  (p.y == 0){
     p.y <- o.y - 50
   }


   ### Graph

   graphics::hist(dif.vec, col = "grey", 
        main = "Distribution of differences in average distance under H0 being true")
   graphics::box()
   graphics::abline(v = obsDif, lwd = 3, col = 4)

   ### legend (for 'Observed difference') and text (p value)

   graphics::legend(o.x, o.y, c("Observed difference"), lwd = 3, col = 4, lty = 1, seg.len = 1)
 
   graphics::text(p.x, p.y, col = "red", paste("pvalue = ", p_out))


}
