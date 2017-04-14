#' @export
StrKclust <- function(strings.vec, nclust = 2, nstart = 1){
   
   # Levenstein distance matrix
   d <- utils::adist(strings.vec)

   # kclust
   c <- stats::kmeans(d, centers = nclust, nstart = nstart)
   graphics::plot(d, type='n')
   graphics::text(d, labels = c$cluster, col = c$cluster)
 
   # export df
   strClust.df <- as.data.frame(cbind(c$cluster, strings.vec))
   colnames(strClust.df) <- c("Cluster", "Strings")
   return(strClust.df)
}
