#' @export
StrHclust <- function(strings.vec, nclust = 2){

   # Levenstein distance matrix
   d <- utils::adist(strings.vec)

   # hclust
   c <- stats::hclust(stats::as.dist(d))
   graphics::plot(c)
   cluster <- stats::cutree(c, nclust)

   # export df
   strClust.df <- as.data.frame(cbind(cluster, strings.vec))
   colnames(strClust.df) <- c("Cluster", "Strings")
   return(strClust.df)
}
