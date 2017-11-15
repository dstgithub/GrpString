#' @export
StrKclust <- function(strings.vec, nclust = 2, nstart = 1, shade = FALSE){
   
   # Levenstein distance matrix
   d <- utils::adist(strings.vec)

   # kclust
   c <- stats::kmeans(d, centers = nclust, nstart = nstart)

   if(shade=="TRUE"){
     cluster::clusplot(d, c$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main='Cluster plot')
   } else{
     cluster::clusplot(d, c$cluster, color=TRUE, shade=FALSE, labels=2, lines=0, main='Cluster plot')
   }
 
   # export df
   strClust.df <- as.data.frame(cbind(c$cluster, strings.vec))
   colnames(strClust.df) <- c("Cluster", "Strings")
   return(strClust.df)
}
