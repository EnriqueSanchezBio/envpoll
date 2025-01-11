
#' Title
#'
#' @description
#' This function performs a Self-Organizing Map (SOM) analysis on the provided data and plots both the counts and the environmental variable codes.
#'
#' @param data A numeric matrix or data frame where rows represent samples and columns represent variables.
#' @param env.data A numeric matrix or data frame of environmental variables with the same number of rows as \code{data}.
#' @param xdim An integer specifying the number of nodes in the x-dimension of the SOM grid.
#' @param ydim An integer specifying the number of nodes in the y-dimension of the SOM grid.
#' @param topo A character string specifying the topology of the SOM grid. Default is \code{"hexagonal"}.
#' @param rlen An integer specifying the number of iterations for training the SOM. Default is 100.
#' @param alpha A numeric vector of length 2 specifying the initial and final learning rates. Default is \code{c(0.05, 0.01)}.
#' @param keep.data A logical indicating whether to keep the original data in the SOM object. Default is \code{TRUE}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{som.model}: The trained SOM model.
#'   \item \code{env.data1}: The aggregated environmental data by SOM units.
#'   \item \code{groups}: A list of indices indicating which samples belong to each SOM unit.
#'   \item \code{som.model1}: A SOM model where the codebook vectors are replaced with aggregated environmental data.
#' }
#'
#'
#' @examples
#' # Example usage:
#' set.seed(123)
#' data <- matrix(runif(150), nrow = 30, ncol = 5)
#' env.data <- matrix(runif(150), nrow = 30, ncol = 5)
#' som.representation2(data, env.data, xdim = 4, ydim = 4)
#'
#'
#' @importFrom kohonen som  somgrid
#' @importFrom graphics par
#' @importFrom stats aggregate
#' @importFrom grDevices grey
#'
#' @export


som.representation2<-function(data,env.data,xdim,ydim,topo="hexagonal",
                              rlen=100,alpha=c(0.05,0.01),keep.data=TRUE)
{ greyscale<-function(n,alpha=0.85){
  grey(c(1:(n))/n,alpha=alpha)[n:1]}

  som.model<-som(data,grid=somgrid(xdim=xdim,ydim=ydim,topo="hexagonal"),rlen=100,
                 alpha=c(0.05,0.01),keep.data=TRUE)
  env.data1<-cbind(aggregate(env.data[,1],by=list(som.model$unit.classif),mean)[,2],
                   aggregate(env.data[,2],by=list(som.model$unit.classif),mean)[,2],
                   aggregate(env.data[,3],by=list(som.model$unit.classif),mean)[,2],
                   aggregate(env.data[,4],by=list(som.model$unit.classif),mean)[,2],
                   aggregate(env.data[,5],by=list(som.model$unit.classif),mean)[,2])
  colnames(env.data1)<-colnames(env.data)
  groups<-as.list(1:nrow(env.data1))
  for (i in 1:nrow(env.data1)){
    groups[[i]]<-which(som.model$unit.classif==i)
  }
  par(mfrow=c(1,2))
  plot(som.model,"counts",palette.name=greyscale)
  som.model1<-som.model
  som.model1$codes<-env.data1
  plot(som.model1,type="codes",palette.name=greyscale)
  return(list(som.model,env.data1,groups,som.model1))
}
