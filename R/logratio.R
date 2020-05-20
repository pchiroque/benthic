#' Calculate the log-ratio for compositional data
#'
#' Take a matrix or data.frame whose rows add to one and returns a matrix which is not of compositional data, but its log ratio applied to the rows
#'
#' @param CC A matrix or data.frame whose rows sum to one or one hundred.
#' @return A matrix composed of the log ratios values.
#' @examples
#' C = matrix(1:20,4)/rowSums(matrix(1:20,4))
#' logratio(C)
#' @export
logratio = function(CC){
  if (min(CC) < 0) stop("Elements of the matrix cannot be negative")
  CC[CC==0] = 1e-5
  if (all(rowSums(CC) == 100)) CC = CC/100
  if (any(rowSums(CC) != 1)) stop("Matrix is not compositional data. All rows must sum 1 or 100")

  t(apply(log10(CC),1,function(x) scale(x,scale=FALSE)))
}
