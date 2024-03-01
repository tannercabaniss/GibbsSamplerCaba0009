#' @title rbvnorm
#'
#' @description This function allows the user to produce a random sample of the bivariate normal distribution.
#'
#' @param iter number of samples to draw from the distribution
#' @param init initial x2 value for sampling
#' @param mu vector of means for x1 and x2
#' @param sigma covariance matrix for x1 and x2
#'
#' @return a list object containing the random sample and relevant metrix (iter, mu, sigma)
#' @export
#'
#' @examples
#' rbvnorm(iter=10000, init=2, mu=c(3,6), sigma = matrix(c(8,-2,-2,4), nrow=2, byrow=TRUE))
rbvnorm <- function(iter, init=0, mu, sigma) {
  mu_x1 <- mu[1]
  mu_x2 <- mu[2]
  s_x1x1 <- sigma[1,1]
  s_x2x2 <- sigma[2,2]
  s_x1x2 <- sigma[1,2]
  s_x2x1 <- s_x1x2

  seed <- init
  rmat <- matrix(NA, nrow=iter, ncol=2)
  rmat[1,1] <- mu_x1
  rmat[1,2] <- seed

  for (i in 2:iter){
    rmat[i, 1] <- rnorm(1, mu_x1 + s_x1x2/s_x2x2*(rmat[i-1,2] - mu_x2), sqrt(s_x1x1 - s_x1x2^2/s_x2x2))
    rmat[i, 2] <- rnorm(1, mu_x2 + s_x2x1/s_x1x1*(rmat[i,1] - mu_x1), sqrt(s_x2x2 - s_x2x1^2/s_x1x1))
  }

  colnames(rmat) <- c("x1", "x2")
  df <- as.data.frame(rmat)
  return(invisible(list(gibbs=df, iter=iter, mu=mu, sigma=sigma)))
}
