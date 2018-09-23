ivmcmc <- function(bg0, y, x, z, flatrf=FALSE, H, nit) {
  lh0 <- -ivlh(bg0, y, x, z, flatrf)
  mcdraws <- matrix(0, nit, length(bg0) + 1)
  bg <- bg0
  lh <- lh0
  Hfac <- chol(H)
  mcdraws[1, ] <- c(bg0, lh0)
  npar <- length(bg)
  accrate <- 1
  for (it in 1:(nit-1)) {
    bgnew <- bg + crossprod(Hfac, rnorm(npar))
    lhnew <- -ivlh(bgnew, y, x, z, flatrf)
    if(exp(lhnew - lh) > runif(1) ) {
      bg <- bgnew
      lh <- lhnew
      accrate <- .1 + .9 * accrate
    } else {
      accrate <- .9 * accrate
    }
    if (it %% 200 == 1) print(paste("accrate: ", accrate, "it+1:", it+1))
    mcdraws[it + 1, ] <- c(bg, lh)
  }
  return(mcdraws)
}