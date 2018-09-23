ivlh <- function(bg, y, xx, z, flatrf=FALSE) {
  ## we add a constant to both stages, as the last columns of x and z.
  x <- xx                             #To avoid name conflict with numgrad's x argument
  if (is.null(dim(x))) x <- matrix(x, ncol=1)
  if (is.null(dim(z))) z <- matrix(z, ncol=1)
  nx <- dim(x)[2]
  nz <- dim(z)[2]
  nobs <- dim(x)[1]
  x <- cbind(x, rep(1, nobs))
  z <- cbind(z, rep(1, nobs))
  beta <- bg[1:(nx+1)]
  gamma <- bg[nx+1 + 1:(nx * (nz + 1))]
  gamma <- matrix(gamma, ncol=nx)
  gamma <- cbind(gamma, c(rep(0,nz), 1))
  xhat <- z %*% gamma
  uy <- y - xhat %*% beta
  ux <- x - xhat
  ux <- ux[ , 1:nx]                   #drop 0 resid for constant
  u <- cbind(uy, ux)
  S <- crossprod(u)
  llh <- -.5 * (nobs - nz) * determinant(S)$modulus
  if (flatrf) llh <- llh + sum(log(svd(gamma)$d))
  ## flatrf make the prior flat on the reduced form in the just-id'd case
  ## In any case, it downweights parts of parameter space with gamma~0, beta~infty
  return(-llh)                        #negative for csminwelNew
}