ivlh2 <- function(bg, yy, yx, yz, xx, xz, zz, nobs, flatrf=TRUE) {
  if (is.null(dim(xx))) nx <- 1 else nx <- dim(xx)[1]
  if (is.null(dim(zz))) nz <- 1 else nz <- dim(zz)[1]
  beta <- bg[1:nx]
  gamma <- bg[nx + 1:(nx * nz)]
  gamma <- matrix(gamma, ncol=nx)
  pi1 <- gamma %*% beta
  Syy <- yy - 2 * yz %*% pi1 + crossprod(pi1, zz) %*% pi1
  Syx <- yx - t(xz %*% pi1) - xz %*% pi1 + crossprod(pi1, zz) %*% gamma
  Sxx <- xx - xz %*% gamma - t(xz %*% gamma) + crossprod(gamma, zz) %*% gamma
  S <- matrix(0, nx + 1, nx + 1)
  S[1,1] <- Syy
  S[1, 1 + 1:nx] <- Syx
  S[1 + 1:nx, 1] <- t(Syx)
  S[1 + 1:nx, 1 + 1:nx] <- Sxx
  llh <- -.5 * (nobs - nz) * determinant(S)$modulus
  if (flatrf) llh <- llh + sum(log(svd(gamma)$d))
  return (-llh)                       #negative to use with a minimization routine
}