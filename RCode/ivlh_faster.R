#' ivlh2
#'
#' LIML likelihood based on sample second moments
#'
#' Model is y = c(x, w) %*% beta + eps, x = cbind(w,z) %*% gamma + nu
#'
#' @param bg Parameters of the model: coefficients on x and w in the y
#' equation, followed by nx columns of coefficients on w and z in the x
#' equations.
#' @param Vyxwz Second moment matrix of the data, with variables in the order
#' of the parameter name.
#' @param nobs Number of observations
#' @param nx Number of right-hand-side endogenous variables
#' @param nw Number of right-hand side exogenous variables
#' @param nz Number of exogenous variables not appearing in the y equation
#' 
ivlh_faster <- function(bg, Vyxwz, nobs, nx, nw, nz, flatrf=TRUE) {
  Vscale <- sqrt(diag(Vyxwz))
  beta <- bg[1:(nx + nw)]
  gamma <- bg[nx + nw + 1:(nx * (nw + nz))]
  gamma <- matrix(gamma, ncol=nx)
  pi1 <- c(1, - rep(0, nx), - (gamma %*% beta[1:nx] + c(beta[nx + 1:nw], rep(0, nz))))
  pi2 <- rbind(rep(0, nx), diag(nx), -gamma)
  pi12 <- cbind(pi1, pi2)
  S <- crossprod(pi12, Vyxwz) %*% pi12
  llh <- -(nobs - nz) * sum(log(diag(chol(S))))
  if (flatrf) llh <- llh + sum(log(svd(gamma)$d))
  return (-llh)                       #negative to use with a minimization routine
}
#' @export