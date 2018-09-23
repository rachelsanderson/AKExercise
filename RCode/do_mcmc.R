do_mcmc <- function(y, x, w, z, nx, nw, nz, its){
        data = cbind(y, x, w, z)
        Vyxwz = t(data)%*%data

        nobs = dim(data)[1]

        bg0 = rep(1, nx+nw +nx*(nw+nz))

        ll <- function(x){
          return(ivlh_faster(x, Vyxwz, nobs, nx, nw, nz, flatrf=TRUE))
        }
        
        #Get initial jump distribution from ML of likelihood fnc.
        opt <- csminwelNew(ll, bg0, H0=diag(nx+nw +nx*(nw+nz)), nit=10000)
        H0 <- opt$H

        #do 100k reps to get a better jump
        draws = ivmcmc2(opt$xh, Vyxwz, nobs, nx, nw, nz, flatrf=TRUE, H0, nit=100000)
        newHess = cov(draws[,1:length(bg0)])

        newDraws = ivmcmc2(opt$xh, Vyxwz, nobs, nx, nw, nz, flatrf=TRUE, newHess, nit=its)
        return(newDraws)
}
