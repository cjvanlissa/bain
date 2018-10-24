.onLoad <- function(libname, pkgname){
  t.test.default <<- stats:::t.test.default


  body(t.test.default)[[18]] <<- quote({rval <- list(statistic = tstat, parameter = df, p.value = pval,
                                                    conf.int = cint, estimate = estimate, null.value = mu, alternative = alternative,
                                                    method = method, data.name = dname);
  if(hasArg("y")&!paired){
    rval$n <- c(nx, ny)
    rval$v <- c(vx, vy)
  } else {
    rval$n <- nx
    rval$v <- vx
  }
  })
  body(t.test.default)[[19]] <<- quote({class(rval) <- c("bain_htest", "htest")})
}

.onUnload <- function(libname, pkgname){
  t.test.default <- stats:::t.test.default
}
