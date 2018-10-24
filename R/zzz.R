.onLoad <- function(libname, pkgname){
  t.test.default <<- stats:::t.test.default


  body(t.test.default)[[18]] <<- quote({rval <- list(statistic = tstat, parameter = df, p.value = pval,
                                                    conf.int = cint, estimate = estimate, null.value = mu, alternative = alternative,
                                                    method = method, data.name = dname);
  rval$n <- if(hasArg("y")){
    n = c(nx, ny)
  } else {
    nx
  };
  rval$v <- if(hasArg("y")){
    n = c(vx, vy)
  } else {
    vx
  }
  })
}

.onUnload <- function(libname, pkgname){
  t.test.default <- stats:::t.test.default
}
