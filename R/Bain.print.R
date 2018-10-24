#' @method print Bain
#' @export
print.Bain <- function(x,...){
  fun_type<-as.character(x$call)[1]
  # Inputs a bain result object
  if(fun_type=="Bain"){
    print(x$fit_com_table)
  }

  # Inputs a bain t-test result object
  if(fun_type=="Bain_ttest"||fun_type=="Bain_ttestData"){
    output<-do.call(cbind,x[-length(x)])
    print(data.frame(output,row.names = ""))
  }

  # Inputs a bain anova or ancova test result object
  if(fun_type=="Bain_anova"||fun_type=="Bain_ancova"||fun_type=="Bain_regression"
     ||fun_type=="Bain_anova_cm"||fun_type=="Bain_ancova_cm"||fun_type=="Bain_regression_cm"){
    output<-data.frame(do.call(cbind,x[-c(length(x)-1,length(x))]))
    names(output)<-c("f","c","BF.c","PMPa","PMPb")
    rownames(output)<-paste("H",1:nrow(output),sep = "")
    print(output)
  }

}

#' @method print bain
#' @export
print.bain <- function(x, ...){
  if (FALSE) {
    #print result
    writeLines("Choice of b")
    writeLines(paste("J", rank_hyp))
    writeLines(c("N", n), sep = " ")
    writeLines("", sep = "\n")
    writeLines(c("b", formatC(
      b, digits = 3, format = "f"
    )), sep = " ")
    writeLines(" ")
    cat("Estimates and covariance matrix of parameters",
        "Estimates",
        sep = "\n")
    writeLines(paste((
      formatC(estimate, digits = 3, format = "f")
    )), sep = " ")
    writeLines(" ")
    cat("Posterior Covariance Matrix", "\n")
    write.table(
      data.frame(formatC(
        thetacovpost, digits = 3, format = "f"
      )),
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
    )
    cat("Prior Covariance Matrix", "\n")
    write.table(
      data.frame(formatC(
        thetacovprior, digits = 3, format = "f"
      )),
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
    )
    writeLines(" ")
    cat("Hypothesis testing result", sep = "\n")
    write.table(
      capture.output(fctable),
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
    )
    writeLines(" ")
    cat("BF-matrix", sep = "\n")
    write.table(
      capture.output(data.frame(
        formatC(BFmatrix, digits = 3, format = "f")
      )),
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
    )
    writeLines(" ")

  }
}
