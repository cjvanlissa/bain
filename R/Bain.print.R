#' @method print Bain
#' @export
print.Bain <- function(x,...){
  fun_type<-as.character(x$call)[1]
  # Inputs a bain result object
  if(fun_type=="Bain"){
    print(x$fit_com_table)
  }

  # Inputs a bain t_test result object
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
print.bain <- function(x, stats = c("Fit", "Com", "BF.u", "BF.c","PMPa", "PMPb"),
                       digits = 3,
                       na.print = "", ...){

# Modifications by Herbert
# if gocomplement is TRUE, fits has 12 columns and print.bain is modified
# such that also the column with PMPc is printed.
# if the hypotheses specified cover (almost) the complete parameter space
# an extra message is displayed.

  fits <- as.matrix(x$fit)

  if (dim(fits)[2] == 12){stats[7] <- "PMPc"}

  dat <- fits[, stats]
  miss_val <- is.na(dat)
  dat <- formatC(dat, digits = digits, format = "f")
  dat[miss_val] <- ""
  model_type <- class(x$model)[1]
  if(model_type == "lm"){
    model_type <- paste0(model_type, " (", attr(x, "which_model"), ")")
  }
  cat("Bayesian informative hypothesis testing for an object of class ", model_type, ":\n\n", sep = "")

  prmatrix(dat,
           quote = FALSE,
           na.print = na.print)


  if (dim(fits)[2] == 12){
  cat("\nHypotheses:\n ", paste(rownames(dat)[c(-nrow(dat),-(nrow(dat)-1))], ": ", x$hypotheses, sep = "", collapse = "\n  "))}
  if (dim(fits)[2] == 11){
    cat("\nHypotheses:\n ", paste(rownames(dat)[-nrow(dat)], ": ", x$hypotheses, sep = "", collapse = "\n  "))}

  cat("\n\nNote: BF.u denotes the Bayes factor of the hypothesis at hand versus the unconstrained hypothesis Hu. BF.c denotes the Bayes factor of the hypothesis at hand versus its complement. PMPa contains the posterior model probabilities of the hypotheses specified. PMPb adds Hu, the unconstrained hypothesis. PMPc adds Hc, the complement of the union of the hypotheses specified.")

  if (dim(fits)[2] == 12 & !is.na(fits[dim(fits)[1],6])   ){
  if(fits[dim(fits)[1],6] < .05){
  cat("\n\nNote: If the complexity of Hc is smaller than .05 the hypotheses specified (almost) completely cover the parameter space and therefore PMPa should be used.")}
  }

  if(!is.null(x[["warnings"]])){
    warning("Bain analysis returned the following warnings:\n  ", paste(1:length(x$warnings), ". ", x$warnings, sep = "", collapse = "\n  "))
  }
}



