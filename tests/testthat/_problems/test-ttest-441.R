# Extracted from test-ttest.R:441

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "bain", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
t_test_old <- function(x, ...) UseMethod("t_test_old")
t_test_old.default <-
  function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
           mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
           ...)
  {

    alternative <- match.arg(alternative)

    if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
      stop("'mu' must be a single number")
    if(!missing(conf.level) &&
       (length(conf.level) != 1 || !is.finite(conf.level) ||
        conf.level < 0 || conf.level > 1))
      stop("'conf.level' must be a single number between 0 and 1")
    if( !is.null(y) ) {
      dname <- paste(deparse(substitute(x)),"and",
                     deparse(substitute(y)))
      if(paired)
        xok <- yok <- complete.cases(x,y)
      else {
        yok <- !is.na(y)
        xok <- !is.na(x)
      }
      y <- y[yok]
    }
    else {
      dname <- deparse(substitute(x))
      if (paired) stop("'y' is missing for paired test")
      xok <- !is.na(x)
      yok <- NULL
    }
    x <- x[xok]
    if (paired) {
      x <- x-y
      y <- NULL
    }
    nx <- length(x)
    mx <- mean(x)
    vx <- var(x)
    if(is.null(y)) {
      if(nx < 2) stop("not enough 'x' observations")
      df <- nx-1
      stderr <- sqrt(vx/nx)
      if(stderr < 10 *.Machine$double.eps * abs(mx))
        stop("data are essentially constant")
      tstat <- (mx-mu)/stderr
      method <- if(paired) "Paired t_test" else "One Sample t_test"
      estimate <-
        setNames(mx, if(paired)"mean of the differences" else "mean of x")
    } else {
      ny <- length(y)
      if(nx < 1 || (!var.equal && nx < 2))
        stop("not enough 'x' observations")
      if(ny < 1 || (!var.equal && ny < 2))
        stop("not enough 'y' observations")
      if(var.equal && nx+ny < 3) stop("not enough observations")
      my <- mean(y)
      vy <- var(y)
      method <- paste(if(!var.equal)"Welch", "Two Sample t_test")
      estimate <- c(mx,my)
      names(estimate) <- c("mean of x","mean of y")
      if(var.equal) {
        df <- nx+ny-2
        v <- 0
        if(nx > 1) v <- v + (nx-1)*vx
        if(ny > 1) v <- v + (ny-1)*vy
        v <- v/df
        stderr <- sqrt(v*(1/nx+1/ny))
      } else {
        stderrx <- sqrt(vx/nx)
        stderry <- sqrt(vy/ny)
        stderr <- sqrt(stderrx^2 + stderry^2)
        df <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
      }
      if(stderr < 10 *.Machine$double.eps * max(abs(mx), abs(my)))
        stop("data are essentially constant")
      tstat <- (mx - my - mu)/stderr
    }
    if (alternative == "less") {
      pval <- pt(tstat, df)
      cint <- c(-Inf, tstat + qt(conf.level, df) )
    }
    else if (alternative == "greater") {
      pval <- pt(tstat, df, lower.tail = FALSE)
      cint <- c(tstat - qt(conf.level, df), Inf)
    }
    else {
      pval <- 2 * pt(-abs(tstat), df)
      alpha <- 1 - conf.level
      cint <- qt(1 - alpha/2, df)
      cint <- tstat + c(-cint, cint)
    }
    cint <- mu + cint * stderr
    names(tstat) <- "t"
    names(df) <- "df"
    names(mu) <- if(paired || !is.null(y)) "difference in means" else "mean"
    attr(cint,"conf.level") <- conf.level
    # The following lines have been changed by Caspar van Lissa,
    # package maintainer of bain
    # Original:
    # rval <- list(statistic = tstat, parameter = df, p.value = pval,
    #              conf.int = cint, estimate = estimate, null.value = mu,
    #              alternative = alternative,
    #              method = method, data.name = dname)
    # class(rval) <- "htest"
    # Substituted by:
    rval <- list(statistic = tstat, parameter = df, p.value = pval,
                 conf.int = cint, estimate = estimate, null.value = mu, alternative = alternative,
                 method = method, data.name = dname);
    if(!is.null(y)&!paired){
      rval$n <- c(nx, ny)
      rval$v <- c(vx, vy)
    } else {
      rval$n <- nx
      rval$v <- vx
    }
    class(rval) <- c("t_test", "htest")
    return(rval)
  }
t_test_old.formula <- function(formula, data, subset, na.action, ...)
{
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- setNames(split(mf[[response]], g), c("x", "y"))
  y <- do.call(t_test_old.default, c(DATA, list(...)))
  y$data.name <- DNAME
  if (length(y$estimate) == 2L)
    names(y$estimate) <- paste0("mean of group", levels(g))
  y
}
data(sesamesim)
x<-sesamesim$postnumb
ttest <- t_test(x)
set.seed(100)
z <- bain(ttest, "x=30; x>30; x<30")
cov1<-list(matrix(c(sd(x)^2/length(x)),1,1))
estimate<-mean(x)
names(estimate)<-c("m1")
set.seed(100)
zd <-bain(estimate,"m1=30;m1>30;m1<30",n=length(x),Sigma=cov1,group_parameters=1,joint_parameters = 0)
x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y, var.equal = FALSE)
set.seed(100)
z <- bain(ttest, "x=y; x>y; x<y")
cov1<-list(matrix(c(sd(x)^2/length(x)),1,1),matrix(c(sd(y)^2/length(y)),1,1))
estimate<-c(mean(x),mean(y))
samp <- c(length(x),length(y))
names(estimate)<-c("m1","m2")
set.seed(100)
zd <-bain(estimate,"m1=m2; m1>m2; m1<m2",n=samp,Sigma=cov1,group_parameters=1,joint_parameters = 0)
x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y, var.equal = TRUE)
set.seed(100)
z <- bain(ttest, "x=y; x>y; x<y")
pooled <- ((length(x)-1)*sd(x)^2+(length(y)-1)*sd(y)^2)/(length(x)-1+length(y)-1)
cov1<-list(matrix(c(pooled),1,1)/length(x),matrix(c(pooled),1,1)/length(y))
estimate<-c(mean(x),mean(y))
samp <- c(length(x),length(y))
names(estimate)<-c("m1","m2")
set.seed(100)
zd <-bain(estimate,"m1=m2; m1>m2; m1<m2",n=samp,Sigma=cov1,group_parameters=1,joint_parameters = 0)
sesamesim$sex<-as.factor(sesamesim$sex)
ttest <- t_test(postnumb~sex,data=sesamesim, var.equal = TRUE)
set.seed(100)
zh<-bain(ttest, "group1=group2; group1>group2; group1<group2")
x<-sesamesim$prenumb
y<-sesamesim$postnumb
ttest <- t_test(x,y,paired = TRUE)
set.seed(100)
z <- bain(ttest, "difference=0; difference>0; difference<0")
d <- x - y
cov1<-list(matrix(c(sd(d)^2/length(d)),1,1))
estimate<-mean(d)
names(estimate)<-c("dd")
set.seed(100)
zd <-bain(estimate,"dd=0;dd>0;dd<0",n=length(d),Sigma=cov1,group_parameters=1,joint_parameters = 0)
x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y, var.equal = TRUE)
set.seed(100)
z <- bain(ttest, "x - y > -1 & x - y < 1")
pooled <- ((length(x)-1)*sd(x)^2+(length(y)-1)*sd(y)^2)/(length(x)-1+length(y)-1)
cov1<-list(matrix(c(pooled),1,1)/length(x),matrix(c(pooled),1,1)/length(y))
estimate<-c(mean(x),mean(y))
samp <- c(length(x),length(y))
names(estimate)<-c("m1","m2")
set.seed(100)
zd <-bain(estimate,"m1 - m2 > -1 & m1 - m2 < 1",n=samp,Sigma=cov1,group_parameters=1,joint_parameters = 0)

# test -------------------------------------------------------------------------
expect_equal(zd$fit$BF,z$fit$BF)
