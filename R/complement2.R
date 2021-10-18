# ==============================================================================
# THE OUTPUT FROM THIS FUNCTION CAN BE USED TO ADD ONE ROW AND ONE COLUMN TO THE
# MAIN OUTPUT TABLE FROM BAIN, e.g., - denotes numbers from the old output table
# and x is the output object from the PMPcomplement function:
#
#       Fit          Com         BF.u     BF.c  PMPa  PMPb    PMPc
#  H1    -            -           -         -    -      -    x$PMPc
#  H2    -            -           -         -    -      -    x$PMPc
#  Hu                                                   -
#  Hc   x$fitcomp  x$comcomp  x$BF[last]                     x$PMPc
# ==============================================================================
# When adding future modules to bain check
# 1. That no new variable names containing = are allowed in addition to =~
#    When additional names containing = are allowed, modify STEP 1.
# 2. That no new arguments are added to the call to bain. If new arguments
#   are added, modify the call to bain in STEP 5.
# ==============================================================================


PMPcomplement<-function(results){

  # determine the number of hypotheses
  Nhypo <- length(results$hypotheses)
  # collect the names of the estimates
  varnames <- names(results$estimates)

  # ========================================================================
  # STEP 1: CHECK FOR =~ AND TEMPORARILY EXCLUDE HYPO'S WITH =
  # ========================================================================

  # determine which hypotheses contain "=" (0 = yes-exclude, 1 = no-include)
  # after replacing =~ by ~ because =~ is used in lavaan parameter names
  equal <- vector(mode = "integer",length = Nhypo)
  equal[1:Nhypo] <- 0
  temphypo <- results$hypotheses
  temphypo <- gsub("=~", "~", temphypo)
  for (t in 1:Nhypo){
    if (!grepl("=", temphypo[t],  fixed = TRUE)) equal[t] <- 1
  }

  # STOP USING TEMPHYPO (BECAUSE TEMPHYPO IS ONLY USED TO LOOK FOR =)
  # AND RETURN TO USING THE HYPOTHESES WITH =~ ETC.

  hypo <- results$hypotheses

  # ========================================================================
  # STEP 2: EXCLUDE HYPOTHESES WITH INTERNALLY CONFLICTING
  # CONSTRAINTS - RECORD THIS ALSO IN EQUAL, that is, EQUAL = 0 DENOTES
  # HYPOTHESIS CONTAINING =, EQUAL = -1 DENOTES INCONSISTENT HYPOTHESES,
  # EQUAL = 1 DENOTES NO "="s AND CONSISTENT
  # ========================================================================

  for (t in 1:Nhypo){
    if (equal[t] == 1){
    check <- checkconsist(varnames,hypo[t])
    if (check == 1) equal[t] <- -1
    }
  }

  # =======================================================================
  # STEP 3: MAKE ALL 2ND, 3RD, ETC. ORDER HYPOTHESES OF THE FIRST ORDER
  # HYPOTHESES THAT SCORE 1 IN EQUAL
  # =======================================================================

  # determine the number of combined hypotheses containing
  # only inequality constraints AND create a vector Hcombi to store
  # these hypotheses
  Nineq <- sum(equal == 1)
  Ncombi <- 0

  if (Nineq > 1){
  for (t in 2:Nineq){
  Ncombi <- Ncombi + choose(Nineq,t)
  }
  }
  Hcombi <- vector(mode = "character",length = Ncombi)

  # combine the 1st order hypotheses with equal = 1 into Hcombi

  combitel <- 0

  if (Nineq > 1) {

    for (combi in 2:Nineq)  {   # number of elements in the combination

      # compute all possible combies

      combies <- combn(Nhypo,combi)

      # pick up all combi's and place in Hcombi
      for (tc in 1:dim(combies)[2]){
        hit <- 1
        for (tr in 1:combi){

          # check for each column if each element has equal = 1
          # if yes combine the corresponding hypotheses and place in Hcombi

          if (equal[combies[tr,tc]] != 1) hit <-0
        }
        if (hit == 1) {
          combitel <- combitel + 1
          Hcombi[combitel] <- paste(results$hypotheses[combies[1:combi,tc]], collapse="&")
        }
      }

    } # end of outer for loop
  } # end of if statement

  # ============================================================================
  # STEP 4: DETERMINE FOR EACH COMBINED HYPOTHESIS IF IT IS CONSISTENT
  # (PROCESS = 1) OR INCONSISTENT (PROCESS = 0)
  # ============================================================================

  if (combitel > 0){
  Process <- vector(mode = "integer",length = length(Hcombi))
  Process[1:length(Hcombi)] <-1
  for (t in 1:combitel){
      check <- checkconsist(varnames,Hcombi[t])
      if (check == 1) Process[t] <- 0
  }
  }

  # ============================================================================
  # STEP 5: THE fit AND complexity OF THE FIRST ORDER HYPOTHESES ARE CONTAINED
  # IN RESULTS. RUN A BAIN ANALYSIS TO DETERMINE THE fit AND complexity OF THE
  # CONSISTENT (PROCESS = 1) 2ND AND HIGHER ORDER COMBINATIONS. NOTE THAT, IF
  # PROCESS = 0, fit AND complexity WILL BE SET TO 0
  # ============================================================================

  # create vector containing all first and second etc. order combi BFs
  fit <- vector(mode = "numeric",length = (Nhypo + combitel))
  com <- vector(mode = "numeric",length = (Nhypo + combitel))
  # assign the first order fit and com
  fit[1:Nhypo] <- results$fit$Fit[1:Nhypo]
  com[1:Nhypo] <- results$fit$Com[1:Nhypo]

  # only execute if their is at least one combined hypothesis
  if (combitel > 0){

  # create a vector to contain the "not internally conflicting"
  # combined hypotheses
  Hcombisubset <- vector(mode = "character",length = sum(Process))

  Subtel <- 1
  for (Htel in 1:combitel){
  if (Process[Htel] == 0){fit[Nhypo+Htel] <- 0
  com[Nhypo+Htel] <- 0}
  if (Process[Htel] == 1){
  Hcombisubset[Subtel] <- Hcombi[Htel]
  Subtel <- Subtel + 1
  }
  }

  # only compute combi-fits-and-comps if there is at least one
  # internally consistent combination

  if (sum(Process) > 0) {

  subforbain <- paste0(Hcombisubset, collapse=";")

  callfrac <- results$call$fraction
  if (is.null(callfrac)){callfrac <- 1}
  callstd <- results$call$standardize
  if (is.null(callstd)){callstd <- FALSE}

  resultscombi <-bain(results$estimates,Sigma=results$Sigma,n=results$n, subforbain,
                 group_parameters = results$group_parameters,
                 joint_parameters = results$joint_parameters,
                 fraction = callfrac, standardize = callstd,gocomplement = FALSE)

  # store the subforbain fit and com
  Subtel <- 1
  for (Htel in 1:combitel){
    if (Process[Htel] == 1){
      fit[Nhypo + Htel] <- resultscombi$fit$Fit[Subtel]
      com[Nhypo + Htel] <- resultscombi$fit$Com[Subtel]
      Subtel <- Subtel + 1
    }
  }
  }

  # determine for each combi hypothesis if it is second, third, etc. order
  order <- vector(mode = "integer",length = (combitel))

  loctel <- 1
  for (ordertel in 2:sum(equal == 1)){
    numord <- choose(sum(equal == 1),ordertel)
    order[loctel:(loctel + numord -1)] <- ordertel
    loctel <- loctel + numord
  }

  } # END OF IF STATEMENT

  # ============================================================================
  # STEP 6: COMPUTE BFcu AND PMPc
  # ============================================================================

  # translate all the fits and coms into fit and com of the complement

  # compute the joint fit of all non = hypos
  jointfit <- 0
  for (tel in 1:Nhypo){

  if (equal[tel] == 1){jointfit <- jointfit + fit[tel]}
  }

  if (combitel > 0){

  for (tel in 1:combitel){
  # now minus 1x the second order
  # now plus 1x the third order
  # now minus 1x the fourth order
  # etc.
  if (order[tel] %% 2 == 0) {jointfit <- jointfit - fit[Nhypo + tel]}
  if (order[tel] %% 2 == 1) {jointfit <- jointfit + fit[Nhypo + tel]}
  }
  }

  # compute the joint com of all non = hypos
  jointcom <- 0
  for (tel in 1:Nhypo){

  if (equal[tel] == 1){jointcom <- jointcom + com[tel]}
  }

  if (combitel > 0){

  for (tel in 1:combitel){

  # now minus the even orders
  # then plus the odd orders
  # etc.
  if (order[tel] %% 2 == 0) {jointcom <- jointcom - com[Nhypo + tel]}
  if (order[tel] %% 2 == 1) {jointcom <- jointcom + com[Nhypo + tel]}
  }
  }

# ============================================================================
# STEP 7: CONSTRUCT THE AUGMENTED RESULTS TABLE
#         Only print Fit and Com if Com >= .05.
# ============================================================================

  BF <- vector(mode = "numeric", length = (Nhypo + 1))
  PMPc <- vector(mode = "numeric",length = (Nhypo + 1))
  results$fit["Hc",]<-NA
  results$fit[,"PMPc"]<-NA
  # remove gocomplement from the list
  results <- results[names(results)!="gocomplement"]

  complf <- 1 - jointfit
  if (complf < 0){complf <- 0}
  complc <- 1 - jointcom

  if (complc < .05) {
  BF[1:Nhypo] <- results$fit$BF.u[1:Nhypo]
  BF[Nhypo + 1] <- NA
  PMPc[1:(Nhypo + 1)] <- NA
  results$fit[,"PMPc"] <- c(PMPc[1:Nhypo], NA, PMPc[Nhypo+1])
  results$fit["Hc","BF.u"] <- BF[Nhypo + 1]
  results$fit["Hc","Fit"] <- complf
  results$fit["Hc","Com"] <- complc
  }

  # compute BFcu and PMPc if there is at least one combined hypothesis
  if (combitel > 0 & complc >= .05){
  BF[1:Nhypo] <- results$fit$BF.u[1:Nhypo]
  BF[Nhypo + 1] <- complf / complc
  PMPc <- BF/sum(BF)
  results$fit[,"PMPc"] <- c(PMPc[1:Nhypo], NA, PMPc[Nhypo+1])
  results$fit["Hc","BF.u"] <- BF[Nhypo + 1]
  results$fit["Hc","Fit"] <- complf
  results$fit["Hc","Com"] <- complc
  }

  # compute BFcu and PMPc if there are zero first order hypotheses with only ><
  if (combitel == 0 & Nineq == 0 & complc >= .05){
  BF[1:Nhypo] <- results$fit$BF.u[1:Nhypo]
  BF[Nhypo + 1] <- NA
  PMPc <- results$fit$PMPb
  complf <- NA
  complc <- NA
  results$fit[,"PMPc"] <- c(PMPc[1:Nhypo], NA, PMPc[Nhypo+1])
  results$fit["Hc","BF.u"] <- BF[Nhypo + 1]
  results$fit["Hc","Fit"] <- complf
  results$fit["Hc","Com"] <- complc
  }

  # compute BFcu and PMPc if there is one first order hypotheses with only ><
  if (combitel ==0 & Nineq == 1 & complc >= .05){
    BF[1:Nhypo] <- results$fit$BF.u[1:Nhypo]
    BF[Nhypo + 1] <- complf/complc
    PMPc <- BF/sum(BF)
    results$fit[,"PMPc"] <- c(PMPc[1:Nhypo], NA, PMPc[Nhypo+1])
    results$fit["Hc","BF.u"] <- BF[Nhypo + 1]
    results$fit["Hc","Fit"] <- complf
    results$fit["Hc","Com"] <- complc
  }

return(results)

} # END OF FUNCTION PMPcomplement

# =====================================================================
# BELOW FOLLOW FUNCTIONS USED BY THE MAIN FUNCTION
# 1) CHECKCONSIST - DETERMINES IF A HYPOTHESIS IS INTERNALLY CONSISTENT
# 2) MAKEFULRANK - MAKES A MATRIX FULL RANK
# 3) SPLIT - SPLIT REXCLC AND RINCLC INTO ABOUTS AND NOT-ABOUTS
#    REMOVE DUPLICATE ABOUTS
# =====================================================================

checkconsist <- function(varnames,hypo){
#  Rrres <- bain:::parse_hypothesis(varnames,hypo)

  Rrres <- parse_hypothesis(varnames,hypo)
  Rexclc <- Rrres$hyp_mat[[1]][,1:dim(Rrres$hyp_mat[[1]])[2]-1]
  Rinclc <- Rrres$hyp_mat[[1]][,1:dim(Rrres$hyp_mat[[1]])[2]]

  # ============================================================
  # determine if the hypothesis is of full rank excluding the constant
  # when a hypothesis is of full rank (without the constant):
  # - there are no "inconsistent" constraints (inconsistency leads to
  #   rank reduction)
  # - there are no "about equality" constraints
  # - there are no redundant constraints (redundancy leads to rank reduction)
  # Note that it is sufficient to look at the matrix without the constant
  # because only for hypotheses where the constant does not play a role
  # bain can determine the prior mean. E.g. a > b > c +2 & a > b < c+2 can
  # be processed, but not a > b > c +2 & a > b < c+5.

  # when a hypothesis is not of full rank (without the constant):
  # - there are redundant constraints, that is, all(lincoef > 0)
  # - it is inconsistent, that is, all(lincoef <= 0)
  # - it contains about equalities that may or may not be consistent

  # if of full rank, check <- 0 if inconsistent check <- 1

  # 1. Assume Rexclc is of full rank
  check <- 0

  # 2. Split Rexclc and Rinclc into abouts and others
    Rexabout <- split(Rexclc,Rinclc)$Rexabout
    Rexnone <- split(Rexclc,Rinclc)$Rexnone
    Rinabout <- split(Rexclc,Rinclc)$Rinabout
    Rinnone <- split(Rexclc,Rinclc)$Rinnone

  # 3. If Rexclc is not of rull rank, check if each about is consistent.
  #    Note that, if each about is consistent, all the abouts together are
  #    consistent. This is because for mutually inconsistent abouts the prior
  #    mean cannot be determined, e.g.,
  #    -1 < a-b < 1 & -1 < b-c < 1 & 4 < a - c < 8

  # Rinabout contains 0 or 2, 4 etc. rows, nut never 1. Therefore
  # nrow(Rinabout) will never render NULL

  if (qr(Rexclc)$rank < Rrres$n_constraints[2] & nrow(Rinabout) > 1){
    for (r1 in 1:(nrow(Rinabout)-1)){
      for (r2 in (r1+1):nrow(Rinabout)){
        tworow <- Rexabout[c(r1,r2),]
        if (qr(tworow)$rank == 1 &
            !identical(Rexabout[r1,],Rexabout[r2,])&
            (Rinabout[r1,dim(Rinabout)[2]] + Rinabout[r2,dim(Rinabout)[2]]) >= 0)
        {check <- 1}
        if (check == 1){break}
      }
      if (check == 1){break}
    }
  }

  # 4. If check=0 and Rexnone is not of full rank, check if the others
  #    are consistent.

  # Rexnone can contain 1 row therefore first check if nrow(Renone) == NULL
  # if not then only continue of it contains at least two rows (one row is
  # always consistent)

  if (!is.null(nrow(Rexnone))){
  if (nrow(Rexnone) > 1 & check == 0){

# First of all, if Rexnone is of full rank all is in good order.

  if (qr(Rexnone)$rank < nrow(Rexnone)){

# Secondly, makefullrank of allbutone (which may lead to a deletion of rows).

    # for loop over each row of the hypothesis
    for (t in 1:nrow(Rexnone)){

      allbutone <- Rexnone[(-1*t),]
      # if allbutone not of full rank, remove rows until it is
      # if allbutone consists of one row, do not call makefullrank()
      if (!is.null(dim(allbutone)[1])) {allbutone <- makefullrank(allbutone)}
      one <- Rexnone[t,]

# Thirdly, if the (possibly reduced) allbutone + one is of full rank,
# all is in good order. Otherwise, find the dependency and check if all
# coefficients are smaller or equal to 0 (inconsistent, check = 1), or
# not (redundant, check =0).

      allbutoneplusone <- rbind(allbutone,one)

      if (qr(allbutoneplusone)$rank < nrow(allbutoneplusone)){
      lincoef <- 0
      if (is.null(dim(allbutone)[1])) {lincoef <- qr.solve(allbutone,one)} else {lincoef <- qr.solve(t(allbutone),one)}
      lincoef <- round(lincoef,2)

      if (all(lincoef <= 0)) {check <- 1
      break}
      }
    } # END OF FOR LOOP
  } # END OF IF STATEMENT

  }
  }

  #
  #   # 5. If check=0 and Rexcl is not of full rank, check if the full rank
  #   # version of the abouts can be used to predict the full rank version of the
  #   # others and vice versa.
  #
  #   if (qr(Rexclc)$rank < Rrres$n_constraints[2] & check == 0){
  #
  # # ==========================================================================
  #   # IT APPEARS STEP 5. IS NOT NECESSARY BECAUSE ABOUTS AND OTHERS CANNOT
  #   # BE MUTUALLY INCONSISTENT IF BAIN CAN COMPUTE THE PRIOR MEAN!!
  #   # TO DO TO DO MAKE CHECKS FOR THIS SITUATION
  # # ==========================================================================
  #
  #   } # END OF IF STATEMENT

  return(check)
} # END OF FUNCTION CHECKCONSIST


# =================================================================
makefullrank <- function(consmat){
  # remove linearly dependent rows from consmat in order to
  # make it of full rank - procedure: remove rows and check if
  # rank remains the same

  s <- 1
  repeat{

    removeone <- consmat[(-1*s),]

    if (qr(consmat)$rank == qr(removeone)$rank){
      consmat <- removeone
    } else {s <- s+1}

    if (is.null(dim(consmat)[1])){break}

    if(dim(consmat)[1] == qr(consmat)$rank) {
      break
    }
  }

  return(consmat)

} # end function makefullrank

# =================================================================
split <- function(Rexclc,Rinclc){

  # this functions splits Rexclc and Rinclc in parts only containing
  # about equality constraints and not containing about equality constraints
  # note that, "a > b > c & c > b > a" and "A>B+2 & A<B+2" are inconsistent
  # about equality constraints, all duplicate constraints are removed from
  # the resulting about equality matrices

  if (is.null(dim(Rexclc)[1])){
    Ncol <- length(Rexclc)
    Nrows <- 1
    } else {
    Ncol <- dim(Rexclc)[2]
    Nrows <- nrow(Rexclc)
    }

#  define a vector containing the row numbers for the about equalities
#  define a vector containing the row numbers for other constraints

  abrows <- vector(mode = "integer",length = Nrows)
  abrows[1:Nrows] <- 0
  rowcount <- 0

  if (Nrows > 1){

  for (r in 1:(Nrows-1)){
    for (t in (r+1):Nrows){

      if (!(r %in% abrows) & !(t %in% abrows)){

         # determine the possible multiplicative constant between both rows
         lincoef <- 1
         for (h in 1: Ncol){
         if (Rexclc[r,h] != 0 & Rexclc[t,h] != 0){
         lincoef <- abs(Rexclc[r,h]/Rexclc[t,h])
         }
         }

         Rtemp <- lincoef * Rexclc[r,]
         if (identical(Rtemp,-1*Rexclc[t,]) == TRUE){
         abrows[rowcount+1] <- r
         abrows[rowcount+2] <- t
         rowcount <- rowcount + 2
         }

         } # END IF STATEMENT

    } # END FOR STATEMENT

  } # END FOR STATEMENT
  }

if (sum(abrows) == 0){Rexabout <- t(Rexclc)[0,]
  Rexnone <- Rexclc} else
{ Rexabout <- Rexclc[abrows,]
  Rexnone <- Rexclc[-1*abrows,]}

if (sum(abrows) == 0){Rinabout <- t(Rinclc)[0,]
  Rinnone <- Rinclc} else
{ Rinabout <- Rinclc[abrows,]
  Rinnone <- Rinclc[-1*abrows,]}

  # remove possible duplicates of about equalities from Rexabout and Rinabout
  # important to note that -1 < a < 1 & 2 < a < 3 will not be processed by
  # bain because the prior mean cannot be determined, therefore this situation
  # does not have to be accounted for in the code

  if (sum(abrows)>0){

  Ncol <- dim(Rinabout)[2]
  Nrows <- nrow(Rinabout)

  abrowsub <- vector(mode = "integer",length = Nrows)
  abrowsub <- c(1:Nrows)

  if (Nrows > 2){

    rowcount <- 0

    for (r in 1:(Nrows-1)){
      for (t in (r+1):Nrows){

# determine the possible multiplicative constant between both rows
        lincoef <- 1
        for (h in 1:(Ncol-1)){
          if (Rinabout[r,h] != 0 & Rinabout[t,h] != 0){
            lincoef <- abs(Rinabout[r,h]/Rinabout[t,h])
          }
        }

        Rintemp <- lincoef * Rinabout[r,]

        if (identical(Rintemp,Rinabout[t,]) == TRUE){
        abrowsub <- replace(abrowsub, abrowsub==t, 0)
        }
        # END IF STATEMENT

    } # END FOR STATEMENT

    } # END FOR STATEMENT

    Rexabout <- Rexabout[abrowsub,]
    Rinabout <- Rinabout[abrowsub,]
  }
  }

  splitres <- list("Rexabout" = Rexabout, "Rexnone" = Rexnone,
                   "Rinabout" = Rinabout, "Rinnone" = Rinnone)

  return(splitres)

}


