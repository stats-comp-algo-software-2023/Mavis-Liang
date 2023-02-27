
####################### Helper functions ######################

########## For cholesky ########
## Find linear regression coefficients estimates via MLE,
## with cholesky decomposition while solving matrices
chol_finder <- function(design, outcome){
  ## With least-square, we have,
  ## beta_est = (X^T X) X^T y.
  ## Which is solving beta_est in (X^T X) beta_est = X^T y.
  ## This is equivalent to solving Ax = b,
  ## where, A and b are the following

  A <- t(design) %*% design
  b <- t(design) %*% outcome

  ## solve "Ax = b" question via cholesky decomp
  L <- chol(A)
  beta_est <- backsolve(t(L), forwardsolve(L, t(A) %*% b))
  return(c(beta_est))
}


############ FOR BFGS ############
## The log-likelihood function
gaussian_logLi <- function(beta, X, Y, noise_var = 1){
  sigma <- sqrt(noise_var)
  Sigma_inv <- (1/sigma) * diag(nrow = length(Y))

  f <- -0.5 * t(Y - X %*% beta) %*% Sigma_inv %*% (Y - X %*% beta)
  return(f)
}
## The gradient
gradient <- function(beta, X = design, Y = outcome){
  quad_grad <- function(x, Sigma_inv) {
    return(-Sigma_inv %*% x)
  }

  gr = - t(X) %*% quad_grad(x = Y - X %*% beta,
                                Sigma_inv = diag(nrow = length(Y)))
  return(gr)
}

##
BFGS_finder <- function(design, outcome){
  beta_init <- c(0,0,0,0)

  BFGS <- stats::optim(beta_init,
                       function(beta) gaussian_logLi(beta = beta,
                                                     X = design,
                                                    Y = outcome),

                       method = "BFGS")
  return(BFGS$par)
}


##################### Exported functions ####################
#' @export hiper_glm
hiper_glm <- function(design, outcome, model='linear', method = 'chol'){

  ## Check validity
  supported_models <- c('linear')
  if(!(model %in%  supported_models)){stop(sprintf("The model %s is not supported.", model))
  }
  supported_methods <- c('chol', 'BFGS')
  if(!(method %in%  supported_methods)){stop(sprintf("The method %s is not supported.", method))
  }

  warning("The function is continuing to be implemented")

  ## MLE
  if(method=="chol"){
    beta_est <- chol_finder(design = design, outcome = outcome)
  }
  if(method=="BFGS"){
    beta_est <- BFGS_finder(design = design, outcome = outcome)
  }

  ## Output
  hglm_out <- list(coef = beta_est)
  class(hglm_out) <- "hglm"
  hglm_out
}


#' @export coef.hglm
coef.hglm <- function(hglm_out){
  return(hglm_out$coef)
}

#' @export vcov.hglm
vcov.hglm <- function(hglm_out){
  return(diag(length(hglm_out$coef)))
}

#' @export print.hglm
print.hglm <- function(hglm_out){
  return("printing...")
}


coef(hiper_glm(design = design, outcome = outcome, method = "chol"))
coef(hiper_glm(design = design, outcome = outcome, method = "BFGS"))
