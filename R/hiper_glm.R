#' @export hiper_glm
#' @export coef.hglm
#' @export vcov.hglm
#' @export print.hglm

hiper_glm <- function(design, outcome, model='linear'){
  supported_models <- c('linear')
  if(!(model %in%  supported_models)){stop(sprintf("The model %s is not supported.", model))
}
  
  warning("The function is yet to be implemented")
  ## TODO: 
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  hglm_out
}


coef.hglm <- function(hglm_out){
  return("This is the coefs")
}


vcov.hglm <- function(hglm_out){
  return("This is the matrix")
}

print.hglm <- function(hglm_out){
  return("printing...")
}

