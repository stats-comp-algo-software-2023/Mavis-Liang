#' @export hiper_glm
#' @export coef.hglm
#' @export vcov.hglm
#' @export print.hglm

hiper_glm <- function(design, outcome){
  warning("The function is yet to be implemented")
  ##TODO: 
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  hglm_out
}


coef.hglm <- function(obj){
  return("This is the coefs")
}


vcov.hglm <- function(obj){
  return("This is the matrix")
}

print.hglm <- function(obj){
  return("printing...")
}

