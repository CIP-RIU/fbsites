
#' Convertion to sexagesimal to decimal
#'
#' Returns the convertion of sexagesimal system to decimal system.
#' @author Omar Benites, Franklin Plasencia
#' @param grade grades
#' @param minute minutes
#' @param second seconds
#' @export
#'
seg2dec <- function(grade = 0, minute = 0, second = 0){

  grade <- grade
  minute <- minute/60
  second <- second/3600

  total <- grade + minute + second
  total <- round(total,5) #need 5 decimal values.
  return(total)
}

