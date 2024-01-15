jed_pvalues <- function(d = 1.86,
                        delta_sd_ratio,
                        d_delta_ratio,
                        test_type){
  
  delta = (1/d_delta_ratio)*d
  
  sd = (1/delta_sd_ratio)*delta
  
  p_value_sup = pnorm((d - delta)/sd, lower.tail = TRUE)
  
  p_value_equ = 2*pnorm(- d / sd, lower.tail = TRUE)
  
  p_value_inf = pnorm(-(d + delta)/sd, lower.tail = TRUE)
  
  if (test_type == "sup") {
    return(p_value_sup)}
  
  else if (test_type == "equ") {
    return(p_value_equ)}
  
  else if (test_type == "inf") {
    return(p_value_inf)}
  
  else if (!test_type %in% c("sup", "equ", "inf")) {
    return("wrong type")}
}