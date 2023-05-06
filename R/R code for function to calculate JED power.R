jed_power <- function(sigma1 = 1, 
                      sigma2 = 1,
                      delta = 1,
                      alpha,
                      n1, 
                      n2){
  
  sigma_d = sqrt((sigma1^2/n1)+ (sigma2^2/n2))
  
  z_1_alpha = qnorm(alpha, lower.tail=FALSE)
  
  W_power = pnorm(delta/sigma_d - z_1_alpha, lower.tail = TRUE)
  
  return(W_power)
}
