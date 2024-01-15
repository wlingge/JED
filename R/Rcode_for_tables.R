library(tidyverse)
library(knitr)
library(kableExtra)
library(gridExtra)

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


# Tables
## Table 1
delta_sd_ratio = c(1,2,3,4)
d_delta_ratio = c(0.1,0.2,0.4,1,1.5,2,3,4)
test_type = c("sup","equ","inf")

table_expand_1 <- expand_grid("delta_sd_ratio" = delta_sd_ratio, 
                              "d_delta_ratio"= d_delta_ratio, 
                              "test_type" = test_type) 

pval_table_1 <- mapply(jed_pvalues,
                       delta_sd_ratio = table_expand_1$delta_sd_ratio,
                       d_delta_ratio = table_expand_1$d_delta_ratio,
                       test_type= table_expand_1$test_type) %>%
  round(3)

table_1 <- cbind(table_expand_1, pval_table_1)%>%
  mutate(d_delta_ratio = paste(d_delta_ratio, "delta")) %>%
  pivot_wider(names_from = d_delta_ratio,values_from = pval_table_1) %>%
  mutate_at(c("0.1 delta", "0.2 delta", "0.4 delta", "1 delta", "1.5 delta", "2 delta", "3 delta", "4 delta"), 
            ~case_when(. > 0.999 ~ ">0.999",
                       . < 0.001 ~ "<0.001",
                       TRUE ~ as.character(.))) %>%
  mutate(delta_sd_ratio = paste0(delta_sd_ratio, " std err"),
         test_type = case_when(test_type == "sup" ~ "p(+)",
                               test_type == "equ" ~ "p(0)",
                               test_type == "inf" ~ "p(-)"))

kable(table_1, booktabs = TRUE, linesep = "", caption = "p_value table",
      col.names = c("delta", "p_values", colnames(table_1)[-c(1,2)])) %>%
  kableExtra::collapse_rows(1, latex_hline = "major") %>%
  kableExtra::add_header_above(., c(" " = 2, "d" = 8))


## Table 2
alpha = c(0.1, 0.05, 0.01)
n = c(10, 20, 30, 60)
n_ratio <- c(1, 1.5, 2.33)
delta <- c(1.0, 1.5)

table_expand_2 <- expand_grid("alpha" = alpha, 
                              "n"= n, 
                              "n_ratio" = n_ratio,
                              "delta" = delta) %>%
  mutate(n1 = round(n/(n_ratio+1)),
         n2 = n - n1)%>%
  mutate(sigma1 = 1,
         sigma2 = 1) %>%
  mutate(n_ratio = case_when(n %in% c(30,60) & n_ratio == 2.33 ~ 2,
                             TRUE ~ n_ratio))

power_table_1 <- mapply(jed_power,
                        sigma1 = table_expand_2$sigma1,
                        sigma2 = table_expand_2$sigma2,
                        n1= table_expand_2$n1, 
                        n2 = table_expand_2$n2, 
                        alpha = table_expand_2$alpha,
                        delta = table_expand_2$delta) %>%
  round(3)


table_2 <- cbind(table_expand_2, power_table_1) %>%
  mutate(alpha_test = paste0("alpha = ", alpha),
         delta = paste0("delta = ", delta)) %>%
  select(alpha_test, n, n_ratio, delta, power_table_1) %>%
  pivot_wider(names_from = c(alpha_test, delta), values_from = power_table_1) %>%
  arrange(n, n_ratio) %>%
  select_at(c("n", "n_ratio",
              "alpha = 0.1_delta = 1", "alpha = 0.05_delta = 1",
              "alpha = 0.01_delta = 1",
              "alpha = 0.1_delta = 1.5", "alpha = 0.05_delta = 1.5",
              "alpha = 0.01_delta = 1.5"))

kable(table_2, booktabs = TRUE, linesep = "",
      col.names = c("n", "n_ratio",
                    "alpha = 0.1", "alpha = 0.05",
                    "alpha = 0.01",
                    "alpha = 0.1", "alpha = 0.05",
                    "alpha = 0.01")) %>%
  kable_styling(font_size = 6, latex_options = "HOLD_position") %>%
  column_spec(3:8, width = "1.6cm") %>%
  add_header_above(c(" " = 2, "delta = 1.0" = 3,
                     "delta = 1.5" = 3)) %>%
  collapse_rows(1:2, row_group_label_position = 'stack', latex_hline = "none")


## Table 3
sigma_ratio <- c(1, 1.5, 2)
n_ratio <- c(1)
alpha = c(0.1, 0.05, 0.01)
n = c(10, 20, 30, 60)
delta <- c(1.0, 1.5)

table_expand_3 <- expand.grid( "sigma_ratio" = sigma_ratio, 
                               "alpha"= alpha, 
                               "n" = n,
                               "delta" = delta) %>%
  mutate(n1 = n/(n_ratio+1),
         n2 = n - n1,
         sigma1 = 1,
         sigma2 = sigma1*sigma_ratio)

power_table_2 <- mapply(jed_power,
                        n1 = table_expand_3$n1,
                        n2 = table_expand_3$n2,
                        sigma1 = table_expand_3$sigma1,
                        sigma2 = table_expand_3$sigma2,
                        alpha = table_expand_3$alpha,
                        delta = table_expand_3$delta) %>%
  round(3)


table_3 <- cbind(table_expand_3, power_table_2) %>%
  mutate(alpha_test = paste0("alpha = ", alpha)) %>%
  select(alpha_test, n, sigma_ratio, delta, power_table_2) %>%
  pivot_wider(names_from = c(alpha_test, delta), values_from = power_table_2) %>%
  arrange(n, sigma_ratio) %>%
  select_at(c("n", "sigma_ratio",
              "alpha = 0.1_1", "alpha = 0.05_1", "alpha = 0.01_1",
              "alpha = 0.1_1.5", "alpha = 0.05_1.5", "alpha = 0.01_1.5"))

kable(table_3, booktabs = TRUE, linesep = "",
      col.names = c("n", "sigma_ratio",
                    "alpha = 0.1", "alpha = 0.05", "alpha = 0.01",
                    "alpha = 0.1", "alpha = 0.05", "alpha = 0.01")) %>%
  kable_styling(font_size = 6, latex_options = "HOLD_position") %>%
  column_spec(3:8, width = "1.6cm")%>%
  add_header_above(c(" " = 2, "delta = 1" = 3,
                     "delta = 1.5" = 3)) %>%
  collapse_rows(1:2, row_group_label_position = 'stack', latex_hline = "none")
