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

# Tables
## Table 22222
alpha = c(0.1, 0.05, 0.01)
n = c(10, 20, 30, 60)
n_ratio <- c(1, 1.5, 2.33)
delta <- c(1.0, 1.5)

table_expand_1 <- expand_grid("alpha" = alpha, 
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
                        sigma1 = table_expand_1$sigma1,
                        sigma2 = table_expand_1$sigma2,
                        n1= table_expand_1$n1, 
                        n2 = table_expand_1$n2, 
                        alpha = table_expand_1$alpha,
                        delta = table_expand_1$delta) %>%
  round(3)


table_1 <- cbind(table_expand_1, power_table_1) %>%
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

kable(table_1, booktabs = TRUE, linesep = "",
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


## Table 2
sigma_ratio <- c(1, 1.5, 2)
n_ratio <- c(1)
alpha = c(0.1, 0.05, 0.01)
n = c(10, 20, 30, 60)
delta <- c(1.0, 1.5)

table_expand_2 <- expand.grid( "sigma_ratio" = sigma_ratio, 
                               "alpha"= alpha, 
                               "n" = n,
                               "delta" = delta) %>%
  mutate(n1 = n/(n_ratio+1),
         n2 = n - n1,
         sigma1 = 1,
         sigma2 = sigma1*sigma_ratio)

power_table_2 <- mapply(jed_power,
                        n1 = table_expand_2$n1,
                        n2 = table_expand_2$n2,
                        sigma1 = table_expand_2$sigma1,
                        sigma2 = table_expand_2$sigma2,
                        alpha = table_expand_2$alpha,
                        delta = table_expand_2$delta) %>%
  round(3)


table_2 <- cbind(table_expand_2, power_table_2) %>%
  mutate(alpha_test = paste0("alpha = ", alpha)) %>%
  select(alpha_test, n, sigma_ratio, delta, power_table_2) %>%
  pivot_wider(names_from = c(alpha_test, delta), values_from = power_table_2) %>%
  arrange(n, sigma_ratio) %>%
  select_at(c("n", "sigma_ratio",
              "alpha = 0.1_1", "alpha = 0.05_1", "alpha = 0.01_1",
              "alpha = 0.1_1.5", "alpha = 0.05_1.5", "alpha = 0.01_1.5"))

kable(table_2, booktabs = TRUE, linesep = "",
      col.names = c("n", "sigma_ratio",
                    "alpha = 0.1", "alpha = 0.05", "alpha = 0.01",
                    "alpha = 0.1", "alpha = 0.05", "alpha = 0.01")) %>%
  kable_styling(font_size = 6, latex_options = "HOLD_position") %>%
  column_spec(3:8, width = "1.6cm")%>%
  add_header_above(c(" " = 2, "delta = 1" = 3,
                     "delta = 1.5" = 3)) %>%
  collapse_rows(1:2, row_group_label_position = 'stack', latex_hline = "none")
