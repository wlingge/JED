library(tidyverse)
library(knitr)
library(kableExtra)
library(gridExtra)

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