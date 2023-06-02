
library(dplyr)
library(forestplot)

ppm_res <- read.csv("/Users/brenna/Documents/School/Research/Air measurement/Env Health Persp/ppm results.csv")
names(ppm_res) <- c("variable", "mean", "se", "lower", "upper", "ztest", "zval")
ppm_res$mean <- round(ppm_res$mean, 2)

ppm_res %>%
  forestplot(labeltext = c(variable, mean, ztest),
             #clip = c(0.7, 2.25),
             boxsize = 0.25,
             line.margin = 0.2,
             xlog = FALSE,
             vertices = TRUE,
             xlab = "Odds Ratio, 95% CI") %>%
  # fp_set_style(box = "#f21905",
  #              line = "black") %>%
  # fp_add_header(variable = "Variable",
  #               ztest = "p-value",
  #               estimate = "estiamte") %>%
  #fp_append_row(variable = "intercept", # intercept
  #              mean = 0.98,
  #              lower = 0.95,
  #              upper = 1.02,
  #              p_val = "0.346",
  #              significance = ".",
  #              is.summary = TRUE) %>% 
  fp_set_zebra_style("#f7f7f7")
