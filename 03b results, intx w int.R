


#res_split <- split(res, list(nchar(res$row_names) < 12,  nchar(res$row_names) > 5))

# exponentiate only
#res_split$TRUE.FALSE <- res_split$TRUE.FALSE %>% 
#  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
#                function(x) exp(x)))
#names(res_exp) <- paste(names(res_exp), "exp", sep = "_")
res




res_hi_order <- res


## independent effects
#  exp(B1)
intercept <- res[which(res$variable == "(Intercept)"), ]
res
#adding intercept vals

res_ind <- res[which(nchar(res$variable) <= 5), ]
res_ind <- res_ind  %>%
  mutate(estimate = estimate + intercept[1, 2]) %>%
  mutate(s.e. =  s.e. + intercept[1, 3]) %>%
  mutate(ci95.lo =  ci95.lo + intercept[1, 4]) %>%
  mutate(ci95.hi =  ci95.hi + intercept[1, 5])

res_ind <- res_ind %>%
  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
                function(x) round(exp(x), 10)))
res_ind




## first-order interactions
res_1_order <- res[which(nchar(res$variable) < 12), ]
#  exp(B1 + B2 + B1*B2)
names(res_1_order)
res_1_order <- cbind(res_1_order, str_split_fixed(res_1_order$variable, ":", 2))
res_1_order$urban_pov <- res_1_order$`2`
res_1_order$urban_pov <- ifelse(res_1_order$urban_pov == "", NA, res_1_order$urban_pov)
res_1_order$fx_1 <- res_1_order$`1`
# urban intx
res_1_order_urban <- res_1_order %>%
  filter(res_1_order$urban_pov %in% c("urban", NA))

# pov intx
res_1_order_pov <- res_1_order %>%
  filter(res_1_order$urban_pov %in% c("pov", NA))

# summing un-exponentiated results
res_1_order_urban_1 <- aggregate(res_1_order_urban[, c(2:5)], 
                                 by = list(res_1_order_urban$fx_1), FUN = sum)
res_1_order_urban_2 <- data.frame("urban", res_1_order_urban[res_1_order_urban$variable == "urban",
                                                             c(2:5)]) %>%
  rename(variable = X.urban.)
for(i in 2:10) {
  res_1_order_urban_1[i, c(2:5)] <- res_1_order_urban_1[i, c(2:5)] + res_1_order_urban_2[, c(2:5)]
}
# checking res_1_order_urban_1

res_1_order_pov_1 <- aggregate(res_1_order_pov[, c(2:5)], 
                               by = list(res_1_order_pov$fx_1), FUN = sum)
res_1_order_pov_2 <- data.frame("pov", res_1_order_pov[res_1_order_pov$variable == "pov",
                                                       c(2:5)]) %>%
  rename(variable = X.pov.)
for(i in 2:10) {
  res_1_order_pov_1[i, c(2:5)] <- res_1_order_pov_1[i, c(2:5)] + res_1_order_pov_2[, c(2:5)]
}
# checking res_1_order_pov_1

# getting intercept rows for summing
intercept <- res_1_order_urban_1[which(res_1_order_urban_1$Group.1 == "(Intercept)"), ]
res
#adding intercept vals
res_1_order_urban_1 <- res_1_order_urban_1  %>%
  mutate(estimate = estimate + intercept[1, 2]) %>%
  mutate(s.e. =  s.e. + intercept[1, 3]) %>%
  mutate(ci95.lo =  ci95.lo + intercept[1, 4]) %>%
  mutate(ci95.hi =  ci95.hi + intercept[1, 5])
res_1_order_pov_1 <- res_1_order_pov_1  %>%
  mutate(estimate =  estimate + intercept[1, 2]) %>%
  mutate(s.e. =  s.e. + intercept[1, 3]) %>%
  mutate(ci95.lo =  ci95.lo + intercept[1, 4]) %>%
  mutate(ci95.hi = ci95.hi + intercept[1, 5])

# exponentiating summed coefficients
res_1_order_urban <- res_1_order_urban_1 %>%
  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
                function(x) round(exp(x), 10)))
res_1_order_urban$Group.1 <- paste0(res_1_order_urban$Group.1, "_urban")
res_1_order_pov <- res_1_order_pov_1 %>%
  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
                function(x) round(exp(x), 10)))
res_1_order_pov$Group.1 <- paste0(res_1_order_pov$Group.1, "_pov")

# results
res_1_order <- rbind(res_1_order_urban,
                     res_1_order_pov) %>%
  rename(variable = Group.1) %>%
  filter(!variable %in% c("pov_pov", "urban_urban"))#,
#"(Intercept)_urban",
#"(Intercept)_pov"))



## higher order interactions
#  exp(B1 + B2 + B3 + B1*B2 + B2*B3 + B1*B3 + B1*B2*B3)
res_hi_order <- res

# prep for a loop to sum un-exponentiated results
aian_urban_pov <- c("aian:urban:pov", 
                    "aian:pov", "urban:pov",
                    "aian", "urban", "pov")
asian_urban_pov <- c("asian:urban:pov", 
                     "asian:pov", "urban:pov",
                     "asian", "urban", "pov")
black_urban_pov <- c("black:urban:pov", 
                     "black:pov", "urban:pov",
                     "black", "urban", "pov")
hisp_urban_pov <- c("hisp:urban:pov", 
                    "hisp:pov", "urban:pov",
                    "hisp", "urban", "pov")
nhpi_urban_pov <- c("nhpi:urban:pov", 
                    "nhpi:pov", "urban:pov",
                    "nhpi", "urban", "pov")
other_urban_pov <- c("other:urban:pov", 
                     "other:pov", "urban:pov",
                     "other", "urban", "pov")
tom_urban_pov <- c("tom:urban:pov", 
                   "tom:pov", "urban:pov",
                   "tom", "urban", "pov")

x <- data.frame(aian_urban_pov, asian_urban_pov,
                black_urban_pov, hisp_urban_pov,
                nhpi_urban_pov, other_urban_pov,
                tom_urban_pov)

# summing un-exponentiated results
intx_list = vector("list", length = 7)
for(i in 1:7) {
  df <- data.frame(sum(res_hi_order[which(res_hi_order$variable %in% x[, i]), ][, 2]),
                   sum(res_hi_order[which(res_hi_order$variable %in% x[, i]), ][, 3]),
                   sum(res_hi_order[which(res_hi_order$variable %in% x[, i]), ][, 4]),
                   sum(res_hi_order[which(res_hi_order$variable %in% x[, i]), ][, 5])
  )
  names(df) <- c("estimate", "s.e.", "ci95.lo", "ci95.hi")
  intx_list[[i]] <- df
}

intx_list = do.call(rbind, intx_list)
intx_list$variable <- data.frame(t(x[1, 1:7]))[, "X1"]

# add intercept
intx_list <- intx_list  %>%
  mutate(estimate =  estimate + intercept[1, 2]) %>%
  mutate(s.e. =  s.e. + intercept[1, 3]) %>%
  mutate(ci95.lo =  ci95.lo + intercept[1, 4]) %>%
  mutate(ci95.hi = ci95.hi + intercept[1, 5])

# exponentiate
res_hi_order <- intx_list %>%
  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
                function(x) round(exp(x), 10))) %>%
  relocate(variable, .before = estimate)

## make sure to reset res_hi_order at beginning
#e.g., black*pov*urban
res
exp(-0.07741732 + 0.23702428 + -0.08937544 + 0.43888216 + -1.12244172 + 0.76155779)

head(res_hi_order)
head(res_1_order)
head(res_ind)

res_interpreted <- rbind(res_ind[, 1:5],
                         res_1_order[, 1:5], 
                         res_hi_order)







#### test forest plot
library(forestplot)
names(res_interpreted) <- c("variable", "mean", "se", "lower", "upper")

res_interpreted <- res_interpreted[order(res_interpreted$variable), ]
res_interpreted$mean <- res_interpreted$mean * 10
res_interpreted$lower <- res_interpreted$lower * 10
res_interpreted$upper <- res_interpreted$upper * 10

res_interpreted %>%
  forestplot(labeltext = c(variable, mean),
             clip = c(0, 0.5),
             boxsize = 0.25,
             #line.margin = 0.2,
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

library(sjPlot)
str(test)
str(fit)
test <- lm(total.x ~ aian.x, data = bg_vars_ruca)
plot(urban)
plot_model(fit$internal$glmfit, type = "pred", terms = c("black", "urban", "pov"))

plot(fit$Q)
