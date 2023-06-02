


#res_split <- split(res, list(nchar(res$row_names) < 12,  nchar(res$row_names) > 5))

# exponentiate only
#res_split$TRUE.FALSE <- res_split$TRUE.FALSE %>% 
#  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
#                function(x) exp(x)))
#names(res_exp) <- paste(names(res_exp), "exp", sep = "_")

res_ind <- res[which(nchar(res$variable) <= 5), ]

res_1_order <- res[which(nchar(res$variable) < 12), ]

res_hi_order <- res


# independent effects
#  exp(B1)
res_ind <- res_ind %>%
 mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
               function(x) exp(x)))
res_ind


# first-order interactions
#  exp(B1 + B2 + B1*B2)

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

# exponentiating summed coefficients
res_1_order_urban <- res_1_order_urban_1 %>%
  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
                function(x) round(exp(x), 4)))
res_1_order_urban$Group.1 <- paste0(res_1_order_urban$Group.1, "_urban")
res_1_order_pov <- res_1_order_pov_1 %>%
  mutate(across(c(estimate, s.e., ci95.lo, ci95.hi),
                function(x) round(exp(x), 4)))
res_1_order_pov$Group.1 <- paste0(res_1_order_pov$Group.1, "_pov")

# results
res_1_order_urban
res_1_order_pov


pov <- res_1_order[which(res_1_order$variable == "pov"), ]


aggregate(x$Frequency, by=list(Category=x$Category), FUN=sum)



res_1_order[which(order$variable != "aian:pov"), ]

#intx_name <- ifelse(intx$variable == "pov", "pov", "aian")
x_1 <- intx[which(intx$variable == "aian"), "estimate"]
x_2 <- intx[which(intx$variable == "urban"), "estimate"]
x_1_2 <- intx[which(grepl("aian", intx$variable) & 
                      grepl("urban", intx$variable)), "estimate"]


# higher order interactions
#  exp(B1 + B2 + B3 + B1*B2 + B2*B3 + B1*B3 + B1*B2*B3)
res_hi_order






results <- cbind(res, res_exp[, 2:5])

head(res)
res

intx_list = vector("list", length = 17)
for(i in c(11:24)) {
#  int <- fixed_z[which(fixed_z$coef == "mu_z"), "mean"]
  inde <- substr(res[11, "variable"], 1, 10)
  intx_1 <- substr(res[11, "variable"], 6, 15)
  #print(intx_name_2)
  #intx <- fixed_z[i, "mean"]
  intx_name <- ifelse(inde == "urban", intx_1, inde)
  fx_1 <- res[which(res$variable == intx_name), "estimate"]
  fx_2 <- res[which(res$variable == "urban"), "estimate"]
  fx_3 <- res[which(res$variable == intx_), "estimate"] ## variables aren't the same name
  
  prop_fx <- exp(intx_name + fx_1 + fx_2)
  ci_025 <- exp(fixed_z[which(fixed_z$coef == "mu_z"), "X0.025quant"] +
                  fixed_z[i, "X0.025quant"] +
                  fixed_z[which(fixed_z$coef == intx_name), "X0.025quant"] +
                  fixed_z[which(fixed_z$coef == "x_pov_z"), "X0.025quant"]) /
    (1 + exp(fixed_z[which(fixed_z$coef == "mu_z"), "X0.025quant"] +
               fixed_z[i, "X0.025quant"] +
               fixed_z[which(fixed_z$coef == intx_name), "X0.025quant"] +
               fixed_z[which(fixed_z$coef == "x_pov_z"), "X0.025quant"]))
  ci_975 <- exp(fixed_z[which(fixed_z$coef == "mu_z"), "X0.975quant"] +
                  fixed_z[i, "X0.975quant"] +
                  fixed_z[which(fixed_z$coef == intx_name), "X0.975quant"] +
                  fixed_z[which(fixed_z$coef == "x_pov_z"), "X0.975quant"]) /
    (1 + exp(fixed_z[which(fixed_z$coef == "mu_z"), "X0.975quant"] +
               fixed_z[i, "X0.975quant"] +
               fixed_z[which(fixed_z$coef == intx_name), "X0.975quant"] +
               fixed_z[which(fixed_z$coef == "x_pov_z"), "X0.975quant"]))
  sig <- ifelse((fixed_z[i, "X0.025quant"] > 0 & fixed_z[i, "X0.975quant"] > 0) |
                  (fixed_z[i, "X0.025quant"] < 0 & fixed_z[i, "X0.975quant"] < 0),
                "sig",
                "not sig")
  dir <- ifelse(sig == "sig" & fixed_z[i, "mean"] > 0, "+", "-")
  df <- data.frame(coef = fixed_z[i, "coef"], 
                   prob = prop_fx,
                   ci_025 = ci_025,
                   ci_975 = ci_975,
                   sig = sig,
                   dir = dir)
  intx_list[[i]] <- df
}
intx_list = do.call(rbind, intx_list)
intx_list