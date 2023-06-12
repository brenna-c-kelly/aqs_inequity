

library(grid)
library(ggpubr)
library(ggplot2)
library(RColorBrewer)

res_pm <- data.frame(summary(fit_pm)$coefs.SE.CI) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(across(c(Estimate, S.E., CI95.lo, CI95.hi),
                function(x) round(exp(x), 4)))
res_co <- data.frame(summary(fit_co)$coefs.SE.CI) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(across(c(Estimate, S.E., CI95.lo, CI95.hi),
                function(x) round(exp(x), 4)))
res_no2 <- data.frame(summary(fit_no2)$coefs.SE.CI) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(across(c(Estimate, S.E., CI95.lo, CI95.hi),
                function(x) round(exp(x), 4)))
res_o3 <- data.frame(summary(fit_o3)$coefs.SE.CI) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(across(c(Estimate, S.E., CI95.lo, CI95.hi),
                function(x) round(exp(x), 4)))
res_pb <- data.frame(summary(fit_pb)$coefs.SE.CI) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(across(c(Estimate, S.E., CI95.lo, CI95.hi),
                function(x) round(exp(x), 4)))
res_so2 <- data.frame(summary(fit_so2)$coefs.SE.CI) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(across(c(Estimate, S.E., CI95.lo, CI95.hi),
                function(x) round(exp(x), 4)))


fixed <- tibble::rownames_to_column(res_fig, "var")
names(fixed) <- c("var", "coef")

# lead interactions
#coefs <- res$summary.fixed

## Intercept
b0 <- fixed[which(fixed$var == "(Intercept)"), 2]

## poverty coefficient and prediction values (x-axis)
b1 <- fixed[which(fixed$var == "pov"), 2]
x1 <- seq(-3, 3, length = 100)

## asian coefficient and prediction values (colors)
b2 <- fixed[which(fixed$var == "aian"), 2]
x2 <- seq(-3, 3)

## rural/urban coefficient and prediction values (facet)
b3 <- fixed[which(fixed$var == "urban"), 2]
x3 <- seq(-3, 3)

## interaction coefficient 1
b4 <- fixed[which(fixed$var == "aian:pov"), 2]
## interaction coefficient 2
b5 <- fixed[which(fixed$var == "aian:urban"), 2]
## interaction coefficient 3
b6 <- fixed[which(fixed$var == "pov:urban"), 2]
## higher order interaction coefficient
b7 <- fixed[which(fixed$var == "aian:pov:urban"), 2]

## Prediction grid
myx <- expand.grid(aian = x1, pov = x2)

## Expected log counts for grid
myx$log_theta <- b0 + b1 * myx$pov + b2 * myx$aian + #b3 * myx$urban +
  b4 * (myx$pov * myx$aian)# + b5 * (myx$urban * myx$asian) +
  #b6 * (myx$pov * myx$urban) #+ b7 * (myx$urban * myx$asian * myx$pov)

myx$theta_fix <- exp(myx$log_theta)

## Inverse link transform
#myx$theta <- exp(myx$log_theta)
summary(myx$theta_fix)
## Plots - convert perwhite to factors
#myx$nonwhite_c <- as.factor(myx$nonwhite_c)
myx$pov <- as.factor(myx$pov)
#myx$urban <- as.factor(myx$urban)
#summary(myx$theta_fix)

## Plot it
asian_poverty <- ggline(myx, x = "aian", y = "theta_fix", 
                        col = "pov", numeric.x.axis = TRUE, 
                        size = 1.5, plot_type = 'l',
                        xlab = "Standardized Percent Other Race",
                        ylab = "Risk Potential", alpha = 0.5) +
  scale_colour_manual(values = rev(brewer.pal(8, "RdYlBu")),) +
  labs(color = "Standardized \nPercent Below \nPoverty Line") +
  #geom_hline(yintercept = mean(pop_tox$rsei.score), linetype = "dashed") +
  #ggtitle("Race/Poverty: All") +
  theme(legend.position = "right")
asian_poverty


summary(res)

###     Binomial interactions
figs_z = vector("list", length = 6)
z_intx <- c("x_aia_z", "x_asi_z",
            "x_nhp_z", "x_tom_z", "x_oth_z",
            "x_his_z")
for(i in z_intx) {
  b0 <- coefs[which(rownames(coefs) == "mu_z"), 1]
  
  ## violent crime coefficient and prediction values (x-axis)
  b1 <- coefs[which(rownames(coefs) == "x_pov_z"), 1]
  x1 <- seq(-4, 2, length = 100) ## Violent crime
  
  ## percent white coefficient and prediction values (colors)
  b2 <- coefs[which(rownames(coefs) == i), 1]
  x2 <- seq(-4, 2) ## PerWhite
  
  ## interaction coefficient
  b3 <- coefs[which(rownames(coefs) == paste("x_pov_z:", i, sep = "")), 1]
  
  ## Prediction grid
  myx <- expand.grid(i = x1, x_pov_z = x2)
  
  str(myx)
  ## Expected log counts for grid
  myx$log_theta <- b0 + b1 * myx$x_pov_z + b2 * myx$i +
    b3 * (myx$x_pov_z * myx$i)
  
  myx$theta_fix <- exp(myx$log_theta) / (1 + exp(myx$log_theta))
  
  ## Inverse link transform
  #myx$theta <- exp(myx$log_theta)
  summary(myx$theta_fix)
  ## Plots - convert perwhite to factors
  #myx$nonwhite_c <- as.factor(myx$nonwhite_c)
  myx$x_pov_z <- as.factor(myx$x_pov_z)
  summary(myx$x_pov_z)
  
  summary(myx$theta_fix)
  #races <- c("American Indian/Alaska Native",
  #           "Asian", "Native Hawaiian/Pacific Islander",
  #           "Two or More Races", "Some Other Race", "Hispanic/Latinx")
  ## Plot it
  
  myx$sig <- ifelse(i %in% c("x_bla_z", "x_his_z"), 1, 0.5)
  race_poverty <- ggline(myx, x = "i", y = "theta_fix", 
                         col = "x_pov_z", numeric.x.axis = TRUE, 
                         size = 1.5, plot_type = 'l',
                         alpha = sig,
                         xlab = paste("Percent", i),
                         ylab = "Probability of Being Polluted", alpha = 0.5) +
    scale_colour_manual(values = rev(brewer.pal(8, "RdYlBu")),) +
    labs(color = "Percent Below \nPoverty Line") +
    #geom_hline(yintercept = mean(pop_tox$rsei.score), linetype = "dashed") +
    #ggtitle("Race/Poverty: All") +
    theme(legend.position = "right")
  race_poverty
  figs_z[[i]] <- race_poverty
}


ggarrange(figs_z$x_oth_z,
          legend = "right",
          common.legend = TRUE)



figs_y = vector("list", length = 6)
y_intx <- c("x_bla_y", "x_aia_y", "x_asi_y",
            "x_nhp_y", "x_tom_y", "x_oth_y",
            "x_his_y")
for(i in y_intx) {
  b0 <- coefs[which(rownames(coefs) == "mu_y"), 1]
  
  ## violent crime coefficient and prediction values (x-axis)
  b1 <- coefs[which(rownames(coefs) == "x_pov_y"), 1]
  x1 <- seq(-3, 3, length = 100) ## Violent crime
  
  ## percent white coefficient and prediction values (colors)
  b2 <- coefs[which(rownames(coefs) == i), 1]
  x2 <- seq(-3, 3) ## PerWhite
  
  ## interaction coefficient
  b3 <- coefs[which(rownames(coefs) == paste("x_pov_y:", i, sep = "")), 1]
  
  ## Prediction grid
  myy <- expand.grid(i = x1, x_pov_y = x2)
  
  str(myy)
  ## Expected log counts for grid
  myy$log_theta <- exp(b0 + b1 * myy$x_pov_y + b2 * myy$i +
                         b3 * (myy$x_pov_y * myy$i))
  
  myy$theta_fix <- exp(myy$log_theta) / (1 + exp(myy$log_theta))
  
  ## Inverse link transform
  #myx$theta <- exp(myx$log_theta)
  summary(myy$theta_fix)
  ## Plots - convert perwhite to factors
  #myx$nonwhite_c <- as.factor(myx$nonwhite_c)
  myy$x_pov_y <- as.factor(myy$x_pov_y)
  
  summary(myy$theta_fix)
  #races <- c("American Indian/Alaska Native",
  #           "Asian", "Native Hawaiian/Pacific Islander",
  #           "Two or More Races", "Some Other Race", "Hispanic/Latinx")
  ## Plot it
  
  #myx$sig <- ifelse(i %in% c("x_bla_y", "x_his_y"), 1, 0.5)
  race_poverty <- ggline(myy, x = "i", y = "log_theta", 
                         col = "x_pov_y", numeric.x.axis = TRUE, 
                         size = 1.5, plot_type = 'l',
                         # alpha = sig,
                         xlab = paste("Percent", i),
                         ylab = "Risk Potential", alpha = 0.5) +
    scale_colour_manual(values = rev(brewer.pal(8, "Spectral")),) +
    labs(color = "Percent Below \nPoverty Line") +
    #geom_hline(yintercept = mean(pop_tox$rsei.score), linetype = "dashed") +
    #ggtitle("Race/Poverty: All") +
    theme(legend.position = "right")
  race_poverty
  figs_y[[i]] <- race_poverty
}

ggarrange(figs_y$x_bla_y,
          figs_y$x_aia_y,
          figs_y$x_asi_y,
          figs_y$x_nhp_y,
          figs_y$x_tom_y,
          figs_y$x_oth_y,
          figs_y$x_his_y,
          legend = "right",
          common.legend = TRUE)

figs_z = vector("list", length = 6)
z_intx <- c("x_bla_z", "x_aia_z", "x_asi_z",
            "x_nhp_z", "x_tom_z", "x_oth_z",
            "x_his_z")
for(i in z_intx) {
  b0 <- coefs[which(rownames(coefs) == "mu_z"), 1]
  
  ## violent crime coefficient and prediction values (x-axis)
  b1 <- coefs[which(rownames(coefs) == "x_pov_z"), 1]
  x1 <- seq(-3, 3, length = 100) ## Violent crime
  
  ## percent white coefficient and prediction values (colors)
  b2 <- coefs[which(rownames(coefs) == i), 1]
  x2 <- seq(-3, 3) ## PerWhite
  
  ## interaction coefficient
  b3 <- coefs[which(rownames(coefs) == paste("x_pov_z:", i, sep = "")), 1]
  
  ## Prediction grid
  myz <- expand.grid(i = x1, x_pov_z = x2)
  
  str(myz)
  ## Expected log counts for grid
  myz$log_theta <- b0 + b1 * myz$x_pov_z + b2 * myz$i +
    b3 * (myz$x_pov_z * myz$i)
  
  myz$theta_fix <- exp(myz$log_theta) / (1 + exp(myz$log_theta))
  
  ## Inverse link transform
  #myx$theta <- exp(myx$log_theta)
  summary(myz$theta_fix)
  ## Plots - convert perwhite to factors
  #myx$nonwhite_c <- as.factor(myx$nonwhite_c)
  myz$x_pov_z <- as.factor(myz$x_pov_z)
  
  summary(myz$theta_fix)
  #races <- c("American Indian/Alaska Native",
  #           "Asian", "Native Hawaiian/Pacific Islander",
  #           "Two or More Races", "Some Other Race", "Hispanic/Latinx")
  ## Plot it
  
  #myx$sig <- ifelse(i %in% c("x_bla_y", "x_his_y"), 1, 0.5)
  race_poverty <- ggline(myz, x = "i", y = "theta_fix", 
                         col = "x_pov_z", numeric.x.axis = TRUE, 
                         size = 1.5, plot_type = 'l',
                         # alpha = sig,
                         xlab = paste("Percent", i),
                         ylab = "Change in Risk Potential", alpha = 0.5) +
    scale_colour_manual(values = rev(brewer.pal(8, "Spectral")),) +
    labs(color = "Percent Below \nPoverty Line") +
    #geom_hline(yintercept = mean(pop_tox$rsei.score), linetype = "dashed") +
    #ggtitle("Race/Poverty: All") +
    theme(legend.position = "right")
  race_poverty
  figs_z[[i]] <- race_poverty
}
figs_z$x_tom_z

ggarrange(figs_z$x_bla_z,
          figs_z$x_aia_z,
          figs_z$x_asi_z,
          figs_z$x_nhp_z,
          figs_z$x_tom_z,
          figs_z$x_oth_z,
          figs_z$x_his_z,
          legend = "right",
          common.legend = TRUE)


ggline(myz, x = "i", y = "theta_fix", 
       col = "x_pov_z", numeric.x.axis = TRUE, 
       size = 1.5, plot_type = 'l',
       # alpha = sig,
       xlab = paste("Percent", i),
       ylab = "Change in Risk Potential", alpha = 0.5)


