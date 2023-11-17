library(tidyverse)

# select relevant columns ------------------------------------------------------
pts <- pts %>%
  select(Specimen,
         Sex,
         fWHR)

# fit model --------------------------------------------------------------------
mod_pts <- 
  glm(
    Sex ~
      scale(fWHR),
    data = pts,
    family = binomial()
  )

# summary
summary(mod_pts)
summary(mod_pts)$coefficients[1, ]


# predicted values for plot ----------------------------------------------------
fWHR <- 
  seq(
    min(pts$fWHR),
    max(pts$fWHR),
    length.out = 100
  )

newdata_pts <- 
  data.frame(
    fWHR = fWHR
  )

preds_pts <- 
  predict(
    mod_pts, 
    newdata = newdata_pts, 
    type = "link", 
    se.fit = TRUE
  )

fit_link_pts <-                          # fitted values on the scale of the linear predictor
  preds_pts$fit

fit_response_pts <-                      # apply inverse of link function to map fitted values from the linear predictor scale to the response scale (logit)
  mod_pts$family$linkinv(fit_link_pts)

fit_pts <-
  as.data.frame(
    cbind(
      fit_response_pts, 
      fWHR
    )
  )

# confidence intervals ---------------------------------------------------------
# as with fitted values, calculate upper/lower confints first on the scale of the linear predictor, then apply the inverse of the link function to map the confints from the linear predictor scale to response scale (logit)

critval <- 1.96 ## approx 95% CI

upr_link_pts <-
  preds_pts$fit + (critval * preds_pts$se.fit)

lwr_link_pts <- 
  preds_pts$fit - (critval * preds_pts$se.fit)

upr_response_pts <- 
  mod_pts$family$linkinv(upr_link_pts)

lwr_response_pts <- 
  mod_pts$family$linkinv(lwr_link_pts)

upr_pts <-
  as.data.frame(
    cbind(
      upr_response_pts, 
      fWHR
    )
  )

lwr_pts <-
  as.data.frame(
    cbind(
      lwr_response_pts, 
      fWHR
    )
  )

# plot -------------------------------------------------------------------------
plot_pts <- pts

plot_pts <- plot_pts %>%
  mutate(
    Sex = ifelse(Sex == "Female", 
                 0, 
                 1)
  )

plot_pts %>%
  
  ggplot(
    aes(
      x = fWHR, 
      y = Sex)
  ) +
  
  geom_point(
    alpha = 0.15,
    size = 5
  ) +
  
  geom_line(
    fit_pts,
    mapping = aes(
      x = fWHR, 
      y = fit_response_pts
    ),
    col = "blue"
  ) +
  
  geom_line(
    data = newdata_pts, 
    mapping = aes(
      x = fWHR, 
      y = upr_response_pts
    ), 
    linetype = "dashed"
  ) + 
  
  geom_line(
    data = newdata_pts, 
    mapping = aes(
      x = fWHR, 
      y = lwr_response_pts
    ),
    linetype = "dashed"
  ) +
  
  theme_bw() +
  
  ggtitle(
    expression(
      paste(
        "d) ", 
        italic("P. troglodytes schweinfurthii")
      )
    )
  ) +
  
  xlab("Adjusted fWHR") +
  
  ylab("Probability Specimen is Male") +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("logit_pts.png", width = 3.5)
