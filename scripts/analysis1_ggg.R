library(tidyverse)

# select relevant columns ------------------------------------------------------
ggg <- ggg %>%
  select(Specimen,
         Sex,
         fWHR)

# fit model --------------------------------------------------------------------
mod_ggg <- 
  glm(
    Sex ~
      scale(fWHR),
    data = ggg,
    family = binomial()
  )

# summary
summary(mod_ggg)
summary(mod_ggg)$coefficients[1, ]


# predicted values for plot ----------------------------------------------------
fWHR <- 
  seq(
    min(ggg$fWHR),
    max(ggg$fWHR),
    length.out = 100
  )

newdata_ggg <- 
  data.frame(
    fWHR = fWHR
  )

preds_ggg <- 
  predict(
    mod_ggg, 
    newdata = newdata_ggg, 
    type = "link", 
    se.fit = TRUE
  )

fit_link_ggg <-                          # fitted values on the scale of the linear predictor
  preds_ggg$fit

fit_response_ggg <-                      # apply inverse of link function to map fitted values from the linear predictor scale to the response scale (logit)
  mod_ggg$family$linkinv(fit_link_ggg)

fit_ggg <-
  as.data.frame(
    cbind(
      fit_response_ggg, 
      fWHR
    )
  )

# confidence intervals ---------------------------------------------------------
# as with fitted values, calculate upper/lower confints first on the scale of the linear predictor, then apply the inverse of the link function to map the confints from the linear predictor scale to response scale (logit)

critval <- 1.96 ## approx 95% CI

upr_link_ggg <-
  preds_ggg$fit + (critval * preds_ggg$se.fit)

lwr_link_ggg <- 
  preds_ggg$fit - (critval * preds_ggg$se.fit)

upr_response_ggg <- 
  mod_ggg$family$linkinv(upr_link_ggg)

lwr_response_ggg <- 
  mod_ggg$family$linkinv(lwr_link_ggg)

upr_ggg <-
  as.data.frame(
    cbind(
      upr_response_ggg, 
      fWHR
    )
  )

lwr_ggg <-
  as.data.frame(
    cbind(
      lwr_response_ggg, 
      fWHR
    )
  )

# plot -------------------------------------------------------------------------
plot_ggg <- ggg

plot_ggg <- plot_ggg %>%
  mutate(
    Sex = ifelse(Sex == "Female", 
                 0, 
                 1)
  )

plot_ggg %>%
  
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
    fit_ggg,
    mapping = aes(
      x = fWHR, 
      y = fit_response_ggg
    ),
    col = "blue"
  ) +
  
  geom_line(
    data = newdata_ggg, 
    mapping = aes(
      x = fWHR, 
      y = upr_response_ggg
    ), 
    linetype = "dashed"
  ) + 
  
  geom_line(
    data = newdata_ggg, 
    mapping = aes(
      x = fWHR, 
      y = lwr_response_ggg
    ),
    linetype = "dashed"
  ) +
  
  theme_bw() +
  
  ggtitle(
    expression(
      paste(
        "b) ", 
        italic("G. gorilla gorilla")
      )
    )
  ) +
  
  xlab("Adjusted fWHR") +
  
  ylab("Probability Specimen is Male") +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("logit_ggg.png", width = 3.5)
