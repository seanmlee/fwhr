library(tidyverse)

# select relevant columns ------------------------------------------------------
ptt <- ptt %>%
  select(Specimen,
         Sex,
         fWHR)

# fit model --------------------------------------------------------------------
mod_ptt <- 
  glm(
    Sex ~
      scale(fWHR),
    data = ptt,
    family = binomial()
  )

# summary
summary(mod_ptt)
summary(mod_ptt)$coefficients[1, ]


# predicted values for plot ----------------------------------------------------
fWHR <- 
  seq(
    min(ptt$fWHR),
    max(ptt$fWHR),
    length.out = 100
  )

newdata_ptt <- 
  data.frame(
    fWHR = fWHR
  )

preds_ptt <- 
  predict(
    mod_ptt, 
    newdata = newdata_ptt, 
    type = "link", 
    se.fit = TRUE
  )

fit_link_ptt <-                          # fitted values on the scale of the linear predictor
  preds_ptt$fit

fit_response_ptt <-                      # apply inverse of link function to map fitted values from the linear predictor scale to the response scale (logit)
  mod_ptt$family$linkinv(fit_link_ptt)

fit_ptt <-
  as.data.frame(
    cbind(
      fit_response_ptt, 
      fWHR
    )
  )

# confidence intervals ---------------------------------------------------------
# as with fitted values, calculate upper/lower confints first on the scale of the linear predictor, then apply the inverse of the link function to map the confints from the linear predictor scale to response scale (logit)

critval <- 1.96 ## approx 95% CI

upr_link_ptt <-
  preds_ptt$fit + (critval * preds_ptt$se.fit)

lwr_link_ptt <- 
  preds_ptt$fit - (critval * preds_ptt$se.fit)

upr_response_ptt <- 
  mod_ptt$family$linkinv(upr_link_ptt)

lwr_response_ptt <- 
  mod_ptt$family$linkinv(lwr_link_ptt)

upr_ptt <-
  as.data.frame(
    cbind(
      upr_response_ptt, 
      fWHR
    )
  )

lwr_ptt <-
  as.data.frame(
    cbind(
      lwr_response_ptt, 
      fWHR
    )
  )

# plot -------------------------------------------------------------------------
plot_ptt <- ptt

plot_ptt <- plot_ptt %>%
  mutate(
    Sex = ifelse(Sex == "Female", 
                 0, 
                 1)
  )

plot_ptt %>%
  
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
    fit_ptt,
    mapping = aes(
      x = fWHR, 
      y = fit_response_ptt
    ),
    col = "blue"
  ) +
  
  geom_line(
    data = newdata_ptt, 
    mapping = aes(
      x = fWHR, 
      y = upr_response_ptt
    ), 
    linetype = "dashed"
  ) + 
  
  geom_line(
    data = newdata_ptt, 
    mapping = aes(
      x = fWHR, 
      y = lwr_response_ptt
    ),
    linetype = "dashed"
  ) +
  
  theme_bw() +
  
  ggtitle(
    expression(
      paste(
        "c) ", 
        italic("P. troglodytes troglodytes")
      )
    )
  ) +
  
  xlab("Adjusted fWHR") +
  
  ylab("Probability Specimen is Male") +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("logit_ptt.png", width = 3.5)
