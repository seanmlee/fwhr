library(tidyverse)

# select relevant columns ------------------------------------------------------
ppn <- ppn %>%
  select(Specimen,
         Sex,
         fWHR)

# fit model --------------------------------------------------------------------
mod_ppn <- 
  glm(
    Sex ~
      scale(fWHR),
    data = ppn,
    family = binomial()
  )

# summary
summary(mod_ppn)
summary(mod_ppn)$coefficients[1, ]


# predicted values for plot ----------------------------------------------------
fWHR <- 
  seq(
    min(ppn$fWHR),
    max(ppn$fWHR),
    length.out = 100
  )

newdata_ppn <- 
  data.frame(
    fWHR = fWHR
  )

preds_ppn <- 
  predict(
    mod_ppn, 
    newdata = newdata_ppn, 
    type = "link", 
    se.fit = TRUE
  )

fit_link_ppn <-                          # fitted values on the scale of the linear predictor
  preds_ppn$fit

fit_response_ppn <-                      # apply inverse of link function to map fitted values from the linear predictor scale to the response scale (logit)
  mod_ppn$family$linkinv(fit_link_ppn)

fit_ppn <-
  as.data.frame(
    cbind(
      fit_response_ppn, 
      fWHR
    )
  )

# confidence intervals ---------------------------------------------------------
# as with fitted values, calculate upper/lower confints first on the scale of the linear predictor, then apply the inverse of the link function to map the confints from the linear predictor scale to response scale (logit)

critval <- 1.96 ## approx 95% CI

upr_link_ppn <-
  preds_ppn$fit + (critval * preds_ppn$se.fit)

lwr_link_ppn <- 
  preds_ppn$fit - (critval * preds_ppn$se.fit)

upr_response_ppn <- 
  mod_ppn$family$linkinv(upr_link_ppn)

lwr_response_ppn <- 
  mod_ppn$family$linkinv(lwr_link_ppn)

upr_ppn <-
  as.data.frame(
    cbind(
      upr_response_ppn, 
      fWHR
    )
  )

lwr_ppn <-
  as.data.frame(
    cbind(
      lwr_response_ppn, 
      fWHR
    )
  )

# plot -------------------------------------------------------------------------
plot_ppn <- ppn

plot_ppn <- plot_ppn %>%
  mutate(
    Sex = ifelse(Sex == "Female", 
                 0, 
                 1)
  )

plot_ppn %>%
  
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
    fit_ppn,
    mapping = aes(
      x = fWHR, 
      y = fit_response_ppn
    ),
    col = "blue"
  ) +
  
  geom_line(
    data = newdata_ppn, 
    mapping = aes(
      x = fWHR, 
      y = upr_response_ppn
    ), 
    linetype = "dashed"
  ) + 
  
  geom_line(
    data = newdata_ppn, 
    mapping = aes(
      x = fWHR, 
      y = lwr_response_ppn
    ),
    linetype = "dashed"
  ) +
  
  theme_bw() +
  
  ggtitle(
    expression(
      paste(
        "e) ", 
        italic("P. paniscus")
      )
    )
  ) +
  
  xlab("Adjusted fWHR") +
  
  ylab("Probability Specimen is Male") +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("logit_ppn.png", width = 3.5)
