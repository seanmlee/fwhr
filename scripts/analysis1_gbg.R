library(tidyverse)

# select relevant columns ------------------------------------------------------
gbg <- gbg %>%
  select(Specimen,
         Sex,
         fWHR)

# fit model --------------------------------------------------------------------
mod_gbg <- 
  glm(
    Sex ~
      scale(fWHR),
    data = gbg,
    family = binomial()
    )

# summary
summary(mod_gbg)
summary(mod_gbg)$coefficients[1, ]


# predicted values for plot ----------------------------------------------------
fWHR <- 
  seq(
    min(gbg$fWHR),
    max(gbg$fWHR),
    length.out = 100
    )

newdata_gbg <- 
  data.frame(
    fWHR = fWHR
    )

preds_gbg <- 
  predict(
    mod_gbg, 
    newdata = newdata_gbg, 
    type = "link", 
    se.fit = TRUE
  )

fit_link_gbg <-                          # fitted values on the scale of the linear predictor
  preds_gbg$fit

fit_response_gbg <-                      # apply inverse of link function to map fitted values from the linear predictor scale to the response scale (logit)
  mod_gbg$family$linkinv(fit_link_gbg)

fit_gbg <-
  as.data.frame(
    cbind(
      fit_response_gbg, 
      fWHR
    )
  )

# confidence intervals ---------------------------------------------------------
# as with fitted values, calculate upper/lower confints first on the scale of the linear predictor, then apply the inverse of the link function to map the confints from the linear predictor scale to response scale (logit)

critval <- 1.96 ## approx 95% CI

upr_link_gbg <-
  preds_gbg$fit + (critval * preds_gbg$se.fit)

lwr_link_gbg <- 
  preds_gbg$fit - (critval * preds_gbg$se.fit)

upr_response_gbg <- 
  mod_gbg$family$linkinv(upr_link_gbg)

lwr_response_gbg <- 
  mod_gbg$family$linkinv(lwr_link_gbg)

upr_gbg <-
  as.data.frame(
    cbind(
      upr_response_gbg, 
      fWHR
      )
    )

lwr_gbg <-
  as.data.frame(
    cbind(
      lwr_response_gbg, 
      fWHR
      )
    )

# plot -------------------------------------------------------------------------
plot_gbg <- gbg

plot_gbg <- plot_gbg %>%
  mutate(
    Sex = ifelse(Sex == "Female", 
                 0, 
                 1)
    )

plot_gbg %>%
  
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
    fit_gbg,
    mapping = aes(
      x = fWHR, 
      y = fit_response_gbg
      ),
    col = "blue"
    ) +
  
  geom_line(
    data = newdata_gbg, 
    mapping = aes(
      x = fWHR, 
      y = upr_response_gbg
      ), 
    linetype = "dashed"
    ) + 
  
  geom_line(
    data = newdata_gbg, 
    mapping = aes(
      x = fWHR, 
      y = lwr_response_gbg
      ),
    linetype = "dashed"
    ) +
  
  theme_bw() +
  
  ggtitle(
    expression(
      paste(
        "a) ", 
        italic("G. beringei graueri")
        )
      )
    ) +
  
  xlab("Adjusted fWHR") +
  
  ylab("Probability Specimen is Male") +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    )

ggsave("logit_gbg.png", width = 3.5)
