library(coefplot)

# female -----------------------------------------------------------------------
# factorize
female$Species <- 
  factor(
    female$Species, 
    levels = c("Gbg", 
               "Ggg", 
               "Ptt",
               "Pts",
               "Ppn")
    )

# fit model
mod_female <- lm(
  fWHR ~
    Species - 1,
  data = female
  )

summary(mod_female)

coefplot(
  mod_female,
  title = "Female",
  xlab = "Adjusted fWHR",
  ylab = "Species",
  innerCI = 2,
  lwdInner = 0.5,
  pointSize = 3
  ) + 
  
  scale_y_discrete(
    labels = c(
      expression(italic("G. b. graueri"), 
                 italic("G. g. gorilla"),
                 italic("P. t. troglodytes"),
                 italic("P. t. schweinfurthii"),
                 italic("P. paniscus"))
      )
    ) +
  
  xlim(0.005, 0.02) +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("female.png")

# male -------------------------------------------------------------------------
# factorize
male$Species <- 
  factor(
    male$Species, 
    levels = c("Gbg", 
               "Ggg", 
               "Ptt",
               "Pts",
               "Ppn")
    )

mod_male <- lm(
  fWHR ~
    Species - 1,
  data = male
  )

summary(mod_male)

coefplot(
  mod_male,
  title = "Male",
  xlab = "Adjusted fWHR",
  ylab = "Species",
  innerCI = 2,
  lwdInner = 0.5,
  pointSize = 3
  ) + 
  
  scale_y_discrete(
    labels = c(
      expression(italic("G. b. graueri"), 
                 italic("G. g. gorilla"),
                 italic("P. t. troglodytes"),
                 italic("P. t. schweinfurthii"),
                 italic("P. paniscus"))
      )
    ) +
  
  xlim(0.005, 0.02) +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("male.png")
