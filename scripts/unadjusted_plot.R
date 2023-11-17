library(tidyverse)

# prep unadjusted data ---------------------------------------------------------
unadjusted <- dat

# rename
unadjusted$Species <- gsub("Gbg", 
                           "G. b. graueri", 
                           unadjusted$Species)

unadjusted$Species <- gsub("Ggg", 
                           "G. g. gorilla", 
                           unadjusted$Species)

unadjusted$Species <- gsub("Ptt", 
                           "P. t. troglodytes", 
                           unadjusted$Species)

unadjusted$Species <- gsub("Pts", 
                           "P. t. schweinfurthii", 
                           unadjusted$Species)

unadjusted$Species <- gsub("Ppn", 
                           "P. paniscus", 
                           unadjusted$Species)

# factorize
unadjusted$Species <- 
  factor(
    unadjusted$Species, 
    levels = c(
      "G. b. graueri", 
      "G. g. gorilla", 
      "P. t. troglodytes",
      "P. t. schweinfurthii",
      "P. paniscus"
      )
    )

# plot -------------------------------------------------------------------------
unadjusted %>% 
  ggplot(
    aes(
      x = Sex,
      y = fWHR_raw,
      group = Sex
      )
    ) +
  
  geom_boxplot(outlier.shape = NA) +
  
  geom_jitter(
    size = 2,
    width = 0.15,
    alpha = 0.3
    ) +
  
  ggtitle("") +
  
  xlab("Sex") +
  
  ylab("Unadjusted fWHR") +
  
  theme_light() +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "italic")
  ) +

  facet_grid(~Species)

ggsave("figure2_unscaled.png")
