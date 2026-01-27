library(tidyverse)
library(ggplot2)

setwd("/Users/albajorquera/Desktop/BV Project")
data <- read_csv("bv_data.csv") 

# bv_data contains the merged data from two 2 x 2 experiments manipulating the factors
# NP Referentiality (NP | QP) and GenderMatch (Match | Mismatch). The only difference 
# between these experiments is pronoun position. In one experiment pronouns were preverbal,
# and in the other they were postverbal. My end goal is to run a 2 x 2 x 2 experiment with the
# factors Referentiality, GenderMatch and Position (Preverbal | Postverbal). In that experiment
# I want to test for a Referentiality x GenderMatch x Position interaction. What I need help with
# is figuring out how to run simulations and a power analysis to find out how many participants I'd 
# need to detect the 3-way interaction, if it was "real".

# The data in bv_data excluding filler items and any outliers based on accuracy or RTs.

## 1. DESCRIPTIVES AND PLOTS

# Making a df with the descriptives
descriptives <- data %>% 
  filter(Region %in% c(8, 9)) %>% 
  group_by(Region, Condition, Position) %>% 
  summarize(
    meanRT = mean(RT, na.rm = TRUE),
    meanLogRT = mean(LogRT, na.rm = TRUE),
    SD = sd(RT, na.rm = TRUE),
    N = sum(!is.na(RT)),
    SE = SD / sqrt(N))

descriptives <- descriptives %>% 
                      mutate(Region = recode(Region,
                          `8`= "Critical",
                          `9`="Spillover")) %>% 
                      mutate(Condition = factor(Condition,
                        levels = c("RefM", "RefMM", "QuantM", "QuantMM")
                      ))

# Plotting mean raw RTs by region, condition, position
barplot <- descriptives %>% ggplot(aes(x=Condition, y=meanRT, fill = Condition)) +
  geom_col() +
  geom_errorbar(aes(ymin = meanRT - SE, ymax = meanRT + SE)) +
  facet_grid(Position ~Region) +
  scale_fill_manual(values = c(
    "RefM" = "steelblue2",
    "RefMM" = "tomato2",
    "QuantM" = "steelblue3",
    "QuantMM" = "tomato3")) +
  theme_classic(base_size =16)

barplot
ggsave("barplot.png", width = 23, heigh = 23, units = "cm")

## 2. MODELS
# contrasts gendermatch
data$GenderMatch <- as.factor(data$GenderMatch)
contrasts(data$GenderMatch) <- c(0.5, -0.5)

# contrasts referentiality
data$Referentiality <- as.factor(data$Referentiality)
contrasts(data$Referentiality) <- c(0.5, -0.5)

# contrasts position
data$Position <- as.factor(data$Position)
contrasts(data$Position) <- c(0.5, -0.5)

# models
m_simple <- lm(LogRT~GenderMatch*Referentiality*Position, data = data)
summary(m_simple)

m1 <- lmer(LogRT~GenderMatch*Referentiality*Position +
                 (1 + GenderMatch*Referentiality | SubjID),
                  data = data, REML = F)
summary(m1)

m2 <- lmer(LogRT~GenderMatch*Referentiality*Position +
             (1 + GenderMatch*Referentiality | SubjID) + (1 + GenderMatch*Referentiality | Item),
              data = data, REML = F)
summary(m2)

m3 <- lmer(LogRT~GenderMatch*Referentiality*Position +
             (1 + GenderMatch*Referentiality | SubjID) + (1 + GenderMatch | Item),
           data = data, REML = F)
summary(m3)

anova(m1,m2) #m2 better than m1
anova(m2,m3) #m2 better than m3

# POWER ANALYSIS
# I want to figure out how many participants I'd need to detect a GenderMatch x Referentiality x Position
# interaction. How do I simulate that and ran a power analysis?