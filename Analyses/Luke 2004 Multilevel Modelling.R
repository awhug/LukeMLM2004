# Background

# This analysis is from Douglas A. Luke's "Multilevel Modeling" (2004) by Sage University Press. The dataset is taken from a tobacco control policy study, with the main goal to identify important influences on voting on tobacco-related legislation by members of Congress from 1997 to 2000 (Luke & Krauss, 2004). 
# The aim of this analysis is to reproduce the results of Luke (2004) using lme4 and the tidyverse (ggplot, dplyr) from Doug Luke's dataset.
# The dependent variable is Voting % ("VotePct"), the percentage of time that a senator or representative voted in a "pro-tobacco" direction during those four years.The variable can range from 0.0 (never voted protobacco) to 1.O (always voted pro-tobacco). 
# Independent variables for each politician include Political Party and Money (pactotal). We also include two higher level independent variables: State and tobacco farm economy of state (in acres) 

# Set up
library(tidyverse)
options(scipen = 99)
load("../Data/tobvote.rda")

#  Load tobvote from data file
tobacco <- tobvote %>% 
  select(lastname, firstnam, state, house, pactotal, party, votedpro, votenum, acres) %>% 
  group_by(state, lastname, firstnam, house)%>% 
  mutate(vote = (as.numeric(votedpro)-1),      #  Convert vote to binary numeric 
         VotePct = (sum(vote)/votenum)) %>%  #  Calculate % pro-tobacco votes
  select(-vote, -votedpro) %>% 
  unique()                         #  Reduce data to one row per representative

## Tables 2.2 & 2.3 "Structure of Level-1 Tobacco Data Set" and "Structure of Level-2 Tobacco Data Set"
head(tobacco)
# Note there is a discrepancy here - the text had N = 527. We appear to be missing one congress person from California. Otherwise the data match exactly.

# Visualisations

## Figure 2.2 "Average Pro-Tobacco Vote Percentage by Congress Members - 1997 to 2000"

#  Need to install "fiftystater" package for Alaska and Hawaii
if(!require(fiftystater)){
  if(!require(devtools)){install.packages("devtools")}
  else devtools::install_github("wmurphyrd/fiftystater");
  install.packages("mapproj")}
library("fiftystater")
library("mapproj")

vote_by_state <- tobacco %>% 
  group_by(state) %>% 
  summarise(Avg = mean(VotePct)) %>% 
  mutate(statename = tolower(state.name[match(state, state.abb)]))

state_plot <- ggplot(vote_by_state, aes(map_id = statename)) + 
  #  map points to the fifty_states shape data
  geom_map(aes(fill = Avg), map = fifty_states)+ 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_text(aes(state.center$x, state.center$y, label = state.abb), size = 3) + 
  coord_map() +
  scale_fill_gradient2(name = "Avg Pro-Tobacco Vote", high = "grey10") +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) 

state_plot

# Alaska and Hawaii labels don't play nice. Will come back to this. https://cran.r-project.org/web/packages/fiftystater/vignettes/fiftystater.html

## Figure 2.3 "Relationship of Money and Voting % for 5 Large States"

five_states_plot <- tobacco %>% 
  filter(state == "CA" |state ==  "FL" |state ==  "IL" |state ==  "NY" |state ==  "TX") %>% 
  ggplot(aes(x = pactotal, y = VotePct, shape = state, linetype = state)) + 
  geom_point() + 
  stat_smooth(method = "lm", se =FALSE, fullrange = T, color = "black", size=.3) +
  ylim(0,1) + xlim(0,80) + 
  labs(x = "PAC Contributions ($K)", y = "Pro-Tobacco Voting (%)") +
  theme_classic() 

five_states_plot

# Graph mirrors that of the book very closely.

## Figure 2.4 "OLS Fits of Voting % and Money for 50 States"

# Note this may take some time to run
ols_states_plot <- ggplot(tobacco, aes(x = pactotal, y = VotePct)) + 
  geom_point(shape = 1, size = 1.25, alpha = .8) +
  stat_smooth(method = "lm", se =FALSE, fullrange = T, 
              color = "black", size=.3, alpha = .75) + 
  facet_wrap( ~ state) + 
  theme_bw() + 
  theme(panel.spacing = unit(0, "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "PAC Money", y = "Vote %") +
  ylim(0,1) + xlim(0,100) 

ols_states_plot

# Aesthetics aren't exactly the same, but OLS fits are more or less identical and now in alphabetical order.

# Model Fitting
# This is implemented in lme4, rather than nmle. Slight differences in estimation are evident, but results are similar.

## Table 2.5 "Partial R Output for the Null Model"

library(lme4)
library(lmerTest)

null_fit <- lmer(VotePct ~ 1 + (1 | state), 
                 data = tobacco, REML = F)
summary(null_fit)

# Model fits differ somewhat, but result is extremely close.

## Table 2.7 "Partial R Output for Model 1"

model1_fit <- lmer(VotePct ~ pactotal + party +   #  Fixed effects
                     (pactotal + party | state),  #  Random effects
                   data = tobacco, REML = F)
summary(model1_fit)

# Results are similar.

## Table 2.9a "Parameter Estimates and Model Fit for Three Models"

model2_fit <- lmer(VotePct ~ pactotal + party + acres + 
                     (pactotal + party | state), 
                   data = tobacco, REML = F)
summary(model2_fit)

# Fixed effects estimates and variance components are roughly similar. lmerTest provides significance tests for the former, which resemble published results, with the exception of 'acres' variable which is here non-significant. Neither lmerTest nor lme4 provide significance tests for variance components.
# Difficult to say why differences emerge - may be due to software (HLM providing the published estimates?) implementing a different form of test. Need to double check this.

## Table 2.9b "Parameter Estimates and Model Fit for Three Models"

model3_fit <- lmer(VotePct ~ pactotal + party + acres + 
                     pactotal*acres + party*acres +      #  Interactions
                     (pactotal + party | state), 
                   data = tobacco, REML = F)
summary(model3_fit)

# Again, t-value and significance levels differ from published results. Unclear why this may be, although lme4 provides a warning about rescaling the variable (acres ranges from many observations of zero to >1,000).

# More Visualisations

## Figure 2.5 "Predicted Pro-Tobacco Voting Percent for Democrats by Amount of Tobacco Acreage"

library(emmeans)

acreages <- c("No Acreage", "Moderate Acreage", "High Acreage")
demo_data <- emmeans(model3_fit, c("party", "pactotal", "acres"), 
                     at = list(party = "Democrat", 
                               pactotal = seq.int(0, 100, 10),
                               acres = c(0, 33, 200)),
                     type = "response") %>% as.data.frame %>% 
  mutate(acres = factor(acres, labels = acreages))

demo_plot <- ggplot(demo_data, aes(y = emmean, x = pactotal, group = acres)) +
  geom_line(aes(linetype = acres)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = .05) +
  theme_bw() +
  coord_cartesian(ylim = c(0,.96), xlim = c(2, 98)) +
  labs(y = "Predicted Voting - Democrats (%)",
       x = "PAC Contributions ($K)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.15),
        legend.title = element_blank())

demo_plot

##  Figure 2.6 "Boxplots of Within-State residuals for Model 3"

residual_plot <- ggplot(model3_fit, aes(y = residuals(model3_fit), x = reorder(state, desc(state)))) +
  geom_boxplot(outlier.shape = 1, size = .4) + 
  coord_flip() + 
  geom_hline(yintercept = 0) +
  theme_classic() + labs(x = "State", y = "Residuals")

residual_plot

#  Now in alphabetical order. Visualisation is approximately the same.

##  Figure 2.7 "Scatterplot of Standardised Residuals by Fitted Values for Model 3 by Party"

std_residual_plot <- ggplot(model3_fit, aes(.fitted, .resid)) + 
  geom_point(shape = 1) +
  facet_wrap(~ party) + 
  theme_bw() + theme(panel.grid.minor = element_blank()) +
  geom_text(aes(label = ifelse(.resid > 0.5 | .resid <= -0.5, #  Label if extreme
                               as.character(state), '')),     
            hjust = 0, vjust = 0) +
  labs(x = "Fitted Values", y = "Standardised Values")

std_residual_plot

#  Visualisation is approximately the same.

##  Figure 2.8 "Normal QQ-Plot of Residuals of Model 3 by Party"

qplot_party <- ggplot(model3_fit, aes(sample = residuals(model3_fit, scaled = FALSE))) + 
  geom_point(stat = "qq", shape = 1) + 
  coord_flip() + 
  facet_wrap( ~ party) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  labs(x = "Quantiles of Standard Normal", y = "Residuals")

qplot_party

##  Figure 2.9 "Normal QQ-Plot of Random Effects of Model 3"

random_effects <- as.data.frame(ranef(model3_fit))

qplot_re <- random_effects %>% 
  ggplot(aes(sample = condval)) + 
  stat_qq(aes(group = 1), alpha = 0.5) +
  coord_flip() + theme_bw() +
  facet_wrap(~term,  scales = "free") +
  labs(x = "Quantiles of Standard Normal", y = "Random Effects")

qplot_re

## Figure 2.10 "Scatterplot Matrix"

library(GGally)
scatterplot_re <- random_effects %>% 
  select(-condsd) %>% 
  spread(term, condval, convert = TRUE) %>% 
  GGally::ggpairs(columns = c("(Intercept)", "pactotal", "partyRepublican")) + 
  theme_bw()

scatterplot_re

# Best Linear Unbiased Estimators

## Obtained via `coef()` in lme4. A.K.A best linear unbiased estimators (BLUEs) or predictors (BLUPs), conditional mode of parameters, "shrinkage estimates". 

## Table 2.11

# Prepare the data
state_data <- tobacco %>% 
  group_by(state) %>% 
  summarise(acres = first(acres),
            legislators = n())

# Best Linear Unbiased Estimates
fixed_effects <- fixef(model3_fit)
blues <- coef(model3_fit)$state
# Change "(Intercept)" to "Intercept" for convenience
colnames(blues)[1] <- names(fixed_effects)[1] <- "Intercept"

# Custom function to create the EB estimates
adj_est <-  function(a){
  term <- ifelse(a == "Intercept", "acres", paste(a, ":acres", sep = ""))
  blues[[a]] + (state_data[["acres"]] * fixed_effects[[term]])
}

# Calculate EB estimates
eb_estimates <- blues %>% 
  transmute(State = state_data[["state"]],
            Legislators = state_data[["legislators"]],
            Intercept = adj_est("Intercept"),
            Party = adj_est("partyRepublican"),
            Money = adj_est("pactotal"),
            Acres = state_data[["acres"]])

eb_estimates

## Figure 2.11 "Bayesian Estimated Regression Lines for Model 3 by Party"

eb_predictions <- eb_estimates %>% 
  transmute(Dmin = Intercept + Money,
            Dmax = Intercept + (Money * 120),
            Rmin = Intercept + Party + Money,
            Rmax = Intercept + Party + (Money * 120),
            state = state_data[["state"]])

eb_party_plot <- eb_predictions %>% ggplot() + 
  geom_segment(aes(x = 0, xend = 120, y = Dmin, yend = Dmax), 
               linetype = "solid", alpha = .75) +
  geom_segment(aes(x = 0, xend = 120, y = Rmin, yend = Rmax), 
               linetype = "longdash", alpha = .75) +
  theme_minimal() +
  coord_cartesian(ylim = c(0,.96), xlim = c(5, 115)) +
  labs(y = "Predicted Pro-Tobacco Voting (%)",
       x = "PAC Contributions ($K)")

eb_party_plot

# Figure 2.12

# Text states that this is a plot where "party is ignored". But the EB estimates will give different intercepts and slopes depending upon the party. Arbitrarily choose one and bias the plot. Take the mean across parties, and we bias again because some states lean Republican, or Democrat. 
# The solution adopted here is to calculate a weighted mean of the EB estimates by the proportion of Republican legislators. This is done by calculating the ratio of R/D for each state, then using these as weights to average the minimum and maximum predicted values calculated above for each state, using a call to `sapply`.

# Find ratio of Democrat to Republicans
party_ratio <- tobacco %>% 
  group_by(state, party) %>% 
  summarise(legislators = n()) %>% 
  mutate(Pct_Republican = legislators / sum(legislators)) %>% 
  complete(party, fill = list(Pct_Republican = 0)) %>% 
  ungroup() %>% select(-legislators) %>% 
  spread(key = party, value = Pct_Republican)

# Custom function to find weighted EB estimates
eb_weighted <- function(D, R){
  sapply(1:50, function(x){
    weighted.mean(x = c(eb_predictions[[D]][x], eb_predictions[[R]][x]), 
                  w = c(party_ratio[["Democrat"]][x], party_ratio[["Republican"]][x]))
  })
}
eb_by_state <- data.frame(state = state_data[["state"]],
                          Avgmin = eb_weighted("Dmin", "Rmin"),
                          Avgmax = eb_weighted("Dmax", "Rmax"))

# Now plot
eb_state_plot <- tobacco %>% 
  ggplot(aes(x = pactotal, y = VotePct)) + 
  geom_point(shape = 1, size = 1.25, alpha = .8) + 
  coord_cartesian(ylim = c(0,1), xlim = c(0, 100)) +
  facet_wrap( ~ state) +
  geom_segment(data = eb_by_state, 
               aes(x = 0, xend = 120, 
                   y = Avgmin, yend = Avgmax)) +
  theme_bw() + 
  theme(panel.spacing = unit(0, "lines"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = "Vote %", x = "PAC Money")

eb_state_plot

# Centering

## Table 2.12

mean_pac <- mean(tobacco$pactotal)
tobacco <- tobacco %>% 
  group_by(state) %>% 
  mutate(pactotal_gc = pactotal - mean_pac,
         pactotal_sc = pactotal - mean(pactotal))

model3_gc <- lmer(VotePct ~ pactotal_gc + party + acres + 
                    pactotal_gc*acres + party*acres + 
                    (pactotal_gc + party | state), 
                  data = tobacco, REML = F)
model3_sc <- lmer(VotePct ~ pactotal_sc + party + acres + 
                    pactotal_sc*acres + party*acres + 
                    (pactotal_sc + party | state), 
                  data = tobacco, REML = F)

summary(model3_gc)

# Generalised Linear Models

## Table 3.2

tobacco_votes <- tobvote %>% 
  select(lastname, firstnam, state, house, pactotal, party, votedpro, votenum, acres) %>%
  mutate(vote = (as.numeric(votedpro)-1)) %>% 
  group_by(state) %>% 
  mutate(legislator = paste(firstnam, lastname))

model4_fit <- glmer(vote ~ 1 + party + pactotal + (1 | legislator), 
                    data = tobacco_votes, 
                    family = binomial)

summary(model4_fit)

# Need to add dispersion index. Code is nicked from the blmeco package
dispersion_glmer <- function(modelglmer){
  # computing  estimated scale  ( binomial model) following  D. Bates:
  # That quantity is the square root of the penalized residual sum of
  # squares divided by n, the number of observations, evaluated as:
  n <- length(resid(modelglmer))
  return(  sqrt( sum(c(resid(modelglmer),modelglmer@u) ^2) / n ) ) 
} 

dispersion_glmer(model4_fit)

## Figure 3.2

glmer_predicted <- emmeans(model4_fit, c("party", "pactotal"), 
                           at = list(party = c("Republican", "Democrat"), 
                                     pactotal = 1:120),
                           type = "response") %>% as.data.frame()

glmer_predicted %>% 
  ggplot(aes(y = prob, x = pactotal, group = party)) +
  geom_line(aes(linetype = party)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = .05) +
  lims(y = c(0, 1)) +
  labs(x = "Pac Contributions ($K)",
       y = "Predicted Pro-Tobacco Voting Probabilities") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.9, 0.15),
        legend.title = element_blank())

## Table 3.3
# Note: This may take several minutes, depending upon the speed of your computer.

model5_fit <- glmer(vote ~ party + pactotal + acres + 
                      (party + pactotal | legislator) + (1 | state), 
                    data = tobacco_votes, 
                    family = binomial)
summary(model5_fit)

## Figure 3.3

glmer2_predicted <- emmeans(model5_fit, c("party", "pactotal", "acres"), 
                            at = list(party = c("Republican", "Democrat"), 
                                      pactotal = 1:120,
                                      acres = c(0, 10, 100)),
                            type = "response") %>% as.data.frame()

glmer2_predicted %>% 
  unite("party_acres", party, acres) %>% 
  ggplot(aes(y = prob, x = pactotal, group = party_acres)) +
  geom_line(aes(linetype = party_acres)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = .05) +
  lims(y = c(0, 1)) +
  labs(x = "Pac Contributions ($K)",
       y = "Predicted Pro-Tobacco Voting Probabilities") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.25),
        legend.title = element_blank())
