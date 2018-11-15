#Background

#This analysis is from Douglas A. Luke's "Multilevel Modeling" (2004) by Sage University Press. The dataset is taken from a tobacco control policy study, with the main goal to identify important influences on voting on tobacco-related legislation by members of Congress from 1997 to 2000 (Luke & Krauss, 2004). 
#The aim of this analysis is to reproduce the results of Luke (2004) using lme4 and the tidyverse (ggplot, dplyr) from Doug Luke's dataset.
#The dependent variable is Voting % ("VotePct"), the percentage of time that a senator or representative voted in a "pro-tobacco" direction during those four years.The variable can range from 0.0 (never voted protobacco) to 1.O (always voted pro-tobacco). 
#Independent variables for each politician include Political Party and Money (pactotal). We also include two higher level independent variables: State and tobacco farm economy of state (in acres) 

#Data Preparation

library(tidyverse)
library(lme4)
library(lmerTest)

#Need to calculate the voting percentage to achieve same dependent variable as Luke (2004).
#Load tobvote from data file
tobacco <- tobvote %>% 
  select(lastname, firstnam, state, house, pactotal, party, votedpro, votenum, acres) %>% #extract variables of interest
  group_by(state, lastname, house)%>% #Arrange in state and last name order
  mutate(vote = (as.numeric(votedpro)-1), #covert voting behaviour to binary numeric data
         VotePct = round((sum(vote)/votenum),2)) %>% #calculate percentage of pro-tobacco votes
  select(-vote, -votedpro) %>% #remove nuisance variables
  unique() #reduce data frame to one row per representative

##Tables 2.2 & 2.3 "Structure of Level-1 Tobacco Data Set" and "Structure of Level-2 Tobacco Data Set"
head(tobacco)
#Note there is a discrepancy here - the text had N = 527. We appear to be missing one congress person from California. Otherwise the data match exactly.

#Visualisations

##Figure 2.2 "Average Pro-Tobacco Vote Percentage by Congress Members - 1997 to 2000"

##Need to install package for Alaska and Hawaii
#install.packages("devtools")
#devtools::install_github("wmurphyrd/fiftystater")
require("fiftystater")
vote.by.state <- tobacco %>% group_by(state) %>% 
  summarise(Avg = mean(VotePct)) %>% 
  mutate(statename = tolower(state.name[match(state, state.abb)]))

state.plot <- ggplot(vote.by.state, aes(map_id = statename)) + 
  # map points to the fifty_states shape data
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
state.plot

#Alaska and Hawaii labels don't play nice. Will come back to this. https://cran.r-project.org/web/packages/fiftystater/vignettes/fiftystater.html

##Figure 2.3 "Relationship of Money and Voting % for 5 Large States"

five.states.plot <- tobacco %>% 
filter(state == "CA" |state ==  "FL" |state ==  "IL" |state ==  "NY" |state ==  "TX") %>% 
ggplot(aes(x = pactotal, y = VotePct, shape = state, linetype = state)) + 
geom_point() + ylim(0,1) + xlim(0,80) + labs(x = "PAC Contributions ($K)", y = "Pro-Tobacco Voting (%)") +
stat_smooth(method = "lm", se =FALSE, fullrange = T, color = "black", size=.3) + theme_classic() 
five.states.plot

#Graph mirrors that of the book very closely.

##Figure 2.4 "OLS Fits of Voting % and Money for 50 States"

#Note this may take some time to run
ols.states.plot <- ggplot(tobacco, aes(x = pactotal, y = VotePct)) + geom_point(shape=1, size =.5) +
ylim(0,1) + xlim(0,100) +
stat_smooth(method = "lm", se =FALSE, fullrange = T, color = "black", size=.3) + 
facet_wrap( ~ state) + theme_classic() +
labs(x = "PAC Money", y = "Vote %")
ols.states.plot

#Aesthetics aren't exactly the same, but OLS fits are more or less identical and now in alphabetical order.

#Model Fitting
#This is implemented in lme4, rather than nmle. Slight differences in estimation are evident, but results are similar.

##Table 2.5 "Partial R Output for the Null Model"

null.fit <- lmer(VotePct ~ 1 + (1 | state), tobacco, REML = F)
summary(null.fit)

#Model fits differ somewhat, but result is extremely close.

##Table 2.7 "Partial R Output for Model 1"

model1.fit <- lmer(VotePct ~ 1+ pactotal + party + (pactotal + party | state), tobacco, REML = F)
summary(model1.fit)

#Results are similar.

##Table 2.9a "Parameter Estimates and Model Fit for Three Models"

model2.fit <- lmer(VotePct ~ 1 + pactotal + party + acres + (pactotal + party | state), tobacco, REML = F)
summary(model2.fit)

#Fixed effects estimates and variance components are roughly similar. lmerTest provides significance tests for the former, which resemble published results, with the exception of 'acres' variable which is here non-significant. Neither lmerTest nor lme4 provide significance tests for variance components.
#Difficult to say why differences emerge - may be due to software (HLM providing the published estimates?) implementing a different form of test. Need to double check this.

##Table 2.9b "Parameter Estimates and Model Fit for Three Models"

model3.fit <- lmer(VotePct ~ 1 + pactotal + party + acres + pactotal*acres + party*acres + (pactotal + party | state), tobacco, REML = F)
summary(model3.fit)

#Again, t-value and significance levels differ from published results. Unclear why this may be, although lme4 provides a warning about rescaling the variable (acres ranges from many observations of zero to >1,000).

#More Visualisations

##Figure 2.5 "Predicted Pro-Tobacco Voting Percent for Democrats by Amount of Tobacco Acreage"
#NOT RUN - COME BACK TO THIS
#model3.re <- ranef(model3.fit) %>% .$state
#tobacco %>% group_by(state) %>% 
#  summarise(mean=mean(acres)) %>% 
#  mutate(quantile = cut(mean, breaks = c(0,0.1,200,500), labels = F, include.lowest = T))
#Currently not implemented - still need to determine how cut-offs between no acreage, moderate acreage and high acreage were determined by Luke (2004).

##Figure 2.6 "Boxplots of Within-State residuals for Model 3"

residual.plot <- ggplot(model3.fit, aes(y = residuals(model3.fit), x = reorder(state, desc(state)))) +
  geom_boxplot(outlier.shape = 1, size = .4) + coord_flip() + geom_hline(yintercept = 0) +
  theme_classic() + labs(x = "State", y = "Residuals")
residual.plot

#Now in alphabetical order. Visualisation is approximately the same.

##Figure 2.7 "Scatterplot of Standardised Residuals by Fitted Values for Model 3 by Party"

std.residual.plot <- ggplot(model3.fit, aes(.fitted, .resid)) + geom_point(shape = 1) +
  facet_wrap(~ party) + theme_bw() + theme(panel.grid.minor = element_blank()) +
  geom_text(aes(label=ifelse(.resid>0.5 | .resid <=-0.5,as.character(state),'')),hjust=0,vjust=0) +
  labs(x="Fitted Values",y="Standardised Values")
std.residual.plot

#Visualisation is approximately the same.

##Figure 2.8 "Normal QQ-Plot of Residuals of Model 3 by Party"

qplot.party <- ggplot(model3.fit, aes(sample = residuals(model3.fit, scaled = FALSE))) + 
  geom_point(stat = "qq", shape = 1) + coord_flip() + facet_wrap(~party) +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  labs(x="Quantiles of Standard Normal", y="Residuals")
qplot.party