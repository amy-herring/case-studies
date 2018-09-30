library(tidyverse)
library(lme4)
library(brms)
library(rstan)
#devtools::install_github("hrbrmstr/albersusa")
library(albersusa) #plotting
library(cowplot) # plotting
library(dplyr)
library(directlabels)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

setwd("~/Documents/GitHub/case-studies/decks")
marriage.data=foreign::read.dta('data/gay_marriage_megapoll.dta',convert.underscore=TRUE)
Statelevel=foreign::read.dta('data/state_level_update.dta',convert.underscore=TRUE)
Census=foreign::read.dta('data/poststratification 2000.dta',convert.underscore=TRUE)

Statelevel=Statelevel %>% rename(state=sstate) #rename to state

marriage.data=Statelevel %>%
  select(state,p.evang,p.mormon,kerry.04) %>%
  right_join(marriage.data)

# combine demographic groups and label them
marriage.data$race.female <- (marriage.data$female *3) + marriage.data$race.wbh
marriage.data$race.female=factor(marriage.data$race.female,levels=1:6,
                                 labels=c("WhMale","BlMale","HMale","WhFem","BlFem","HFem"))
marriage.data$age.edu.cat <- 4 * (marriage.data$age.cat -1) + marriage.data$edu.cat
marriage.data$age.edu.cat=factor(marriage.data$age.edu.cat,levels=1:16,
                                 labels=c("18-29,<HS","18-29,HS","18-29,SC","18-29,CG",
                                          "30-44,<HS","30-44,HS","30-44,SC","30-44,CG",
                                          "45-64,<HS","45-64,HS","45-64,SC","45-64,CG",
                                          "65+,<HS","65+,HS","65+,SC","65+,CG"))

marriage.data$p.evang <- Statelevel$p.evang[marriage.data$state.initnum]
# proportion of evangelicals in respondent's state
marriage.data$p.mormon <-Statelevel$p.mormon[marriage.data$state.initnum]
# proportion of LDS church members in respondent's state
marriage.data$p.relig <- marriage.data$p.evang + marriage.data$p.mormon
# combined evangelical + LDS proportions
marriage.data$kerry.04 <- Statelevel$kerry.04[marriage.data$state.initnum]
# John Kerry's % of 2-party vote in respondent's state in 2004
marriage.data <- marriage.data %>%
  filter(state!="")

# Census variables
Census<-Census %>%
  rename(state=cstate, age.cat=cage.cat, edu.cat=cedu.cat,region=cregion)
Census$race.female <- (Census$cfemale *3) + Census$crace.WBH 
Census$race.female=factor(Census$race.female,levels=1:6,
                           labels=c("WhMale","BlMale","HMale","WhFem","BlFem","HFem"))
Census$age.edu.cat <- 4 * (Census$age.cat-1) + Census$edu.cat 
Census$age.edu.cat=factor(Census$age.edu.cat,levels=1:16,
                           labels=c("18-29,<HS","18-29,HS","18-29,SC","18-29,CG",
                                    "30-44,<HS","30-44,HS","30-44,SC","30-44,CG",
                                    "45-64,<HS","45-64,HS","45-64,SC","45-64,CG",
                                    "65+,<HS","65+,HS","65+,SC","65+,CG"))
Census <- Statelevel %>%
  select(state,p.evang,p.mormon,kerry.04) %>%
  right_join(Census)
Census <- Census %>% mutate(p.relig=p.evang+p.mormon)

# Get state averages
mod.disag=marriage.data%>%
  group_by(state) %>%
  summarise(support=mean(yes.of.all)) %>%
  mutate(model="no_ps")

# Find average within each group

grp.means<-marriage.data%>% 
  group_by(state,region,race.female,age.edu.cat,p.relig,kerry.04) %>%
  summarize(support=mean(yes.of.all,na.rm=TRUE))

# add group % in each state
grp.means<- Census %>%
  select(state, region, kerry.04, race.female, age.edu.cat, p.relig, cpercent.state) %>%
  right_join(grp.means)

# sum scaled average and get total state averages

mod.disag.ps<-grp.means %>%
  group_by(state) %>%
  summarize(support=sum(support * cpercent.state, na.rm=TRUE)) %>%
  mutate(model="ps")



#compare poststratified and empirical means -- nice plot!
disag.point <- bind_rows(mod.disag,mod.disag.ps) %>%
  group_by(model) %>%
  arrange(support, .by_group=TRUE) %>%
  ggplot(aes(x=support,y=forcats::fct_inorder(state),color=model)) +
  geom_point() + theme_classic() +
  theme(legend.position='none') +
  directlabels::geom_dl(aes(label=model),method='smart.grid') +
  ylab('state')

disag.point


#make a function so we don't have to type over and over
compare_scat=function(d) {
  return(
    ggplot(data=d, aes(x=support,y=support1))+geom_text(aes(label=state),hjust=0.5,vjust=0.25) +
      geom_abline(slope=1,intercept=0) +
      xlim(c(0,0.7)) + ylim(c(0,0.7)) +
      xlab("support") + ylab("poststrat support") +
      coord_fixed()
  )
}

disag.scat=bind_cols(mod.disag,mod.disag.ps) %>% compare_scat()
plot_grid(disag.point,disag.scat)
# here we see a lot of variation between poststratified averages
# and on right we see poststratified averages tend to be pushed towards zero

#to explore, note within each state each group is a certain % of the total pop
# let's look at total % of demographic groups included in the survey by state

grp.means %>%
  group_by(state) %>%
  summarize(total_percent=sum(cpercent.state, na.rm=TRUE)) %>%
  filter(state != "") %>%
  ggplot(aes(x=state,y=total_percent)) +
  geom_text(aes(label=state),hjust=0.5,vjust=0.25) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  coord_fixed(ratio=8)+ylim(c(0,1.1))

#Ahh, survey does not have responses from each demographic group in each state,
#poststratification is assuming the missing demographic groups have 0% support
# this is not good -- even though we have no black men from SD in the survey
# there are some in the state. We underestimate the level of support by assuming
# no black men in SD support gay marriage.
# One advantage of fitting a multilevel model is that we do have responses from
# black men in nearby states and other US states, which we can use to make
# a better guess about the level of support for gay marriage among black
# men in SD




# Multilevel model fit via ML
# Note not every demographic group sampled w/in each state so would good
# to be able to infer missing demographic groups using support for the group
# outside of the state

ml.mod=glmer(yes.of.all~(1|race.female)+(1|age.edu.cat)+(1|state)+(1|region)+
               p.relig+kerry.04, data=marriage.data,
             family=binomial(link="logit"))

# use predict package to make predictions
# first note nobody from AK or HI in survey
marriage.data %>%
  filter(state=="AK", state=="HI")

#however we can use predict function to estimate opinion not only in the states
#but also broken out by % of demographic groups inside them
#this allows us to do poststratification
# we'll look at these predictions later!
ps.ml.mod <- Census %>%
  mutate(support=predict(ml.mod,newdata=.,allow.new.levels=TRUE,type='response')) %>%
  mutate(support=support*cpercent.state) %>%
  group_by(state) %>%
  summarize(support=sum(support))

# Now we fit a fully Bayesian model, with same data model as the ML model
# but with some weakly informative priors on the SD's of varying intercepts
# that will help with borrowing of information and convergence

bayes.mod=brm(yes.of.all~(1|race.female)+(1|age.edu.cat)+(1|state)+(1|region)+
               p.relig+kerry.04, data=marriage.data,
             family=bernoulli(),
             prior=c(set_prior("normal(0,0.2)",class='b'),
                     set_prior("normal(0,0.2)",class='sd',group="race.female"),
                     set_prior("normal(0,0.2)",class='sd',group="age.edu.cat"),
                     set_prior("normal(0,0.2)",class='sd',group="state"),
                     set_prior("normal(0,0.2)",class='sd',group="region")))

# what are advantages of a Bayesian approach? Most obvious is total accounting
# of uncertainty -- particularly look at deviation of group-level intercepts

library(tidybayes)
ml_sd<- broom::tidy(ml.mod) %>%
  filter(stringr::str_detect(term,"sd_"))

bayes.mod %>%
  gather_samples(`sd.*`,regex=TRUE) %>%
  ungroup() %>%
  mutate(group=stringr::str_replace_all(term,c("sd_"="","__Intercept"=""))) %>%
  ggplot(aes(y=group,x=estimate)) +
  ggridges::geom_density_ridges(aes(height=..density..),
                                rel_min_height=0.01,stat="density",
                                scale=1.5) +
  geom_point(data=ml_sd)
#wow, this is cool!  The dots are the point estimates from the frequentist
# model, but the Bayesian model gives you an idea of the distribution of values, 
# from which we could sample. 

#next let's get the point estimate and poststratify from the Bayesian model

ps.bayes.mod <- bayes.mod %>%
  add_predicted_samples(newdata=Census, allow_new_levels=TRUE) %>%
  rename(support = pred) %>%
  mean_qi() %>%
  mutate(support = support * cpercent.state) %>%
  group_by(state) %>%
  summarize(support = sum(support))

#now moving forward, we will consider comparisons across the 3 approaches
#  let's compare scatter plots:

mod.disag[nrow(mod.disag) + 1,] = list("AK", mean(mod.disag$support), "no_ps")
mod.disag[nrow(mod.disag) + 1,] = list("HI", mean(mod.disag$support), "no_ps")

disag.ml <- bind_cols(mod.disag, ps.ml.mod) %>% compare_scat() +
  xlab("disag model") + ylab("ml mod")
disag.bayes <- bind_cols(mod.disag, ps.bayes.mod) %>% compare_scat() + 
  xlab("disag model") + ylab("bayes model")
ml.bayes <- bind_cols(ps.ml.mod, ps.bayes.mod) %>% compare_scat() + 
  xlab("ml model") + ylab("bayes model")

plot_grid(disag.ml, disag.bayes, ml.bayes)

# here we see we have quite similar predictions from the ML and Bayes models
# note that both models disagree a bit with the disaggregated model

# now let's evaluate prediction taking advantage of the UQ from the Bayesian approach

#sample from posterior to get predicted probabilities for each Census combination
predict_val=predict(bayes.mod, newdata=Census, allow_new_levels=TRUE, 
            nsamples=500, summary=FALSE)
# we can use these predicted probabilities to estimate election outcomes
# under a variety of assumptions (everyone votes, or applying other data
# on how frequently people in each demographic group vote)




