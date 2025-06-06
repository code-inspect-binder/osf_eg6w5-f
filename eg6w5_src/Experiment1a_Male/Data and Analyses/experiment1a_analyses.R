### Experiment 1a - Compromise and Asymmetric Decoys in Hiring
### Last updated: 121317

# R version 3.4.2 (2017-09-28)
# load packages
if(!require(car)) install.packages("car"); require(car) # version 2.1.6
if(!require(effects)) install.packages("effects"); require(effects) # version 4.0.0
if(!require(emmeans)) install.packages("emmeans"); require(emmeans) # version 0.9.1
if(!require(Hmisc)) install.packages("Hmisc"); require(Hmisc) # version 4.0.3
if(!require(lme4)) install.packages("lme4"); require(lme4) # version 1.1.14
if(!require(lmerTest)) install.packages("lmerTest"); require(lmerTest) # version 2.0.36
if(!require(MuMIn)) install.packages("MuMIn"); require(MuMIn) # version 1.40.0
if(!require(reshape2)) install.packages("reshape2"); require(reshape2) # version 1.4.2
if(!require(scales)) install.packages("scales"); require(scales) # version 0.5.0

# clear workspace environment
rm(list=ls())

# import data
dat <- read.csv("experiment1a_data.csv")
str(dat)
names(dat)
dat$subject <- as.factor(dat$subject) # make subject a factor

## participant demographics
nlevels(dat$subject) # total number of participants
table(dat$sex) # participant sex
round(mean(dat$age, na.rm = TRUE), 2) # mean age
round(sd(dat$age, na.rm = TRUE), 2) # age sd

# number of participants in each condition
table(dat$condition)

# melt data into long format for likelihood of hiring
dat.longh <- melt(dat, id.vars=c("subject", "condition", "responseorder"), 
                 measure.vars=c("h.pity.target", "h.envy.target", "h.pityAD", "h.envyAD", "h.pityC", "h.envyC"), na.rm = FALSE)
names(dat.longh)[names(dat.longh)=="variable"] <- "target"
names(dat.longh)[names(dat.longh)=="value"] <- "rating"
str(dat.longh)

# mean ratings of targets and decoys by condition
tapply(dat.longh$rating, list(dat.longh$condition, dat.longh$target), mean)
tapply(dat.longh$rating, list(dat.longh$condition, dat.longh$target), function(x) sd(x)/sqrt(length(x)))

# only interested in ratings of pity and envy targets, so subset
dat.longh1 <- dat.longh[dat.longh$target %in% c("h.pity.target","h.envy.target"), ]
dat.longh1 <- droplevels.data.frame(dat.longh1)

## fit linear mixed effects models for likelihood of hiring
# null model
hirelmer0 <- lmer(rating ~ 1 + (1|subject), data = dat.longh1, REML = FALSE)
summary(hirelmer0)

# reduced model with no interaction
hirelmer1 <- lmer(rating ~ target + condition + (1|subject), data = dat.longh1, REML = FALSE)
summary(hirelmer1)
lmerTest::anova(hirelmer1)

# hypothesized model
hirelmer2 <- lmer(rating ~ target*condition + (1|subject), data = dat.longh1, REML = FALSE)
summary(hirelmer2)
lmerTest::anova(hirelmer2)
plot(allEffects(hirelmer2))

anova(hirelmer0, hirelmer2) # LRT, hypothesized model better fit than null
anova(hirelmer1, hirelmer2) # LRT, hypothesized model beter fit than reduced model

# checking to see if response order has an effect
hirelmer3 <- lmer(rating ~ target*condition + responseorder + (1|subject), data = dat.longh1, REML = FALSE)
summary(hirelmer3)
lmerTest::anova(hirelmer3)

anova(hirelmer2, hirelmer3) # LRT, response order does improve model fit

# checking to see if response order interacts with target
hirelmer4 <- lmer(rating ~ target*condition + target*responseorder + (1|subject), data = dat.longh1, REML = FALSE)
summary(hirelmer4)
lmerTest::anova(hirelmer4)

anova(hirelmer3, hirelmer4) # LRT, target*responseorder interaction does improve model fit

# checking for three-way interaction
hirelmer5 <- lmer(rating ~ target*condition*responseorder + (1|subject), data = dat.longh1, REML = FALSE)
summary(hirelmer5)
lmerTest::anova(hirelmer5)

anova(hirelmer4, hirelmer5) # LRT, three-way interaction doesn't improve model fit

# best model: two, two-way interaction model 
summary(hirelmer4)
round(r.squaredGLMM(hirelmer4)[1], 3)
lmerTest::anova(hirelmer4) # Satterthwaite approx for df
lmerTest::anova(hirelmer4, ddf = "Kenward-Roger") # Kenward-Roger approx for df (no different than Satterthwaite)
plot(allEffects(hirelmer4)) # preliminary plot for visualization

# normality of residuals
qqnorm(resid(hirelmer4))
qqline(resid(hirelmer4))

# paired contrasts on estimated marginal means to unpack interactions
emm_options(lmer.df = "satterthwaite")

# target x condition
hirelmer4.em <- emmeans::emmeans(hirelmer4, specs = ~ target*condition | target)
hirelmer4.em
contrast(hirelmer4.em, method = "trt.vs.ctrl", adjust = "none") # paired contrasts with no correction
confint(contrast(hirelmer4.em, method = "trt.vs.ctrl", adjust = "none")) # 95% confidence intervals
contrast(hirelmer4.em, method = "trt.vs.ctrl", adjust = "holm") # paired contrasts with holm correction
s <- as.data.frame(summary(hirelmer4.em))

# Cohen's d: dividing emmeans contrasts by residual standard deviation
residSD <- attr(VarCorr(hirelmer4), "sc") # residual standard deviation
hirelmer4.em.effsize <- as.data.frame(summary(contrast(hirelmer4.em, method = "trt.vs.ctrl", adjust = "none")))[c("contrast", "target", "estimate")]
hirelmer4.em.effsize$estimate <- hirelmer4.em.effsize$estimate / residSD
hirelmer4.em.effsize

# target x response order
hirelmer4.emorder <- emmeans::emmeans(hirelmer4, specs = ~ target*responseorder | target)
hirelmer4.emorder
contrast(hirelmer4.emorder, method = "pairwise", adjust = "none")
contrast(hirelmer4.emorder, method = "pairwise", adjust = "holm")
confint(contrast(hirelmer4.emorder, method = "pairwise", adjust = "none"))

# Cohen's d: dividing emmeans contrasts by residual standard deviation
hirelmer4.emorder.effsize <- as.data.frame(summary(contrast(hirelmer4.emorder, method = "trt.vs.ctrl", adjust = "none")))[c("contrast", "target", "estimate")]
hirelmer4.emorder.effsize$estimate <- hirelmer4.emorder.effsize$estimate / residSD
hirelmer4.emorder.effsize

# mean difference between candidates in each condition
round(s[s$target == 'h.envy.target' & s$condition == 'Baseline', "emmean"] - s[s$target == 'h.pity.target' & s$condition == 'Baseline', "emmean"],3)
round(s[s$target == 'h.envy.target' & s$condition == 'PityAD', "emmean"] - s[s$target == 'h.pity.target' & s$condition == 'PityAD', "emmean"],3)
round(s[s$target == 'h.envy.target' & s$condition == 'PityC', "emmean"] - s[s$target == 'h.pity.target' & s$condition == 'PityC', "emmean"],3)
round(s[s$target == 'h.envy.target' & s$condition == 'EnvyAD', "emmean"] - s[s$target == 'h.pity.target' & s$condition == 'EnvyAD', "emmean"],3)
round(s[s$target == 'h.envy.target' & s$condition == 'EnvyC', "emmean"] - s[s$target == 'h.pity.target' & s$condition == 'EnvyC', "emmean"],3)

## fit linear mixed effects models for willingness-to-pay
# melt data into long format for WTP
dat.longw <- melt(dat, id.vars=c("subject", "condition", "responseorder"), 
                  measure.vars=c("w.pity.target", "w.envy.target", "w.pityAD", "w.envyAD", "w.pityC", "w.envyC"), na.rm = FALSE)
names(dat.longw)[names(dat.longw)=="variable"] <- "target"
names(dat.longw)[names(dat.longw)=="value"] <- "rating"
str(dat.longw)
head(dat.longw)

# mean WTP of targets and decoys by condition
tapply(dat.longw$rating, list(dat.longw$condition, dat.longw$target), mean)

# transform WTP by adding 1 and taking log
hist(dat.longw$rating)
symbox(dat.longw$rating + 1)
dat.longw$rating.sqrt <- sqrt(dat.longw$rating)
hist(dat.longw$rating.sqrt)

# only interested in ratings of pity and envy targets, so subset
dat.longw1 <- dat.longw[dat.longw$target %in% c("w.envy.target","w.pity.target"), ]
dat.longw1 <- droplevels.data.frame(dat.longw1)

## fit linear mixed effects models for WTP
# null model
WTPfit0 <- lmer(rating.sqrt ~ 1 + (1|subject), data = dat.longw1, REML = FALSE)
summary(WTPfit0)

# reduced model with no interaction
WTPfit1 <- lmer(rating.sqrt ~ target + condition + (1|subject), data = dat.longw1, REML = FALSE)
summary(WTPfit1)
lmerTest::anova(WTPfit1)

# hypothesized model
WTPfit2 <- lmer(rating.sqrt ~ target*condition + (1|subject), data = dat.longw1, REML = FALSE)
summary(WTPfit2)
lmerTest::anova(WTPfit2)

anova(WTPfit0, WTPfit2) # LRT, hypothesized model is a better fit than null
anova(WTPfit1, WTPfit2) # LRT, reduced model is a better fit than hypothesized model

# checking if response order has a significant effect
WTPfit3 <- lmer(rating.sqrt ~ target + condition + responseorder + (1|subject), data = dat.longw1, REML = FALSE)
summary(WTPfit3)
lmerTest::anova(WTPfit3)

anova(WTPfit1, WTPfit3) # LRT, responseorder doesn't improve model fit

# best model: reduced model with no interaction 
summary(WTPfit1)
round(r.squaredGLMM(WTPfit1)[1], 3)
lmerTest::anova(WTPfit1) # Satterthwaite approx for df
lmerTest::anova(WTPfit1, ddf = "Kenward-Roger") # Kenward-Roger approx for df (no different than Satterthwaite)
plot(allEffects(WTPfit1)) # preliminary plot for visualization

# normality of residuals
qqnorm(resid(WTPfit1))
qqline(resid(WTPfit1))

# estimated marginal means
WTPfit1.em <- emmeans::emmeans(WTPfit1, specs = ~ target)
WTPfit1.em
contrast(WTPfit1.em, method = "trt.vs.ctrl", adjust = "none") # paired contrasts with no correction
confint(contrast(WTPfit1.em, method = "trt.vs.ctrl", adjust = "none")) # 95% confidence intervals
contrast(WTPfit1.em, method = "trt.vs.ctrl", adjust = "holm") # paired contrasts with holm co

## figures
forfig <- s[c("target", "condition", "emmean", "lower.CL", "upper.CL")]
forfig

# plot
# pdf("experiment1a_figure.pdf", width = 8.5, height = 8.5)
op <- par(mar=c(5,6,4,2))
plot(1, ylim = c(4.5,6.5), xlim = c(0,3), ylab = "", xlab = "", xaxt = "n", yaxt = "n", cex.axis = 2.5, ann = FALSE)
mgp.axis(side = 1, at = c(0:3), labels = c("PityAD", "PityC", "EnvyAD", "EnvyC"), cex.axis = 2, line = 0)
mgp.axis(side = 2, cex.axis = 2, line = 0, las = 1)
title(ylab = "Likelihood of Hiring", line=3.5, cex.lab=2)
title(xlab = "Decoy Present", line=3, cex.lab=2)
polygon(c(-1,-1,4,4), c(forfig[forfig$target == 'h.pity.target' & forfig$condition == 'Baseline', "lower.CL"],
                        forfig[forfig$target == 'h.pity.target' & forfig$condition == 'Baseline', "upper.CL"],
                        forfig[forfig$target == 'h.pity.target' & forfig$condition == 'Baseline', "upper.CL"],
                        forfig[forfig$target == 'h.pity.target' & forfig$condition == 'Baseline', "lower.CL"]),
                        col = adjustcolor("gray90",alpha.f=0.5), border = "gray70")
polygon(c(-1,-1,4,4), c(forfig[forfig$target == 'h.envy.target' & forfig$condition == 'Baseline', "lower.CL"],
                        forfig[forfig$target == 'h.envy.target' & forfig$condition == 'Baseline', "upper.CL"],
                        forfig[forfig$target == 'h.envy.target' & forfig$condition == 'Baseline', "upper.CL"],
                        forfig[forfig$target == 'h.envy.target' & forfig$condition == 'Baseline', "lower.CL"]),
                        col = adjustcolor("gray90",alpha.f=0.5), border = "gray70")
abline(h = forfig[forfig$target == 'h.pity.target' & forfig$condition == 'Baseline', "emmean"], lty = 3, lwd = 3)
abline(h = forfig[forfig$target == 'h.envy.target' & forfig$condition == 'Baseline', "emmean"], lty = 3, lwd = 3)
arrows(0:3, forfig[forfig$target == 'h.pity.target' & forfig$condition %in% c('PityAD', 'PityC', 'EnvyAD', 'EnvyC'), "lower.CL"][c(3, 4, 1, 2)],
       0:3, forfig[forfig$target == 'h.pity.target' & forfig$condition %in% c('PityAD', 'PityC', 'EnvyAD', 'EnvyC'), "upper.CL"][c(3, 4, 1, 2)],
       length=0, angle=90, code=3, col = alpha(col = "firebrick1", 0.8), lwd = 3)
arrows(0:3, forfig[forfig$target == 'h.envy.target' & forfig$condition %in% c('PityAD', 'PityC', 'EnvyAD', 'EnvyC'), "lower.CL"][c(3, 4, 1, 2)],
       0:3, forfig[forfig$target == 'h.envy.target' & forfig$condition %in% c('PityAD', 'PityC', 'EnvyAD', 'EnvyC'), "upper.CL"][c(3, 4, 1, 2)],
       length=0, angle=90, code=3, col = alpha(col = "royalblue1", 0.8), lwd = 3)
points(x= 2:3, y = forfig[forfig$target == 'h.pity.target' & forfig$condition %in% c('EnvyAD', 'EnvyC'), "emmean"], pch = 16, col = alpha(col = "firebrick1", 1), cex = 3)
points(x= 0:1, y = forfig[forfig$target == 'h.pity.target' & forfig$condition %in% c('PityAD', 'PityC'), "emmean"], pch = 16, col = alpha(col = "firebrick1", 1), cex = 3)
points(x= 2:3, y = forfig[forfig$target == 'h.envy.target' & forfig$condition %in% c('EnvyAD', 'EnvyC'), "emmean"], pch = 16, col = alpha(col = "royalblue1", 1), cex = 3)
points(x= 0:1, y = forfig[forfig$target == 'h.envy.target' & forfig$condition %in% c('PityAD', 'PityC'), "emmean"], pch = 16, col = alpha(col = "royalblue1", 1), cex = 3)
text(1.5, forfig[forfig$target == 'h.pity.target' & forfig$condition == 'Baseline', "emmean"] - 0.08, "Pity Baseline", cex = 2)
text(1.5, forfig[forfig$target == 'h.envy.target' & forfig$condition == 'Baseline', "emmean"] - 0.08, "Envy Baseline", cex = 2)
points(x = 0, y = 6.45, pch = 16, col = alpha(col = "firebrick1"), cex = 2)
text(x = 0.1, y = 6.45, "Pity Candidate", cex = 2, adj = c(0,NA))
points(x = 0, y = 6.35, pch = 16, col = alpha(col = "royalblue1"),cex = 2)
text(x = 0.1, y = 6.35, "Envy Candidate", cex = 2, adj = c(0,NA))
text(x = 3, y = 6.45, "Experiment 1a", cex = 2.2, adj = c(1,NA))
segments(0.1, forfig[forfig$target == 'h.pity.target' & forfig$condition == 'PityAD', "emmean"], 
         0.1, forfig[forfig$target == 'h.pity.target' & forfig$condition == 'Baseline', "emmean"] + 0.03, lwd = 2)
text(0.15, (((forfig[forfig$target == 'h.pity.target' & forfig$condition == 'PityAD', "emmean"]) +
              (forfig[forfig$target == 'h.pity.target' & forfig$condition == 'Baseline', "emmean"] + 0.03)) /2),
     "**", cex = 2, adj = c(0, NA))
segments(1.1, forfig[forfig$target == 'h.pity.target' & forfig$condition == 'PityC', "emmean"], 
         1.1, forfig[forfig$target == 'h.pity.target' & forfig$condition == 'Baseline', "emmean"] + 0.03, lwd = 2)
text(1.15, (((forfig[forfig$target == 'h.pity.target' & forfig$condition == 'PityC', "emmean"]) +
               (forfig[forfig$target == 'h.pity.target' & forfig$condition == 'Baseline', "emmean"] + 0.03)) /2),
     "*", cex = 2, adj = c(0, NA))
par(op)
# dev.off()