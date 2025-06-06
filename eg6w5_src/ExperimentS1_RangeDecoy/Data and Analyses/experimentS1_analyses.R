### Supplemental Experiment 1 - Range-Asymmetric Dominance Decoys in Hiring
### Last updated: 120117

# R version 3.4.2 (2017-09-28)
# load packages
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
dat <- read.csv("experimentS1_data.csv")
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

# melt data into long format
dat.long <- melt(dat, id.vars=c("subject", "condition"), 
                 measure.vars=c("pity.target", "envy.target", "pityAD", "envyAD"), na.rm = FALSE)
names(dat.long)[names(dat.long)=="variable"] <- "target"
names(dat.long)[names(dat.long)=="value"] <- "rating"
str(dat.long)

# mean ratings of targets and decoys by condition
tapply(dat.long$rating, list(dat.long$condition, dat.long$target), mean)
tapply(dat.long$rating, list(dat.long$condition, dat.long$target), function(x) sd(x)/sqrt(length(x)))

# only interested in ratings of pity and envy targets, so subset
dat.long1 <- dat.long[dat.long$target %in% c("pity.target","envy.target"), ]
dat.long1 <- droplevels.data.frame(dat.long1)

## fit linear mixed effects models
# null model
fitlmer0 <- lmer(rating ~ 1 + (1|subject), data = dat.long1, REML = FALSE)
summary(fitlmer0)

# reduced model with no interaction
fitlmer1 <- lmer(rating ~ target + condition + (1|subject), data = dat.long1, REML = FALSE)
summary(fitlmer1)
lmerTest::anova(fitlmer1)

# hypothesis-driven model with interaction
fitlmer2 <- lmer(rating ~ target*condition + (1|subject), data = dat.long1, REML = FALSE)
summary(fitlmer2)
round(r.squaredGLMM(fitlmer2)[1], 3)
lmerTest::anova(fitlmer2) # Satterthwaite approx for df
lmerTest::anova(fitlmer2, ddf = "Kenward-Roger") # Kenward-Roger approx for df (no different than Satterthwaite)
plot(allEffects(fitlmer2)) # preliminary plot for visualization

# normality of residuals
qqnorm(resid(fitlmer2))
qqline(resid(fitlmer2))

# LRT for model fit
anova(fitlmer0, fitlmer2) # hypothesis-driven model is better than null model
anova(fitlmer1, fitlmer2) # hypothesis-driven model is better than reduced model

# paired contrasts on estimated marginal means to unpack interaction
emm_options(lmer.df = "satterthwaite")
fitlmer2.em <- emmeans::emmeans(fitlmer2, specs = ~ target*condition | target)
fitlmer2.em # estimated marginal means
contrast(fitlmer2.em, method = "trt.vs.ctrl", adjust = "none") # paired contrasts with no correction
confint(contrast(fitlmer2.em, method = "trt.vs.ctrl", adjust = "none")) # 95% confidence intervals
contrast(fitlmer2.em, method = "trt.vs.ctrl", adjust = "holm") # paired contrasts with holm correction
s <- as.data.frame(summary(fitlmer2.em))

# mean difference between candidates in each condition
round(s[s$target == 'envy.target' & s$condition == 'Baseline', "emmean"] - s[s$target == 'pity.target' & s$condition == 'Baseline', "emmean"],3)
round(s[s$target == 'envy.target' & s$condition == 'PityAD', "emmean"] - s[s$target == 'pity.target' & s$condition == 'PityAD', "emmean"],3)
round(s[s$target == 'envy.target' & s$condition == 'EnvyAD', "emmean"] - s[s$target == 'pity.target' & s$condition == 'EnvyAD', "emmean"],3)

# Cohen's d: dividing emmeans contrasts by residual standard deviation
residSD <- attr(VarCorr(fitlmer2), "sc")
fitlmer2.effsize <- as.data.frame(summary(contrast(fitlmer2.em, method = "trt.vs.ctrl", adjust = "none")))[c("contrast", "target", "estimate")]
fitlmer2.effsize$estimate <- fitlmer2.effsize$estimate / residSD
fitlmer2.effsize

## figures
forfig <- s[c("target", "condition", "emmean", "lower.CL", "upper.CL")]
forfig

# plot
# pdf("experimentS1_figure.pdf", width = 8.5, height = 8.5)
op <- par(mar=c(5,6,4,2))
plot(1, ylim = c(4.5,6.5), xlim = c(0,3), ylab = "", xlab = "", xaxt = "n", yaxt = "n", cex.axis = 2.5, ann = FALSE)
mgp.axis(side = 1, at = c(1:2), labels = c("Pity R-AD", "Envy R-AD"), cex.axis = 2, line = 0)
mgp.axis(side = 2, cex.axis = 2, line = 0, las = 1)
title(ylab = "Likelihood of Hiring", line=3.5, cex.lab=2)
title(xlab = "Decoy Present", line=3, cex.lab=2)
polygon(c(-1,-1,4,4), c(forfig[forfig$target == 'pity.target' & forfig$condition == 'Baseline', "lower.CL"],
                        forfig[forfig$target == 'pity.target' & forfig$condition == 'Baseline', "upper.CL"],
                        forfig[forfig$target == 'pity.target' & forfig$condition == 'Baseline', "upper.CL"],
                        forfig[forfig$target == 'pity.target' & forfig$condition == 'Baseline', "lower.CL"]),
                        col = adjustcolor("gray90",alpha.f=0.5), border = "gray70")
polygon(c(-1,-1,4,4), c(forfig[forfig$target == 'envy.target' & forfig$condition == 'Baseline', "lower.CL"],
                        forfig[forfig$target == 'envy.target' & forfig$condition == 'Baseline', "upper.CL"],
                        forfig[forfig$target == 'envy.target' & forfig$condition == 'Baseline', "upper.CL"],
                        forfig[forfig$target == 'envy.target' & forfig$condition == 'Baseline', "lower.CL"]),
                        col = adjustcolor("gray90",alpha.f=0.5), border = "gray70")
abline(h = forfig[forfig$target == 'pity.target' & forfig$condition == 'Baseline', "emmean"], lty = 3, lwd = 3)
abline(h = forfig[forfig$target == 'envy.target' & forfig$condition == 'Baseline', "emmean"], lty = 3, lwd = 3)
arrows(1:2, forfig[forfig$target == 'pity.target' & forfig$condition %in% c('PityAD', 'EnvyAD'), "lower.CL"][c(2,1)],
       1:2, forfig[forfig$target == 'pity.target' & forfig$condition %in% c('PityAD', 'EnvyAD'), "upper.CL"][c(2,1)],
       length=0, angle=90, code=3, col = alpha(col = "firebrick1", 0.8), lwd = 3)
arrows(1:2, forfig[forfig$target == 'envy.target' & forfig$condition %in% c('PityAD', 'EnvyAD'), "lower.CL"][c(2, 1)],
       1:2, forfig[forfig$target == 'envy.target' & forfig$condition %in% c('PityAD', 'EnvyAD'), "upper.CL"][c(2, 1)],
       length=0, angle=90, code=3, col = alpha(col = "royalblue1", 0.8), lwd = 3)
points(x= 2, y = forfig[forfig$target == 'pity.target' & forfig$condition %in% c('EnvyAD'), "emmean"], pch = 16, col = alpha(col = "firebrick1", 1), cex = 3)
points(x= 1, y = forfig[forfig$target == 'pity.target' & forfig$condition %in% c('PityAD'), "emmean"], pch = 16, col = alpha(col = "firebrick1", 1), cex = 3)
points(x= 2, y = forfig[forfig$target == 'envy.target' & forfig$condition %in% c('EnvyAD'), "emmean"], pch = 16, col = alpha(col = "royalblue1", 1), cex = 3)
points(x= 1, y = forfig[forfig$target == 'envy.target' & forfig$condition %in% c('PityAD'), "emmean"], pch = 16, col = alpha(col = "royalblue1", 1), cex = 3)
text(1.5, forfig[forfig$target == 'pity.target' & forfig$condition == 'Baseline', "emmean"] - 0.08, "Pity Baseline", cex = 2)
text(1.5, forfig[forfig$target == 'envy.target' & forfig$condition == 'Baseline', "emmean"] - 0.08, "Envy Baseline", cex = 2)
points(x = 0, y = 6.45, pch = 16, col = alpha(col = "firebrick1"), cex = 2)
text(x = 0.1, y = 6.45, "Pity Candidate", cex = 2, adj = c(0,NA))
points(x = 0, y = 6.35, pch = 16, col = alpha(col = "royalblue1"), cex = 2)
text(x = 0.1, y = 6.35, "Envy Candidate", cex = 2, adj = c(0,NA))
text(x = 3, y = 6.45, "Supplemental", cex = 2.2, adj = c(1,NA))
text(x = 3, y = 6.35, "Experiment 1", cex = 2.2, adj = c(1,NA))
segments(0.9, forfig[forfig$target == 'envy.target' & forfig$condition == 'PityAD', "emmean"], 
         0.9, forfig[forfig$target == 'envy.target' & forfig$condition == 'Baseline', "emmean"] - 0.03, lwd = 2)
text(0.88, (((forfig[forfig$target == 'envy.target' & forfig$condition == 'PityAD', "emmean"]) +
               (forfig[forfig$target == 'envy.target' & forfig$condition == 'Baseline', "emmean"] - 0.03)) /2),
     "***", cex = 2, adj = c(1, NA))
segments(2.1, forfig[forfig$target == 'pity.target' & forfig$condition == 'EnvyAD', "emmean"], 
         2.1, forfig[forfig$target == 'pity.target' & forfig$condition == 'Baseline', "emmean"] - 0.03, lwd = 2)
text(2.12, (((forfig[forfig$target == 'pity.target' & forfig$condition == 'EnvyAD', "emmean"]) +
               (forfig[forfig$target == 'pity.target' & forfig$condition == 'Baseline', "emmean"] - 0.03)) /2),
     "**", cex = 2, adj = c(0, NA))
par(op)
# dev.off()