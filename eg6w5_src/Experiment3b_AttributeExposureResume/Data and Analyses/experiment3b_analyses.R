### Experiment 3b - Manipulating Attribute Exposure via Resume-based Cues
### Last updated: 013118

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

# import exposure trial data
expdat <- read.csv("experiment3b_expdata.csv")
str(expdat)
names(expdat)
expdat$subject <- as.factor(expdat$subject) # make subject a factor

# exclusion criterion 1: anyone who answers more than 10% of trials (>3) incorrectly gets excluded
expdat$check <- NA
expdat$check[which(expdat$trial.type == "Which candidate is more competent?" & expdat$choice == "envy.target")] <- "correct"
expdat$check[which(expdat$trial.type == "Which candidate is more competent?" & expdat$choice == "pity.target")] <- "incorrect"
expdat$check[which(expdat$trial.type == "Which candidate is more warm?" & expdat$choice == "pity.target")] <- "correct"
expdat$check[which(expdat$trial.type == "Which candidate is more warm?" & expdat$choice == "envy.target")] <- "incorrect"

exposurecheck <- table(expdat$subject, expdat$check)
failmanip <- names(which(exposurecheck[,2] > 3)) # subjects who failed
length(failmanip) # 366 participants did not pass exclusion
expdat <- expdat[!(expdat$subject %in% failmanip), ] # remove those participants
expdat <- droplevels.data.frame(expdat)

# exclusion criterion 2: anyone that doesn't have 30 exposure trials gets excluded
trialnumcheck <- table(expdat$subject)
not30 <- names(which(trialnumcheck != 30))
length(not30) # 14 participants did not have 30 exposure trials total
expdat <- expdat[!(expdat$subject %in% not30), ] # remove those participants
expdat <- droplevels.data.frame(expdat)

# import likelihood of hiring data
dat <- read.csv("experiment3b_hiredata.csv")
dat$subject <- as.factor(dat$subject)
dat$condition <- as.factor(dat$condition)

# only use subjects that passed exclusion criteria 1 & 2
dat <- dat[(dat$subject %in% expdat$subject), ]
dat <- droplevels.data.frame(dat)
str(dat)

# exclusion criterion 3: anyone who did not report likelihood of hiring for both candidates
hiringcheck <- table(dat$subject)
notboth <- names(which(hiringcheck != 2))
length(notboth) # 1 participants did not report likelihood of hiring for both candidates
dat <- dat[!(dat$subject %in% notboth), ] # remove those participants
dat <- droplevels.data.frame(dat)

# import demographic data
demodat <- read.csv("experiment3b_demodata.csv")
demodat$subject <- as.factor(demodat$subject)

# only use demographic data of subject included in final sample
demodat <- demodat[(demodat$subject %in% dat$subject), ]
demodat <- droplevels.data.frame(demodat)
str(demodat)

# participant demographics
nlevels(demodat$subject) # total number of participants
table(demodat$sex) # participant sex
round(mean(demodat$age, na.rm = TRUE), 2) # mean age
round(sd(demodat$age, na.rm = TRUE), 2) # age sd
table(demodat$employ) # currently employed or employed in last 6 months
round(table(demodat$employ)/sum(table(demodat$employ)),3) 
table(demodat$resume) # experience evaluating resumes
round(table(demodat$resume)/sum(table(demodat$resume)),3) 

# add employment and resume information to dat
dat$employ <- demodat$employ[match(dat$subject, demodat$subject)]
dat$resume <- demodat$resume[match(dat$subject, demodat$subject)]

# number of participants in each condition
table(dat$condition)/2

# mean ratings of targets by condition
tapply(dat$rating, list(dat$condition, dat$target), mean)
tapply(dat$rating, list(dat$condition, dat$target), function(x) sd(x)/sqrt(length(x)))

## fit linear mixed effects models

# null model
fitlmer0 <- lmer(rating ~ 1 + (1|subject), data = dat, REML = FALSE)
summary(fitlmer0)

# reduced model with no interaction
fitlmer1 <- lmer(rating ~ target + condition + (1|subject), data = dat, REML = FALSE)
summary(fitlmer1)
lmerTest::anova(fitlmer1)

# hypothesis-driven model with two-way interaction
fitlmer2 <- lmer(rating ~ target*condition + (1|subject), data = dat, REML = FALSE)
summary(fitlmer2)
round(r.squaredGLMM(fitlmer2)[1], 3)
lmerTest::anova(fitlmer2) # Satterthwaite approx for df
lmerTest::anova(fitlmer2, ddf = "Kenward-Roger") # Kenward-Roger approx for df (no different than Satterthwaite)
plot(allEffects(fitlmer2)) # preliminary plot for visualization

# does experience evaluating resumes matter?
fitlmer3 <- lmer(rating ~ target*condition*resume + (1|subject), data = dat, REML = FALSE)
summary(fitlmer3)
lmerTest::anova(fitlmer3)

# does being employed matter?
fitlmer4 <- lmer(rating ~ target*condition*employ + (1|subject), data = dat, REML = FALSE)
summary(fitlmer4)
lmerTest::anova(fitlmer4)

# normality of residuals
qqnorm(resid(fitlmer2))
qqline(resid(fitlmer2))

# LRT for model fit
anova(fitlmer0, fitlmer2) # hypothesis-driven model better than null model
anova(fitlmer1, fitlmer2) # hypothesis-driven model better than reduced model
anova(fitlmer2, fitlmer3) # hypothesis-driven model better than resume model
anova(fitlmer2, fitlmer4) # hypothesis-driven model better than employment model

# paired contrasts on estimated marginal means to unpack interaction
emm_options(lmer.df = "satterthwaite")
fitlmer2.em <- emmeans::emmeans(fitlmer2, specs = ~ target*condition | target)
fitlmer2.em # estimated marginal means
contrast(fitlmer2.em, method = "pairwise", adjust = "none") # paired contrasts with no correction
confint(contrast(fitlmer2.em, method = "pairwise", adjust = "none")) # 95% confidence intervals
contrast(fitlmer2.em, method = "pairwise", adjust = "holm") # paired contrasts with holm correction
s <- as.data.frame(summary(fitlmer2.em))

# mean difference between candidates
round(s[s$target == 'envy.target' & s$condition == 'competence-bias', "emmean"] - s[s$target == 'pity.target' & s$condition == 'competence-bias', "emmean"],3)
round(s[s$target == 'envy.target' & s$condition == 'equal', "emmean"] - s[s$target == 'pity.target' & s$condition == 'equal', "emmean"],3)
round(s[s$target == 'envy.target' & s$condition == 'warmth-bias', "emmean"] - s[s$target == 'pity.target' & s$condition == 'warmth-bias', "emmean"],3)

# Cohen's d: dividing emmeans contrasts by residual standard deviation
residSD <- attr(VarCorr(fitlmer2), "sc")
fitlmer2.effsize <- as.data.frame(summary(contrast(fitlmer2.em, method = "pairwise", adjust = "none")))[c("contrast", "target", "estimate")]
fitlmer2.effsize$estimate <- fitlmer2.effsize$estimate / residSD
fitlmer2.effsize

# figure
forfig <- s[c("target", "condition", "emmean", "lower.CL", "upper.CL")]
forfig

# plot
# pdf("experiment3b_figure.pdf", width = 8.5, height = 8.5)
op <- par(mar=c(5,6,4,2))
plot(1, ylim = c(4.25,6.75), xlim = c(0,2), ylab = "", xlab = "", xaxt = "n", yaxt = "n", cex.axis = 2.5, ann = FALSE)
mgp.axis(side = 1, at = c(0.5, 1, 1.5), labels = c("Warmth-biased", "Equal", "Competence-biased"), cex.axis = 1.4, line = 0)
mgp.axis(side = 2, cex.axis = 2, line = 0, las = 1)
title(ylab = "Likelihood of Hiring", line=3.5, cex.lab=2)
title(xlab = "Exposure Condition", line=3, cex.lab=2)
arrows(c(0.5,1.05,1.5), forfig[forfig$target == 'pity.target' & forfig$condition %in% c('warmth-bias', 'equal', 'competence-bias'), "lower.CL"][c(3, 2, 1)],
       c(0.5,1.05,1.5), forfig[forfig$target == 'pity.target' & forfig$condition %in% c('warmth-bias', 'equal', 'competence-bias'), "upper.CL"][c(3, 2, 1)],
       length=0, angle=90, code=3, col = alpha(col = "firebrick1", 0.8), lwd = 3)
arrows(c(0.5,0.95,1.5), forfig[forfig$target == 'envy.target' & forfig$condition %in% c('warmth-bias', 'equal', 'competence-bias'), "lower.CL"][c(3, 2, 1)],
       c(0.5,0.95,1.5), forfig[forfig$target == 'envy.target' & forfig$condition %in% c('warmth-bias', 'equal', 'competence-bias'), "upper.CL"][c(3, 2, 1)],
       length=0, angle=90, code=3, col = alpha(col = "royalblue1", 0.8), lwd = 3)
points(x= 0.5, y = forfig[forfig$target == 'pity.target' & forfig$condition == 'warmth-bias', "emmean"], pch = 16, col = alpha(col = "firebrick1", 1), cex = 3)
points(x= 0.5, y = forfig[forfig$target == 'envy.target' & forfig$condition == 'warmth-bias', "emmean"], pch = 16, col = alpha(col = "royalblue1", 1), cex = 3)
points(x= 1.05, y = forfig[forfig$target == 'pity.target' & forfig$condition == 'equal', "emmean"], pch = 16, col = alpha(col = "firebrick1", 1), cex = 3)
points(x= 0.95, y = forfig[forfig$target == 'envy.target' & forfig$condition == 'equal', "emmean"], pch = 16, col = alpha(col = "royalblue2", 1), cex = 3)
points(x= 1.5, y = forfig[forfig$target == 'pity.target' & forfig$condition == 'competence-bias', "emmean"], pch = 16, col = alpha(col = "firebrick1", 1), cex = 3)
points(x= 1.5, y = forfig[forfig$target == 'envy.target' & forfig$condition == 'competence-bias', "emmean"], pch = 16, col = alpha(col = "royalblue1", 1), cex = 3)
points(x = 0, y = 6.7, pch = 16, col = alpha(col = "firebrick1"), cex = 2)
text(x = 0.1, y = 6.7, "Pity Candidate", cex = 2, adj = c(0,NA))
points(x = 0, y = 6.58, pch = 16,  col = alpha(col = "royalblue1"), cex = 2)
text(x = 0.1, y = 6.58, "Envy Candidate", cex = 2, adj = c(0,NA))
text(x = 2, y = 6.7, "Experiment 3b", cex = 2.2, adj = c(1,NA))
arrows(0.5, 5.43, 0.88, 5.43, code = 3, angle = 90, length = 0.05, lwd = 2)
text(0.69, 5.47, "***", cex = 2, adj = c(0.5, NA))
text(0.69, 5.39, "***", cex = 2, adj = c(0.5, NA))
arrows(1.12, 5.43, 1.5, 5.43, code = 3, angle = 90, length = 0.05, lwd = 2)
text(1.31, 5.47, "***", cex = 2, adj = c(0.5, NA))
text(1.31, 5.39, "***", cex = 2, adj = c(0.5, NA))
segments(0.5, 5.23, 1.5, 5.23, lwd = 2)
segments(0.5, 5.23, 0.5, 5.27, lwd = 2)
segments(1.5, 5.23, 1.5, 5.19, lwd = 2)
text(1, 5.19, "***", cex = 2, adj = c(0.5, NA))
segments(0.5, 5.63, 1.5, 5.63, lwd = 2)
segments(0.5, 5.63, 0.5, 5.60, lwd = 2)
segments(1.5, 5.63, 1.5, 5.66, lwd = 2)
text(1, 5.67, "***", cex = 2, adj = c(0.5, NA))
par(op)
# dev.off()