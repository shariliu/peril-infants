# Shari Liu
# Analysis Script - Infants infer the value of goals from the cost of actions
# Last edited: Sept 7, 2017

## Step 0: Load packages ------------------------------------------------------

# create a function to do this more cleanly
ipak <- function (pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("schoRsch", "ggjoy", "boot", "bootstrap", "irr", "vcd","ggplot2", "lattice", "reshape2", "car", "plyr", "MASS", "lme4", "lmerTest", "lmtest",
              "effects", "multcomp", "simr","tibble", "influence.ME", "lsmeans", "Hmisc", "tidyr", "dplyr", "psych", "compute.es", "pwr")  # package names go here
ipak(packages)

## Step 1: Load and reshape data ---------------------------------------------
setwd("/Users/shariliu/Desktop/") ## change to your local directory!
ivc.wide <- read.csv(file = "risk_13mar18.csv", header = TRUE)
head(ivc.wide)
str(ivc.wide)

# convert into long format
ivc.long <- gather(ivc.wide, type, look, avg_lv:prop.hv)
str(ivc.long)

# log transform looks
ivc.long$look <- as.numeric(ivc.long$look)
ivc.long$loglook <- log(ivc.long$look)

# set levels for different kinds of looks
ivc.long$type <- factor(ivc.long$type)
levels(ivc.long$type) <- c("Higher Value", "Lower Value", "Higher Value 1", "Higher Value 2",
                           "Lower Value 1", "Lower Value 2", "Prop Higher Value", "Prop Lower Value")

# subset averaged looks across test pairs (2 observations per participant)
ivc.avg <- dplyr::filter(ivc.long, type == "Lower Value" | type == "Higher Value")
ivc.avg$type <- factor(ivc.avg$type)

# check that the lognormal distribution provides a better fit to the data than the normal distribution
fitdistr(na.omit(ivc.avg$look), "normal")$loglik
fitdistr(na.omit(ivc.avg$look), "lognormal")$loglik

# subset looks for 2 test pairs (4 observations per participant)
ivc.tp <- dplyr::filter(ivc.long, type == "Higher Value 1" | type == "Higher Value 2" |
                          type == "Lower Value 1" | type == "Lower Value 2")

# organize type and test pairs
ivc.tp$tp <- NA
ivc.tp$type <- factor(ivc.tp$type)
ivc.tp$trial <- NA
for (i in 1:nrow(ivc.tp)) {
  if (ivc.tp$type[i] == "Lower Value 1") {
    ivc.tp$tp[i] <- "First"
  }
  if (ivc.tp$type[i] == "Higher Value 1") {
    ivc.tp$tp[i] <- "First"
  }
  if (ivc.tp$type[i] == "Lower Value 2") {
    ivc.tp$tp[i] <- "Second"
  }
  if (ivc.tp$type[i] == "Higher Value 2") {
    ivc.tp$tp[i] <- "Second"
  }
  if ((ivc.tp$type[i] == "Lower Value 1" & ivc.tp$first_test[i] == "LV") | (ivc.tp$type[i] == "Higher Value 1" & ivc.tp$first_test[i] == "HV")) {
    ivc.tp$trial[i] <- "1"
  }
  if ((ivc.tp$type[i] == "Lower Value 2" & ivc.tp$first_test[i] == "LV")  | (ivc.tp$type[i] == "Higher Value 2" & ivc.tp$first_test[i] == "HV")) {
    ivc.tp$trial[i] <- "3"
  }
  if ((ivc.tp$type[i] == "Higher Value 1" & ivc.tp$first_test[i] == "LV")  | (ivc.tp$type[i] == "Lower Value 1" & ivc.tp$first_test[i] == "HV")) {
    ivc.tp$trial[i] <- "2"
  }
  if ((ivc.tp$type[i] == "Higher Value 2" & ivc.tp$first_test[i] == "LV")  | (ivc.tp$type[i] == "Lower Value 2" & ivc.tp$first_test[i] == "HV")) {
    ivc.tp$trial[i] <- "4"
  }
}

ivc.tp$tp <- factor(ivc.tp$tp)
ivc.tp$type <- factor(ivc.tp$type)
levels(ivc.tp$type) <- c("Higher Value", "Higher Value", "Lower Value", "Lower Value")
ivc.tp$trial <- factor(ivc.tp$trial)

# set baseline levels for analysis:
ivc.avg$type <- relevel(ivc.avg$type, ref = "Higher Value")
ivc.avg$experiment <- relevel(ivc.avg$experiment, ref = "Exp.1")



## Step 2: Visualize -------------------------------
ivc.avg$type <- relevel(ivc.avg$type, ref = "Lower Value")

## Fig 3 below requires within subjects CIs.
## Retrieved from : http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#error-bars-for-within-subjects-variables

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}


## get within-subjects CIs for plotting
ivc.avg.within <- summarySEwithin(data = ivc.avg, measurevar = "look", betweenvars = "experiment", withinvars = "type",
                                  idvar = "subj")
ivc.avg.within.log <- summarySEwithin(data = ivc.avg, measurevar = "loglook", betweenvars = "experiment", withinvars = "type",
                                  idvar = "subj")

# boxplot of raw looking time by study, means in white
Fig3 <- ggplot(data = dplyr::filter(ivc.avg, experiment != "RISK" & experiment != "RISKr"), aes(type, look, fill=type))
Fig3 + 
  geom_boxplot(width = 0.5, outlier.colour = NA, alpha=0.6) +
  geom_line(aes(group=subj), alpha = 0.1, linetype = 1) +
  xlab("Test Trial") +
  ylab("Looking Time (s)") +
  coord_cartesian(ylim = c(0,65)) +
  geom_errorbar(data = dplyr::filter(ivc.avg.within, experiment != "RISK" & experiment != "RISKr"), size = .9, width = 0, aes(ymin=look-se, ymax=look+se)) +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=23, size=3) +
  scale_fill_manual(values = c("grey30", "grey90")) +
  facet_grid(~experiment) +
  theme_linedraw(20) + 
  annotate("text", x=1.5, y=63, size=10, label=c("*ß=0.502","*ß=0.408", "*ß=0.403")) +
  theme(legend.position="none")

# boxplot of raw looking time by study, means in white
Fig3 <- ggplot(data = dplyr::filter(ivc.avg), aes(type, loglook, fill=type))
Fig3 + 
  geom_boxplot(width = 0.5, outlier.colour = NA, alpha=0.75) +
  geom_line(aes(group=subj), alpha = 0.1, linetype = 1) +
  geom_point(alpha = 0.1)+
  xlab("Test Trial") +
  ylab("Looking Time (log s)") +
  coord_cartesian(ylim = c(0,5)) +
  geom_errorbar(data = ivc.avg.within.log, size = .9, width = 0, aes(ymin=loglook-se, ymax=loglook+se)) +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=23, size=3) +
  scale_fill_manual(values = c("grey30", "grey95")) +
  facet_grid(~experiment) +
  theme_bw(15) + 
  # annotate("text", x=1.5, y=63, size=5, label=c("*ß=0.502","*ß=0.408", "*ß=0.403", "*ß=0.350")) +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

Fig3 <- ggplot(data = dplyr::filter(ivc.avg), aes(type, look, fill=type))
Fig3 + 
  stat_summary(fun.y=mean, geom="bar", colour = "black", alpha = 0.8)+
  geom_line(aes(group=subj), alpha = 0.1, linetype = 1) +
  geom_point(alpha = 0.1)+
  xlab("Test Trial") +
  ylab("Looking Time (logs)") +
  coord_cartesian(ylim = c(0,65)) +
  geom_errorbar(data = dplyr::filter(ivc.avg.within), size = .9, width = 0, aes(ymin=look-se, ymax=look+se)) +
  # stat_summary(fun.y=mean, geom="point", fill="white", shape=23, size=5) +
  scale_fill_manual(values = c("grey30", "grey95")) +
  facet_grid(~experiment) +
  theme_bw(15) + 
  # annotate("text", x=1.5, y=63, size=5, label=c("*ß=0.502","*ß=0.408", "*ß=0.403", "*ß=0.350")) +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# boxplot of raw looking time by study, means in white
Fig3.log <- ggplot(data = dplyr::filter(ivc.avg), aes(type, loglook, fill=type))
Fig3.log + 
  geom_boxplot(width = 0.5, outlier.colour = NA, alpha=0.6) +
  xlab("Test Trial") +
  ylab("Looking Time (s)") +
  coord_cartesian(ylim = c(0,6)) +
  geom_errorbar(data = ivc.avg.within.log, size = .9, width = 0, aes(ymin=loglook-ci, ymax=loglook+ci)) +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=23, size=5) +
  scale_fill_manual(values = c("grey30", "grey90")) +
  facet_grid(~experiment) +
  theme_light(15) + 
  #annotate("text", x=1.5, y=63, size=5, label=c("*ß=0.502","*ß=0.408", "*ß=0.403")) +
  theme(legend.position="none")


# line plot of data from all subejcts across 2 test pairs
FigS1 <- ggplot(data = dplyr::filter(ivc.avg), aes(type, look, fill=type, group = subj))
FigS1 + 
  geom_line(aes(colour = experiment)) +
  xlab("Test Trial") +
  ylab("Looking Time (s)") +
  stat_summary(fun.y=median, geom="point", shape=20, size=4) +
  facet_grid(~experiment) +
  theme_linedraw(15) + 
  theme(legend.position="none")

# boxplot for total looking during familiarization across experiments
FigS2 <- ggplot(data = ivc.wide, aes(experiment, sum_fam, fill = experiment))
FigS2 + geom_boxplot() +
  coord_cartesian(ylim = c(0,400))+
  stat_summary(fun.y=mean, geom="point", fill="white", shape=23, size=5) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour="white", size = .5, position = position_dodge(width = .9), width = 0) +
  xlab("Experiment") +
  ylab("Total Looking Time During Familiarization (s)") +
  theme_linedraw(15) +
  theme(legend.position="none")

# Plot looking at curves of attention across test trials for all experiments
test1234 <- ggplot(data = ivc.tp, aes(x = trial, y = look, fill = experiment))
test1234 + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=23, size=5) +
  facet_wrap(~experiment) +
  theme_light(15)+
  theme(legend.position="none")

# Plot looking at looks within each test pair by type and order of test events across all experiments
alllooks <- ggplot(data = dplyr::filter(ivc.tp), aes(type, look, fill=type, group = subj))
alllooks + geom_boxplot(aes(group=type))+
  scale_fill_manual(values = c("grey80", "grey80")) +
  geom_line(aes(colour = experiment)) +
  xlab("Test Trial") +
  ylab("Looking Time (log s)") +
  stat_summary(fun.y=median, geom="point", shape=20, size=4) +
  facet_wrap(~experiment+first_test+tp) +
  theme_linedraw(15) + 
  theme(legend.position="none")

## Step 3: Summary statistics -----------------------------------------------
# summary statistics for raw looks
aggregate(look ~ type + experiment, data = ivc.avg, FUN = "mean")
aggregate(look ~ type + experiment, data = ivc.avg, FUN = "median")
aggregate(look ~ type + experiment, data = ivc.avg, FUN = "sd")

# age of infants
aggregate(agem ~ experiment, data = ivc.wide, FUN = "mean")
aggregate(agem ~ experiment , data = ivc.wide, FUN = "range")

# getting estimates for SDs over differences in LTs
sd(dplyr::filter(ivc.wide, experiment == "Exp.1")$avg_lv - dplyr::filter(ivc.wide, experiment == "Exp.1")$avg_hv)/sqrt(24)
sd(dplyr::filter(ivc.wide, experiment == "Exp.2")$avg_lv - dplyr::filter(ivc.wide, experiment == "Exp.2")$avg_hv)/sqrt(24)
sd(dplyr::filter(ivc.wide, experiment == "Exp.3")$avg_lv - dplyr::filter(ivc.wide, experiment == "Exp.3")$avg_hv)/sqrt(32)

## Step 4: Set baseline levels of all categorical factors-------------------------
ivc.avg$type <- relevel(ivc.avg$type, ref = "Higher Value")
ivc.avg$sex <- relevel(ivc.avg$sex, ref = "m")
ivc.avg$first_test <- relevel(ivc.avg$first_test, ref = "HV")
ivc.avg$experiment <- relevel(ivc.avg$experiment, ref = "Exp.1")

## Experiment 1 -------------------------------------------------------------------------

# subset data
exp1 <- dplyr::filter(ivc.avg, experiment == "Exp.1")

# null model
model0 <- lmer(loglook ~ 1 + (1|subj),
               data = exp1, REML = FALSE)
summary(model0); confint(model0)

# hypothesis driven model
model1 <- lmer(loglook ~ type + (1|subj),
               data = exp1, REML = FALSE)
summary(model1)
confint(model1)
plot(model1) # residuals look ok

# standardized values 
model1.beta <- lmer(scale(loglook) ~ type + (1|subj),
                    data = exp1, REML = FALSE)
summary(model1.beta)
fixef(model1.beta); confint(model1.beta)

# are there any influential observations?
plot(influence(model1, "subj"), which="cook",
     cutoff=4/24, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# hypothesis driven model removing influential subjects
model1.cooks <- lmer(loglook ~ type + (1|subj),
                     data = dplyr::filter(exp1, subj != "S1_8"), REML = FALSE)
summary(model1.cooks); confint(model1.cooks)

# standardized values 
model1.beta.cooks <- lmer(scale(loglook) ~ type + (1|subj),
                          data = dplyr::filter(exp1, subj != "S1_8"), REML = FALSE)
summary(model1.beta.cooks)

# exploratory model 1 checking explicitly for order effects
model2 <- lmer(loglook ~ type * first_test + (1|subj),
               data = exp1, REML = FALSE)
summary(model2); confint(model2)

# standardized values
model2.beta <- lmer(scale(loglook) ~ type * first_test + (1|subj),
                    data = exp1, REML = FALSE)
summary(model2.beta)

# are there any influential observations?
plot(influence(model2, "subj"), which="cook",
     cutoff=4/24, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")
cooks.distance.estex(influence(model2, "subj"))

# exploratory model 1 checking explicitly for order effects after removing influential subjects
model2.cooks <- lmer(loglook ~ type * first_test + (1|subj),
                     data = dplyr::filter(exp1, subj != "S1_8"), REML = FALSE)
summary(model2.cooks)
confint(model2.cooks)

# standardized values
model2.beta.cooks <- lmer(scale(loglook) ~ type * first_test + (1|subj),
                          data = dplyr::filter(exp1, subj != "S1_8"), REML = FALSE)
summary(model2.beta.cooks)

# no effect of presentation order. exploratory model 2 checking explicitly for effect of sex, age in months, total looking @ fam, side of HV, and first fam order
model3 <- lmer(loglook ~ type + first_test + sum_fam + first_test + sex + HV_side + first_fam + (1|subj),
               data = exp1, REML = FALSE)
summary(model3); confint(model3)

# standardized values
model3.beta <- lmer(scale(loglook) ~ type + first_test + scale(sum_fam) + first_test + sex + HV_side + first_fam + (1|subj),
                    data = exp1, REML = FALSE)
summary(model3.beta)

# are there any influential observations?
plot(influence(model3, "subj"), which="cook",
     cutoff=4/24, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")
cooks.distance.estex(influence(model3, "subj"))

# after removal of influential values
model3.cooks <- lmer(loglook ~ type + first_test + sum_fam + first_test + sex + HV_side + first_fam + (1|subj),
                     data = dplyr::filter(exp1, subj != "S1_8"), REML = FALSE)
summary(model3.cooks)
confint(model3.cooks)

# standardized values
model2.beta.cooks <- lmer(scale(loglook) ~ type + first_test + scale(sum_fam) + first_test + sex + HV_side + first_fam + (1|subj),
                          data = dplyr::filter(exp1, subj != "S1_8"), REML = FALSE)
summary(model2.beta.cooks)

# comparison across these nested models.
# most parsimonious hypothesis-driven model provides the best fit.
anova(model0, model1, model2, model3)


## Experiment 2 -------------------------------------------------------------------------

# specify dataset
exp2 <- dplyr::filter(ivc.avg, experiment == "Exp.2")

# null model
model0.2 <- lmer(loglook ~ 1 + (1|subj),
                 data = exp2, REML = FALSE)
summary(model0.2); confint(model0.2)

# hypothesis driven model
model1.2 <- lmer(loglook ~ type + (1|subj),
                 data = exp2, REML = FALSE)
summary(model1.2)
confint(model1.2)
plot(model1.2) # residuals look ok

# standardized values 
model1.beta.2 <- lmer(scale(loglook) ~ type + (1|subj),
                      data = exp2, REML = FALSE)
summary(model1.beta.2)
fixef(model1.beta.2); confint(model1.beta.2)

# are there any influential observations?
plot(influence(model1.2, "subj"), which="cook",
     cutoff=4/24, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# exploratory model 1 checking explicitly for order effects
model2.2 <- lmer(loglook ~ type * first_test + (1|subj),
                 data = exp2, REML = FALSE)
summary(model2.2); confint(model2.2)

# standardized values
model2.beta.2 <- lmer(scale(loglook) ~ type * first_test + (1|subj),
                      data = exp2, REML = FALSE)
summary(model2.beta.2)

# are there any influential observations?
plot(influence(model2.2, "subj"), which="cook",
     cutoff=4/24, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")
cooks.distance.estex(influence(model2, "subj"))

# pairwise contrasts
contrasts.model2.2 <- lsmeans(model2.2, specs = ~type*first_test)    # at = list(cond = factor(0))
contrast(contrasts.model2.2, method = "pairwise", adjust="none")    # can use p.adjust 
confint(contrast(contrasts.model2.2, method = "pairwise", adjust="none"))

# summary stats for looks at test depending on presentaiton order of test events
aggregate(look ~ type + first_test, data = dplyr::filter(ivc.avg, experiment == "Exp.2"), FUN=mean)
aggregate(look ~ type + first_test, data = dplyr::filter(ivc.avg, experiment == "Exp.2"), FUN=sd)

# exploratory model 2 checking explicitly for effect of sex, age in months, total looking @ fam, side of HV, and first fam order
model3.2 <- lmer(loglook ~ type * first_test + sum_fam + first_test + sex + HV_side + first_fam + (1|subj),
                 data = exp2, REML = FALSE)
summary(model3.2); confint(model3.2)

# standardized values
model3.beta.2 <- lmer(scale(loglook) ~ type * first_test + scale(sum_fam) + first_test + sex + HV_side + first_fam + (1|subj),
                      data = exp2, REML = FALSE)
summary(model3.beta.2)

# are there any influential observations?
plot(influence(model3.2, "subj"), which="cook",
     cutoff=4/24, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")
cooks.distance.estex(influence(model3, "subj"))

# after removal of influential values
model3.cooks.2 <- lmer(loglook ~ type * first_test + sum_fam + first_test + sex + HV_side + first_fam + (1|subj),
                       data = dplyr::filter(ivc.avg, experiment == "Exp.2" & subj != "S2_2" & subj != "S2_5" & subj != "S2_7"), REML = FALSE)
summary(model3.cooks.2)
confint(model3.cooks.2)

# standardized values
model2.beta.cooks.2 <- lmer(scale(loglook) ~ type * first_test + scale(sum_fam) + first_test + sex + HV_side + first_fam + (1|subj),
                            data = dplyr::filter(ivc.avg, experiment == "Exp.2" & subj != "S2_2" & subj != "S2_5" & subj != "S2_7"), REML = FALSE)
summary(model2.beta.cooks.2)

# comparison across these nested models.
# simpler exploratory model provides the best fit.
anova(model0.2, model1.2, model2.2, model3.2)


## Experiment 3 -------------------------------------------------------------------------

# # Power analysis for sample size of experiment 3 based on experiments 1 and 2 (takes a few min to run)
# model1.study12 <- lmer(loglook ~ type + (1|subj) + (1|experiment),
#                        data = dplyr::filter(ivc.avg, experiment != "Exp.3"))
# summary(model1.study12)


# specify dataset
ivc.avg.exp3 <- dplyr::filter(ivc.avg, experiment == "Exp.3")

# null model
model0.3 <- lmer(loglook ~ 1 + (1|subj),
                 data = ivc.avg.exp3, REML = FALSE)
summary(model0.3); confint(model0.3)

# hypothesis driven model
model1.3 <- lmer(loglook ~ type + (1|subj),
                 data = ivc.avg.exp3, REML = FALSE)
summary(model1.3)
confint(model1.3)

# standardized values 
model1.beta.3 <- lmer(scale(loglook) ~ type + (1|subj),
                      data =ivc.avg.exp3, REML = FALSE)
summary(model1.beta.3)
fixef(model1.beta.3); confint(model1.beta.3)

# are there any influential observations?
plot(influence(model1.3, "subj"), which="cook",
     cutoff=4/32, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# hypothesis driven model removing influential subjects
model1.cooks.3 <- lmer(loglook ~ type + (1|subj),
                       data = dplyr::filter(ivc.avg, experiment == "Exp.3" & subj != "S3_15"), REML = FALSE)
summary(model1.cooks.3); confint(model1.cooks.3)

# standardized values 
model1.beta.cooks.3 <- lmer(scale(loglook) ~ type + (1|subj),
                            data = dplyr::filter(ivc.avg, experiment == "Exp.3" & subj != "S3_15"), REML = FALSE)
summary(model1.beta.cooks.3)

# exploratory model 1 checking explicitly for order effects
model2.3 <- lmer(loglook ~ type * first_test + (1|subj),
                 data = ivc.avg.exp3, REML = FALSE)
summary(model2.3); confint(model2.3)

# standardized values
model2.beta.3 <- lmer(scale(loglook) ~ type * first_test + (1|subj),
                      data = ivc.avg.exp3, REML = FALSE)
summary(model2.beta.3)

# are there any influential observations?
plot(influence(model2.3, "subj"), which="cook",
     cutoff=4/32, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")
cooks.distance.estex(influence(model2.3, "subj"))

# exploratory model 1 checking explicitly for order effects after removal of influential subjects
model2.3.cooks <- lmer(loglook ~ type * first_test + (1|subj),
                       data = dplyr::filter(ivc.avg.exp3, subj != "S3_22"), REML = FALSE)
summary(model2.3.cooks); confint(model2.3.cooks)

# looking within levels of interaction
contrasts.model2.3 <- lsmeans(model2.3.cooks, specs = ~type*first_test)    # at = list(cond = factor(0))
contrast(contrasts.model2.3, method = "pairwise", adjust="none")    # can use p.adjust 
confint(contrast(contrasts.model2.3, method = "pairwise", adjust="none"))

# summary statistics for looks at test depending on presentation order of test events
aggregate(look ~ type + first_test, data = dplyr::filter(ivc.avg, experiment == "Exp.3" & subj != "S3_22"), FUN=mean)
aggregate(look ~ type + first_test, data = dplyr::filter(ivc.avg, experiment == "Exp.3" & subj != "S3_22"), FUN=sd)


# exploratory model 2 checking explicitly for effect of sex, age in months, total looking @ fam, side of HV, and first fam order
model3.3 <- lmer(loglook ~ type * first_test + sum_fam + first_test + sex + HV_side + first_fam + (1|subj),
                 data = ivc.avg.exp3, REML = FALSE)
summary(model3.3); confint(model3.3)

# standardized values
model3.beta.3 <- lmer(scale(loglook) ~ type * first_test + scale(sum_fam) + first_test + sex + HV_side + first_fam + (1|subj),
                      data = ivc.avg.exp3, REML = FALSE)
summary(model3.beta.3)

# are there any influential observations?
plot(influence(model3.3, "subj"), which="cook",
     cutoff=4/32, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")
cooks.distance.estex(influence(model3, "subj"))

# after removal of influential values
model3.cooks.3 <- lmer(loglook ~ type * first_test + sum_fam + first_test + sex + HV_side + first_fam + (1|subj),
                       data = dplyr::filter(ivc.avg, experiment == "Exp.3" & subj != "S3_15" & subj != "S3_17"), REML = FALSE)
summary(model3.cooks.3)
confint(model3.cooks.3)

# standardized values
model2.beta.cooks.3 <- lmer(scale(loglook) ~ type * first_test + scale(sum_fam) + first_test + sex + HV_side + first_fam + (1|subj),
                            data = dplyr::filter(ivc.avg, experiment == "Exp.3" & subj != "S3_15" & subj != "S3_17"), REML = FALSE)
summary(model2.beta.cooks.3)

# comparison across these nested models.
# most parsimonious exploratory model provides the best fit.
anova(model0.3, model1.3, model2.3, model3.3)

# RISK
risk <- dplyr::filter(ivc.wide, experiment == "RISK" | experiment == "RISKr")
risk.10 <- dplyr::filter(ivc.avg, experiment == "RISKr")

model1.risk <- lmer(loglook ~ type * agem + (1|subj),
                        data = dplyr::filter(ivc.avg, experiment == "RISKr"), REML = FALSE)
summary(model1.risk)
plot(allEffects(model1.risk))

plot(influence(model1.risk, "subj"), which="cook",
     cutoff=4/32, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

plot(allEffects(model1.risk))

plot(risk$agem,risk$prop.lv)

## Experiments 1 - 3-------------------------------------------------------------------------

# summary statistics for looks at test by test type across all experiments
aggregate(look ~ type, data = dplyr::filter(ivc.avg), FUN = mean)
aggregate(look ~ type, data = dplyr::filter(ivc.avg), FUN = sd)

# null model
model0.study123 <- lmer(loglook ~ 1 + (1|subj) + (1|experiment),
                        data = ivc.avg, REML = FALSE)
summary(model0.study123)

# hypothesis driven model
model1.study123 <- lmer(loglook ~ type + (1|subj) + (1|experiment),
                        data = ivc.avg, REML = FALSE)
summary(model1.study123)
confint(model1.study123)

# standardized values
model1.study123.beta <- lmer(scale(loglook) ~ type + (1|subj) + (1|experiment),
                             data = ivc.avg, REML = FALSE)
summary(model1.study123.beta)

# any influential observations?
plot(influence(model1.study123, "subj"), which="cook",
     cutoff=4/80, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")
cooks.distance.estex(influence(model1.study12, "subj"))

# hypothesis driven model removing influential case
model1.study123.cooks <- lmer(loglook ~ type + (1|subj),
                              data = dplyr::filter(ivc.avg, subj !="S1_8"), REML = FALSE)
summary(model1.study123.cooks)
confint(model1.study123.cooks)

# standardized values
model1.study123.cooks.beta <- lmer(scale(loglook) ~ type + (1|subj),
                                   data = dplyr::filter(ivc.avg, subj !="S1_8"), REML = FALSE)
summary(model1.study123.cooks.beta)

# exploratory model testing explicitly for differences across experiments
model2.study123 <- lmer(loglook ~ type * experiment + (1|subj),
                        data = ivc.avg, REML = FALSE)
summary(model2.study123)
confint(model2.study123)

# standardized values
model2.study123.beta <- lmer(scale(loglook) ~ type * experiment + (1|subj),
                             data = dplyr::filter(ivc.avg), REML = FALSE)
summary(model2.study123.beta)

plot(influence(model2.study123, "subj"), which="cook",
     cutoff=4/80, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")
cooks.distance.estex(influence(model2.study123, "subj"))

# exploratory model testing for differences across experiments - after removing influential subjects
model2.study123.cooks <- lmer(loglook ~ type * experiment + (1|subj),
                              data = dplyr::filter(ivc.avg, subj != "S1_8"), REML = FALSE)
summary(model2.study123.cooks)
confint(model2.study123.cooks)

# standardized values
model2.study123.beta.cooks <- lmer(scale(loglook) ~ type * experiment + (1|subj),
                                   data = dplyr::filter(ivc.avg, subj != "S1_8"), REML = FALSE)
summary(model2.study123.beta.cooks)

# exploratory model testing explicitly for order effects
model3.study123 <- lmer(loglook ~ type * first_test + (1|subj) + (1|experiment),
                        data = ivc.avg, REML = FALSE)
summary(model3.study123)
confint(model3.study123)

# standardized values
model3.study123.beta <- lmer(scale(loglook) ~ type * first_test + (1|subj) + (1|experiment),
                             data = ivc.avg, REML = FALSE)
summary(model3.study123.beta)
confint(model3.study123.beta)

# are there any influential observations?
plot(influence(model3.study123, "subj"), which="cook",
     cutoff=4/80, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")


# exploratory model testing explicitly for order effect without influential observations
model3.study123.cooks <- lmer(loglook ~ type * first_test + (1|subj) + (1|experiment),
                              data = dplyr::filter(ivc.avg, subj != "S1_8" & subj != "S3_22"), REML = FALSE)
summary(model3.study123.cooks)
confint(model3.study123.cooks)

# standardized values
model3.study123.beta.cooks <- lmer(scale(loglook) ~ type * first_test + (1|subj) + (1|experiment),
                                   data = dplyr::filter(ivc.avg, subj != "S1_8" & subj != "S3_22"), REML = FALSE)
summary(model3.study123.beta.cooks)
confint(model3.study123.beta.cooks)

# exploratory model 2 looking at order effects as well as other predictors
model4.study123 <- lmer(loglook ~ type * first_test + sum_fam + sex + HV_side + first_fam + (1|subj) + (1|experiment),
                        data = ivc.avg, REML = FALSE)
summary(model4.study123)
confint(model4.study123)

# exploratory model 2 looking at order effects as well as other predictors
model4.study123.beta <- lmer(scale(loglook) ~ type * first_test + scale(sum_fam) + sex + HV_side + first_fam + (1|subj) + (1|experiment),
                             data = ivc.avg, REML = FALSE)
summary(model4.study123.beta)
confint(model4.study123.beta)

plot(influence(model4.study123, "subj"), which="cook",
     cutoff=4/80, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# removing influential subjects
model4.study123.cooks <- lmer(loglook ~ type * first_test + sum_fam + sex + HV_side + first_fam + (1|subj) + (1|experiment),
                              data = dplyr::filter(ivc.avg, subj != "S1_8" & subj != "S3_15"), REML = FALSE)
summary(model4.study123)
confint(model4.study123)

# standardized values
model4.study123.beta.cooks <- lmer(scale(loglook) ~ type * first_test + scale(sum_fam) + sex + HV_side + first_fam + (1|subj) + (1|experiment),
                                   data = dplyr::filter(ivc.avg, subj != "S1_8" & subj != "S3_15"), REML = FALSE)
summary(model4.study123.beta)
confint(model4.study123.beta)

# model comparison. best, most parsimonious fit given by simpler exploratory model
anova(model0.study123, model1.study123, model3.study123, model4.study123)

# looking at differences in familiarization across all experiments
aggregate(sum_fam ~ experiment, data = ivc.wide, FUN = mean)
aggregate(sum_fam ~ experiment, data = ivc.wide, FUN = sd)

# null model
model0.fam <- lm(sum_fam ~ 1 ,
                 data = ivc.wide)
summary(model0.fam)

# question-driven model
model1.fam <- lm(sum_fam ~ 1 + experiment,
                 data = ivc.wide)
summary(model1.fam)
confint(model1.fam)

# standardized values
model1.fam.beta <- lm(scale(sum_fam) ~ 1 + experiment,
                      data = ivc.wide)
summary(model1.fam.beta)
confint(model1.fam.beta)

# NON-PARAMETRIC ANALYSES ACROSS EXP 1-3

# binary looking preferences across experiments
relevel(ivc.wide$pref.raw, ref = "HV")
binom.test(table(ivc.wide$pref.raw))
prop.test(table(ivc.wide$experiment, ivc.wide$pref.raw))

# non-parametric differences in LTs 
wilcox.test(ivc.wide$avg_lv, ivc.wide$avg_hv, paired = TRUE, conf.int=TRUE)

# bootstrapped difference in medians
ivc.wide$diff <- ivc.wide$avg_lv - ivc.wide$avg_hv
boot.median <- bootstrap(ivc.wide$diff, 10000, median)  ## 2000 subsamples using replacement
hist(boot.median$thetastar, xlab = "Bootstrap Medians", main = "Bootstrap Histogram", breaks = 30)
abline(v = median((ivc.wide$avg_lv - ivc.wide$avg_hv)), col = "red", lwd = 2, lty = 2)
quantile(boot.median$thetastar, probs = c(0.025, 0.975))               ## empirical quantiles


## RISK Experiment ----------
# boxplot of raw looking time by study, means in white
Fig3 <- ggplot(data = dplyr::filter(ivc.avg), aes(type, look, fill=type))
Fig3 + 
  geom_boxplot(width = 0.5, outlier.colour = NA, alpha=0.75) +
  geom_line(aes(group=subj), alpha = 0.1, linetype = 1) +
  geom_point(alpha = 0.1)+
  xlab("Test Trial") +
  ylab("Looking Time (s)") +
  coord_cartesian(ylim = c(0,65)) +
  geom_errorbar(data = ivc.avg.within, size = .9, width = 0, aes(ymin=look-se, ymax=look+se)) +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=23, size=3) +
  scale_fill_manual(values = c("grey30", "grey95")) +
  facet_grid(~experiment) +
  theme_linedraw(15) + 
  annotate("text", x=1.5, y=63, size=5, label=c("*ß=0.502","*ß=0.408", "*ß=0.403", "*ß=0.350")) +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# specify dataset
ivc.avg.risk <- dplyr::filter(ivc.avg, experiment == "RISKr" | experiment == "Exp.1")
ivc.tp.risk <- dplyr::filter(ivc.avg, experiment == "RISK" | experiment == "RISKr")

# null model
model0.4 <- lmer(loglook ~ 1 + (1|subj),
                 data = ivc.avg.risk, REML = FALSE)
summary(model0.4); confint(model0.4)

# hypothesis driven model
model1.4 <- lmer(loglook ~ type * experiment + (1|subj),
                 data = ivc.avg.risk, REML = FALSE)
summary(model1.4)
confint(model1.4)


# standardized values 
model1.4 <- lmer(scale(loglook) ~ type * experiment + (1|subj) + (1|experiment),
                 data = ivc.avg.risk, REML = FALSE)
summary(model1.4)
fixef(model1.4); confint(model1.4)

risk.sim <- powerCurve(extend(model1.4, along="subj", n=150),
                       along="subj", breaks = c(20, 25, 30, 35, 40), alpha = .10, seed = 123)
plot(risk.sim)
print(risk.sim)

risk.sim.exp123 <- powerCurve(extend(model1.study123, along="subj", n=150),
                       along="subj", breaks = c(20, 25, 30, 35, 40), alpha = .10, seed = 123)
plot(risk.sim.exp123)
print(risk.sim.exp123)


# are there any influential observations?
plot(influence(model1.4, "subj"), which="cook",
     cutoff=4/32, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# hypothesis driven model removing influential subjects
model1.cooks.4 <- lmer(loglook ~ type + (1|subj),
                       data = dplyr::filter(ivc.avg, experiment == "RISK" & subj != "S4_12"), REML = FALSE)
summary(model1.cooks.4); confint(model1.cooks.4)

# standardized values 
model1.beta.cooks.4 <- lmer(scale(loglook) ~ type + (1|subj),
                            data = dplyr::filter(ivc.avg, experiment == "RISK" & subj != "S4_12"), REML = FALSE)
summary(model1.beta.cooks.4)

# exploratory model 1 checking explicitly for order effects
model2.4 <- lmer(loglook ~ type * first_test + (1|subj),
                 data = ivc.avg.risk, REML = FALSE)
summary(model2.4); confint(model2.4)

# standardized values
model2.beta.4 <- lmer(scale(loglook) ~ type * first_test + (1|subj),
                      data = ivc.avg.risk, REML = FALSE)
summary(model2.beta.4)

# are there any influential observations?
plot(influence(model2.4, "subj"), which="cook",
     cutoff=4/32, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")
cooks.distance.estex(influence(model2.3, "subj"))

aggregate(look ~ type + first_test, data = dplyr::filter(ivc.avg.risk, experiment == "RISK"), FUN=mean)
aggregate(look ~ type + first_test, data = dplyr::filter(ivc.avg.risk, experiment == "RISK"), FUN=sd)

# exploratory model 2 checking explicitly for effect of sex, age in months, total looking @ fam, side of HV, and first fam order
model3.4 <- lmer(loglook ~ type + first_test + sum_fam + first_test + sex + HV_side + first_fam + (1|subj),
                 data = ivc.avg.risk, REML = FALSE)
summary(model3.4); confint(model3.4)

# standardized values
model3.beta.4 <- lmer(scale(loglook) ~ type + first_test + scale(sum_fam) + first_test + sex + HV_side + first_fam + (1|subj),
                      data = ivc.avg.exp3, REML = FALSE)
summary(model3.beta.4)

# are there any influential observations?
plot(influence(model3.4, "subj"), which="cook",
     cutoff=4/32, sort=TRUE,
     xlab="Cook´s Distance",
     ylab="Subject ID")

# after removal of influential values
model3.cooks.4 <- lmer(loglook ~ type + first_test + sum_fam + first_test + sex + HV_side + first_fam + (1|subj),
                       data = dplyr::filter(ivc.avg, experiment == "RISK" & subj != "S4_12"), REML = FALSE)
summary(model3.cooks.4)
confint(model3.cooks.4)

# standardized values
model2.beta.cooks.4 <- lmer(scale(loglook) ~ type + first_test + scale(sum_fam) + first_test + sex + HV_side + first_fam + (1|subj),
                            data = dplyr::filter(ivc.avg, experiment == "RISK" & subj != "S4_12"), REML = FALSE)
summary(model2.beta.cooks.4)

# comparison across these nested models.
# most parsimonious exploratory model provides the best fit.
anova(model0.4, model1.4, model2.4, model3.4)



## Analysis of reliability of data from test trials---------------------------------------------------------
setwd("/Users/shariliu/Desktop/") # set to your local directory
reliability <- read.csv(file = "ivc_reliability_deid.csv", header = TRUE)
names(reliability)

study1rel <- dplyr::filter(reliability, Experiment == "barriers")
study2rel <- dplyr::filter(reliability, Experiment == "ramps")

# generate ICCs
icc(study1rel[,3:4], model = "twoway", type = "agreement", unit = "single"); mean(study1rel$Agree) # Study 1: ICC = 0.969, agreement = 94.17%
icc(study2rel[,3:4], model = "twoway", type = "agreement", unit = "single"); mean(na.omit(study2rel$Agree)) # Study 2: ICC = 0.993, agreement = 95.83%


## Analyses responding to reviewers-------------

# Q: Do infants look longer during the first test trial to the lower- vs. higher- value action in Exp 2?
test1.exp123 <- dplyr::filter(ivc.tp, trial == "1" & experiment == "Exp.2")
test1.model <- lm(loglook ~ type,
                  data = test1.exp123)
summary(test1.model)
