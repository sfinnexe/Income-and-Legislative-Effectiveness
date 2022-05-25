#install and load packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","dplyr","lmtest","stargazer","haven","summarytools","Hmisc","ggrepel","scales","psych","interactions")
ipak(packages)

#set working directory
setwd("C:/Users/Owner/OneDrive/Political Statistics 2/Research Paper")
#import data fram
inc.con1 <- read.csv("incomecongress08.csv")

#look at the structure and summary statistics of the data
#'memberparty01' 0 = Democrat 1 = Republican
dfSummary(inc.con1)

str(inc.con1)

inc.con1$inf.adj.faminc <- as.numeric(inc.con1$inf.adj.faminc)
inc.con1$pct.passrej <- inc.con1$pct.passrej * 100

#visualize the relationships between the dep and indep variables in ggplot

ggplot(inc.con1, aes(x = inf.adj.faminc, y = pct.passrej)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = lm) +
  theme_classic()

ggplot(inc.con1, aes(x = maj.size, y = pct.passrej)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = lm) +
  theme_classic()

ggplot(inc.con1, aes(x = presmajpartysame, y = pct.passrej)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic()

ggplot(inc.con1, aes(x = otherch.samepartymaj, y = pct.passrej)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_classic()

#run a basic multivariate regression model with and without controlling for the 
#party of the majority in the other chamber and the party of the president
lm <- lm(pct.passrej ~ inf.adj.faminc + memberparty01 + maj.size + presmajpartysame + otherch.samepartymaj , data = inc.con1)
summary(lm)

#check for covariance between the variables
vcov(lm)

lm0 <- lm(pct.passrej ~ inf.adj.faminc + memberparty01 + maj.size, data = inc.con1)

stargazer(lm0, lm, type = "text", style = "apsr")

#interaction model ; interaction between party and median family income
lm1 <- lm(pct.passrej ~ inf.adj.faminc*memberparty01 + inf.adj.faminc + memberparty01 + maj.size + presmajpartysame + otherch.samepartymaj , data = inc.con1)
stargazer(lm1, type = "text", style = "apsr")

interact_plot(lm1, pred = "inf.adj.faminc", modx = "memberparty01", 
              interval = TRUE, int.width = 0.95, y.label = "Percent of Legislation Passed", 
              x.label = "Inflation adjusted Family Income by DIstrict", modx.labels = c("Democrat", "Republican"),
              legend.main = "Party",line.thickness = 0.3,
              rug=TRUE, rug.sides="bl") + theme_classic()

interact_plot(lm1, pred = "inf.adj.faminc", modx = "memberparty01", 
              interval = TRUE, int.width = 0.95, y.label = "Percent of Legislation Passed", 
              x.label = "Inflation adjusted Family Income by DIstrict", modx.labels = c("Democrat", "Republican"),
              legend.main = "Party",line.thickness = 0.3,
              rug=TRUE, rug.sides="bl", linearity.check = TRUE) + theme_classic()

ggplot(inc.con1, aes(x=pct.passrej, color=as.factor(memberparty01))) + 
  geom_density() +
  theme_classic() +
  xlab("Percent of legislation passed/rejected")

#only keep party caucuses in the majority because keeping minority caucuses in the pct.passrej variable causes inherent correlation/mirroring

majcong <- inc.con1 %>% filter(majinchamb == 1)

lm3 <- lm(pct.passrej ~ inf.adj.faminc*memberparty01 + inf.adj.faminc + memberparty01 + maj.size, data = majcong)
lm2 <- lm(pct.passrej ~ inf.adj.faminc*memberparty01 + inf.adj.faminc + memberparty01 + maj.size + presmajpartysame + otherch.samepartymaj, data = majcong)
stargazer(lm2, lm3, type = "text", style = "apsr")


interact_plot(lm2, pred = "inf.adj.faminc", modx = "memberparty01", 
              interval = TRUE, int.width = 0.95, y.label = "Percent of Legislation Passed", 
              x.label = "Inflation adjusted Family Income by District", modx.labels = c("Democrat", "Republican"),
              legend.main = "Party",line.thickness = 0.3,
              rug=TRUE, rug.sides="bl") + theme_classic()

interact_plot(lm2, pred = "inf.adj.faminc", modx = "memberparty01", 
              interval = TRUE, int.width = 0.95, y.label = "Percent of Legislation Passed", 
              x.label = "Inflation adjusted Family Income by DIstrict", modx.labels = c("Democrat", "Republican"),
              legend.main = "Party",line.thickness = 0.3,
              rug=TRUE, rug.sides="bl", linearity.check = TRUE) + theme_classic()

#recheck distribution of pct.passrej variable to make sure that it is no longer mirrored
ggplot(majcong, aes(x=pct.passrej, color=as.factor(memberparty01))) + 
  geom_density() +
  theme_classic()
