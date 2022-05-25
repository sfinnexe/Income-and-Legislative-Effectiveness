#Function to install and load packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","dplyr","lmtest","stargazer","haven","summarytools","Hmisc","ggrepel","scales","Stack")
ipak(packages)

setwd("C:/Users/Owner/OneDrive/Political Statistics 2/Research Paper")

#median family income 111-116 congress, senate and house
d <- read.csv("C:/Users/Owner/OneDrive/Political Statistics 2/Research Paper/actual data/medfaminc.csv")

str(d)

#recode independents as democrats because they caucus with the democrats
#republicans = 1 ; democrats/(independents caucusing with democrats) = 0 
d$memberparty01 <- if_else(d$memberparty == 200, 1, 0)
count(d, memberparty)
count(d, memberparty01)

#find the mean "median family income"  by congress>chamber>party
d2 <- d %>% group_by(congress, chamber, memberparty01) %>%
  summarise(a_sum=sum(Medfaminc),
            a_mean=(mean(Medfaminc))) 

ggplot(d2, aes(x = memberparty01, y = a_mean)) +
  geom_point() +
  geom_jitter() +
  geom_smooth()


#percent of bills/ammendments passed 
################################################House 
rollcall <- read.csv("C:/Users/Owner/OneDrive/Political Statistics 2/Research Paper/Hall_rollcalls.csv")

#filter for relevant congresses
roll_call <- rollcall %>% filter(congress >= 111 & congress <= 116) 

#create variable for whether a bill passed 
roll_call$passed <- if_else(roll_call$yea_count > 217, 1, 0)

#number of passed bills by congress
rc <- roll_call %>% group_by(congress) %>% count(passed) 

write.csv(rc, "roll call stats.csv")

hrc <- read.csv("roll call stats.csv")

#percent of bills passed that came up for a vote
hrc$pctpassed <- hrc$passed / (hrc$not.passed + hrc$passed)


################################################################################## Senate
srollcall <- read.csv("Sall_rollcalls.csv")

unique(srollcall$vote_result)

#filter for relevant congresses
sroll_call <- srollcall %>% filter(congress >= 111 & congress <= 116) 

#create variable for whether a bill passed 
src <- sroll_call %>% filter(vote_result == "Bill Passed" | vote_result == "Bill Defeated" | 
                               vote_result == "Amendment Agreed to" | vote_result == "Amendment Rejected")
str(src)

src$bills <- if_else(src$vote_result == "Bill Passed" | src$vote_result == "Amendment Agreed to", 1,
                            if_else(src$vote_result == "Bill Defeated" | src$vote_result == "Amendment Rejected", 0, -1))

src1 <- src %>% filter(bills == 1 | bills == 0) %>% group_by(congress) %>% count(bills) 

write.csv(src1, "senate roll call stats.csv")

src2 <- read.csv("senate roll call stats.csv")

#percent of bills and ammendments passed that came up for a vote
src2$pctpassed <- src2$passed / (src2$not.passed + src2$passed)

shrollcall <- Stack(hrc, src2)

rm(d, hrc, rc, roll_call, rollcall, src, src1, src2, sroll_call, srollcall)

#put congress and chamber in the same column
d3 <- d2 %>% unite(cong.chamb, congress, chamber)
sh <- shrollcall %>% unite(cong.chamb, congress, chamber)

#join both data frames
all <- left_join(d3, sh, by = "cong.chamb")

#create new variables
all$presparty <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1)

all$majinchamb <- c(1,0,1,0,0,1,1,0,0,1,1,0,0,1,0,1,0,1,0,1,1,0,0,1)

all$inf.adj.faminc <- c(60877,63651,64641,56708,64646,58714,64298,56559,59867,59613,62325,55114,61490,60927,64722,56098,66102,63158,68080,58476,70852,61619,70177,59692)

all$maj.size <- c(45,-35,16,-6,-18,27,3,-2,-14,22,7,-4,-28,33,-6,4,-18,32,-2,5,17,-15,-5,4)

all$otherch.samepartymaj <- c(1,0,1,0,1,0,0,1,1,0,0,1,0,1,0,1,0,1,0,1,0,1,1,0)

ggplot(all, aes(y = inf.adj.faminc, x = pctpassed)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = lm)

write.csv(all, "congress_income.csv")
