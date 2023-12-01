# Installing necessary packages

install.packages(c("ggplot2", "dplyr", "stargazer", "lfe", "stringr", "fixest", "modelsummary", "kableExtra"))
install.packages("tidyverse")

# Load the necessary library
library(ggplot2)
library(dplyr)
library(stargazer)
library(lfe)
library(stringr)
library(fixest) 
library(modelsummary)
library(kableExtra)
library(tidyverse)

load("C:\\Users\\victo\\Documents\\r-filer\\data_mpyear.rda")
data
# Assuming the data is loaded into a variable named 'data'
# Replace 'data' with the actual name of the data frame in your RDA file


#### FIGURE 1 ####
## Panel a ###

d1_a <- data %>%
  group_by(year) %>%
  summarize(
    sum_all = sum(earnings.all.infl)/1000000,
    sum_con = sum(if_else(con == 1, earnings.all.infl, 0)/1000000),
    sum_lab = sum(if_else(lab == 1, earnings.all.infl, 0)/1000000),
    sum_oth = sum(if_else(oth == 1, earnings.all.infl, 0)/1000000)
  )

d1_b <- data %>%
  group_by(year) %>%
  summarize(
    sum_all = sum(earnings.all.infl)/1000000,
    sum_con = sum(if_else(con == 1, earnings.all.infl, 0)/1000000),
    sum_lab = sum(if_else(lab == 1, earnings.all.infl, 0)/1000000),
    sum_oth = sum(if_else(oth == 1, earnings.all.infl, 0)/1000000)
  )


# Create the plot
ggplot(d1_a, aes(x = year)) +
  geom_line(aes(y = sum_all, group = 1), linetype = "solid", lwd=1) +
  geom_line(aes(y = sum_con, group = 1), linetype = "dashed", lwd=1) +
  geom_line(aes(y = sum_lab, group = 1), linetype = "dotdash", lwd=1) +
  geom_line(aes(y = sum_oth, group = 1), linetype = "dotted", lwd=1) +
  theme_minimal() +
  labs(x = "Year", y = "Total Earnings in Million £") +
  labs(x = "Year", y = "Total Earnings in Million £") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  geom_vline(xintercept = c(2010, 2015), color = "grey", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "grey", linetype = "solid", size = 1) +
  scale_x_continuous(breaks = unique(data$year)) + 
  scale_y_continuous(limits = c(0, 7)) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text.x = element_text(angle = 360)) + 
  theme(axis.line.x = element_line(color="grey", size = 0.5),
        axis.line.y = element_line(color="grey", size = 0.5))

#### Table 1 ####

## Vote rebellions ##

# All

t1ra <- feols(rebel ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave| year + id, cluster = ~ id, data=data)
summary(t1ra)

# Conservatives

t1rc <- feols(rebel ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$con==1,])
summary(t1rc)

# Labour

t1rl <- feols(rebel ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$lab==1,])
summary(t1rl)

## Vote participation ##

# All

t1pa <- feols(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data)
summary(t1pa)

# Conservative

t1pc <- feols(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$con==1,])
summary(t1pc)

# Labour

t1pl <- feols(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$lab==1,])
summary(t1pl)

## Number of parliamentary questions (q) log ##

t1qa <- feols(questcount.log ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data)
summary(t1qa)

t1qc <- feols(questcount.log ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$con==1,])
summary(t1qc)

t1ql <- feols(questcount.log ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$lab==1,])
summary(t1ql)

# Making an output for the regression results

models <- list(
  "All" = t1ra,
  "Conservative" = t1rc,
  "Labour" = t1rl,
  "All" = t1pa,
  "Conservative" = t1rc,
  "Labour" = t1rl,
  "All" = t1qa,
  "Conservative" = t1qc,
  "Labour" = t1ql
)

headers <- c("Vote Rebellion (Share)", "Vote Participation (Share)", "log(Number of Parliamentary Questions +1)", each = 3)

Tab1 <- combine_models(models, groups = headers)

modelsummary(models, stars = TRUE, statistic = NULL, 
             notes = "All regressions include MP and year fixed-effects, as well the following control variables: entered parliament, left parliament, minister,\n
             minister of state, parliamentary secretary, shadow cabinet, frontbench team, committee chair, and committee member",
             output = "latex")


# I cant make it any prettier right now, sorry

#### Figure 2 ####

# Panel A

# Creating the three regressions with vote attendance as independent, and moonlighting (bin 1000) as main dependent, as well as all the controls

f2a_low <- feols(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$con==1 & data$dist.tercile.con==1,])
summary(f2a_low)

f2a_mid <- feols(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$con==1 & data$dist.tercile.con==2,])
summary(f2a_mid)

f2a_hig <- feols(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$con==1 & data$dist.tercile.con==3,])
summary(f2a_hig)

# Calculating confidence intervals. I am quite proud of this
# Confidence errors calculated by copying outputs from the summaries above

f2a <- data.frame(param = c("Lowest tercile", "Middle Tercile", "Highest tercile"),
                  low = c(0.013623 - 1.96*0.023317, 0.016679 - 1.96*0.010774, 0.057663 - 1.96*0.019405),
                  est = c(0.013623, 0.016679, 0.057663),
                  high = c(0.013623 + 1.96*0.023317, 0.016679 + 1.96*0.010774, 0.057663 + 1.96*0.019405))


f2aplot <- ggplot(f2a, aes(x = factor(param, c("Lowest tercile", "Middle Tercile", "Highest tercile")), y=est)) + 
  ggtitle("(a) Effect of Moonlighting on Vote Attendance.\n By constituency distance to London as the crow flies, only Conservative MPs") + 
  geom_pointrange(aes(ymax = high, ymin = low), color = "darkblue") + 
  scale_x_discrete("") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ylab("Regression coefficients") +
  geom_hline(yintercept=0, color="red") + 
  theme_bw() + 
  theme(text = element_text(size=10))

# Panel B - making a fun map!

## I NEED TO CONTINUE FROM HERE TO CONVERT TO FE_OLS TERMS

# Panel C

# Creating the three regressions with vote attendance as dependent, and moonlighting (bin 1000) as main independent.
# These regressions are copy-pasted from author

f2c_low <- feols(expense.accomodation.rent.bin ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$con==1 & data$dist.tercile.con==1,])
summary(f2c_low)

f2c_mid <- feols(expense.accomodation.rent.bin ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$con==1 & data$dist.tercile.con==2,])
summary(f2c_mid)

f2c_hig <- feols(expense.accomodation.rent.bin ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$con==1 & data$dist.tercile.con==3,])
summary(f2c_hig)

# Calculating confidence intervals.
# Confidence errors calculatted by copying outputs from the summaries above

f2c <- data.frame(param = c("Lowest tercile", "Middle Tercile", "Highest tercile"),
                  low = c(0.039433 - 1.96*0.029593, -0.000047 - 1.96*0.043846, 0.096598 - 1.96*0.036407),
                  est = c(0.039433, -0.000047, 0.096598),
                  high = c(0.039433 + 1.96*0.029593, -0.000047 + 1.96*0.043846, 0.096598 + 1.96*0.036407))


f2cplot <- ggplot(f2c, aes(x = factor(param, c("Lowest tercile", "Middle Tercile", "Highest tercile")), y=est)) + 
  ggtitle("(c) Effect of Moonlighting on prob. of claiming London Allowance.\n By constiuency distance to London as the crow flies, only Conservative MPs") + 
  geom_pointrange(aes(ymax = high, ymin = low), color = "darkblue") + 
  scale_x_discrete("") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ylab("Regression coefficients") +
  geom_hline(yintercept=0, color="red") + 
  theme_bw() + 
  theme(text = element_text(size=10))






### Figure 3

# Panel a

# Effect of employment on asking parliamentary questions. Again copied and renamed from author.
# However, here we can just use the dummies for the job so it makes constructing the plot a bit easier

f3a_reg <- feols(questcount.log ~ I(job_director.1000>0) + I(job_board.1000>0) + I(job_consultant.1000>0) + I(job_prof.1000>0) + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster ~ id, data=data[data$con==1,])
summary(f3a_reg)

# Making a df using a slightly more automated method than copy-pasting

f3a <- data.frame(param = c("Director, Chairman, President or Partner", "Board Member", "Consultant or Advisor" , "Professional Position"),
                  low = c(f3a_reg$coefficients[1:4] - (f3a_reg$se[1:4]*1.96)),
                  est = c(f3a_reg$coefficients[1:4]),
                  high = c(f3a_reg$coefficients[1:4] + (f3a_reg$se[1:4]*1.96)))

f3aplot <- ggplot(f3a, aes(x = factor(param, c("Director, Chairman, President or Partner", "Board Member", "Consultant or Advisor", "Professional Position")), y=est)) + 
  ggtitle("(a) Effect of private sector employment on Written parliamentary questions
          \n By job title and industry, only Conservative MPs") + 
  geom_pointrange(aes(ymax = high, ymin = low), color = "darkblue") + 
  scale_x_discrete("") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ylab("Regression coefficients") +
  geom_hline(yintercept=0, color="red") + 
  theme_bw() + 
  theme(text = element_text(size=10))

# Panel B

# First the regression

f3b_reg <- feols(questcount.log ~ I(indcat_health.1000 > 0) + I(indcat_finance.1000 > 0) + I(indcat_consulting.1000 > 0) + I(indcat_knowledge_fp.1000 > 0) + I(indcat_knowledge_nfp.1000 > 0) + I(indcat_goods.1000 > 0) + I(indcat_services.1000 > 0) + I(indcat_other.1000 > 0) + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster ~ id, data=data[data$con==1,])
summary(f3b_ols)

# Making the dataframe for the graph

f3b <- data.frame(param = c("Health ", "Finance ", "Consulting ", "Knowledge,\n for-profit ", 
                            "Knowledge,\n not-for-profit", "Goods", "Services", "Other"),
                  low = c(f3b_reg$coefficients[1:8] - (f3b_reg$se[1:8]*1.96)),
                  est = c(f3b_reg$coefficients[1:8]),
                  high = c(f3b_reg$coefficients[1:8] + (f3b_reg$se[1:8]*1.96)))


f3bplot <- ggplot(f3b, aes(x = factor(reorder(param, -est)), y=est)) + 
  ggtitle("(a) Effect of private sector employment on Written parliamentary questions
          \n By job title and industry, only Conservative MPs") + 
  geom_pointrange(aes(ymax = high, ymin = low), color = "darkblue") + 
  scale_x_discrete("") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ylab("Regression coefficients") +
  geom_hline(yintercept=0, color="red") + 
  theme_bw() + 
  theme(text = element_text(size=10))

### Figure 4

# So this is confusing, because the author has titled his graph somewhat misleadingly.
# What he is investigating is the effect of moonlighting (bin1000) on the effect of asking questions TO each ministry, not "by ministry"
# This necessitates running a seperate FE - regression for questions (log) to each ministry

load("C:\\Users\\victo\\Documents\\r-filer\\data_questions_topic.rda")

# Now we need to weed out the committees and questions to Leader of the House (he is not a minister)

Ministries <- questtopicdata[!(questtopicdata$topic %in% c("Committee on the Electoral Commission",
                                                           "Committee, Parliamentary Standards Authority",
                                                           "House of Commons Commission",
                                                           "Leader of the House")), ]

# Sorting to only Conservatvies

Ministries <- Ministries[Ministries$con == 1,]

# I need to run many regressions and I will try to automate

# Making a list of regression models for each filtered data

#Defining a list
regression_models <- list()

# Grouping the variables (honestly should have done this before)
response_variable <- "questcount.log"
predictor_variables <- c("bin.1000", "minister", "minister.state", "undersec", 
                         "frontbench.team", "com.chair", "com.member", "enter", "leave", 
                         "year", "id")

# Making a loop that will make a list of each regression, which I can then extract estimate for bin.1000
# as well as the standard errors from

for (ministry in m_list) {
  ministry_data <- Ministries[Ministries$topic == ministry, ]

  formula <- as.formula(paste(response_variable, "~", paste(predictor_variables, collapse = " + ")))
  
  model <- feols(formula, data = ministry_data)
  
  regression_models[[ministry]] <- model
}


# Now I have this lovely list. time to combine them in a dataframe!

param <- names(regression_models)
std <- sapply(regression_models, function(model) coeftable(model)[2,2])
est <- sapply(regression_models, function(model) coef(model)["bin.1000"])

# Honestly I should be hired by Harvard, lets go!

f4 <- data.frame(
  param = param,
  low = est - std * 1.96,
  est = est,
  high = est + std * 1.96
)


# Time to make the graph! Making it vertical and a few tweaks since the list is so large now

f4plot <- ggplot(f4, aes(y = factor(reorder(param, -est)), x=est)) + 
  ggtitle("(a) Effect of private sector employment on Written parliamentary questions
          \n By job title and industry, only Conservative MPs") + 
  geom_pointrange(aes(xmax = high, xmin = low), color = "darkblue") + 
  scale_y_discrete("") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylab("Regression coefficients") +
  geom_vline(xintercept=0, color="red") + 
  theme_bw() + 
  theme(text = element_text(size=10))

### Figure 5 ###

# We need new data again, this time about different characteristics about the Ministries. So loading than in

min_charac <- read_csv("C:\\Users\\victo\\Documents\\r-filer\\department_characteristics.csv")

# Renaming and merging in the point estimates

min_charac <- min_charac %>%
  rename("param" = "topic")

f5 <- right_join(min_charac, f4, by='param')

# So now we need four regressions, using the log of these four columns. F

f5_a <- ggplot(f5, aes(x = log(f5$proc.amount + 1), y = f5$est)) + 
  ggtitle("Procurement spending") +
  ylab("Regression estimates for moonlighting") + 
  xlab("log(Procurement spending in Bn.£ + 1)") +
  geom_point(size = 2) + 
  geom_smooth(method = lm) + 
  theme_minimal () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


f5_b <- ggplot(f5, aes(x = log(f5$projects.number + 1), y = f5$est)) + 
  ggtitle("Number of major projects") +
  ylab("Regression estimates for moonlighting") + 
  xlab("log(No. Major projects + 1)") +
  geom_point(size = 2) + 
  geom_smooth(method = lm) + 
  theme_minimal () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

f5_c <- ggplot(f5, aes(x = log(f5$proc.amount + 1), y = f5$est)) + 
  ggtitle("Operating Budget, Bn. £") +
  ylab("Regression estimates for moonlighting") + 
  xlab("log(Operating Budget, Bn. £ + 1)") +
  geom_point(size = 2) + 
  geom_smooth(method = lm) + 
  theme_minimal () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

f5_d <- ggplot(f5, aes(x = log(f5$employees+ 1), y = f5$est)) + 
  ggtitle("Number of employees") +
  ylab("Regression estimates for moonlighting") + 
  xlab("log(No. of employees+ 1)") +
  geom_point(size = 2) + 
  geom_smooth(method = lm) + 
  theme_minimal () + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# NB - This still needs the r-value the author has. Not quite sure how he did it or why, on the
# to-do list I reckon.

### NB: THe author doesnt actually use the lines other than visually. I think it distracts from the message
