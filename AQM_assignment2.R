# Installing necessary packages

install.packages(c("ggplot2, dplyr, stargazer, lfe"))
install.packages("fixest")

# Load the necessary library
library(ggplot2)
library(dplyr)
library(stargazer)
library(lfe)
library(stringr)
library(fixest) 
library(modelsummary)
library(kableExtra)

load("C:\\Users\\vmo\\Desktop\\R_filer\\data_mpyear.rda")
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
# Confidence errors calculatted by copying outputs from the summaries above

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

# Creating the three regressions with vote attendance as independent, and moonlighting (bin 1000) as main dependent.
# These regressions are copy-pasted from author

f2c_low <- felm(expense.accomodation.rent.bin ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$con==1 & data$dist.tercile.con==1,])
summary(f2c_low)

f2c_mid <- felm(expense.accomodation.rent.bin ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$con==1 & data$dist.tercile.con==2,])
summary(f2c_mid)

f2c_hig <- felm(expense.accomodation.rent.bin ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$con==1 & data$dist.tercile.con==3,])
summary(f2c_hig)

# Calculating confidence intervals.
# Confidence errors calculatted by copying outputs from the summaries above

f2c <- data.frame(param = c("Lowest tercile", "Middle Tercile", "Highest tercile"),
                  low = c(0.039433 - 1.96*0.029469, -4.654e-05 - 1.96*4.366e-02, 0.096598 - 1.96*0.036261),
                  est = c(0.039433, -4.654e-05, 0.096598),
                  high = c(0.039433 + 1.96*0.029469, -4.654e-05 + 1.96*4.366e-02, 0.096598 + 1.96*0.036261))


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

f3a_reg <- felm(questcount.log ~ I(job_director.1000>0) + I(job_board.1000>0) + I(job_consultant.1000>0) + I(job_prof.1000>0) + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$con==1,])
summary(f3a_reg)

# This method is maybe a bit cumbersome and manual, but it works and I understand it

f3a <- data.frame(param = c("Director, Chairman, President or Partner", "Board Member", "Consultant or Advisor" , "Professional Position"),
                  low = c(0.49916 - 1.96*0.14991, 0.39235 - 1.96*0.17439, 0.15875 - 1.96*0.13225, 0.19427 - 1.96*0.19640),
                  est = c(0.49916, 0.39235, 0.15875, 0.19427),
                  high = c(0.49916 + 1.96*0.14991, 0.39235 + 1.96*0.17439, 0.15875 + 1.96*0.13225, 0.19427 + 1.96*0.19640))

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

f3b_reg <- felm(questcount.log ~ I(indcat_health.1000 > 0) + I(indcat_finance.1000 > 0) + I(indcat_consulting.1000 > 0) + I(indcat_knowledge_fp.1000 > 0) + I(indcat_knowledge_nfp.1000 > 0) + I(indcat_goods.1000 > 0) + I(indcat_services.1000 > 0) + I(indcat_other.1000 > 0) + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$con==1,])
summary(f3b_reg)

# I very much see that my manual method will get increasingly difficult if I continue.
# So I will try to automate dataframe

f3b <- data.frame(param = c("Health ", "Finance ", "Consulting ", "Knowledge,\n for-profit ", 
                             "Knowledge,\n not-for-profit", "Goods", "Services", "Other"),
                  low = c(f3b_reg$coef[1:8] - (f3b_reg$cse[1:8]*1.96)),
                  est = c(f3b_reg$coef[1:8]),
                  high = c(f3b_reg$coef[1:8] + (f3b_reg$cse[1:8]*1.96)))

# I am very pleased with myself for figurring this out

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
# This necessitates a dataframe distinguishing between which ministry the questions were directed at, which the author thankfully provides

load("C:\\Users\\vmo\\Desktop\\R_filer\\data_questions_topic.rda")
Ministries <- unique(questtopicdata$topic)

# Now we need to weed out the committees and questions to Leader of the House (he is not a minister)

tpcs <- unique(questtopicdata$topic)

# no committees

tpcs <- tpcs[tpcs!=c("Committee on the Electoral Commission",
                     "Committee, Parliamentary Standards Authority",
                     "House of Commons Commission",
                     "Leader of the House")]

# Ok so this is listed straight from author. It's complicated, but I understand it. Will ask if we are allowed

for(i in 1:length(tpcs)){
  tryCatch({	
    m1 <- felm(questcount.log ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id | 0 | id, data=questtopicdata[questtopicdata$topic==tpcs[i] & questtopicdata$con==1,])
    points <- c(points, m1$coefficients[1])
    ses <- c(ses, sqrt(diag(m1$clustervcv))[1])
    lower <- c(lower, (m1$coefficients+qnorm(0.025)*sqrt(diag(m1$clustervcv)))[1])
    upper <- c(upper, (m1$coefficients+qnorm(0.975)*sqrt(diag(m1$clustervcv)))[1])
    topic <- c(topic, tpcs[i])
  }, error=function(e) {
    points <- c(points, NA)
    ses <- c(ses, NA)
    lower <- c(lower, NA)
    upper <- c(upper, NA)
    topic <- c(topic, NA)
  })
}

# Now I have all these lovely lists. time to combine them in a dataframe!

f4 <- data.frame(param = topic,
                 low = lower,
                 est = points,
                 high = upper)

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

### Figure 5

# We need new data again, this time about different characteristics about the Ministries


