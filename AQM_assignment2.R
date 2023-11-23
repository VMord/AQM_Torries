# Installing necessary packages

install.packages(c("ggplot2, dplyr, stargazer, lfe"))

# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(stargazer)
library(lfe)

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

# Vote rebellions

t1ra <- felm(rebel ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id | 0 | id, data=data)
summary(tab1_reb_all)

t1rc <- felm(rebel ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$con==1,])
summary(tab1_reb_con)

t1rl <- felm(rebel ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$lab==1,])
summary(tab1_reb_lab)

# Vote participation

t1pa <- felm(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id | 0 | id, data=data)
summary(tab1_par_all)

t1pc <- felm(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$con==1,])
summary(tab1_par_con)

t1pl <- felm(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$lab==1,])
summary(tab1_par_lab)

# Number of parliamentary questions (q) log

t1qa <- felm(questcount.log ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id | 0 | id, data=data)
summary(tab1_npq_all)

t1qc <- felm(questcount.log ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$con==1,])
summary(tab1_npq_con)

t1ql <- felm(questcount.log ~ bin.1000 + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave | year + id | 0 | id, data=data[data$lab==1,])
summary(tab1_npq_lab)

# Making an output for the regression results

stargazer(t1ra, t1rc, t1rl, t1pa, t1pc, t1pl, t1qa, t1qc, t1ql,
          dep.var.labels=c(rep("Vote Rebellion (Share)", 1), rep("Vote Participation (Share)", 1), rep("log(Number Parliamentary Questions + 1)", 1)),
          covariate.labels=c("Earnings ≥ £1,000"), 
          column.labels=rep(c("All", "Conservative", "Labour"), 3))
  
#### Figure 2 ####

# Creating the three regressions

d2_a_low <- lm(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave, data=data[data$dist.tercile.con==1,])
summary(d2_a_low)

d2_a <- ggplot(data, aes(x = present, y = dist.tercile.con))

