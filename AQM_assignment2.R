# Installing necessary packages

install.packages(c("ggplot2", "dplyr", "stargazer", "lfe", "stringr", "fixest", "modelsummary", "kableExtra"))
install.packages("tidyverse")
install.packages("maps")
install.packages("mapproj")

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
library(maps)
library(mapproj)

load("C:\\Users\\victo\\Documents\\r-filer\\data_mpyear.rda")
data
# Assuming the data is loaded into a variable named 'data'
# Replace 'data' with the actual name of the data frame in your RDA file


#### FIGURE 1 ####
#### FIGURE 1 ####
## Panel a ###

d1_a <- data %>%
  group_by(year) %>%
  summarize(
    sum_all = sum(earnings.all.infl)/1000000,
    sum_con = sum(earnings.all.infl[con == 1]/1000000),
    sum_lab = sum(earnings.all.infl[lab == 1]/1000000),
    sum_oth = sum(earnings.all.infl[oth == 1]/1000000))

# Create the plot

d1_a_f <- ggplot(d1_a, aes(x = year)) +
  geom_line(aes(y = sum_all), linetype = "solid", lwd=1) +
  geom_line(aes(y = sum_con), linetype = "dashed", lwd=1) +
  geom_line(aes(y = sum_lab), linetype = "dotdash", lwd=1) +
  geom_line(aes(y = sum_oth), linetype = "dotted", lwd=1) +
  geom_text(data = data.frame(year = median(d1_a$year), y = d1_a$sum_all[d1_a$year == max(d1_a$year)]), 
            aes(label = "All", x = year, y = y), hjust = 1.4, vjust = 0) +
  geom_text(data = data.frame(year = median(d1_a$year), y = d1_a$sum_con[d1_a$year == median(d1_a$year)]), 
            aes(label = "Con", x = year, y = y), hjust = 1.4, vjust = -0.5) +
  geom_text(data = data.frame(year = median(d1_a$year), y = d1_a$sum_lab[d1_a$year == median(d1_a$year)]), 
            aes(label = "Lab", x = year, y = y), hjust = 1.4, vjust = -1.5) +
  geom_text(data = data.frame(year = median(d1_a$year), y = d1_a$sum_oth[d1_a$year == max(d1_a$year)]), 
            aes(label = "Oth", x = year, y = y), hjust = 1.4, vjust = 0) +
  theme_minimal() +
  labs(x = "Year", y = "Total Earnings in Million £", title = "(a) Total Sector Private Earnings") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  geom_vline(xintercept = c(2010, 2015), color = "grey", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "grey", linetype = "solid", size = 1) +
  scale_x_continuous(breaks = unique(data$year)) + 
  scale_y_continuous(limits = c(0, 7), breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text.x = element_text(angle = 360), 
        axis.line.x = element_line(color="grey", size = 0.5),
        axis.title.x = element_text(size = 8),
        axis.line.y = element_line(color="grey", size = 0.5),
        axis.text.y = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(face = "bold", color = "black", size = 8))
  

### Panel b ###
d1_b <- data %>%
  group_by(year) %>%
  summarize(
    share_all = sum(earnings.all.infl>1000)/sum(earnings.all.infl>=0),
    share_con = sum(if_else(con == 1, earnings.all.infl, 0)>1000)/
      sum(if_else(con == 1, earnings.all.infl, -1)>=0),
    share_lab = sum(if_else(lab == 1, earnings.all.infl, 0)>1000)/
      sum(if_else(lab == 1, earnings.all.infl, -1)>=0),
    share_oth = sum(if_else(oth == 1, earnings.all.infl, 0)>1000)/
      sum(if_else(oth == 1, earnings.all.infl, -1)>=0
      ))

#Create the plot 
d1_b_f <- ggplot(d1_b, aes(x = year)) +
  geom_line(aes(y = share_all, group = 1), linetype = "solid", lwd=1) +
  geom_line(aes(y = share_con, group = 1), linetype = "dashed", lwd=1) +
  geom_line(aes(y = share_lab, group = 1), linetype = "dotdash", lwd=1) +
  geom_line(aes(y = share_oth, group = 1), linetype = "dotted", lwd=1) +
  geom_text(data = data.frame(year = median(d1_b$year), y = d1_b$share_all[d1_a$year == max(d1_b$year)]), 
            aes(label = "All", x = year, y = y), hjust = 1.4, vjust = 0) +
  geom_text(data = data.frame(year = median(d1_b$year), y = d1_b$share_con[d1_a$year == max(d1_b$year)]), 
            aes(label = "Con", x = year, y = y), hjust = 1, vjust = -0.5) +
  geom_text(data = data.frame(year = median(d1_b$year), y = d1_b$share_lab[d1_a$year == min(d1_b$year)]), 
            aes(label = "Lab", x = year, y = y), hjust = 1.4, vjust = 0) +
  geom_text(data = data.frame(year = median(d1_b$year), y = d1_b$share_oth[d1_a$year == max(d1_b$year)]), 
            aes(label = "Oth", x = year, y = y), hjust = 1.4, vjust = 0) +
  theme_minimal() +
  labs(x = "Year", y = "Share of MPs with Outside Income", title = "(b) Share of MPs with Private Sector \n Earnings ≥ 1000") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  geom_vline(xintercept = c(2010, 2015), color = "grey", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "grey", linetype = "solid", size = 1) +
  scale_x_continuous(breaks = unique(data$year)) + 
  scale_y_continuous(limits = c(0, 0.32), breaks = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text.x = element_text(angle = 360), 
        axis.line.x = element_line(color="grey", size = 0.5),
        axis.title.x = element_text(size = 8),
        axis.line.y = element_line(color="grey", size = 0.5),
        axis.text.y = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(face = "bold", color = "black", size = 8))

### Panel c ###

d1_c <- data %>%
  filter(earnings.all.infl > 0) %>%
  group_by(year) %>%
  summarize(mean_nonull_all = mean(earnings.all.infl),
    mean_nonull_con = mean(earnings.all.infl[con == 1]),
    mean_nonull_lab = mean(earnings.all.infl[lab == 1]),
    mean_nonull_oth = mean(earnings.all.infl[oth == 1]
            ))

#create the plot
d1_c_f <- ggplot(d1_c, aes(x = year)) +
  geom_line(aes(y = mean_nonull_all), linetype = "solid", lwd=1) +
  geom_line(aes(y = mean_nonull_con), linetype = "dashed", lwd=1) +
  geom_line(aes(y = mean_nonull_lab), linetype = "dotdash", lwd=1) +
  geom_line(aes(y = mean_nonull_oth), linetype = "dotted", lwd=1) +
  geom_text(data = data.frame(year = median(d1_c$year), y = d1_c$mean_nonull_all[d1_a$year == median(d1_c$year)]), 
            aes(label = "All", x = year, y = y), hjust = -1, vjust = -1.5) +
  geom_text(data = data.frame(year = median(d1_c$year), y = d1_c$mean_nonull_con[d1_a$year == median(d1_c$year)]), 
            aes(label = "Con", x = year, y = y), hjust = -0.8, vjust = -4) +
  geom_text(data = data.frame(year = median(d1_c$year), y = d1_c$mean_nonull_lab[d1_a$year == median(d1_c$year)]), 
            aes(label = "Lab", x = year, y = y), hjust = -0.8, vjust = 1) +
  geom_text(data = data.frame(year = median(d1_c$year), y = d1_c$mean_nonull_oth[d1_a$year == median(d1_c$year)]), 
            aes(label = "Oth", x = year, y = y), hjust = 1.4, vjust = -0.5) + 
  theme_minimal() +
  labs(x = "Year", y = "Mean Annual Earnings in £", title = "(c) Mean Earnings of MPs with Jobs") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 1)
  ) +
  geom_vline(xintercept = c(2010, 2015), color = "grey", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "grey", linetype = "solid", size = 1) +
  scale_x_continuous(breaks = unique(data$year)) + 
  scale_y_continuous(limits = c(0, 55000), breaks = c(0, 10000, 20000, 30000, 40000, 50000)) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text.x = element_text(angle = 360), 
        axis.line.x = element_line(color="grey", size = 0.5),
        axis.title.x = element_text(size = 8),
        axis.line.y = element_line(color="grey", size = 0.5),
        axis.text.y = element_text(angle = 90),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(face = "bold", color = "black", size = 8))

### Panel d ###
d1_d <- data %>%
  summarise(
    sum_director = sum(job_director),
    sum_prof = sum(job_prof),
    sum_consultant = sum(job_consultant),
    sum_board = sum(job_board)
  ) %>%
  pivot_longer(cols = everything(), names_to = "job", values_to = "n") %>%
  mutate(proportion = n / sum(n))


d1_d_f <- ggplot(d1_d, aes(y = proportion, x = fct_reorder(job, proportion))) + 
  geom_col(color = "grey", fill = "grey") + 
  coord_flip() + 
  theme_minimal() +
  labs(x = "", y = "Share", title = "(d) Employment: Share of Job Titles (MP-Years)") +
  scale_x_discrete(labels = c("sum_director" = "Director,\nChairman,\nPresident,\nPartner", 
                              "sum_consultant"="Consultant,\nAdvisor", 
                              "sum_board"="Board\nMember",
                              "sum_prof"="Professional\nPosition")) + 
  scale_y_continuous(limits = c(0, 0.40), breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.line.y = element_line(color="grey", size = 0.3), 
        plot.title = element_text(face = "bold", color = "black", size = 8),
        axis.title.x = element_text(size = 8))


### Panel e ###  
d1_e <- data %>%
  summarise(
    sum_health = sum(indcat_health),
    sum_finance = sum(indcat_finance),
    sum_consulting = sum(indcat_consulting),
    sum_knowledge_profit = sum(indcat_knowledge_fp),
    sum_knowledge_noprofit = sum(indcat_knowledge_nfp),
    sum_goods = sum(indcat_goods),
    sum_services = sum(indcat_services), 
    sum_other = sum(indcat_other)
  ) %>%
  pivot_longer(cols = everything(), names_to = "job", values_to = "n") %>%
  mutate(proportion = n / sum(n))



d1_e_f <- ggplot(d1_e, aes(y = proportion, x = fct_relevel(fct_reorder(job, proportion), 'sum_other'))) + 
  geom_col(color = "grey", fill = "grey") + 
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "Share", title = "(e) Employment: Share in Different Industries \n(MP-Years)") +
  scale_x_discrete(labels = c("sum_goods" = "Goods",
                              "sum_knowledge_profit" ="Knowledge,\nfor-profit",
                              "sum_consulting"= "Consulting", 
                              "sum_finance" = "Finance", 
                              "sum_services" = "Services",
                              "sum_knowledge_noprofit" = "Knowledge,\nnot-for-profit",
                              "sum_health" = "Health", 
                              "sum_other" = "Other")) +
  scale_y_continuous(limits = c(0, 0.25), breaks = c(0, 0.05, 0.10, 0.15, 0.20)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.line.y = element_line(color="grey", size = 0.3), 
        plot.title = element_text(face = "bold", color = "black", size = 8),
        axis.title.x = element_text(size = 8))



grid.arrange(
  d1_a_f, d1_b_f, d1_c_f, 
  d1_d_f, d1_e_f,
  layout_matrix = rbind(c(1, 1, 2, 2, 3, 3),
                        c(4, 4, 4, 5, 5, 5)), 
  top=text_grob("FIGURE 1 Private Sector Earnings of Members of the House of Commons, 2010-2016", 
               size = 15, face = "bold", hjust = 0.7, lineheight = 2))

fig_1 <- arrangeGrob(
  d1_a_f, d1_b_f, d1_c_f, 
  d1_d_f, d1_e_f,
  layout_matrix = rbind(c(1, 1, 2, 2, 3, 3),
                        c(4, 4, 4, 5, 5, 5)), 
  top=text_grob("FIGURE 1 Private Sector Earnings of Members of the House of Commons, 2010-2016", 
                size = 15, face = "bold", hjust = 0.7, lineheight = 2))

ggsave(file="fig_1_assignment2.pdf", fig_1)


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

# Loading in the employment data. Need to limit to only domestic employment and only Conservatives

load("C:\\Users\\victo\\Documents\\r-filer\\employment_coords.rda")

locdata <- locdata[locdata$foreign == 0 & locdata$con == 1,]

# I need to include a "group" variable to make the map work, just 1

locdata$group <- 1
data$group <- 1

# So for the author's version of the map, he features all Conservative MPs in the third group as the crow flies from London.
# Then he makes arrows to where those who moonlight are employed.
# I need to merge that ID to employment address in the data file

locdata$id <- as.numeric(locdata$id)

f2b <- right_join(data, locdata, by = "id")

# Making map of UK, limited to only third tercile

UK <- map_data(map = "world", region = "UK") 

UK_map <- ggplot(data = UK, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") +
  coord_map(ylim = c(50, 55.5)) +
  geom_point(data = data[data$dist.tercile.con == 3,], aes(x = const.x, y = const.y)) + 
  geom_segment(data = locdata[locdata$dist.tercile.con == 3,], aes(x = employ.x, y = employ.y, xend = const.x, yend = const.y), 
               arrow = arrow(length = unit(0.2, "cm"), ends = "first"), col = "red", alpha = 0.7) + 
  theme_void()

### An improved map - our contribution

# Trying to improve map by:
# Making non-moonligther points more transparent
# Making an area of London, and colouring points where arrows terminate in London
# Changing color of arrows depending on distance to London

# Getting coordinates for City of London and saving them

CoL_y <- 51.51279 
CoL_x <- -0.09184

# Merging these into the locdata by taking the difference between x and y, and then adding those together

locdata$london.x <- abs(CoL_x - locdata$employ.x)
locdata$london.y <- abs(CoL_y - locdata$employ.y)
locdata$london <- locdata$london.x + locdata$london.y


UK_map_2 <- ggplot(data = UK, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") +
  coord_map(ylim = c(50, 55.5)) +
  geom_point(data = data[data$dist.tercile.con == 3,], aes(x = const.x, y = const.y), col = "grey", alpha = 0.3, size = 1.5) + 
  geom_point(data = locdata[locdata$dist.tercile.con == 3,], aes(x = const.x, y = const.y), col = "black", alpha = 1, size = 2.5) +
  geom_segment(data = locdata[locdata$dist.tercile.con == 3,], aes(x = employ.x, y = employ.y, xend = const.x, yend = const.y, color = london), 
               arrow = arrow(length = unit(0.2, "cm"), ends = "first"), alpha = 0.35) + 
  scale_color_gradient(low = "red", high = "yellow") +
  theme_void() + 
  theme(legend.position = "none")


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

### Playground

# Merging in constituency distance to London for the year 2016

data2016 <- data[data$year == 2016,]

data2016 <- data2016 %>%
  mutate(london = abs(CoL_x - const.x) + abs(CoL_y - const.y))

# Difference between moonlighting - distance to London

data2016 %>%
  group_by(bin.1000) %>%
  summarise_at(vars(london), list(name = mean))

# Running simple regression

no_controls <- lm(bin.1000 ~ london, data = data2016)
summary(no_controls)

controls <- lm(bin.1000 ~ london + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave, data = data2016)
summary(controls)

lab <- lm(bin.1000 ~ london + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave, data = data2016[data2016$lab == 1,])
summary(lab)

con <- lm(bin.1000 ~ london + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave, data = data2016[data2016$con == 1,])
summary(con)

# Interesting not significant... Hmm trying to do average distance to London

data2016 %>%
  group_by(con) %>%
  summarise_at(vars(london), list(name = mean))

data2016 %>%
  group_by(lab) %>%
  summarise_at(vars(london), list(name = mean))

# Trying to do regression controlling for distance to London and conservative membership

chips <- lm(present ~ bin.1000 + london + con + lab + minister + minister.state + undersec + frontbench.team + shadow.cabinet + com.chair + com.member + enter + leave, data=data2016)
summary(chips)

# Does it also hold with labour?

# Making the London column for the dataset

data <- data %>%
  mutate(london = abs(CoL_x - const.x) + abs(CoL_y - const.y))

# Making the terciles

tert_lab <- quantile(data$london[data$lab == 1], c(0:3/3))

data <- data[data$lab == 1,] %>%
  mutate (dist.tercile.lab = cut(london, 
                                 tert_lab, 
                                 include.lowest = T, 
                                 labels = c(1, 2, 3)))

# Running regressions

lab_1 <- feols(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$lab==1 & data$dist.tercile.lab==1,])
summary(lab_1)

lab_2 <- feols(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$lab==1 & data$dist.tercile.lab==2,])
summary(lab_2)

lab_3 <- feols(present ~ bin.1000 + minister + minister.state + undersec + frontbench.team + com.chair + com.member + enter + leave | year + id, cluster = ~ id, data=data[data$lab==1 & data$dist.tercile.lab==3,])
summary(lab_3)

# Confidence errors calculated by copying outputs from the summaries above

lab_terciles <- data.frame(param = c("Lowest tercile", "Middle Tercile", "Highest tercile"),
                           low = c(lab_1$coefficients[1] - 1.96*lab_1$se[1], lab_2$coefficients[1] - 1.96*lab_2$se[1], lab_3$coefficients[1] - 1.96*lab_3$se[1]),
                           est = c(lab_1$coefficients[1], lab_2$coefficients[1], lab_3$coefficients[1]),
                           high = c(lab_1$coefficients[1] + 1.96*lab_1$se[1], lab_2$coefficients[1] + 1.96*lab_2$se[1], lab_3$coefficients[1] + 1.96*lab_3$se[1]))

# Making a figure like 2a

extraplot <- ggplot(lab_terciles, aes(x = factor(param, c("Lowest tercile", "Middle Tercile", "Highest tercile")), y=est)) + 
  ggtitle("(a) Effect of Moonlighting on Vote Attendance.\n By constituency distance to London as the crow flies, only Labour MPs") + 
  geom_pointrange(aes(ymax = high, ymin = low), color = "darkred") + 
  scale_x_discrete("") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ylab("Regression coefficients") +
  geom_hline(yintercept=0, color="blue") + 
  theme_bw() + 
  theme(text = element_text(size=10))
