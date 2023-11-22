# Load the necessary library
library(ggplot2)
library(dplyr)

load("/Users/thomaspadkjaer/Downloads/data_mpyear.rda")
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

