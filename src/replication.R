#--------------------------------------------------------------------------------------------------
# This script replicates the main findings in Boussalis, Coan, and Poberezhskaya (2016).
# For more details, please consult the paper:

# Constantine Boussalis, Travis G. Coan, and Marianna Poberezhskaya (2016), "Measuring and 
# modeling Russian newspaper coverage of climate change." Forthcoming, Global Environmental Change.

#----------------------------------------------------------------------------------------------------
# INITIALIZE

setwd('~/git_repos/russian_newspaper_coverage_of_CC') # CHANGE THIS DIRECTORY
library(plyr)
library(zoo)
library(ggplot2)
library(ggthemes)
library(coda)
library(brms) # Easy way to estimate hierarchical gamma hurdle model
# User-written helper functions
source('src/utility.R')
options(scipen=999)
load('data/replication_data.Rda')

########### ESTIMATE ###########

# Figure 1 -- Look at hurdle portion only
dv <- "avoid"
results_hurdle <- est.logit.model(dv, dat)

# Figure 2 -- Conditional coverage of specific issues
dvs <- c("science_impacts", "energy_combined", "ir", "irenv")
results = list()
for (dv in dvs) {
  results[[dv]] <- est.beta.model(dv, dat)
}

# Combine estiamtes into a single data frame
estimates_hurdle <- results_hurdle[[1]]
estimates_hurdle$dv_label <- "hurdle"
estimates_beta <- parse.beta.estimates(results)
estimation_results <- as.data.frame(rbind(estimates_beta, estimates_hurdle))

########### PLOT ###########

# Figure 1
load('data/ru_usa_comparison.Rda')

# Make time var
df_fig1$date <- paste(df_fig1$year, df_fig1$quarter, sep = '-')
df_fig1$yq <- as.yearqtr(df_fig1$date, format = "%Y-%q")
df_fig1$t <- as.Date(df_fig1$yq)

# Make labels
usa <- df_fig1[48,]
usa$y <- 755
rus <- df_fig1[49,]
rus$y <- 30
nyt <- df_fig1[45,]
nyt$y <- 380

# Plot
fig1 <- ggplot(df_fig1, aes(t)) +
  geom_line(aes(y = art_cnt_rus), colour = "orangered2") +  
  geom_line(aes(y = art_cnt_usatot), colour = "black") +
  geom_line(aes(y = art_cnt_nyt), colour = "grey") +
  geom_text(aes(x=t, y=y, group=NULL), data=usa, label = "US Prestige Press (n = 5)", size = 1.9) +
  geom_text(aes(x=t, y=y, group=NULL), data=nyt, label = "New York Times", size = 1.9) +
  geom_text(aes(x=t, y=y, group=NULL), data=rus, label = "Russian Newspapers (n = 23)", size = 1.9) +
  labs(x = "", y = "Article Count") +
  scale_y_continuous(breaks = c(0,250,500,750,1000,1250)) +
  theme_classic() +
  theme(
    text = element_text(size=6),
    axis.line = element_line(),
    axis.line.y = element_line(color = "grey95"),
    axis.line.x = element_line(color = "grey95"),
    strip.text = element_text(face="bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.title=element_blank(),
    axis.ticks = element_line(color = "grey40"),
    legend.position="none"
  )
fig1

# Write Figure 1 to output/
pdf("output/figure1.pdf", width=5, height=3)
fig1
dev.off()

# Prepare data for Figures 2 and 3
df_to_plot <- prep.plot.data(estimation_results)

# Figure 2
df_fig2 <- subset(df_to_plot, label_new == "Coverage Decision" & vname != "Intercept")
var_names_mod <- c("Intercept", "Putin", "Kremlin", "Right", "Center", "State\n(owner)", "Opp.\nParty\n(owner)", "Business\n(owner)",
         "Energy", "Inflation", "Disasters")
df_fig2$vname <- mapvalues(df_fig2$vname, from = levels(df_fig2$vname), to = var_names_mod)
fig2 <- ggplot(df_fig2, aes(vname, est)) +
  geom_hline(aes(yintercept = 0), colour="orangered2", size = .2) + 
  geom_pointrange(aes(ymax = upper, ymin = lower), color = "grey45", size = .20, shape = 20) + 
  ylab("Log Odds\n") +
  xlab("") +
  facet_wrap(~label_new) +
  theme_fivethirtyeight() +
  theme(
    text = element_text(size=8),
    rect = element_rect(fill = "white"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    axis.line.x = element_line(color = "grey80"),
    axis.title = element_text(),
    axis.title.x = element_text(face="bold", size = 8),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    strip.text = element_text(face="bold")
  )
fig2

# Write Figure 2 to output/
pdf("output/figure2.pdf", width=5, height=3)
fig2
dev.off()

# Figure 3
df_fig3 <- subset(df_to_plot, label_new != "Coverage Decision" & vname != "Intercept")
fig3 <- ggplot(df_fig3, aes(vname_reverse, est)) +
  geom_hline(aes(yintercept = 0), colour="orangered2", size = .2) + 
  geom_pointrange(aes(ymax = upper, ymin=lower), color = "grey45", size = .30, shape = 20) + 
  ylab("Log Odds") +
  facet_wrap(~label_new, ncol = 2, nrow = 2) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(
    text = element_text(size=10),
    rect = element_rect(fill = "white"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    axis.line.x = element_line(color = "grey80"),
    axis.title = element_text(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 8),
    strip.text = element_text(face="bold")
  )
fig3

# Write Figure 3 to output/
pdf("output/figure3.pdf", width=6, height=4.5)
fig3
dev.off()

