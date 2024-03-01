
#Precision-recall curve simulated data
## load libraries required for analysis
library(yardstick)
library(ggplot2)
library(ggfortify)
library(triptych)
library(dplyr)
library(grid)
library(gridExtra)
library(patchwork)
library(ggpubr)


#MCB-DSC plot
#Load data
sim_data <- readRDS("sim_data.rds")

sim_data_v2 <- sim_data
names(sim_data_v2)[1] <- "IR"
sim_data_v2 <- sim_data_v2 %>%
  select(IR, y)


trpt_sim <- triptych(y = sim_data_v2$y, x=sim_data_v2[,1] )

# Assign plot colors
plot_cols <- c(
  "IR" = "#D55E00"
)

MCBDSC_point_cols_M1 <- c(
  "AMOS" = "black",
  "ASAP" = "black",
  plot_cols[1], # isotonic regression
  "BOM" = "black",
  "CLIM120" = "black",
  "DAFFS" = "black",
  "DAFFS-G" = "black",
  "MAG4VW" = "black",
  "MAG4VWF" = "black",
  "MAG4W" = "black",
  "MAG4WF" = "black",
  "MCEVOL" = "black",
  "MOSWOC" = "black",
  "SIDC" = "black"
)


# Figure 7: Brier and Log Score MCB-DSC Plots.
MCBDSC_M1_BrierScore2 <- mcbdsc(sim_data_v2) |>
  autoplot(
    MCBDSC_repel = TRUE,
    colour_values = MCBDSC_point_cols_M1,
    colour_unc = "#009E73",
    MCB_lim = c(0, 0.0785),
    size_axislabels = 12) +
  ggtitle("(a) Brier Score") +
  theme(plot.title = element_text(size = 12, hjust = 0))

MCBDSC_M1_LogScore <- mcbdsc(sim_data_v2, score = "log_score") |>
  autoplot(
    MCBDSC_repel = TRUE,
    colour_values = MCBDSC_point_cols_M1,
    colour_unc = "#009E73",
    MCB_lim = c(0, 0.2),
    size_axislabels = 12
  ) +
  annotate("text", x = 0.2, y = 0.0405, label = "MAG4VWF", size = 3, hjust = 1) +
  ggtitle("(b) Logarithmic Score") +
  theme(plot.title = element_text(size = 12, hjust = 0))


mcb <- ggarrange(MCBDSC_M1_BrierScore2, MCBDSC_M1_LogScore, ncol = 2) 
      
#save plot
ggsave(filename = "C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar_gh/Paper/Plots_sim/mcb.pdf",
       mcb, width = 20, height = 12, units = "cm")



