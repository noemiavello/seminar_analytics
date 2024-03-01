library(triptych)
library(ggplot2)
library(dplyr)
library(patchwork)


# Load and filter data
vehic_models <- readRDS("vehic_models.rds")
colnames(vehic_models)[colnames(vehic_models) == 'obs'] <- 'y'

trpt_M1full <- triptych(y = vehic_models$y, x=vehic_models[,2:4] )

# Assign plot colors
plot_cols_M1 <- c(
  "LR" = "#E69F00",
  "NB" = "#0072B2",
  "RF" = "#D55E00"
)

MCBDSC_point_cols_M1 <- c(
  "AMOS" = "black",
  "ASAP" = "black",
  plot_cols_M1[3], # ASSA
  "BOM" = "black",
  "CLIM120" = "black",
  "DAFFS" = "black",
  "DAFFS-G" = "black",
  "MAG4VW" = "black",
  "MAG4VWF" = "black",
  "MAG4W" = "black",
  "MAG4WF" = "black",
  "MCEVOL" = "black",
  # plot_cols_M1[4], # MCSTAT
  "MOSWOC" = "black",
  plot_cols_M1[1], # NICT
  plot_cols_M1[2], # NOAA
  "SIDC" = "black"
)


# Figure 7: Brier and Log Score MCB-DSC Plots.
MCBDSC_M1_BrierScore2 <- mcbdsc(vehic_models) |>
  autoplot(
    MCBDSC_repel = TRUE,
    colour_values = MCBDSC_point_cols_M1,
    colour_unc = "#009E73",
    MCB_lim = c(0, 0.0785),
    size_axislabels = 12) +
  ggtitle("(a) Brier Score") +
  theme(plot.title = element_text(size = 12, hjust = 0))

MCBDSC_M1_LogScore <- mcbdsc(vehic_models, score = "log_score") |>
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

MCBDSC_M1_BrierLogScores <- MCBDSC_M1_BrierScore2 + MCBDSC_M1_LogScore
MCBDSC_M1_BrierLogScores

ggsave(
  filename = "C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar_gh/Paper/Plots_app/app_MCBDSC.pdf",
  plot = MCBDSC_M1_BrierLogScores,
  width = 24, height = 12, units = "cm"
)