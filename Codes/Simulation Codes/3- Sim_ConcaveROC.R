
library(triptych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(here)

# Load simulated data
sim_data <- readRDS("sim_data.rds")

# Figure -Concave ROC Curve
ROC_PAV <- triptych::roc(y = sim_data$y, x=sim_data$x, concave = TRUE)
AUC_PAV <- purrr::map(ROC_PAV, \(o) o$estimate$auc) |> unlist()
p_ROC_c <- ROC_PAV |> autoplot() &
  scale_colour_manual(
    labels = c("IR"),
    values = "#E69F00",
    guide = guide_legend(title = "Forecast"))


# Annotate the ROC curve diagrams with AUC values
annotate_auc <- function(auc_values) {
  x_annot <- 0.625
  y_annot <- 0.45
  dodge <- y_annot / 5
  annot_size <- 10 / .pt
  
  annotate(
    geom = "text",
    x = x_annot,
    y = y_annot - (0:1) * dodge,
    label = c("AUC:", sprintf("%0.3f", auc_values)),
    color = c("black", "#E69F00"),
    size = annot_size,
    fontface = c(1),
    hjust = 0
  )
}


p_ROC_c_annot <- p_ROC_c + annotate_auc(AUC_PAV)
concave_sim <- p_ROC_c_annot + ggtitle("Concave ROC Curve")

#save plot
ggsave(filename = "C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar_gh/Paper/Plots_sim/concave_sim.pdf",
       concave_sim, width=5, height=5, units="in")



