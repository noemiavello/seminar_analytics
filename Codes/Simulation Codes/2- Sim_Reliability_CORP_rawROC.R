
setwd("C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar_gh/Codes")

library(here)
source("rel_diag_classic.R")

#load packages
library(reliabilitydiag)
library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(triptych)
library(tidyr)
library(grid)

sim_data <- readRDS("sim_data.rds")


#Plot classical reliability diagram and CORP Reliability Diagram
#using isotonic regression model 

Fig <- tibble(
  enum = letters[1:2],
  bins = c(10, NA),
  reldiag_type = c("BaC", "CORP") %>% rep.int(c(1, 1))
) %>%
  mutate(.,
         plot = purrr::pmap(., function(enum, bins, reldiag_type) {
           if (reldiag_type == "BaC") {
             rel.diag.classic(rlz=sim_data$y, FC=sim_data$x, bins=bins)$p +
               ggtitle(sprintf("(%s) IR / %i Equidistant bins", enum, bins)) +
               xlab("Forecast value") +
               ylab("CEP") +
               theme_bw() +
               theme(aspect.ratio = 1)
           } else if (reldiag_type == "CORP") {
             set.seed(42)
             r <- reliabilitydiag(sim_data$x, y = sim_data$y, n.boot = 100)
             autoplot(r) +
               ggtitle(sprintf("(%s) IR / CORP", enum)) +
               annotate(
                 "text",
                 x = .125,
                 y = .94,
                 label = sprintf("MCB = .%03d",
                                 round(summary(r)$miscalibration * 1000)),
                 color = "red"
               ) +
               annotate(
                 "text",
                 x = .125,
                 y = .88,
                 label = sprintf("DSC = .%03d",
                                 round(summary(r)$discrimination * 1000))
               ) +
               annotate(
                 "text",
                 x = .125,
                 y = .82,
                 label = sprintf("UNC = .%03d",
                                 round(summary(r)$uncertainty * 1000))
               )
           }})
  )

purrr::pmap(Fig, function(enum, bins, reldiag_type, plot) {
  ggsave(
    here(sprintf("C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar_gh/Paper/Plots_sim/Fig__%s_%s_%s.pdf", enum, bins, reldiag_type)),
    plot,
    width=5, height=5, units="in"
  )
})


#Raw ROC Curve

# Figure -Raw ROC Curve
ROC_raw <- triptych::roc(y = sim_data$y, x=sim_data$x, concave = FALSE)
AUC_raw <- purrr::map(ROC_raw, \(o) o$estimate$auc) |> unlist()
p_ROC_a <- ROC_raw |> autoplot() &
  scale_colour_manual(
    labels = c("Isotonic regression"),
    values = "#E69F00",
    guide = guide_legend(title = "Forecast") +
      theme(
        legend.position = "bottom"))  

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

p_ROC_a_annot <- p_ROC_a + annotate_auc(AUC_raw)
raw_roc <- p_ROC_a_annot + ggtitle("(c) Original ROC Curve")


layout1 <- grid.arrange(arrangeGrob(grobs=Fig$plot,ncol=2), raw_roc)

#save plots
ggsave(filename = "C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar_gh/Paper/Plots_sim/layout1.pdf",
       layout1, width=9, height=7, units="in")


