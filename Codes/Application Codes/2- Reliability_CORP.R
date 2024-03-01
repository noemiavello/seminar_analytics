
setwd("C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar_gh/Codes")

library(here)
source("rel_diag_classic.R")
library(reliabilitydiag)
library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyr)
library(ggpubr)


app_data <- readRDS("vehic_models.rds")

#NAIVE BAYES
Fig1 <- tibble(
  bins = c(10, NA),
  reldiag_type = c("BaC", "CORP") %>% rep.int(c(1, 1))
) %>%
  mutate(.,
         plot = purrr::pmap(., function(bins, reldiag_type) {
           if (reldiag_type == "BaC") {
             rel.diag.classic(rlz=app_data$obs, FC=app_data[["NB"]], bins=bins)$p +
               ggtitle(sprintf(" (a) Naive Bayes / 10 Equidistant Bins", bins)) +
               xlab("Forecast value") +
               ylab("CEP") +
               theme_bw() +
               theme(aspect.ratio = 1, plot.title = element_text(size = 10, face = "bold"))
           } else if (reldiag_type == "CORP") {
             set.seed(42)
             r <- reliabilitydiag(app_data[["NB"]], y = app_data$obs, n.boot = 100)
             autoplot(r) +
               ggtitle("(b) Naive Bayes / CORP") +
               theme(plot.title = element_text(size = 10, face = "bold")) +
               annotate(
                 "text",
                 x = .125,
                 y = .94,
                 size = 3,
                 label = sprintf("MCB = .%03d",
                                 round(summary(r)$miscalibration * 1000)),
                 color = "red"
               ) +
               annotate(
                 "text",
                 x = .125,
                 y = .88,
                 size = 3,
                 label = sprintf("DSC = .%03d",
                                 round(summary(r)$discrimination * 1000))
               ) +
               annotate(
                 "text",
                 x = .125,
                 y = .82,
                 size = 3,
                 label = sprintf("UNC = .%03d",
                                 round(summary(r)$uncertainty * 1000))
               )
           }})
  )

purrr::pmap(Fig1, function(bins, reldiag_type, plot) {
  ggsave(
    here(sprintf("C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar/Seminar Paper (paper + code)/Codigos V2/Plots/Fig1.pdf",bins, reldiag_type)),
    plot,
    width=5, height=5, units="in"
  )
})

grid.arrange(grobs = Fig1$plot, ncol=2)


###############################################################

#RANDOM FOREST 

Fig2 <- tibble(
  bins = c(10, NA),
  reldiag_type = c("BaC", "CORP") %>% rep.int(c(1, 1))
) %>%
  mutate(.,
         plot = purrr::pmap(., function(bins, reldiag_type) {
           if (reldiag_type == "BaC") {
             rel.diag.classic(rlz=app_data$obs, FC=app_data[["RF"]], bins=bins)$p +
               ggtitle(sprintf("(c) Random Forest / %i Equidistant Bins",bins)) +
               xlab("Forecast value") +
               ylab("CEP") +
               theme_bw() +
               theme(aspect.ratio = 1, plot.title = element_text(size = 10, face = "bold"))
           } else if (reldiag_type == "CORP") {
             set.seed(42)
             r <- reliabilitydiag(app_data[["RF"]], y = app_data$obs, n.boot = 100)
             autoplot(r) +
               ggtitle("(d) Random Forest / CORP") +
               theme(plot.title = element_text(size = 10, face = "bold")) +
               annotate(
                 "text",
                 x = .125,
                 y = .94,
                 size = 3,
                 label = sprintf("MCB = .%03d",
                                 round(summary(r)$miscalibration * 1000)),
                 color = "red"
               ) +
               annotate(
                 "text",
                 x = .125,
                 y = .88,
                 size = 3,
                 label = sprintf("DSC = .%03d",
                                 round(summary(r)$discrimination * 1000))
               ) +
               annotate(
                 "text",
                 x = .125,
                 y = .82,
                 size = 3,
                 label = sprintf("UNC = .%03d",
                                 round(summary(r)$uncertainty * 1000))
               )
           }})
  )

purrr::pmap(Fig2, function(bins, reldiag_type, plot) {
  ggsave(
    here(sprintf("C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar/Seminar Paper (paper + code)/Codigos V2/Plots/Fig2_RF.pdf", bins, reldiag_type)),
    plot,
    width=5, height=5, units="in"
  )
})

grid.arrange(grobs = Fig2$plot, ncol=2)



###############################################################

#LOGISTIC MODEL

Fig3 <- tibble(
  bins = c(10, NA),
  reldiag_type = c("BaC", "CORP") %>% rep.int(c(1, 1))
) %>%
  mutate(.,
         plot = purrr::pmap(., function(bins, reldiag_type) {
           if (reldiag_type == "BaC") {
             rel.diag.classic(rlz=app_data$obs, FC=app_data[["Logistic"]], bins=bins)$p +
               ggtitle(sprintf("(e) Logistic Regression / %i Equidistant Bins",bins)) +
               xlab("Forecast value") +
               ylab("CEP") +
               theme_bw() +
               theme(aspect.ratio = 1, plot.title = element_text(size = 10, face = "bold"))
           } else if (reldiag_type == "CORP") {
             set.seed(42)
             r <- reliabilitydiag(app_data[["Logistic"]], y = app_data$obs, n.boot = 100)
             autoplot(r) +
               ggtitle("(f) Logistic Regression / CORP") +
               theme(plot.title = element_text(size = 10, face = "bold")) +
               annotate(
                 "text",
                 x = .125,
                 y = .94,
                 size = 3,
                 label = sprintf("MCB = .%03d",
                                 round(summary(r)$miscalibration * 1000)),
                 color = "red"
               ) +
               annotate(
                 "text",
                 x = .125,
                 y = .88,
                 size = 3,
                 label = sprintf("DSC = .%03d",
                                 round(summary(r)$discrimination * 1000))
               ) +
               annotate(
                 "text",
                 x = .125,
                 y = .82,
                 size = 3,
                 label = sprintf("UNC = .%03d",
                                 round(summary(r)$uncertainty * 1000))
               )
           }})
  )

purrr::pmap(Fig3, function(bins, reldiag_type, plot) {
  ggsave(
    sprintf("C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar/Seminar Paper (paper + code)/Codigos V2/Plots/Fig3_LOG.pdf", bins, reldiag_type),
    plot,
    width=5, height=5, units="in"
  )
})

grid.arrange(grobs = Fig3$plot, ncol=2)


#plots

#layout3 <- ggarrange(Fig1$plot[[1]], Fig1$plot[[2]], Fig2$plot[[1]] ,Fig2$plot[[2]], Fig3$plot[[1]],Fig3$plot[[2]],
 #         labels = c("A", "D", "B", "E", "C", "F"), font.label = list(size = 8), align = "h", widths = c(8,8,8,8,8,8), vjust = 4, hjust= -17,
  #        ncol = 2, nrow = 4, heights = c(12, 12, 12, 12, 12, 12))



layout3 <- ggarrange(Fig1$plot[[1]], Fig1$plot[[2]], Fig2$plot[[1]] ,Fig2$plot[[2]], 
                     Fig3$plot[[1]],Fig3$plot[[2]], nrow = 3, ncol = 2
         )


ggsave(filename = "C:/Users/Noe/Desktop/IBE Master/3rd Semester/Seminar_gh/Paper/Plots_app/layout3.pdf", layout3, width = 9, height = 12, units = "in")


