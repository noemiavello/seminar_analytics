
## load libraries required for analysis
library(yardstick)
library(ggplot2)
library(ggfortify)

truth <- data_test$LOAN_DEFAULT
pred_1 <- ifelse(glm_probs >= .5, 1, 0)
pred_2 <- ifelse(naive_probs2[,2] >= .5, 1, 0)
pred_3 <- forest_probs







dat <- data.frame(truth = as.factor(truth), pred_1 = as.numeric(pred_1), pred_2 = as.numeric(pred_2), pred_3 = as.numeric(pred_3))

#logistic
ggplot2::autoplot(pr_curve(dat, truth, pred_1),  size_axislabels = 15)+
  ggtitle("(a) Logistic model") 

options(repr.plot.width =9, repr.plot.height =9) 
pr_curve(dat, truth, pred_1) %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_path(linetype = 'solid', color ="#E69F00", linewidth = 0.8) +
  coord_equal() +
  ggtitle("(a) Logistic model")  +
  theme_bw() + 
  theme(axis.text=element_text(size=13),axis.title=element_text(size=12))


#NB
autoplot(pr_curve(dat, truth, pred_2), ts.linetype = 'dashed') +
  ggtitle("(b) Naive Bayes model") 

options(repr.plot.width =9, repr.plot.height =9) 
pr_curve(dat, truth, pred_2) %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_path(linetype = 'solid', color ="#0072B2", linewidth = 0.8) +
  coord_equal() +
  ggtitle("(a) Naive Bayes model")  +
  theme_bw() + 
  theme(axis.text=element_text(size=13),axis.title=element_text(size=12))

#RF
autoplot(pr_curve(dat, truth, pred_3), ts.linetype = 'dashed') +
  ggtitle("(c) Random Forest model") 

pr_curve(dat, truth, pred_3) %>%
  ggplot(aes(x = recall, y = precision)) +
  geom_path(linetype = 'solid', color ="#D55E00", linewidth = 0.8) +
  coord_equal() +
  ggtitle("(c) Random Forest model")  +
  theme_bw() + 
  theme(axis.text=element_text(size=13),axis.title=element_text(size=12))



