#Regressione univariata

regr_uni <- glm(infetto ~ proc_inv, 
               data = db,
               family = binomial("logit"))

summary(regr_uni)

gtsummary::tbl_regression(regr_uni)

