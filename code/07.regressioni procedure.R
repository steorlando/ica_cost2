#Regressione univariata

regr_uni <- glm(infetto ~ proc_inv_re, 
               data = db,
               family = binomial("logit"))

summary(regr_uni)
