#Regressione univariata

regr_uni <- glm(infetto ~ proc_inv, 
               data = db,
               family = binomial("logit"))

summary(regr_uni)

gtsummary::tbl_regression(regr_uni)


#Regressione multivariata
regr_multi <- glm(
  infetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db,
  family = binomial("logit")
)

summary(regr_multi)

tbl_regression(regr_multi)
