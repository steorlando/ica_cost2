#Regressione univariata
db_regr <- db_prop

# sto togliendo i costi = zero, ma poi devo imputarli per bene ####
db_regr <- db_regr[db_regr$cost != 0, ]

db_regr <- db_regr %>% 
  mutate(cost_ln = log(cost))


univariata <- tbl_uvregression(data = db_regr,
                               method = glm,
                               family = binomial("logit"),
                               y = infetto,
                               exponentiate = T)

univariata

#Regressione multivariata
#Reparto a rischio - reparto non a rischio 
#risk department - no_risk department ifelse
multivariata <- glm(
  infetto ~ proc_inv_real + sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + batterio_pos, 
  data = db,
  family = binomial("logit")
)

multivariata %>% tbl_regression()
