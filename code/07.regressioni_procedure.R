# Dataframe per indagare ruolo procedure invasive 
db_regr <- db %>% 
  dplyr::select(infetto,
                proc_inv,
                proc_inv_real,
                sdo1_sesso,
                sdo1_eta, 
                sdo1_cittad,
                education,
                profession_simple,
                sdo1_modali,
                sdo1_degenza,
                terapia, 
                decessodico,
                reparto
  )


# Cross-table procedure invasive e infetto ####
t_inv <- db_regr %>% 
  tabyl(proc_inv, infetto) %>% 
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns

t_inv_real <- db_regr %>% 
  tabyl(proc_inv_real, infetto) %>% 
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns

univariata <- tbl_uvregression(data = db_regr,
                               method = glm,
                               y = infetto,
                               method.args = list(family = binomial),
                               exponentiate = T)


#Regressione multivariata
#Reparto a rischio - reparto non a rischio 
#risk department - no_risk department ifelse
# poi si rifa l'uivariata (codice scritto)

db_multi <- db_regr %>% 
  filter(!reparto == "UOSD Oculistica")

model_multi <- glm(
  infetto ~ proc_inv_real + sdo1_eta + education + profession_simple + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto, 
  data = db_multi,
  family = binomial("logit")
)

multivariata <- model_multi %>% 
  tbl_regression(exponentiate = T) 

