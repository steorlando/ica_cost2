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
                reparto,
                risk_dep
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

univariata

db_corr <- db_regr %>% 
  dplyr::select(-c(proc_inv, reparto))

corrplot::corrplot(db_corr)

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

multivariata
# prova usando il reparto "aggregato"
model_multi_2 <- glm(
  infetto ~ 
    proc_inv_real + 
    sdo1_eta + 
    education + 
    #profession_simple + 
    sdo1_modali + 
    terapia + 
    #sdo1_degenza + 
    #decessodico +
    risk_dep
  , 
  data = db_regr,
  family = binomial("logit")
)

multivariata_2 <- model_multi_2 %>% 
  tbl_regression(exponentiate = T) 

multivariata_2

# Analisi per procedura
db_procedure <- db %>% 
  dplyr::select(starts_with("code_"), infetto)

univariata_proc <- tbl_uvregression(data = db_procedure,
                               method = glm,
                               y = infetto,
                               method.args = list(family = binomial),
                               exponentiate = T)

univariata_proc

db_proc_sum <- db_procedure %>% 
  tbl_summary(by = infetto) %>% 
  add_p %>% 
  add_overall()
db_proc_sum
