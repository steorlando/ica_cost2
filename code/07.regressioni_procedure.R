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

# Analisi univariata ####
univariata <- tbl_uvregression(data = db_regr,
                               method = glm,
                               y = infetto,
                               method.args = list(family = binomial),
                               exponentiate = T)



#Regressione multivariata


db_multi <- db_regr %>% 
  filter(!reparto == "UOSD Oculistica")

model_multi <- glm(
  infetto ~ proc_inv_real + sdo1_eta + education + profession_simple + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto, 
  data = db_multi,
  family = binomial("logit")
)

multivariata <- model_multi %>% 
  tbl_regression(exponentiate = T) 


# analisi multivariabile definitiva ####
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


# Analisi per procedura ####
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


# Definisci i codici delle procedure invasive 
codici_procedure_invasive <- c(311, 3129, 3891, 3893, 3894, 3895, 598, 5794, 8607, 8622, 8628, 8962, 8964, 9604, 9605, 9670, 9671, 9672)

# Crea una lista per conservare i risultati
results <- list()

# Esegui una regressione per ogni codice di procedura
for (code in codici_procedure_invasive) {
  
  # Costruisci la formula per la regressione
  formula <- as.formula(paste("infetto ~ ", paste0("code_", code), " + sdo1_eta + education + sdo1_modali + terapia + risk_dep", sep = ""))
  
  # Esegui la regressione logistica
  model <- glm(formula, data = db, family = binomial("logit"))
  
  # Estrai l'odds ratio, l'intervallo di confidenza e il p-value per la procedura, se esiste
  code_str <- paste0("code_", code,"TRUE")
  if (code_str %in% rownames(summary(model)$coefficients)) {
    coef <- summary(model)$coefficients[code_str, ]
    odds_ratio <- exp(coef["Estimate"])
    conf_int <- exp(confint(model)[code_str, ])
    p_value <- coef["Pr(>|z|)"]
  } else {
    odds_ratio <- NA
    conf_int <- c(NA, NA)
    p_value <- NA
  }
  
  # Formatta l'odds ratio, l'intervallo di confidenza e il p-value
  odds_ratio <- round(odds_ratio, 2)
  conf_int <- round(conf_int, 2)
  if (p_value < 0.001) {
    p_value <- "<0.001"
  } else if (p_value < 0.01) {
    p_value <- "<0.01"
  } else if (p_value < 0.05) {
    p_value <- "<0.05"
  } else {
    p_value <- round(p_value, 3)
  }
  
  # Aggiungi i risultati alla lista
  results[[as.character(code)]] <- c(odds_ratio, conf_int, p_value)
}

# Combina tutti i risultati in un dataframe
results_df <- do.call(rbind, results)
colnames(results_df) <- c("Odds Ratio", "Lower 95% CI", "Upper 95% CI", "p-value")

# Trasforma la matrice in un tibble
results_tibble <- as_tibble(results_df, rownames = "Procedure Code")


# Crea un dataframe con il numero di pazienti per procedura
patient_counts <- db %>%
  dplyr::select(starts_with("code_")) %>%
  gather("Procedure Code", "Value") %>%
  filter(Value == TRUE) %>%
  group_by(`Procedure Code`) %>%
  summarise(N = n()) %>%
  mutate(`Procedure Code` = gsub("code_", "", `Procedure Code`))

# Esegui il left join
final_results <- left_join(results_tibble, patient_counts, by = "Procedure Code") %>% 
  relocate("N", .after = "Procedure Code")

# Definisci i codici delle procedure a rischio
procedure_risk <- c(311, 3129, 3893, 8962, 9672)

# Crea la nuova colonna
db <- db %>%
  mutate(proc_risk = rowSums(dplyr::select(., paste0("code_", procedure_risk)) == TRUE) > 0)

t_inv_risk <- db %>% 
  tabyl(proc_risk, infetto) %>% 
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns


model_multi_3 <- glm(
  infetto ~ 
    proc_risk + 
    sdo1_eta + 
    education + 
    #profession_simple + 
    sdo1_modali + 
    terapia + 
    #sdo1_degenza + 
    #decessodico +
    risk_dep
  , 
  data = db,
  family = binomial("logit")
)


multivariata_3 <- model_multi_3 %>% 
  tbl_regression(exponentiate = T) 

multivariata_3
