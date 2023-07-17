# Creazione della tabella di contingenza
tabella <- table(db_regr$proc_inv_real, db_regr$infetto)

# Calcolo dell'Odds Ratio
or <- oddsratio(tabella)

# Stampa dell'Odds Ratio
print(or$measure)


# Install the ResourceSelection package if not already installed
install.packages('ResourceSelection')

# Load the package
library(ResourceSelection)

# Assuming your model is called 'model'
# And your data frame is called 'df'
# Here 'response' is your binary outcome variable and 'predictors' are your predictor variables
model <- glm(response ~ predictors, data = df, family = binomial())

# Calculate the Hosmer-Lemeshow test
hoslem_test <- hoslem.test(model_multi$y, fitted(model_multi))

# Print the results
print(hoslem_test)




super_corr <- function(df) {
  
  # function to get chi square p value and Cramers V
  f = function(x,y) {
    tbl = df %>% dplyr::select(x,y) %>% table()
    chisq_pval = round(chisq.test(tbl)$p.value, 4)
    cramV = round(cramersV(tbl), 2) 
    data.frame(x, y, chisq_pval, cramV) }
  
  # create unique combinations of column names
  # sorting will help getting a better plot (upper triangular)
  df_comb = data.frame(t(combn(sort(names(df)), 2)), stringsAsFactors = F)
  
  # apply function to each variable combination
  df_res = map2_df(df_comb$X1, df_comb$X2, f)
  
  # plot results
  df_res %>%
    ggplot(aes(x,y,fill=chisq_pval))+
    geom_tile()+
    geom_text(aes(x,y,label=cramV), size = 3)+
    scale_fill_gradient(low="red", high="yellow")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 6),
          axis.text.y = element_text(angle = 45, size = 6),
          axis.title = element_blank(),
          legend.position = "none") 
  
}

super_corr(db_corr)

# Load the necessary library
library(ggplot2)

# Create a boxplot for age
ggplot(db, aes(x = "", y = sdo1_eta)) +
  geom_boxplot() +
  theme(axis.title.x=element_blank()) +
  ggtitle("Boxplot of Age")

# Create a boxplot for length of stay
ggplot(db, aes(x = "", y = degenza)) +
  geom_boxplot() +
  theme(axis.title.x=element_blank()) +
  ggtitle("Boxplot of Length of Stay")

# Create a histogram for age
ggplot(db, aes(x = sdo1_eta)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  ggtitle("Histogram of Age")

# Create a histogram for length of stay
ggplot(db, aes(x = degenza)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  ggtitle("Histogram of Length of Stay")

lunghi <- db %>% 
  filter(degenza > 90)
summary(db$degenza)

frq(db$proc_inv)
frq(db$proc_inv_real)

# multivariabile per ciascuna procedura ####
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

# Visualizza il risultato
print(results_tibble)

frq(db$infetto)
export(final_results, "output/procedure.xlsx")


db_procedure <- db %>% 
  dplyr::select(starts_with("code_"))
db_procedure <- db_procedure %>% 
  mutate(proc_sum = rowSums(db_procedure)) %>% 
  mutate(invasiva = proc_sum > 0)

frq(db_procedure$invasiva)

