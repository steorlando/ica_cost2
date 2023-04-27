# casi per la professione, dopo che mi risponde Claudia

profession = case_when(
  sdo1_profes == 1 ~ "Employed",
  sdo1_profes == 2 ~ "Unemplyed",
  sdo1_profes == 3 ~ "Unemployed",
  sdo1_profes == 4 ~ "Unemployed",
  sdo1_profes == 5 ~ "Unemployed",
  sdo1_profes == 6 ~ "Unemployed"
)

db_prova <- db_orig %>% 
  mutate(giorni = SDO1_DatRic - SDO1_DatDim) %>% 
  select(c(SDO1_DatRic, SDO1_DatDim, giorni, SDO1_Degenza))

table(db_orig$SDO1_UOR)  
uor <- db_orig %>% 
  group_by(SDO1_Invio) %>% 
  summarise(n = n()) %>% 
  print()


library(usethis)
use_git_config(user.name = "Stefano Orlando", user.email = "steorlando@gmail.com")
usethis::git_default_branch_configure()


names(db_orig)

#UOR (unità operativa di ricovero) = reparto di ammissione 
frq(db_orig$SDO1_UOR)

#con causa_ext intende se è stato ricoverato per una "causa esterna"
#sono codici con formato "E+NNNN" presenti nell'ICD9 nella sezione diagnosi
frq(db_select$sdo1_causa_ext)

#sdo1_terapia e terapia penso siano gli stessi dati, nella sdo in formato carattere 
#e in terapia in formato dicotomico dove 1= terapia chirurgica e 0= terapia medica 
head(db_select$sdo1_terapia)
head(db_select$terapia)



#db_orig$sdo1_DiaPri sono i codici della diagnosi primaria presenti nell'ICD9
#le successive da sdo1_DiaSe1 a sdo1_DiaSe5 sono le successive diagnosi secondare 
db_orig$SDO1_DiaSe1 <- as.integer(db_orig$SDO1_DiaSe1) #la DiaPri è inserita come numero intero 
db_orig$SDO1_DiaSe2 <- as.integer(db_orig$SDO1_DiaSe2) #le DiaSe come carattere
db_orig$SDO1_DiaSe3 <- as.integer(db_orig$SDO1_DiaSe3) #ho trasformato anche le DiaSe in intero
db_orig$SDO1_DiaSe4 <- as.integer(db_orig$SDO1_DiaSe4)
db_orig$SDO1_DiaSe5 <- as.integer(db_orig$SDO1_DiaSe5)




summary(db)
frq(db$sdo1_uor)
frq(db$reparto)

db1 <- db %>% 
  select(sdo1_dia_pri) %>% 
  arrange(sdo1_dia_pri)

names(db_orig)
frq(db_orig$RERICOVERO)

db_old <- import("data/db_old.csv") #Import main database

names(db)
frq(db$sdo1_degenza)
unique(db$sdo1_dia_pri)

hist(db$sdo1_costo_ln)

db$sdo1_costo_ln <- log(db$sdo1_costo)

names(db)

frq(db_orig$SANGUE)

frq(db_prop$cost_ln)

names(db)

db_prova <- db %>% 
  dplyr::select(c("acinetobacter", "escherichia_coli", "klebsiella_pnm", "pseudomonas", 
                  "clostridium", "candida", "enterococcus", "staphylococcus", "sangue", "respiratorio", "urinario", "ferita", "rettale" ))

db_prova1 <- db_prova %>% mutate_all(function(x) x == 0)

names(db_prova1)

# Creare due tabelle separate per i due gruppi di colonne
df_microorganismi <- db_prova1 %>%
  pivot_longer(cols = c("acinetobacter", "escherichia_coli", "klebsiella_pnm", "pseudomonas", "clostridium", "candida", "enterococcus", "staphylococcus"),
               names_to = "microorganismo",
               values_drop_na = TRUE)

df_origine <- db_prova1 %>%
  pivot_longer(cols = c("sangue", "respiratorio", "urinario", "ferita", "rettale"),
               names_to = "origine",
               values_to = "presente",
               values_drop_na = TRUE) %>%
  filter(presente == TRUE)

# Unire le due tabelle usando l'indice del dataframe originale
result <- left_join(df_microorganismi, df_origine, by = c("rowid" = "rowid"))

# Rimuovere la colonna "presente" e l'indice del dataframe originale (rowid)
result <- result %>% select(-c(presente.x, presente.y, rowid))

# Visualizzare il risultato
print(result)

skim(db_prop)


matched_data <- db_prop[unlist(match_object[c("index.treated", "index.control")]), ]

# Step 4: Perform quantile regression on matched data --------------------------
# Esegui la regressione quantilica al 50° quantile (mediana)
qr_model <- rq(cost ~ infetto + sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
               data = matched_data, tau = 0.5)

# Visualizza i risultati del modello
summary(qr_model)

hist(db_prop$cost)

# Installa e carica il pacchetto ggplot2 se non è già installato
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

# Supponiamo che il tuo dataframe si chiami 'db_prop'
# Assicurati che 'infetto' sia un fattore
db_prop$infetto <- as.factor(db_prop$infetto)

# Crea il density plot utilizzando ggplot
ggplot(db_prop, aes(x = cost, fill = infetto)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribuzione del costo per gruppi Infetto e Non Infetto",
       x = "Costo",
       y = "Densità",
       fill = "Infetto") +
  theme_minimal()

matched_data$infetto <- as.factor(matched_data$infetto)

# Crea il density plot utilizzando ggplot
ggplot(matched_data, aes(x = cost, fill = infetto)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribuzione del costo per gruppi Infetto e Non Infetto",
       x = "Costo",
       y = "Densità",
       fill = "Infetto") +
  theme_minimal()


# Calcola le medie per ciascun gruppo
media_infetto <- mean(db_prop$cost[db_prop$infetto == TRUE])
media_non_infetto <- mean(db_prop$cost[db_prop$infetto == FALSE])

# Crea il density plot utilizzando ggplot e aggiungi le linee delle medie
ggplot(db_prop, aes(x = cost, fill = infetto)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = media_infetto, color = "Infetto"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = media_non_infetto, color = "Non Infetto"), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Infetto" = "red", "Non Infetto" = "blue"), labels = c("Media Infetto", "Media Non Infetto")) +
  labs(title = "Distribuzione del costo per gruppi Infetto e Non Infetto",
       x = "Costo",
       y = "Densità",
       fill = "Infetto",
       color = "Medie") +
  theme_minimal()

# Calcola le medie per ciascun gruppo
media_infetto <- median(matched_data$cost[matched_data$infetto == TRUE])
media_non_infetto <- median(matched_data$cost[matched_data$infetto == FALSE])

# Crea il density plot utilizzando ggplot e aggiungi le linee delle medie
ggplot(matched_data, aes(x = cost, fill = infetto)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = media_infetto, color = "Infetto"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = media_non_infetto, color = "Non Infetto"), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Infetto" = "red", "Non Infetto" = "blue"), labels = c("Media Infetto", "Media Non Infetto")) +
  labs(title = "Distribuzione del costo per gruppi Infetto e Non Infetto",
       x = "Costo",
       y = "Densità",
       fill = "Infetto",
       color = "Medie") +
  theme_minimal()


##Verifico se gli infetti corrispondono ai siti di infezione
db_prop$num_siti <- rowSums(db_prop[ ,c("sangue", "urinario",   #mi dice se l'infezione è stata trovata in più siti
                                   "rettale", "respiratorio", 
                                   "ferita")] == 0) # nel db 0 vuol dire che ha l'infezione

db_prop$siti <- ifelse(db_prop$num_siti >= 1, T, F)  #mi dice se almeno in un sito c'è l'infezione

if(all(db_prop$infetto == db_prop$siti & db_prop$infetto == T)) {
    print("si")
  } else {
    print("no")
  }

#cerco valori non corrispondenti
corr <- data.frame(db_prop$infetto, db_prop$siti)

no_corr <- corr[corr$db_prop.infetto != corr$db_prop.siti, ]

no_corr

frq(db_prop$siti)

db$num_infez_sito <- rowSums(db[ ,c("sangue", "urinario",
                                   "rettale", "respiratorio", 
                                   "ferita")] == 0) # nel db 0 vuol dire che ha l'infezione

db$sito <- ifelse(db$num_infez_sito >= 1, T, F) 

frq(db$sito)
frq(db$infetto)

table(db$sito, db$infetto)
db <- db %>%
  mutate(infetto_true = ifelse(infetto == T, T, 
                               ifelse(sito == T, T, F)))
frq(db$infetto_true)


min(db$data_ricovero)
max(db$data_ricovero)

hist(db$cost_ln)

# Extract p-value from match_object summary
match_summary <- summary(match_object)
p_value <- as.numeric(row.names(match_summary$est)[4])

# Print the p-value
print(p_value)
match_summary$est

df_ica

desc <- as_flex_table(summary4)
save_as_docx(desc, path = "output/descrittive.docx")

db_stp <- db_orig %>% 
  filter(STAPHYLOCOCCUS == 0)
