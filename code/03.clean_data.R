# devo ricodificare le categoriche e le continue per quello che sono ####
frq(db_select$sdo1_tit_stu)
db <- db_select %>%
  mutate(sdo1_costo = as_numeric(sdo1_costo)) %>%     # e metto il costo numerico
  mutate(sdo1_sesso = ifelse(sdo1_sesso == 1, "F", "M"), # decodifico il sesso in M e F
         sdo1_cittad = ifelse(sdo1_cittad == 1, "Italian", "Non-national"), # Per il momento divido tra italiani e non, se poi trovo la decodifica posso considerare anche EU ed extra EU 
         sdo1_sta_civ = case_when(                     #stato civile
           sdo1_sta_civ == 1 ~ "Not married",
           sdo1_sta_civ == 2 ~ "Married",
           sdo1_sta_civ == 3 ~ "Separated",
           sdo1_sta_civ == 4 ~ "Divorced",
           sdo1_sta_civ == 5 ~ "Widow",
           sdo1_sta_civ == 6 ~ "Co-living",
           sdo1_sta_civ == 9 ~ "Unknown",
         ),
         family = case_when(                            #stato civile diviso tra singolo e convivente con altri
           sdo1_sta_civ %in% c("Not married", "Separated", "Divorced", "Widow") ~ "Living alone",
           sdo1_sta_civ %in% c("Married", "Co-living") ~ "Living with others"
         ),
         education = case_when(                     #titolo di studio
           sdo1_tit_stu == 0 ~ "No title",
           sdo1_tit_stu == 1 ~ "Primary school",
           sdo1_tit_stu == 2 ~ "Primary school",
           sdo1_tit_stu == 3 ~ "Diploma",
           sdo1_tit_stu == 4 ~ "University degree or higher",
           sdo1_tit_stu == 5 ~ "University degree or higher",
           sdo1_tit_stu == 9 ~ "Unknown",
         ),
         profession = case_when(
           startsWith(as.character(sdo1_profes), "1") ~ "Employed",
           sdo1_profes == "2" ~ "Unemployed",
           sdo1_profes == "3" ~ "Looking for a Job",
           sdo1_profes == "4" ~ "Student",
           sdo1_profes == "5" ~ "Housewife",
           sdo1_profes == "6" ~ "Retired/Disable",
         ),
         profession_simple = case_when(
           startsWith(as.character(sdo1_profes), "1") ~ "Employed/Student/Housewife",
           sdo1_profes == "2" ~ "Unemployed",
           sdo1_profes == "3" ~ "Unemployed",
           sdo1_profes == "4" ~ "Employed/Student/Housewife",
           sdo1_profes == "5" ~ "Employed/Student/Housewife",
           sdo1_profes == "6" ~ "Retired/Disable",
         ),
         data_ricovero = as_date(sdo1_dat_ric)
         ) %>% 
  dplyr::select(-sdo1_dat_ric)

## Creo variabile job_type usando la seconda cifra dei lavoratori impiegati ####

db <- db %>%
  mutate(job_type = case_when(
    str_sub(as.character(sdo1_profes), 2, 2) == "1" ~ "Entrepreneur-Self-employed",
    str_sub(as.character(sdo1_profes), 2, 2) == "2" ~ "Other self-employed",
    str_sub(as.character(sdo1_profes), 2, 2) == "3" ~ "Director",
    str_sub(as.character(sdo1_profes), 2, 2) == "4" ~ "Office worker",
    str_sub(as.character(sdo1_profes), 2, 2) == "5" ~ "Worker",
    str_sub(as.character(sdo1_profes), 2, 2) == "6" ~ "Other dependent",
    TRUE ~ "Not employed"
  ))
    
## Creo variabile job_sector usando la terza cifra dei lavoratori impiegati####
db <- db %>%
  mutate(job_sector = case_when(
    str_sub(as.character(sdo1_profes), 3, 3) == "1" ~ "Agriculture, hunting and fishing",
    str_sub(as.character(sdo1_profes), 3, 3) == "2" ~ "Factory",
    str_sub(as.character(sdo1_profes), 3, 3) == "3" ~ "Commerce, public services",
    str_sub(as.character(sdo1_profes), 3, 3) == "4" ~ "Public administration",
    str_sub(as.character(sdo1_profes), 3, 3) == "5" ~ "Other private services",
    TRUE ~ "Not employed"
  ))



# Tolgo le variabili che abbiamo ricodificato con altro nome ####
db <- db %>% 
  dplyr::select(-c(sdo1_tit_stu
            ))

# Organizzo al meglio le variabili categoriche non ordinate ####
db <- db %>% 
  mutate(sdo1_sesso = factor(sdo1_sesso),
         sdo1_cittad = as_factor(sdo1_cittad),
         sdo1_sta_civ = as_factor(sdo1_sta_civ),
         family = as_factor(family),
         education = factor(education, levels = c("Primary school", "Diploma", "University degree or higher")),
         profession = factor(profession, levels = c("Employed", "Student", "Housewife", "Retired/Disable", "Looking for a Job", "Unemployed")),
         profession_simple = factor(profession_simple, levels = c("Employed/Student/Housewife", "Retired/Disable", "Unemployed"))
         )

# modalità di ammissione ####
db <- db %>%
  mutate( 
    sdo1_modali = case_when(
      sdo1_modali == 1 ~ "Scheduled",
      sdo1_modali == 2 ~ "Urgent",
      sdo1_modali == 4 ~ "Scheduled with preospedalization"
    )
    )
# tipo di dimissione ####
db <- db %>%
  mutate(
    sdo1_tip_dim = case_when(
      sdo1_tip_dim == 1 ~ "Ordinary discharge at residential facilities",
      sdo1_tip_dim == 2 ~ "Transport to a different health istitution",
      sdo1_tip_dim == 3 ~ "Transport to rehabilitation center or long-term care",
      sdo1_tip_dim == 4 ~ "Voluntary discharge",
      sdo1_tip_dim == 5 ~ "Death",
      sdo1_tip_dim == 6 ~ "Protected discharge",
      sdo1_tip_dim == 7 ~ "Discharge to the same IRC",
      sdo1_tip_dim == 8 ~ "Home discharge",
      sdo1_tip_dim == 9 ~ "Home discharge with Home Care"
    )
  )


# causa esterna ####
db <- db %>%
  mutate(
    sdo1_causa_ext = case_when(
      startsWith(sdo1_causa_ext, "E8") ~ "Outside accident",
      startsWith(sdo1_causa_ext, "E9") ~ "Other accident"
    )
  )

# reparto di ammissione ####

#transformo in carattere perchè startsWith vuole formato carattere
db$sdo1_uor <- as.character(db$sdo1_uor) 

db <- db %>%
  mutate(reparto_cod = if_else(nchar(sdo1_uor) == 3, paste0("0", sdo1_uor), sdo1_uor)) %>% 
  mutate(reparto_cod = substring(reparto_cod,1,2))

reparti <- import("data/reparti.xlsx") %>% 
  clean_names()


db <- left_join(db, reparti, by = "reparto_cod") %>% dplyr::select(-sdo1_uor)


# Diagnosi primaria ####
db <- db %>%           #ho creato la variabile diapri_cod riducendo alle prime tre cifre
  mutate(diapri_cod =      #sdo1_diapri
    substr(sdo1_dia_pri, 1, 3)
  )


db <- db %>%     #creo variabile dia_pri come da lista wikipedia 
  mutate(          
    dia_pri = case_when(
      diapri_cod < 139 ~ "Infectious and parasitic diseases",
      diapri_cod < 239 ~ "Neoplasms",
      diapri_cod < 279 ~ "Endocrine, nutritional and metabolic diseases, and immunity disorders",
      diapri_cod < 289 ~ "Diseases of the blood",
      diapri_cod < 319 ~ "Mental disorders",
      diapri_cod < 389 ~ "Diseases of the nervous system",
      diapri_cod < 459 ~ "Diseases of the circulatory system",
      diapri_cod < 519 ~ "Diseases of the respiratory system",
      diapri_cod < 579 ~ "Diseases of the digestive system",
      diapri_cod < 629 ~ "Diseases of the genitourinary system",
      diapri_cod < 679 ~ "Complications of pregnancy and childbirth",
      diapri_cod < 709 ~ "Diseases of the skin",
      diapri_cod < 739 ~ "Diseases of the musculoskeletal system",
      diapri_cod < 759 ~ "Congenital anomalies",
      diapri_cod < 779 ~ "Conditions originating in the perinatal period",
      diapri_cod < 799 ~ "Symptoms, signs, and ill-defined conditions",
      diapri_cod < 999 ~ "Injury and poisoning",
      startsWith(diapri_cod, "V") ~ "Supplemental classification"
    ))

db <- db %>%        #Rimuovo sdo1_diapri 
  dplyr::select(-c(sdo1_dia_pri
  ))

#Continuo la ricodifica della diagnosi con le successive 
db <- db %>%
  mutate(diase1_cod = substr(sdo1_dia_se1, 1, 3)) %>%
  mutate(diase2_cod = substr(sdo1_dia_se2, 1, 3)) %>%
  mutate(diase3_cod = substr(sdo1_dia_se3, 1, 3)) %>%
  mutate(diase4_cod = substr(sdo1_dia_se4, 1, 3)) %>%
  mutate(diase5_cod = substr(sdo1_dia_se5, 1, 3)) 

ricodifica <- function(x) {
  case_when(
    x == "" ~ "",
    x < 139 ~ "Infectious and parasitic diseases",
    x < 239 ~ "Neoplasms",
    x < 279 ~ "Endocrine, nutritional and metabolic diseases, and immunity disorders",
    x < 289 ~ "Diseases of the blood",
    x < 319 ~ "Mental disorders",
    x < 389 ~ "Diseases of the nervous system",
    x < 459 ~ "Diseases of the circulatory system",
    x < 519 ~ "Diseases of the respiratory system",
    x < 579 ~ "Diseases of the digestive system",
    x < 629 ~ "Diseases of the genitourinary system",
    x < 679 ~ "Complications of pregnancy and childbirth",
    x < 709 ~ "Diseases of the skin",
    x < 739 ~ "Diseases of the musculoskeletal system",
    x < 759 ~ "Congenital anomalies",
    x < 779 ~ "Conditions originating in the perinatal period",
    x < 799 ~ "Symptoms, signs, and ill-defined conditions",
    x < 999 ~ "Injury and poisoning",
    startsWith(x, "V") ~ "Supplemental classification"
  )
}

db$diase1 <- ricodifica(db$diase1_cod)
db$diase2 <- ricodifica(db$diase2_cod)
db$diase3 <- ricodifica(db$diase3_cod)
db$diase4 <- ricodifica(db$diase4_cod)
db$diase5 <- ricodifica(db$diase5_cod)

db <- db %>%  #Elimino variabili già ricodificate
  dplyr::select(-c(sdo1_dia_se1, sdo1_dia_se2, sdo1_dia_se3,
            sdo1_dia_se4, sdo1_dia_se5))


# Tipo terapia surgical o medical ####
db <- db %>%
  mutate(
    terapia = case_when(
      sdo1_terapia == "C" ~ "Surgical",
      sdo1_terapia == "M" ~ "Medical"
    )
  )

db <- db %>%  #Elimino la variabile già ricodificata
  dplyr::select(-c(sdo1_terapia))


# Inserisco anche gli altri 72 sogg  #### 
batterio_mancante <- import("data/Batterio_mancante2.xlsx") %>% 
  clean_names()


db <- left_join(db, batterio_mancante, by = "id")

# Lista delle variabili senza il suffisso _y
variabili <- c("acinetobacter", "escherichia_coli", "klebsiella_pnm", "pseudomonas", "clostridium", "candida", "enterococcus", "staphylococcus")

# Loop attraverso le variabili
for (var in variabili) {
  # Trova il nome della corrispondente variabile con il suffisso _y
  var_y <- paste0(var, "_y")
  
  # Aggiungi i valori della variabile _y alla variabile senza il suffisso _y
  db[[var]] <- ifelse(is.na(db[[var_y]]), db[[var]], as.integer(!db[[var_y]]))
  
  # Rimuovi la variabile con il suffisso _y dal dataset
  db[[var_y]] <- NULL
}

# Trasformazione dei valori nella variabile "other_microorg"
# Trasformazione dei valori nella variabile "other_microorg"
db$other_microorg <- ifelse(is.na(db$other_microorg), 1, ifelse(db$other_microorg == TRUE, 0, db$other_microorg))

# Creo le variabili numero_di_infezioni e almeno_una_infezione #####
db$num_infezioni <- rowSums(db[ ,c("acinetobacter", "escherichia_coli",
                                         "klebsiella_pnm", "pseudomonas", 
                                         "clostridium", "candida", "enterococcus", 
                                         "staphylococcus", "other_microorg")] == 0) # nel db 0 vuol dire che ha l'infezione



db$batterio_pos <- ifelse(db$num_infezioni >= 1, T, F) 

db <- db %>%
  mutate(sangue =       ifelse(s1_risultato != "", 0, 1),
         urinario =     ifelse(u1_risultato != "", 0, 1),
         rettale =      ifelse(g1_risultato != "", 0, 1),
         respiratorio = ifelse(r1_risultato != "", 0, 1),
         ferita =       ifelse(t1_risultato != "", 0, 1),
         altro_sito =   ifelse(n1_risultato != "", 0, 1))

db$num_infez_sito <- rowSums(db[ ,c("sangue", "urinario",
                                    "rettale", "respiratorio", 
                                    "ferita", "altro_sito")] == 0) # nel db 0 vuol dire che ha l'infezione

db$sito_pos <- ifelse(db$num_infez_sito >= 1, T, F) 


db <- db %>% 
  dplyr::select(-c("s1_risultato",
                                      "g1_risultato",
                                      "u1_risultato",
                                      "r1_risultato",
                                      "t1_risultato",
                                      "n1_risultato")) 


db <- db %>% mutate(infetto = ifelse(sito_pos == T, T, F))  # l'infezione si basa sui siti perchè i batteri vanno corretti

# Variabili per analisi procedure ####

#Creo variabile proc_inv che mi dice T se il pz ha subito almeno una proc invasiva di quelle definite elenco
int_inv <- as.integer(c(311, 3129, 3891, 3893, 3894, 3895, 598, 5794, 8607, 8622,  
                        8628, 8962, 8964, 9604, 9605, 9670, 9671, 9672))  #codici elenco

db$proc_inv <- ifelse(db$sdo1_int_pri %in% int_inv | db$sdo1_int_se1 %in% int_inv | db$sdo1_int_se2 %in% int_inv |
                        db$sdo1_int_se3 %in% int_inv | db$sdo1_int_se4 %in% int_inv | db$sdo1_int_se5 %in% int_inv, "TRUE", "FALSE")


db$all_procedures <- paste(db$sdo1_int_pri, db$sdo1_int_se1, db$sdo1_int_se2,
                           db$sdo1_int_se3, db$sdo1_int_se4, db$sdo1_int_se5, sep = ",")

get_invasive_codes <- function(all_procedures, int_inv) {
  procedure_list <- as.integer(unlist(strsplit(all_procedures, ",")))
  invasive_procedures <- procedure_list[procedure_list %in% int_inv]
  return(paste(invasive_procedures, collapse = ","))
}

db$invasive_codes <- sapply(db$all_procedures, get_invasive_codes, int_inv = int_inv)

for(code in int_inv){
  colname = paste("code", code, sep = "_")
  db[[colname]] <- sapply(db$invasive_codes, function(x) {
    as.character(code) %in% strsplit(x, ",")[[1]]
  })
}

# Get the column names that start with "code_"
code_cols <- grep("^code_", names(db), value = TRUE)

# Iterate over the code_cols and set the value to FALSE when proc_inv_real is FALSE
for(col in code_cols){
  db[[col]][db$proc_inv_real == FALSE] <- FALSE
}

#Creo variabile cod_proc che mi dice per i pz che hanno avuto TRUE il codice di 
#quale procedura hanno avuto
db <- db %>%  
  mutate(cod_proc = case_when(
    sdo1_int_pri %in% int_inv & TRUE ~ sdo1_int_pri,
    sdo1_int_se1 %in% int_inv & TRUE ~ sdo1_int_se1,
    sdo1_int_se2 %in% int_inv & TRUE ~ sdo1_int_se2,
    sdo1_int_se3 %in% int_inv & TRUE ~ sdo1_int_se3,
    sdo1_int_se4 %in% int_inv & TRUE ~ sdo1_int_se4,
    sdo1_int_se5 %in% int_inv & TRUE ~ sdo1_int_se5,
    TRUE ~ NA_integer_
  ))

#Aggiungo la variabile drg_soglia associata ai drg
db$drgnum <- as.character(db$drgnum)

db <- db %>%
 mutate(drgnum = ifelse(nchar(drgnum) == 1, paste0("00", drgnum),
                        ifelse(nchar(drgnum) == 2, paste0("0", drgnum), as.character(drgnum))))

DRG <- import("data/DRG.xlsx") %>%
  clean_names()

db <- left_join(db, DRG, by = 'drgnum')



#Creo variabile binaria sup_soglia che mi indica se i giorni di degenza hanno superato la soglia
db$drg_soglia <- as.integer(db$drg_soglia)

db$sup_soglia <- ifelse(db$sdo1_degenza > db$drg_soglia, TRUE, FALSE)

db$diff_soglia <- db$degenza - db$drg_soglia


# aggiusto il decesso
db <- db %>% 
  mutate(decessodico = ifelse(decessodico == 0, TRUE, FALSE))

# creo una variabile di costo trasformata con il log per le analisi ####

# rinomino il costo per renderla più chiara

db <- db %>% 
  rename(cost = sdo1_costo) %>% 
  mutate(cost_ln = log(cost))


#creo data primo tampone
db <- db %>% 
  mutate(data_sangue = as_date(ifelse(sangue == 0, s1_a_dp1, NA )),
         data_urinario = as_date(ifelse(urinario == 0, u1_a_dp1, NA)),
         data_rettale = as_date(ifelse(rettale == 0, g1_a_dp1, NA)),
         data_respiratorio = as_date(ifelse(respiratorio == 0, r1_a_dp1, NA)),
         data_ferita = as_date(ifelse(ferita == 0, t1_a_dp1, NA)),
         data_altro_sito = as_date(ifelse(altro_sito == 0, n1_a_dp1, NA))
  )

db <- db %>%
  mutate(across(starts_with("data_"), ~ if_else(is.na(.), as.Date('2100-01-01'), as.Date(.)))) %>%
  mutate(sample_data = pmin(data_sangue, 
                         data_urinario, 
                         data_rettale, 
                         data_respiratorio,
                         data_ferita,
                         data_altro_sito, na.rm = TRUE)) %>%
  mutate(sample_data = if_else(sample_data == as.Date('2100-01-01'), NA_Date_, sample_data)) %>%
  mutate(date_inv = ifelse(cod_proc %in% c(311,3129,9604,9605,9670,9671,9672), as.Date(data_respiratorio), ""),
         date_inv = ifelse(cod_proc %in% c(3891,3893,3894,3895,8607,8962,8964), as.Date(data_sangue), date_inv),
         date_inv = ifelse(cod_proc %in% c(598, 5794), as.Date(data_urinario), date_inv),
         date_inv = ifelse(cod_proc %in% c(8622,8628), as.Date(data_ferita), date_inv),
         date_inv = ifelse(date_inv == 47482, "", date_inv),
         ) %>%
  mutate(date_inv = as.numeric(date_inv))


db <- db %>%  
  mutate(date_inv = as.Date(date_inv, origin = "1970-01-01"))

db <- db %>%
  mutate(across(starts_with("data_"), ~ if_else(. == as.Date('2100-01-01'), "", as.character(.))))


# includere in db i campi con le date delle procedure
# convertire quei campi in un formato data comprensibile (sono strani)

# CORREZIONE DATE CON 7 CARATTERI ####

db$sdo1_dat_in_p <- as.Date(db$sdo1_dat_in_p, origin = "1899-12-30")

# Elenco delle variabili da trasformare
variabili_date <- c("sdo1_d_in_se1", "sdo1_d_in_se2", "sdo1_d_in_se3", "sdo1_d_in_se4", "sdo1_d_in_se5")

# Trasforma le date in formato numerico in formato carattere, aggiungendo uno zero per quelle con 7 cifre
for (var in variabili_date) {
  db[[var]] <- as.character(db[[var]])  # Trasforma la variabile in carattere
  db[[var]] <- ifelse(nchar(db[[var]]) == 7, paste0("0", db[[var]]), db[[var]])  # Aggiunge uno zero per le date con 7 cifre
}

# Trasforma le variabili carattere in formato data "%d%m%Y"
for (var in variabili_date) {
  db[[var]] <- as.Date(db[[var]], format = "%d%m%Y")
}


# procedure invasive ####
# creare un campo data_prc_inv in cui la data viene riportata solo se il codice è uno di quelle delle procedure invasive
codici_procedure_invasive <- c(311, 3129, 3891, 3893, 3894, 3895, 598, 5794, 8607, 8622, 8628, 8962, 8964, 9604, 9605, 9670, 9671, 9672)

# Creazione della variabile "data_proc_inv"
db <- db %>%
  mutate(data_proc_inv = case_when(
    sdo1_int_pri %in% codici_procedure_invasive ~ sdo1_dat_in_p,
  sdo1_int_se1 %in% codici_procedure_invasive ~ sdo1_d_in_se1,
    sdo1_int_se2 %in% codici_procedure_invasive ~ sdo1_d_in_se2,
    sdo1_int_se3 %in% codici_procedure_invasive ~ sdo1_d_in_se3,
    sdo1_int_se4 %in% codici_procedure_invasive ~ sdo1_d_in_se4,
    sdo1_int_se5 %in% codici_procedure_invasive ~ sdo1_d_in_se5,
    TRUE ~ NA_Date_
  ))

frq(db$proc_inv)
frq(db$proc_inv_real)

# creare un campo proc_inv_real che è TRUE se la data_prc_inv + 2 giorni < date_inv
db <- db %>%
  mutate(data_proc_inv = as_date(data_proc_inv),
         date_inv = as_date(date_inv),
         proc_inv_real = if_else(data_proc_inv + days(2) > date_inv , FALSE, as.logical(proc_inv))) %>% 
  mutate(proc_inv_real = ifelse(proc_inv == TRUE & is.na(date_inv) & infetto == FALSE, TRUE, proc_inv_real)) %>% 
  mutate(proc_inv_real = ifelse(is.na(proc_inv_real), FALSE, proc_inv_real))

# tolgo quelli ricoverati con più di 60 giorni ####

db <- db %>% filter(sdo1_degenza < 61)



# eliminare reparti e diagnosi con meno di X utenti ####

soglia_acc <- 5

reparti_null <- db %>%
  group_by(reparto) %>% 
  summarise(patients = n()) %>% 
  filter(patients < soglia_acc) %>% 
  mutate(filt = T) %>% 
  dplyr::select(reparto, filt)

db <- left_join(db, reparti_null) %>% mutate(filt = ifelse(is.na(filt), FALSE, filt))

db <- db %>% 
  filter(!filt == TRUE) %>% 
  dplyr::select(-c(filt))

db <- db %>% 
  mutate(reparto = as.factor(reparto))

# Imposta "UOC Neurochirurgia" come livello di riferimento
db$reparto <- fct_relevel(db$reparto, "UOC Neurochirugia")

# Creo variabile con reparto a rischio ####

# Dopo aver fatto una univariata ligistica rispetto alla infezione, i seguenti reparti risultano 
# con un odds ratio maggiore di 1 e p < 0.05 (in realtà quasi tutti 0.0001)

db <- db %>% 
  mutate(risk_dep = ifelse(reparto %in% c(
    "UOC Cardiochirurgia",
    "UOC Endocrinologia e Diabetologia",
    "UOC Malattie apparato respiratorio",
    "UOC Malattie Infettive",
    "UOC Medicina interna",
    "UOSD OBI e Medicina d'urgenza",
    "UOSD Patologie linfoproliferative",
    "UOSD Terapia Intensiva"
  ), TRUE, FALSE)
  )



