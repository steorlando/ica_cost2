# devo ricodificare le categoriche e le continue per quello che sono

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
         )
         )

# Tolgo le variabili che abbiamo ricodificato con altro nome
db <- db %>% 
  select(-c(sdo1_tit_stu
            ))

# Organizzo al meglio le variabili categoriche non ordinate
db <- db %>% 
  mutate(sdo1_sesso = factor(sdo1_sesso),
         sdo1_cittad = as_factor(sdo1_cittad),
         sdo1_sta_civ = as_factor(sdo1_sta_civ),
         family = as_factor(family),
         education = factor(education, levels = c("No title", "Primary school", "Diploma", "University degree or higher")),
         profession = factor(profession, levels = c("Employed", "Student", "Housewife", "Retired/Disable", "Looking for a Job", "Unemployed")),
         profession_simple = factor(profession_simple, levels = c("Employed/Student/Housewife", "Retired/Disable", "Unemployed"))
         )

frq(db$sdo1_modali)
#db_orig$sdo1_modali è la modalità di ammissione 
db <- db %>%
  mutate( 
    sdo1_modali = case_when(
      sdo1_modali == 1 ~ "Scheduled",
      sdo1_modali == 2 ~ "Urgent",
      sdo1_modali == 4 ~ "Scheduled with preospedalization"
    )
    )
#db_orig$sdo1_tipdim è il tipo di dimissione 
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


#sdo1_causa_ext è la causa esterna 
db <- db %>%
  mutate(
    sdo1_causa_ext = case_when(
      startsWith(sdo1_causa_ext, "E8") ~ "Outside accident",
      startsWith(sdo1_causa_ext, "E9") ~ "Other accident"
    )
  )

#sdo1_uor reparto di ammissione

#transformo in carattere perchè startsWith vuole formato carattere
db$sdo1_uor <- as.character(db$sdo1_uor) 

db <- db %>%
  mutate(reparto_cod = if_else(nchar(sdo1_uor) == 3, paste0("0", sdo1_uor), sdo1_uor)) %>% 
  mutate(reparto_cod = substring(reparto_cod,1,2))

reparti <- import("data/reparti.xlsx") %>% 
  clean_names()



db <- left_join(db, reparti, by = "reparto_cod") %>% select(-sdo1_uor)

# Sto sistemando le variabili seguendo la tabella excel. Sono arrivato a "Invio" e devo continuare dalla linea 28




# Imposto etichette (lo stavo facendo una ad una ma mi sono interrotto, meglio farlo alla fine)
#db <- db %>% 
 # var_labels(sdo1_costo = "Reimbursed cost", 
  #          sdo1_sesso = "Sex",
   #         sdo1_cittad = "Nationality",
    #        sdo1_sta_civ = "Civil status", #controllare che si dica così'
     #       family = "Familiar status", #anche qui non so se è l'etichetta giusta
      #      )
