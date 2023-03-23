db_master <- db_orig %>% 
  select(-c(SDO1_Cognome, SDO1_Nome, SDO1_Nome_completo, SDO1_CodFis))

export(db_master, "output/master.csv")

