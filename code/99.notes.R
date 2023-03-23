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

