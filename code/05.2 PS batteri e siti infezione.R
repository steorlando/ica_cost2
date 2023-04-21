### PS analysis acinetobacter vs non infetti   ####
#mi creo db_acineto per eseguire modifiche e analisi senza toccare db_prop
db_acineto <- db_prop
db_acineto$acinetobacter <- ifelse(db_acineto$acinetobacter == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo acinetobacter
db_acineto$acineto_vs_noninfetto <- ifelse(db_acineto$infetto == 0 & db_acineto$acinetobacter == 0, 0,
                                           ifelse(db_acineto$infetto == 1 & db_acineto$acinetobacter == 1, 1,
                                                  ifelse(db_acineto$infetto == 1 & db_acineto$acinetobacter == 0, "", ""))
)

db_acineto <- db_acineto[db_acineto$acineto_vs_noninfetto !="", ] #elimino campi vuoti (infetti di altri batteri)

#Faccio matching con PS
db_acineto$acineto_vs_noninfetto <- ifelse(db_acineto$acineto_vs_noninfetto == 1, T, F) #trasformo in logico

model_acineto <- glm(
  acineto_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_acineto,
  family = binomial("logit")
)



#matching using PS 
# Get PS values
ps_values_ac <- model_acineto$fitted.values

# Define outcome and treatment vector
outcome_ac <- db_acineto$cost_ln
treatment_ac <- db_acineto$acineto_vs_noninfetto

# Matching
match_obj_acineto <- Match(
  Y = outcome_ac,            # vector with the outcome
  Tr = treatment_ac,         # vector with treatment
  X = ps_values_ac,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_acineto <- bal.tab(
  match_obj_acineto,
  acineto_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_acineto,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_acineto)

bal.plot(
  match_obj_acineto,
  formula = acineto_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_acineto,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_acineto)


### PS analysis Klebsiella vs non infetti  ####
#mi creo db_klebsiella per eseguire modifiche e analisi senza toccare db_prop
db_klebsiella <- db_prop
db_klebsiella$klebsiella_pnm <- ifelse(db_klebsiella$klebsiella_pnm == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo klebsiella
db_klebsiella$klebsiella_vs_noninfetto <- ifelse(db_klebsiella$infetto == 0 & db_klebsiella$klebsiella_pnm== 0, 0,
                                           ifelse(db_klebsiella$infetto == 1 & db_klebsiella$klebsiella_pnm == 1, 1,
                                                  ifelse(db_klebsiella$infetto == 1 & db_klebsiella$klebsiella_pnm == 0, "", ""))
)

db_klebsiella <- db_klebsiella[db_klebsiella$klebsiella_vs_noninfetto !="", ] #elimino campi vuoti (infetti di altri batteri)

#Faccio matching con PS
db_klebsiella$klebsiella_vs_noninfetto <- ifelse(db_klebsiella$klebsiella_vs_noninfetto == 1, T, F) #trasformo in logico

model_klebsiella <- glm(
  klebsiella_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_klebsiella,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_kl <- model_klebsiella$fitted.values

# Define outcome and treatment vector
outcome_kl <- db_klebsiella$cost_ln
treatment_kl <- db_klebsiella$klebsiella_vs_noninfetto

# Matching
match_obj_klebsiella <- Match(
  Y = outcome_kl,            # vector with the outcome
  Tr = treatment_kl,         # vector with treatment
  X = ps_values_kl,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_klebsiella <- bal.tab(
  match_obj_klebsiella,
  klebsiella_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_klebsiella,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_klebsiella)

bal.plot(
  match_obj_klebsiella,
  formula = klebsiella_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_klebsiella,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_klebsiella)


### PS analysis clostridium vs non infetti   ####
#mi creo db_clostridium per eseguire modifiche e analisi senza toccare db_prop
db_clostridium <- db_prop
db_clostridium$clostridium <- ifelse(db_clostridium$clostridium == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo clostridium
db_clostridium$clostridium_vs_noninfetto <- ifelse(db_clostridium$infetto == 0 & db_clostridium$clostridium == 0, 0,
                                                 ifelse(db_clostridium$infetto == 1 & db_clostridium$clostridium == 1, 1,
                                                        ifelse(db_clostridium$infetto == 1 & db_clostridium$clostridium == 0, "", ""))
)

db_clostridium <- db_clostridium[db_clostridium$clostridium_vs_noninfetto !="", ] #elimino campi vuoti (infetti di altri batteri)

#Faccio matching con PS
db_clostridium$clostridium_vs_noninfetto <- ifelse(db_clostridium$clostridium_vs_noninfetto == 1, T, F) #trasformo in logico

model_clostridium <- glm(
  clostridium_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_clostridium,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_cl <- model_clostridium$fitted.values

# Define outcome and treatment vector
outcome_cl <- db_clostridium$cost_ln
treatment_cl <- db_clostridium$clostridium_vs_noninfetto

# Matching
match_obj_clostridium <- Match(
  Y = outcome_cl,            # vector with the outcome
  Tr = treatment_cl,         # vector with treatment
  X = ps_values_cl,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_clostridium <- bal.tab(
  match_obj_clostridium,
  clostridium_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_clostridium,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_clostridium)

bal.plot(
  match_obj_clostridium,
  formula = clostridium_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_clostridium,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_clostridium)


### PS analysis enterococcus vs non infetti   ####
#mi creo db_entero per eseguire modifiche e analisi senza toccare db_prop
db_entero <- db_prop
db_entero$enterococcus <- ifelse(db_entero$enterococcus == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo enterococco
db_entero$entero_vs_noninfetto <- ifelse(db_entero$infetto == 0 & db_entero$enterococcus == 0, 0,
                                                   ifelse(db_entero$infetto == 1 & db_entero$enterococcus == 1, 1,
                                                          ifelse(db_entero$infetto == 1 & db_entero$enterococcus == 0, "", ""))
)

db_entero <- db_entero[db_entero$entero_vs_noninfetto !="", ] #elimino campi vuoti (infetti di altri batteri)

#Faccio matching con PS
db_entero$entero_vs_noninfetto <- ifelse(db_entero$entero_vs_noninfetto == 1, T, F) #trasformo in logico

model_entero <- glm(
  entero_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_entero,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_ent <- model_entero$fitted.values

# Define outcome and treatment vector
outcome_ent <- db_entero$cost_ln
treatment_ent <- db_entero$entero_vs_noninfetto

# Matching
match_obj_entero <- Match(
  Y = outcome_ent,            # vector with the outcome
  Tr = treatment_ent,         # vector with treatment
  X = ps_values_ent,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_entero <- bal.tab(
  match_obj_entero,
  entero_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_entero,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_entero)

bal.plot(
  match_obj_entero,
  formula = entero_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_entero,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_entero)


### PS analysis escherichia_coli vs non infetti   ####
#mi creo db_escherichia per eseguire modifiche e analisi senza toccare db_prop
db_escherichia <- db_prop
db_escherichia$escherichia_coli <- ifelse(db_escherichia$escherichia_coli == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo escherichia_coli
db_escherichia$escherichia_vs_noninfetto <- ifelse(db_escherichia$infetto == 0 & db_escherichia$escherichia_coli == 0, 0,
                                                   ifelse(db_escherichia$infetto == 1 & db_escherichia$escherichia_coli == 1, 1,
                                                          ifelse(db_escherichia$infetto == 1 & db_escherichia$escherichia_coli == 0, "", ""))
)

db_escherichia <- db_escherichia[db_escherichia$escherichia_vs_noninfetto !="", ] #elimino campi vuoti (infetti di altri batteri)

#Faccio matching con PS
db_escherichia$escherichia_vs_noninfetto <- ifelse(db_escherichia$escherichia_vs_noninfetto == 1, T, F) #trasformo in logico

model_escherichia <- glm(
  escherichia_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_escherichia,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_es <- model_escherichia$fitted.values

# Define outcome and treatment vector
outcome_es <- db_escherichia$cost_ln
treatment_es <- db_escherichia$escherichia_vs_noninfetto

# Matching
match_obj_escherichia <- Match(
  Y = outcome_es,            # vector with the outcome
  Tr = treatment_es,         # vector with treatment
  X = ps_values_es,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_escherichia <- bal.tab(
  match_obj_escherichia,
  escherichia_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_escherichia,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_escherichia)

bal.plot(
  match_obj_escherichia,
  formula = escherichia_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_escherichia,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_escherichia)


### PS analysis pseudomonas vs non infetti   ####
#mi creo db_pseudo per eseguire modifiche e analisi senza toccare db_prop
db_pseudo <- db_prop
db_pseudo$pseudomonas <- ifelse(db_pseudo$pseudomonas == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo pseudomonas
db_pseudo$pseudo_vs_noninfetto <- ifelse(db_pseudo$infetto == 0 & db_pseudo$pseudomonas == 0, 0,
                                                   ifelse(db_pseudo$infetto == 1 & db_pseudo$pseudomonas == 1, 1,
                                                          ifelse(db_pseudo$infetto == 1 & db_pseudo$pseudomonas == 0, "", ""))
)

db_pseudo <- db_pseudo[db_pseudo$pseudo_vs_noninfetto !="", ] #elimino campi vuoti (infetti di altri batteri)

#Faccio matching con PS
db_pseudo$pseudo_vs_noninfetto <- ifelse(db_pseudo$pseudo_vs_noninfetto == 1, T, F) #trasformo in logico

model_pseudo <- glm(
  pseudo_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_pseudo,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_ps <- model_pseudo$fitted.values

# Define outcome and treatment vector
outcome_ps <- db_pseudo$cost_ln
treatment_ps <- db_pseudo$pseudo_vs_noninfetto

# Matching
match_obj_pseudo <- Match(
  Y = outcome_ps,            # vector with the outcome
  Tr = treatment_ps,         # vector with treatment
  X = ps_values_ps,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment
balance_pseudo <- bal.tab(
  match_obj_pseudo,
  pseudo_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_pseudo,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_pseudo)

bal.plot(
  match_obj_pseudo,
  formula = pseudo_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_pseudo,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_pseudo)


### PS analysis candida vs non infetti   ####
#mi creo db_candida per eseguire modifiche e analisi senza toccare db_prop
db_candida <- db_prop
db_candida$candida <- ifelse(db_candida$candida == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo candida
db_candida$candida_vs_noninfetto <- ifelse(db_candida$infetto == 0 & db_candida$candida == 0, 0,
                                                   ifelse(db_candida$infetto == 1 & db_candida$candida == 1, 1,
                                                          ifelse(db_candida$infetto == 1 & db_candida$candida == 0, "", ""))
)

db_candida <- db_candida[db_candida$candida_vs_noninfetto !="", ] #elimino campi vuoti (infetti di altri batteri)

#Faccio matching con PS
db_candida$candida_vs_noninfetto <- ifelse(db_candida$candida_vs_noninfetto == 1, T, F) #trasformo in logico

model_candida <- glm(
  candida_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_candida,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_ca <- model_candida$fitted.values

# Define outcome and treatment vector
outcome_ca <- db_candida$cost_ln
treatment_ca <- db_candida$candida_vs_noninfetto

# Matching
match_obj_candida <- Match(
  Y = outcome_ca,            # vector with the outcome
  Tr = treatment_ca,         # vector with treatment
  X = ps_values_ca,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_candida <- bal.tab(
  match_obj_candida,
  candida_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_candida,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_candida)

bal.plot(
  match_obj_candida,
  formula = candida_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_candida,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_candida)


### PS analysis staphylococcus vs non infetti   ####
#mi creo db_staphylo per eseguire modifiche e analisi senza toccare db_prop
db_staphylo <- db_prop
db_staphylo$staphylococcus <- ifelse(db_staphylo$staphylococcus == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo acinetobacter
db_staphylo$staphylo_vs_noninfetto <- ifelse(db_staphylo$infetto == 0 & db_staphylo$staphylococcus == 0, 0,
                                                   ifelse(db_staphylo$infetto == 1 & db_staphylo$staphylococcus == 1, 1,
                                                          ifelse(db_staphylo$infetto == 1 & db_staphylo$staphylococcus == 0, "", ""))
)

db_staphylo <- db_staphylo[db_staphylo$staphylo_vs_noninfetto !="", ] #elimino campi vuoti (infetti di altri batteri)

#Faccio matching con PS
db_staphylo$staphylo_vs_noninfetto <- ifelse(db_staphylo$staphylo_vs_noninfetto == 1, T, F) #trasformo in logico

model_staphylo <- glm(
  staphylo_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_staphylo,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_st <- model_staphylo$fitted.values

# Define outcome and treatment vector
outcome_st <- db_staphylo$cost_ln
treatment_st <- db_staphylo$staphylo_vs_noninfetto

# Matching
match_obj_staphylo <- Match(
  Y = outcome_st,            # vector with the outcome
  Tr = treatment_st,         # vector with treatment
  X = ps_values_st,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_staphylo <- bal.tab(
  match_obj_staphylo,
 staphylo_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_staphylo,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_staphylo)

bal.plot(
  match_obj_staphylo,
  formula = staphylo_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_staphylo,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_staphylo)

----------------------------------


  
### PS analysis sangue vs non infetti   ####
#mi creo db_sangue per eseguire modifiche e analisi senza toccare db_prop
db_sangue <- db_prop
db_sangue$sangue <- ifelse(db_sangue$sangue == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo sangue
db_sangue$sangue_vs_noninfetto <- ifelse(db_sangue$infetto == 0 & db_sangue$sangue == 0, 0,
                                ifelse(db_sangue$infetto == 1 & db_sangue$sangue == 1, 1,
                                                    ifelse(db_sangue$infetto == 1 & db_sangue$sangue == 0, "", "")))


db_sangue <- db_sangue[db_sangue$sangue_vs_noninfetto !="", ] #elimino campi vuoti (infetti in altri siti)


#Faccio matching con PS
db_sangue$sangue_vs_noninfetto <- ifelse(db_sangue$sangue_vs_noninfetto == 1, T, F) #trasformo in logico

model_sangue <- glm(
  sangue_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_sangue,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_sangue <- model_sangue$fitted.values

# Define outcome and treatment vector
outcome_sangue <- db_sangue$cost_ln
treatment_sangue <- db_sangue$sangue_vs_noninfetto

# Matching
match_obj_sangue <- Match(
  Y = outcome_sangue,            # vector with the outcome
  Tr = treatment_sangue,         # vector with treatment
  X = ps_values_sangue,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_sangue <- bal.tab(
  match_obj_sangue,
  sangue_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_sangue,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_sangue)

bal.plot(
  match_obj_sangue,
  formula = sangue_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_sangue,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_sangue)


### PS analysis urinario vs non infetti   ####
#mi creo db_urinario per eseguire modifiche e analisi senza toccare db_prop
db_urinario <- db_prop
db_urinario$urinario <- ifelse(db_urinario$urinario == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo urinario
db_urinario$urinario_vs_noninfetto <- ifelse(db_urinario$infetto == 0 & db_urinario$urinario == 0, 0,
                                         ifelse(db_urinario$infetto == 1 & db_urinario$urinario == 1, 1,
                                                ifelse(db_urinario$infetto == 1 & db_urinario$urinario == 0, "", "")))


db_urinario <- db_urinario[db_urinario$urinario_vs_noninfetto !="", ] #elimino campi vuoti (infetti in altri siti)


#Faccio matching con PS
db_urinario$urinario_vs_noninfetto <- ifelse(db_urinario$urinario_vs_noninfetto == 1, T, F) #trasformo in logico

model_urinario <- glm(
  urinario_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_urinario,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_ur <- model_urinario$fitted.values

# Define outcome and treatment vector
outcome_ur <- db_urinario$cost_ln
treatment_ur <- db_urinario$urinario_vs_noninfetto

# Matching
match_obj_urinario <- Match(
  Y = outcome_ur,            # vector with the outcome
  Tr = treatment_ur,         # vector with treatment
  X = ps_values_ur,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_urinario <- bal.tab(
  match_obj_urinario,
  urinario_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_urinario,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_urinario)

bal.plot(
  match_obj_urinario,
  formula = urinario_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_urinario,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_urinario)


### PS analysis rettale  vs non infetti   ####
#mi creo db_rett per eseguire modifiche e analisi senza toccare db_prop
db_rett <- db_prop
db_rett$rettale <- ifelse(db_rett$rettale == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo rettale
db_rett$rett_vs_noninfetto <- ifelse(db_rett$infetto == 0 & db_rett$rettale == 0, 0,
                                         ifelse(db_rett$infetto == 1 & db_rett$rettale == 1, 1,
                                                ifelse(db_rett$infetto == 1 & db_rett$rettale == 0, "", "")))


db_rett <- db_rett[db_rett$rett_vs_noninfetto !="", ] #elimino campi vuoti (infetti in altri siti)


#Faccio matching con PS
db_rett$rett_vs_noninfetto <- ifelse(db_rett$rett_vs_noninfetto == 1, T, F) #trasformo in logico

model_rett <- glm(
  rett_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_rett,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_rett <- model_rett$fitted.values

# Define outcome and treatment vector
outcome_rett <- db_rett$cost_ln
treatment_rett <- db_rett$rett_vs_noninfetto

# Matching
match_obj_rett <- Match(
  Y = outcome_rett,            # vector with the outcome
  Tr = treatment_rett,         # vector with treatment
  X = ps_values_rett,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_rett <- bal.tab(
  match_obj_rett,
  rett_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_rett,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_rett)

bal.plot(
  match_obj_rett,
  formula = rett_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_rett,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_rett)


### PS analysis respiratorio  vs non infetti   ####
#mi creo db_resp per eseguire modifiche e analisi senza toccare db_prop
db_resp <- db_prop
db_resp$respiratorio <- ifelse(db_resp$respiratorio == 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo respiratorio
db_resp$resp_vs_noninfetto <- ifelse(db_resp$infetto == 0 & db_resp$respiratorio == 0, 0,
                                     ifelse(db_resp$infetto == 1 & db_resp$respiratorio == 1, 1,
                                            ifelse(db_resp$infetto == 1 & db_resp$respiratorio == 0, "", "")))


db_resp <- db_resp[db_resp$resp_vs_noninfetto !="", ] #elimino campi vuoti (infetti in altri siti)


#Faccio matching con PS
db_resp$resp_vs_noninfetto <- ifelse(db_resp$resp_vs_noninfetto == 1, T, F) #trasformo in logico

model_resp <- glm(
  resp_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_resp,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_resp <- model_resp$fitted.values

# Define outcome and treatment vector
outcome_resp <- db_resp$cost_ln
treatment_resp <- db_resp$resp_vs_noninfetto

# Matching
match_obj_resp <- Match(
  Y = outcome_resp,            # vector with the outcome
  Tr = treatment_resp,         # vector with treatment
  X = ps_values_resp,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_resp <- bal.tab(
  match_obj_resp,
  resp_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_resp,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_resp)

bal.plot(
  match_obj_resp,
  formula = resp_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_resp,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_resp)



## PS analysis ferita  vs non infetti   ####
#mi creo db_ferita per eseguire modifiche e analisi senza toccare db_prop
db_ferita <- db_prop
db_ferita$ferita <- ifelse(db_ferita$ferita== 0, 1, 0) #inverto i valori di 0 e 1

#Mi creo variabile non infetti vs infetti solo ferita
db_ferita$ferita_vs_noninfetto <- ifelse(db_ferita$infetto == 0 & db_ferita$ferita == 0, 0,
                                     ifelse(db_ferita$infetto == 1 & db_ferita$ferita == 1, 1,
                                            ifelse(db_ferita$infetto == 1 & db_ferita$ferita == 0, "", "")))


db_ferita <- db_ferita[db_ferita$ferita_vs_noninfetto !="", ] #elimino campi vuoti (infetti in altri siti)


#Faccio matching con PS
db_ferita$ferita_vs_noninfetto <- ifelse(db_ferita$ferita_vs_noninfetto == 1, T, F) #trasformo in logico

model_ferita <- glm(
  ferita_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_ferita,
  family = binomial("logit")
)

#matching using PS 
# Get PS values
ps_values_fe <- model_ferita$fitted.values

# Define outcome and treatment vector
outcome_fe <- db_ferita$cost_ln
treatment_fe <- db_ferita$ferita_vs_noninfetto

# Matching
match_obj_ferita <- Match(
  Y = outcome_fe,            # vector with the outcome
  Tr = treatment_fe,         # vector with treatment
  X = ps_values_fe,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Balance assessment 
balance_ferita <- bal.tab(
  match_obj_ferita,
  ferita_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_ferita,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_ferita)

bal.plot(
  match_obj_ferita,
  formula = ferita_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_ferita,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_ferita)
