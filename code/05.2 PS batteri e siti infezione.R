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
outcome_ac <- db_acineto$cost
outcome_ac_ln <- db_acineto$cost_ln
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

match_obj_acineto_ln <- Match(
  Y = outcome_ac_ln,            # vector with the outcome
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

##print(balance_acineto)

bal.plot(
  match_obj_acineto,
  formula = acineto_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_acineto,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_acineto)
summary(match_obj_acineto_ln)

#Aggiusto cost_ln
cost_ac_exp <- exp(match_obj_acineto_ln$est)
cost_ac_ln_agg <- (cost_ac_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_ac <- match_obj_acineto$est - 2 * match_obj_acineto$se.standard
upper_ac <- match_obj_acineto$est + 2 * match_obj_acineto$se.standard

lower_ac_ln <- match_obj_acineto_ln$est - 2 * match_obj_acineto_ln$se.standard
upper_ac_ln <- match_obj_acineto_ln$est + 2 * match_obj_acineto_ln$se.standard

low_ac_ln <- ((exp(lower_ac_ln)) - 1) * 100
high_ac_ln <- ((exp(upper_ac_ln)) - 1) * 100


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
outcome_kl <- db_klebsiella$cost
outcome_kl_ln <- db_klebsiella$cost_ln
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

match_obj_klebsiella_ln <- Match(
  Y = outcome_kl_ln,            # vector with the outcome
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

#print(balance_klebsiella)

bal.plot(
  match_obj_klebsiella,
  formula = klebsiella_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_klebsiella,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_klebsiella)
summary(match_obj_klebsiella_ln)

#Aggiusto cost_ln
cost_kl_exp <- exp(match_obj_klebsiella_ln$est)
cost_kl_ln_agg <- (cost_kl_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_kl <- match_obj_klebsiella$est - 2 * match_obj_klebsiella$se.standard
upper_kl <- match_obj_klebsiella$est + 2 * match_obj_klebsiella$se.standard

lower_kl_ln <- match_obj_klebsiella_ln$est - 2 * match_obj_klebsiella_ln$se.standard
upper_kl_ln <- match_obj_klebsiella_ln$est + 2 * match_obj_klebsiella_ln$se.standard

low_kl_ln <- ((exp(lower_kl_ln)) - 1) * 100
high_kl_ln <- ((exp(upper_kl_ln)) - 1) * 100

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
outcome_cl <- db_clostridium$cost
outcome_cl_ln <- db_clostridium$cost_ln
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

match_obj_clostridium_ln <- Match(
  Y = outcome_cl_ln,            # vector with the outcome
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

#print(balance_clostridium)

bal.plot(
  match_obj_clostridium,
  formula = clostridium_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_clostridium,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_clostridium)
summary(match_obj_clostridium_ln)

#Aggiusto cost_ln
cost_cl_exp <- exp(match_obj_clostridium_ln$est)
cost_cl_ln_agg <- (cost_cl_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_cl <- match_obj_clostridium$est - 2 * match_obj_clostridium$se.standard
upper_cl <- match_obj_clostridium$est + 2 * match_obj_clostridium$se.standard

lower_cl_ln <- match_obj_clostridium_ln$est - 2 * match_obj_clostridium_ln$se.standard
upper_cl_ln <- match_obj_clostridium_ln$est + 2 * match_obj_clostridium_ln$se.standard

low_cl_ln <- ((exp(lower_cl_ln)) - 1) * 100
high_cl_ln <- ((exp(upper_cl_ln)) - 1) * 100

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
outcome_ent <- db_entero$cost
outcome_ent_ln <- db_entero$cost_ln
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

match_obj_entero_ln <- Match(
  Y = outcome_ent_ln,            # vector with the outcome
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

#print(balance_entero)

bal.plot(
  match_obj_entero,
  formula = entero_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_entero,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_entero)
summary(match_obj_entero_ln)

#Aggiusto cost_ln
cost_ent_exp <- exp(match_obj_entero_ln$est)
cost_ent_ln_agg <- (cost_ent_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_ent <- match_obj_entero$est - 2 * match_obj_entero$se.standard
upper_ent <- match_obj_entero$est + 2 * match_obj_entero$se.standard

lower_ent_ln <- match_obj_entero_ln$est - 2 * match_obj_entero_ln$se.standard
upper_ent_ln <- match_obj_entero_ln$est + 2 * match_obj_entero_ln$se.standard

low_ent_ln <- ((exp(lower_ent_ln)) - 1) * 100
high_ent_ln <- ((exp(upper_ent_ln)) - 1) * 100


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
outcome_es <- db_escherichia$cost
outcome_es_ln <- db_escherichia$cost_ln
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

match_obj_escherichia_ln <- Match(
  Y = outcome_es_ln,            # vector with the outcome
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

#print(balance_escherichia)

bal.plot(
  match_obj_escherichia,
  formula = escherichia_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_escherichia,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_escherichia)
summary(match_obj_escherichia_ln)

#Aggiusto cost_ln
cost_es_exp <- exp(match_obj_escherichia_ln$est)
cost_es_ln_agg <- (cost_es_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_es <- match_obj_escherichia$est - 2 * match_obj_escherichia$se.standard
upper_es <- match_obj_escherichia$est + 2 * match_obj_escherichia$se.standard

lower_es_ln <- match_obj_escherichia_ln$est - 2 * match_obj_escherichia_ln$se.standard
upper_es_ln <- match_obj_escherichia_ln$est + 2 * match_obj_escherichia_ln$se.standard

low_es_ln <- ((exp(lower_es_ln)) - 1) * 100
high_es_ln <- ((exp(upper_es_ln)) - 1) * 100


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
outcome_ps <- db_pseudo$cost
outcome_ps_ln <- db_pseudo$cost_ln
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

match_obj_pseudo_ln <- Match(
  Y = outcome_ps_ln,            # vector with the outcome
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

#print(balance_pseudo)

bal.plot(
  match_obj_pseudo,
  formula = pseudo_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_pseudo,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_pseudo)
summary(match_obj_pseudo_ln)

#Aggiusto cost_ln
cost_pseudo_exp <- exp(match_obj_pseudo_ln$est)
cost_pseudo_ln_agg <- (cost_pseudo_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_pseudo <- match_obj_pseudo$est - 2 * match_obj_pseudo$se.standard
upper_pseudo <- match_obj_pseudo$est + 2 * match_obj_pseudo$se.standard

lower_pseudo_ln <- match_obj_pseudo_ln$est - 2 * match_obj_pseudo_ln$se.standard
upper_pseudo_ln <- match_obj_pseudo_ln$est + 2 * match_obj_pseudo_ln$se.standard

low_pseudo_ln <- ((exp(lower_pseudo_ln)) - 1) * 100
high_pseudo_ln <- ((exp(upper_pseudo_ln)) - 1) * 100

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
outcome_ca <- db_candida$cost
outcome_ca_ln <- db_candida$cost_ln
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

match_obj_candida_ln <- Match(
  Y = outcome_ca_ln,            # vector with the outcome
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

#print(balance_candida)

bal.plot(
  match_obj_candida,
  formula = candida_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_candida,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_candida)
summary(match_obj_candida_ln)

#Aggiusto cost_ln
cost_ca_exp <- exp(match_obj_candida_ln$est)
cost_ca_ln_agg <- (cost_ca_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_ca <- match_obj_candida$est - 2 * match_obj_candida$se.standard
upper_ca <- match_obj_candida$est + 2 * match_obj_candida$se.standard

lower_ca_ln <- match_obj_candida_ln$est - 2 * match_obj_candida_ln$se.standard
upper_ca_ln <- match_obj_candida_ln$est + 2 * match_obj_candida_ln$se.standard

low_ca_ln <- ((exp(lower_ca_ln)) - 1) * 100
high_ca_ln <- ((exp(upper_ca_ln)) - 1) * 100

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
outcome_st <- db_staphylo$cost
outcome_st_ln <- db_staphylo$cost_ln
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

match_obj_staphylo_ln <- Match(
  Y = outcome_st_ln,            # vector with the outcome
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

#print(balance_staphylo)

bal.plot(
  match_obj_staphylo,
  formula = staphylo_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_staphylo,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_staphylo)
summary(match_obj_staphylo_ln)

#Aggiusto cost_ln
cost_st_exp <- exp(match_obj_staphylo_ln$est)
cost_st_ln_agg <- (cost_st_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_st <- match_obj_staphylo$est - 2 * match_obj_staphylo$se.standard
upper_st <- match_obj_staphylo$est + 2 * match_obj_staphylo$se.standard

lower_st_ln <- match_obj_staphylo_ln$est - 2 * match_obj_staphylo_ln$se.standard
upper_st_ln <- match_obj_staphylo_ln$est + 2 * match_obj_staphylo_ln$se.standard

low_st_ln <- ((exp(lower_st_ln)) - 1) * 100
high_st_ln <- ((exp(upper_st_ln)) - 1) * 100
  
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
outcome_sangue <- db_sangue$cost
outcome_sangue_ln <- db_sangue$cost_ln
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

match_obj_sangue_ln <- Match(
  Y = outcome_sangue_ln,            # vector with the outcome
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

#print(balance_sangue)

bal.plot(
  match_obj_sangue,
  formula = sangue_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_sangue,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_sangue)
summary(match_obj_sangue_ln)

#Aggiusto cost_ln
cost_sangue_exp <- exp(match_obj_sangue_ln$est)
cost_sangue_ln_agg <- (cost_sangue_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_sangue <- match_obj_sangue$est - 2 * match_obj_sangue$se.standard
upper_sangue <- match_obj_sangue$est + 2 * match_obj_sangue$se.standard

lower_sangue_ln <- match_obj_sangue_ln$est - 2 * match_obj_sangue_ln$se.standard
upper_sangue_ln <- match_obj_sangue_ln$est + 2 * match_obj_sangue_ln$se.standard

low_sangue_ln <- ((exp(lower_sangue_ln)) - 1) * 100
high_sangue_ln <- ((exp(upper_sangue_ln)) - 1) * 100

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
outcome_ur <- db_urinario$cost
outcome_ur_ln <- db_urinario$cost_ln
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

match_obj_urinario_ln <- Match(
  Y = outcome_ur_ln,            # vector with the outcome
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

#print(balance_urinario)

bal.plot(
  match_obj_urinario,
  formula = urinario_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_urinario,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_urinario)
summary(match_obj_urinario_ln)

#Aggiusto cost_ln
cost_ur_exp <- exp(match_obj_urinario_ln$est)
cost_ur_ln_agg <- (cost_ur_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_ur <- match_obj_urinario$est - 2 * match_obj_urinario$se.standard
upper_ur <- match_obj_urinario$est + 2 * match_obj_urinario$se.standard

lower_ur_ln <- match_obj_urinario_ln$est - 2 * match_obj_urinario_ln$se.standard
upper_ur_ln <- match_obj_urinario_ln$est + 2 * match_obj_urinario_ln$se.standard

low_ur_ln <- ((exp(lower_ur_ln)) - 1) * 100
high_ur_ln <- ((exp(upper_ur_ln)) - 1) * 100


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
outcome_resp <- db_resp$cost
outcome_resp_ln <- db_resp$cost_ln
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

match_obj_resp_ln <- Match(
  Y = outcome_resp_ln,            # vector with the outcome
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

#print(balance_resp)

bal.plot(
  match_obj_resp,
  formula = resp_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_resp,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_resp)
summary(match_obj_resp_ln)

#Aggiusto cost_ln
cost_resp_exp <- exp(match_obj_resp_ln$est)
cost_resp_ln_agg <- (cost_resp_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_resp <- match_obj_resp$est - 2 * match_obj_resp$se.standard
upper_resp <- match_obj_resp$est + 2 * match_obj_resp$se.standard

lower_resp_ln <- match_obj_resp_ln$est - 2 * match_obj_resp_ln$se.standard
upper_resp_ln <- match_obj_resp_ln$est + 2 * match_obj_resp_ln$se.standard

low_resp_exp <- exp(lower_resp_ln)
low_resp_exp_agg <- (low_resp_exp - 1) * 100

low_resp_ln <- ((exp(lower_resp_ln)) - 1) * 100
high_resp_ln <- ((exp(upper_resp_ln)) - 1) * 100

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
outcome_fe <- db_ferita$cost
outcome_fe_ln <- db_ferita$cost_ln
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

match_obj_ferita_ln <- Match(
  Y = outcome_fe_ln,            # vector with the outcome
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

#print(balance_ferita)

bal.plot(
  match_obj_ferita,
  formula = ferita_vs_noninfetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_ferita,
  var.name = "sdo1_degenza", which = "both"
)

# Outcome analysis
summary(match_obj_ferita)
summary(match_obj_ferita_ln)

#Aggiusto cost_ln
cost_fe_exp <- exp(match_obj_ferita_ln$est)
cost_fe_ln_agg <- (cost_fe_exp - 1) * 100

# Compute confidence intervals -----------------------------------------
lower_fe <- match_obj_ferita$est - 2 * match_obj_ferita$se.standard
upper_fe <- match_obj_ferita$est + 2 * match_obj_ferita$se.standard

lower_fe_ln <- match_obj_ferita_ln$est - 2 * match_obj_ferita_ln$se.standard
upper_fe_ln <- match_obj_ferita_ln$est + 2 * match_obj_ferita_ln$se.standard

low_fe_ln <- ((exp(lower_fe_ln)) - 1) * 100
high_fe_ln <- ((exp(upper_fe_ln)) - 1) * 100


#Creo dataframe 

df_ica <- data.frame(
  "Batteri/Siti" = c("Overall", "Acinobacter", "klebsiella", "Clostridium", "Enterococcus", 
                    "Escherichiacoli", "Pseudomonas", "Candida", "Staphylococcus",
                    "Blood", "Urinary", "Respiratory", "Wound"),
  "Eff_cost" = c(match_object$est, match_obj_acineto$est, match_obj_klebsiella$est, match_obj_clostridium$est,
            match_obj_entero$est, match_obj_escherichia$est, match_obj_pseudo$est, match_obj_candida$est,
            match_obj_staphylo$est, match_obj_sangue$est, match_obj_urinario$est, 
            match_obj_resp$est, match_obj_ferita$est),
 "c_low" = c(lower, lower_ac, lower_kl, lower_cl, lower_ent, lower_es, lower_pseudo, lower_ca, lower_st,
             lower_sangue, lower_ur, lower_resp, lower_fe),
 "c_high" = c(upper, upper_ac, upper_kl, upper_cl, upper_ent, upper_es, upper_pseudo, upper_ca, upper_st,
              upper_sangue, upper_ur, upper_resp, upper_fe),
 "Eff_ln" = c(cost_ln_agg, cost_ac_ln_agg, cost_kl_ln_agg, cost_cl_ln_agg, cost_ent_ln_agg,
              cost_es_ln_agg, cost_pseudo_ln_agg, cost_ca_ln_agg, cost_st_ln_agg,
              cost_sangue_ln_agg, cost_ur_ln_agg, cost_resp_ln_agg, cost_fe_ln_agg),
 "ln_low" = c(low_ln, low_ac_ln, low_kl_ln, low_cl_ln, low_ent_ln, low_es_ln, low_pseudo_ln, low_ca_ln, low_st_ln,
              low_sangue_ln, low_ur_ln, low_resp_ln, low_fe_ln),
 "ln_high" = c(high_ln, high_ac_ln, high_kl_ln, high_cl_ln, high_ent_ln, high_es_ln, high_pseudo_ln, 
               high_ca_ln, high_st_ln, high_sangue_ln, high_ur_ln, high_resp_ln, high_fe_ln)
)

zero_to_one <- function(x) {ifelse(x == 0,1,0)
  }

db_count <- db_prop %>% 
  dplyr::select(c(infetto,
                  acinetobacter,
                  klebsiella_pnm,
                  clostridium,
                  enterococcus,
                  escherichia_coli,
                  pseudomonas,
                  candida,
                  staphylococcus,
                  sangue,
                  urinario,
                  respiratorio,
                  ferita)) %>% 
  mutate(infetto = ifelse(infetto == TRUE, 0, 1)) %>% 
  mutate_all(zero_to_one)

db_count <- db_count %>% 
  mutate(first = "") %>% 
  relocate(first, .before = infetto) %>% 
  adorn_totals(where = "row") %>% 
  dplyr::select(-first)
  
# Assuming your dataframe is named 'df'
last_row <- db_count[nrow(db_count), ]

# Convert the last row to a vector
n_paz <- as.vector(unlist(last_row))

df_ica <- df_ica %>% 
  mutate(Infetti = n_paz) %>% 
  relocate(Infetti, .before = Eff_cost)

export(df_ica, "output/risultati.xlsx")

summary(match_obj_clostridium)
