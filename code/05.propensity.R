# Evaluation of covariates distributions by treatment groups

db_prop <- db %>% 
  dplyr::select(infetto,
                proc_inv_real,
         sdo1_sesso,
         sdo1_eta, 
         sdo1_modali,
         sdo1_degenza,
         terapia, 
         decessodico,
         education,
         profession_simple,
         #reparto,
         risk_dep,
         #dia_pri, #abbiamo deciso di escluderla
         cost,
         sangue,   #inserisco in db_prop variabili relative alla sede dell'infezione 
         urinario,
         rettale,
         respiratorio,
         ferita,
         acinetobacter,  #inserisco in db_prop variabili relative ai batteri
         klebsiella_pnm,
         clostridium,
         enterococcus,
         escherichia_coli,
         pseudomonas,
         candida,
         staphylococcus
          #inserisco in db_prop num_infezioni poi vediamo come usarle
         )



# sto togliendo i costi = zero, ma poi devo imputarli per bene ####
db_prop <- db_prop[db_prop$cost != 0, ]

# togliamo le infezioni rettali perchè non siamo sicuri che siano ICA
#db_prop <- db_prop[db_prop$rettale != 0, ]  #lascio rettale perchè è stato ricodificato

#db_prop <- db_prop %>% 
  #dplyr::select(-rettale)

db_prop <- db_prop %>% 
  mutate(cost_ln = log(cost))

db_prop <- db_prop %>% 
  var_labels(sdo1_sesso         = "Sex",
             sdo1_eta           = "Age",
             sdo1_cittad        = "National-Non-national",
             sdo1_sta_civ       = "Civil status",
             family             = "Living alone",
             education          = "Education level",
             profession         = "Occupational status",
             profession_simple  = "Employed-Unemployed-Retired",
             reparto            = "Department",
             sdo1_modali        = "Admission modality",
             sdo1_tip_dim       = "Type of discharge",
             sdo1_causa_ext     = "External Reason",             # Verificare
             sdo1_degenza       = "Days of stay",
             sdo1_attesa        = "Days in waiting list",
             terapia            = "Medical-Surgical",
             cost               = "Real reimbursement",
             cost_ln            = "Log Real Reimbursement",
             num_infezioni      = "Infections number",
             infetto            = "Infection detected",
             dia_pri            = "Primary diagnosis",
             proc_inv_real      = "Invasive procedure",
             decessodico        = "Died",
             risk_dep           = "High-risk department"
  )


summary3 <- db_prop %>% 
  tbl_summary(by = infetto) %>% 
  add_p %>% 
  add_overall()

summary3


# Step 1: PS estimation with logistic regression -----------------------

model_type <- 0

if (model_type == 1) {

model <- glm(
  infetto ~ proc_inv_real + sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + risk_dep, 
  data = db_prop,
  family = binomial("logit")
)
} else {
   model <- glm(
     infetto ~ proc_inv_real + sdo1_eta + sdo1_modali + terapia  + risk_dep, 
     data = db_prop,
     family = binomial("logit")
   )
 }
   
# model %>% tbl_regression(exponentiate = T)


# Get PS values
ps_values <- model$fitted.values

# Step 2: matching using PS --------------------------------------------
# Define outcome and treatment vector
outcome <- db_prop$cost
outcome_ln <- db_prop$cost_ln
treatment <- db_prop$infetto

# from Matching package requires the treatment vector to be a logical

# Matching
match_object_ln <- Match(
  Y = outcome_ln,            # vector with the outcome
  Tr = treatment,         # vector with treatment
  X = ps_values,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

match_object <- Match(
  Y = outcome,            # vector with the outcome
  Tr = treatment,         # vector with treatment
  X = ps_values,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 1,                  # 1:2 matching,
  ties = FALSE,
  replace = FALSE
)

# Step 3: balance assessment -------------------------------------------

if (model_type == 1) {
balance_table <- bal.tab(
  match_object,
  infetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + risk_dep + proc_inv_real, 
  data = db_prop,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)
} else {
  balance_table <- bal.tab(
    match_object,
    infetto ~ sdo1_eta + sdo1_modali  + terapia  + risk_dep + proc_inv_real, 
    data = db_prop,
    continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
    un = T, stats = c('means.diffs', 'variance.ratios')
  )
}

#print(balance_table)

# se lo rifacciamo per l'articolo cambiando qualcosa, qui è utile fare i bar plot con le variabili
# aggiustate (il codice sta nei file di MG) bar.plot (match_object, etc.)

# Step 4: outcome analysis
summary(match_object)
summary(match_object_ln)

#Aggiusto cost_ln
cost_ln_agg <- ((exp(match_object_ln$est)) - 1) * 100

# Compute confidence intervals -----------------------------------------
lower <- match_object$est - 2 * match_object$se.standard
upper <- match_object$est + 2 * match_object$se.standard

lower_ln <- match_object_ln$est - 2 * match_object_ln$se.standard
upper_ln <- match_object_ln$est + 2 * match_object_ln$se.standard


low_ln <- ((exp(lower_ln)) - 1) * 100
high_ln <- ((exp(upper_ln)) - 1) * 100

# Get matched datasets
db_match <- bind_rows(
  db_prop[match_object$index.treated, ],
  db_prop[match_object$index.control, ]
)

# creo un db per le descrittive in cui inverto  zero con TRUE e 1 con FALSE

# Define the columns to be transformed
columns_to_transform <- c("rettale", "sangue", "urinario", "respiratorio", "ferita", "acinetobacter", "klebsiella_pnm", "clostridium", "enterococcus", "escherichia_coli", "pseudomonas", "candida", "staphylococcus")

# Transform the dataframe
db_desc <- db_prop %>%
  mutate_at(.vars = columns_to_transform, .funs = ~ ifelse(. == 0, TRUE, FALSE))

# Descrittive con database migliorato 
summary4 <- db_desc %>% 
  tbl_summary(by = infetto) %>% 
  add_p %>% 
  add_overall()

summary4

