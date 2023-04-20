#prova senza costo = 0

db_prova <- db

db_prova<- db_prova[db_prova$cost != 0, ]

db_prova <- db_prova %>% 
  mutate(cost_ln = log(cost))


reparti_null <- db_prova %>%
  group_by(reparto) %>% 
  summarise(infected = sum(infetto)) %>% 
  filter(infected < 5) %>% 
  mutate(filt = 1) %>% 
  dplyr::select(reparto, filt)

diag_null <- db_prova %>%
  group_by(dia_pri) %>% 
  summarise(patients = n()) %>% 
  filter(patients < 5) %>% 
  mutate(filt1 = 1) %>% 
  dplyr::select(dia_pri, filt1)

db_prova <- left_join(db_prova, reparti_null) %>% mutate(filt = ifelse(is.na(filt),0,filt))
db_prova <- left_join(db_prova, diag_null) %>% mutate(filt1 = ifelse(is.na(filt1),0,filt1))

db_prova <- db_prova %>% 
  filter(!filt == 1) %>% 
  filter(!filt1 == 1) %>% 
  dplyr::select(-c(filt, filt1))

summary2 <- db_prova %>% 
  tbl_summary(by = infetto) %>% 
  add_p %>% 
  add_overall()

summary2


# Step 1: PS estimation with logistic regression -----------------------
model <- glm(
  infetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_prova,
  family = binomial("logit")
)

model %>% tbl_regression(exponentiate = T)

# Get PS values
ps_values <- model$fitted.values

# Step 2: matching using PS --------------------------------------------
# Define outcome and treatment vector
outcome <- db_prova$cost
treatment <- db_prova$infetto
outcome_ln <- db_prova$cost_ln

# from Matching package requires the treatment vector to be a logical

# Matching
match_object <- Match(
  Y = outcome,            # vector with the outcome
  Tr = treatment,         # vector with treatment
  X = ps_values,          # vector with individual propensity scores
  estimand = "ATT",       # average treatment effect on treated
  M = 2,                  # 1:2 matching,
  ties = FALSE,           # If there are multiple matches, takes only 1
  replace = FALSE         # Control subjects used only once
)


# Step 3: balance assessment -------------------------------------------
balance_table <- bal.tab(
  match_object,
  infetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_prova,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_table)

bal.plot(
  match_object,
  formula = infetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_prova,
  var.name = "sdo1_eta", which = "both"
)


# Step 4: outcome analysis
summary(match_object)

# Compute confidence intervals -----------------------------------------
# Without caliper
rd_1 <- match_object$est
lower_1 <- match_object$est - 2 * match_object$se.standard
upper_1 <- match_object$est + 2 * match_object$se.standard

c(lower_1, rd_1, upper_1)


# Get matched datasets
db_match <- bind_rows(
  db_prop[match_object$index.treated, ],
  db_prop[match_object$index.control, ]
)

# Logistic regression to quantify OR
# Without caliper
db_match_model<- glm(
  cost ~ infetto + sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv,
  data = db_match,
  family = gaussian("identity")
)

or <- exp(coef(db_match_model)[2])
lower_or <- exp(confint(db_match_model)[2, 1])
upper_or <- exp(confint(db_match_model)[2, 2])

c(lower_or, or, upper_or)


# But the marginal OR is needed to evaluate the ATT
marg_means_1 <- emmeans(db_match_model, specs = "trt")

marg_means_1@linfct

att_1 <- pairs(marg_means_1, reverse = TRUE, type = "response")
confint(att_1)





