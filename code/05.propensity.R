# Evaluation of covariates distributions by treatment groups

db_prop <- db %>% 
  dplyr::select(infetto,
         sdo1_sesso,
         sdo1_eta, 
         sdo1_modali,
         sdo1_degenza,
         terapia, 
         decessodico,
         education,
         profession_simple,
         reparto,
         dia_pri,
         proc_inv,
         cost_ln,
         cost
         )

reparti_null <- db_prop %>%
  group_by(reparto) %>% 
  summarise(infected = sum(infetto)) %>% 
  filter(infected < 5) %>% 
  mutate(filt = 1) %>% 
  dplyr::select(reparto, filt)

diag_null <- db_prop %>%
  group_by(dia_pri) %>% 
  summarise(patients = n()) %>% 
  filter(patients < 5) %>% 
  mutate(filt1 = 1) %>% 
  dplyr::select(dia_pri, filt1)

db_prop <- left_join(db_prop, reparti_null) %>% mutate(filt = ifelse(is.na(filt),0,filt))
db_prop <- left_join(db_prop, diag_null) %>% mutate(filt1 = ifelse(is.na(filt1),0,filt1))

db_prop <- db_prop %>% 
  filter(!filt == 1) %>% 
  filter(!filt1 == 1) %>% 
  dplyr::select(-c(filt, filt1))

summary2 <- db_prop %>% 
  tbl_summary(by = infetto) %>% 
  add_p %>% 
  add_overall()

summary2

# sto togliendo i costi = zero, ma poi devo imputarli per bene ####
dp_prop <- db_prop %>% 
  filter(!cost_ln == 0)


# Step 1: PS estimation with logistic regression -----------------------
model <- glm(
  infetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_prop,
  family = binomial("logit")
)

model %>% tbl_regression(exponentiate = T)

# Get PS values
ps_values <- model$fitted.values

# Step 2: matching using PS --------------------------------------------
# Define outcome and treatment vector
outcome <- db_prop$cost
treatment <- db_prop$infetto

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
  data = db_prop,
  continuous = "std", binary = "std", s.d.denom = "treated", disp = c('means', 'sds'),
  un = T, stats = c('means.diffs', 'variance.ratios')
)

print(balance_table)

bal.plot(
  match_object,
  formula = infetto ~ sdo1_eta + sdo1_modali + sdo1_degenza + terapia + decessodico + reparto + dia_pri + proc_inv, 
  data = db_prop,
  var.name = "sdo1_eta", which = "both"
)
# Step 4: outcome analysis ---------------------------------------------
# 4A) Unadjusted analysis ----------------------------------------------
summary(match_object)


# Compute confidence intervals -----------------------------------------
# Without caliper
rd_1 <- match_object$est
lower_1 <- match_object$est - 2 * match_object$se.standard
upper_1 <- match_object$est + 2 * match_object$se.standard

c(lower_1, rd_1, upper_1)

# With caliper
rd_2 <- match_object_2$est
lower_2 <- match_object_2$est - 2 * match_object$se.standard
upper_2 <- match_object_2$est + 2 * match_object$se.standard

c(lower_2, rd_2, upper_2)

# Get matched datasets
clotbusting_matched_1 <- bind_rows(
  clotbusting_db[match_object$index.treated, ],
  clotbusting_db[match_object$index.control, ]
)

clotbusting_matched_2 <- bind_rows(
  clotbusting_db[match_object_2$index.treated, ],
  clotbusting_db[match_object_2$index.control, ]
)

# Logistic regression to quantify drug's effect with OR
# Without caliper
unadj_model_1 <- glm(
  death_30_days ~ trt, data = clotbusting_matched_1,
  family = binomial("logit")
)

or_unadj_1 <- exp(coef(unadj_model_1)[2])
lower_or_unadj_1 <- exp(confint(unadj_model_1)[2, 1])
upper_or_unadj_1 <- exp(confint(unadj_model_1)[2, 2])

c(lower_or_unadj_1, or_unadj_1, upper_or_unadj_1)

# With caliper
unadj_model_2 <- glm(
  death_30_days ~ trt, data = clotbusting_matched_2,
  family = binomial("logit")
)

or_unadj_2 <- exp(coef(unadj_model_2)[2])
lower_or_unadj_2 <- exp(confint(unadj_model_2)[2, 1])
upper_or_unadj_2 <- exp(confint(unadj_model_2)[2, 2])

c(lower_or_unadj_2, or_unadj_2, upper_or_unadj_2)

# 4B) Adjusted analysis ------------------------------------------------
# Adjusting for confounders may still reduce bias due to residual
# imbalance in the matched set

# Without caliper
adj_model_1 <- glm(
  death_30_days ~ trt + risk + age + severity,
  data = clotbusting_matched_1, family = binomial("logit")
)

summary(adj_model_1)

# Exponentiating the coefficients of treatment returns the conditional
# effect of the new drug in terms of OR
or_cond_1 <- exp(coef(adj_model_1)[2])
lower_or_cond_1 <- exp(confint(adj_model_1)[2, 1])
upper_or_cond_1 <- exp(confint(adj_model_1)[2, 2])

c(lower_or_cond_1, or_cond_1, upper_or_cond_1)

# But the marginal OR is needed to evaluate the ATT
marg_means_1 <- emmeans(adj_model_1, specs = "trt")

marg_means_1@linfct

att_1 <- pairs(marg_means_1, reverse = TRUE, type = "response")
confint(att_1)

# With caliper
adj_model_2 <- glm(
  death_30_days ~ trt + risk + age + severity,
  data = clotbusting_matched_2, family = binomial("logit")
)

summary(adj_model_2)

# Exponentiating the coefficients of treatment returns the conditional
# effect of the new drug in terms of OR
or_cond_2 <- exp(coef(adj_model_2)[2])
lower_or_cond_2 <- exp(confint(adj_model_2)[2, 1])
upper_or_cond_2 <- exp(confint(adj_model_2)[2, 2])

c(lower_or_cond_2, or_cond_2, upper_or_cond_2)

# But the marginal OR is needed to evaluate the ATT
marg_means_2 <- emmeans(adj_model_2, specs = "trt")

marg_means_2@linfct

att_2 <- pairs(marg_means_2, reverse = TRUE, type = "response")
confint(att_2)


