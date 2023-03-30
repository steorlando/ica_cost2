db <- db %>% 
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
             infetto            = "Infection detected"
  )
