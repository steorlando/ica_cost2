# Import databases ####
db_orig <- import("data/db_old.csv") #Import main database

# Il vecchio DB è salvato è, per confonderci le idee, si chiama master.csv. Ha qualche variabile in più alla fine
# Poi vediamo se sono variabili che ci servono oppure no

db_select <- db_orig %>%  # Elimino alcune variabili che non mi servono
  dplyr::select(c(1:72, 456:492, c(S1_Risultato,
                                   G1_Risultato,
                                   U1_Risultato,
                                   R1_Risultato,
                                   T1_Risultato,
                                   N1_Risultato,
                                   ends_with ("DP1")))) %>% 
  dplyr::select(-c("INFCODinSOSPETTEICA","perRERICOVERO", "SDO1_Polo")) %>% # in particolare le prime tre perchè sono vuote
  clean_names()

db_select <- db_select %>%  # queste le tolgo perchè non ho idea di cosa si tratti o mi sembrano inutili per l'analisi
  dplyr::select(-c("sdo1_prog_sdo", 
            "sdo1_com_nas",
            "sdo1_dat_nas",
            "sdo1_com_res",
            "sdo1_cir_rom",
            "sdo1_cod_irc",
            "sdo1_pro_ric",
            #"sdo1_uor", # Mi chiedo se sia una variabile importante perchè magari le maggiori infezioni sono nelle UOR dove i DRG sono più alti, quindi se la UOR influenza i costi, e quelle più "care" sono anche a rischio infezione è un confounder
            #"sdo1_dat_ric", # è la data del ricovero, potrebbe essere utile se ci sono differenze nei DRG a seconda del periodo dell'anno o dei giorni della settimana
            "sdo1_invio",
            "sdo1_irc_pro",
            "sdo1_regime",
            "sdo1_tipo",
            #"sdo1_modali", #modalità di ammissione, l'ho inserita anche in db_select
            "sdo1_trauma",
            "sdo1_onere",
            "sdo1_uo_tra1",
            "sdo1_dat_tr1",           
            "sdo1_uo_tra2",
            "sdo1_dat_tr2",
            "sdo1_uo_tra3",
            "sdo1_dat_tr3",
            "sdo1_uod",
            "sdo1_dat_dim",
            #"sdo1_tip_dim", #tipo di dimissione, l'ho inserita anche in db_select
            "sdo1_irc_tra",
            "sdo1_ris_aut",
            #"sdo1_int_pri",  #intervento primario
            #"sdo1_dat_in_p", #data intervento primario
            #"sdo1_int_se1",  #intervento secondario 1
            #"sdo1_d_in_se1", #data intervento secondario 1
            #"sdo1_int_se2", #intervento secondario 2           
            #"sdo1_d_in_se2", #data intervento secondario 2
            #"sdo1_int_se3", #intervento secondario 3
            #"sdo1_d_in_se3", #data intervento secondario 3
            #"sdo1_int_se4", #intervento secondario 4
            #"sdo1_d_in_se4", #data intervento secondario 4         
            #"sdo1_int_se5", #intervento secondario 5
            #"sdo1_d_in_se5", #data intervento secondario 5
            "sdo1_acce_dh",
            "sdo1_d_pren",
            "sdo1_class_prio",
            "sdo1_drgv24",
            "sdo1_mdcv24",            
            "sdo1_err_cod",
            "sdo1_tip_tar",
            "sdo1_dat_pro",           
            "sdo1_deg_ann",
            "sdo1_drgv19",
            "sdo1_mdcv19",
            "sdo1_sconosciuti",
            "reparti_medici",
            "reparti_chirurgici",
            "sesso",
            "sdo1_num_sch"            
            ))
