# per farmi una idea di quello che ho

Hmisc::describe(db)
psych::describe(db$costo)

# Create Report with Data Explorer
config <- configure_report(
  add_plot_prcomp = FALSE) # tolgo la principal component analysis

create_report(db, y = "costo", config = config)

frq(db_select$sdo1_cittad)
