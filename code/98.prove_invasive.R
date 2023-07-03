# Creazione della tabella di contingenza
tabella <- table(db_regr$proc_inv_real, db_regr$infetto)

# Calcolo dell'Odds Ratio
or <- oddsratio(tabella)

# Stampa dell'Odds Ratio
print(or$measure)


# Install the ResourceSelection package if not already installed
install.packages('ResourceSelection')

# Load the package
library(ResourceSelection)

# Assuming your model is called 'model'
# And your data frame is called 'df'
# Here 'response' is your binary outcome variable and 'predictors' are your predictor variables
model <- glm(response ~ predictors, data = df, family = binomial())

# Calculate the Hosmer-Lemeshow test
hoslem_test <- hoslem.test(model_multi$y, fitted(model_multi))

# Print the results
print(hoslem_test)
