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


super_corr <- function(df) {
  
  # function to get chi square p value and Cramers V
  f = function(x,y) {
    tbl = df %>% dplyr::select(x,y) %>% table()
    chisq_pval = round(chisq.test(tbl)$p.value, 4)
    cramV = round(cramersV(tbl), 2) 
    data.frame(x, y, chisq_pval, cramV) }
  
  # create unique combinations of column names
  # sorting will help getting a better plot (upper triangular)
  df_comb = data.frame(t(combn(sort(names(df)), 2)), stringsAsFactors = F)
  
  # apply function to each variable combination
  df_res = map2_df(df_comb$X1, df_comb$X2, f)
  
  # plot results
  df_res %>%
    ggplot(aes(x,y,fill=chisq_pval))+
    geom_tile()+
    geom_text(aes(x,y,label=cramV), size = 3)+
    scale_fill_gradient(low="red", high="yellow")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 6),
          axis.text.y = element_text(angle = 45, size = 6),
          axis.title = element_blank(),
          legend.position = "none") 
  
}

super_corr(db_corr)

# Load the necessary library
library(ggplot2)

# Create a boxplot for age
ggplot(db, aes(x = "", y = sdo1_eta)) +
  geom_boxplot() +
  theme(axis.title.x=element_blank()) +
  ggtitle("Boxplot of Age")

# Create a boxplot for length of stay
ggplot(db, aes(x = "", y = degenza)) +
  geom_boxplot() +
  theme(axis.title.x=element_blank()) +
  ggtitle("Boxplot of Length of Stay")

# Create a histogram for age
ggplot(db, aes(x = sdo1_eta)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  ggtitle("Histogram of Age")

# Create a histogram for length of stay
ggplot(db, aes(x = degenza)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  ggtitle("Histogram of Length of Stay")

lunghi <- db %>% 
  filter(degenza > 90)
summary(db$degenza)

frq(db$proc_inv)
frq(db$proc_inv_real)
