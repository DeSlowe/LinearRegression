## ----setup, include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  eval = TRUE,    # Ensure all chunks execute
  echo = TRUE,    # Show the code in the output
  warning = FALSE, # Suppress warnings
  message = FALSE  # Suppress messages
)

## ------------------------------------------------------------------------------------

# https://rpubs.com/DelPiano/1282648

## ------------------------------------------------------------------------------------
library(tidyverse)
library(DescTools)
library(GGally)
library(caret)
library(kableExtra)
library(pander)
library(skimr)
library(ISLR)
library(MASS)


## ------------------------------------------------------------------------------------
# Set the working directory
setwd("C:\\Users\\Windows 10\\OneDrive\\Desktop\\LUISS LESSON\\02 script\\Lesson 3 Linear Regression")
# Import the dataset
df = read.csv("sampled_house_prices.csv", header=T, sep=';')

# df = Credit

str(df)
# Mutate character variables into categorical ones
df = df %>% mutate(across(where(is.character), as.factor))
# Show data
df %>%
  slice(1:10) %>% 
  kbl() %>%
  kable_styling()


## ------------------------------------------------------------------------------------
for (col in colnames(df)) {
  if (is.numeric(df[[col]])) {
    PlotFdist(df[[col]], main = col, xlab = col)  
  }
}


## ------------------------------------------------------------------------------------
df <- df %>% 
  mutate(
    across(c(bedrooms, bathrooms, floors, view, grade), as.factor),
    yr_renovated = as.factor(case_when(
      yr_renovated == 0 ~ "0",
      TRUE ~ "1"
    )))
  


## ------------------------------------------------------------------------------------
# Pair Plot for numerical variables
df %>% select_if(is.numeric) %>% ggpairs()


## ------------------------------------------------------------------------------------
ggplot(df, aes(x = price, y = sqft_living)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Price ~ Sq Feet", x = "Rating", y = "Price")


## ------------------------------------------------------------------------------------
set.seed(1234)
train_index <- createDataPartition(df$price, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]


## ------------------------------------------------------------------------------------
# Apply linear regression model
model <-  lm(price ~ sqft_living, data = train_data)
summary(model)


## ------------------------------------------------------------------------------------
# Trasformazione logaritmica delle delle variabili
df_log = df %>% mutate(across(is.numeric,log1p)) 

# Residuals Histogram
PlotFdist(df_log$price, main='log_price')
df_log %>% select_if(is.numeric) %>% ggpairs()


## ------------------------------------------------------------------------------------
train_index <- createDataPartition(df_log$price, p = 0.8, list = FALSE)
train_data <- df_log[train_index, ]
test_data <- df_log[-train_index, ]

# Apply linear regression model
model <-  lm(price ~ sqft_living, data = train_data)
summary(model)


## ------------------------------------------------------------------------------------
# Generiamo le previsioni sui dati originali
predictions <- exp(predict(model, newdata = test_data, type = "response"))
         # Trasformazione inversa


## ------------------------------------------------------------------------------------
# Evaluate the model using R-squared
actual_values <- exp(test_data$price)  
rss <- sum((actual_values - predictions)^2)  
tss <- sum((actual_values - mean(actual_values))^2)  
rsquared <- 1 - (rss / tss)

print(paste("R2:", round(rsquared, 2)))


## ------------------------------------------------------------------------------------
# Make a DataFrame
residuals_df <- data.frame(
  predictions = predictions,
  actual_values = actual_values,
  residuals = actual_values - predictions
)



## ------------------------------------------------------------------------------------
# Residual vs Fitted
ggplot(residuals_df, aes(x = predictions, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values", y = "Residuals") +
  theme_minimal()


## ------------------------------------------------------------------------------------
# Residuals Histogram
PlotFdist(residuals_df$residuals, main='Residuals')


## ------------------------------------------------------------------------------------
cooksd <- cooks.distance(model)

# Plot della Cook's Distance
plot(cooksd, pch="*", cex=2, main="Influential Observations by Cook's Distance", 
     xlab = "Osservazioni", ylab = "Cook's Distance")

# Aggiungi una linea di cutoff (4/n)
abline(h = 4 / length(cooksd), col="red")

# Aggiungi etichette rosse per gli outlier (Cook's distance > 4 * media)
text(x = 1:length(cooksd), y = cooksd, 
     labels = ifelse(cooksd > 4 * mean(cooksd, na.rm = TRUE), names(cooksd), ""), 
     col = "red", pos = 3)



## ------------------------------------------------------------------------------------
# Calcolare la Cook's Distance
cooksd <- cooks.distance(model)

# Identificare gli outlier (valori di Cook's Distance > 4/n)
threshold <- 4 / length(cooksd)
outliers_cooks <- which(cooksd > threshold)

# Rimuovere gli outlier dal dataset
df_clean_cooks <- df_log[-outliers_cooks, ]

# Visualizzare il nuovo modello senza gli outlier
model_clean_cooks <- lm(price ~ sqft_living, data = df_clean_cooks)
plot(model_clean_cooks)



## ------------------------------------------------------------------------------------
factor_vars = df %>%
  select_if(is.factor) %>% names

# Reshape data: Gather all categorical variables into one column
df_long_cat <-  df %>%
  pivot_longer(cols = all_of(factor_vars), names_to = "Variable", values_to = "Category")
factor_vars


## ------------------------------------------------------------------------------------
# Create the boxplot
ggplot(df_long_cat, aes(x = Category, y = price, fill = Category)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_x")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

## ------------------------------------------------------------------------------------
set.seed(1234)
train_index <- createDataPartition(df_log$price, p = 0.8, list = FALSE)
train_data <- df_log[train_index, ]
test_data <- df_log[-train_index, ]

# Apply linear regression model
model <-  lm(price ~ ., data = train_data)
summary(model)


## ------------------------------------------------------------------------------------
# Fit del modello
initial_model <- lm(price ~ ., data = train_data)
 
# Forward selection (starting with an empty model)
forward_model <- step(lm(price ~ 1, data = train_data), 
                      scope = formula(initial_model), 
                      direction = "forward", trace=0)
# Estrarre la matrice dei coefficienti
coeffs <- summary(model)$coefficients

# Filtrare solo i coefficienti con p-value ≤ 0.05
significant_coeffs <- coeffs[coeffs[, "Pr(>|t|)"] <= 0.05, ]

# Visualizzare i coefficienti significativi
significant_coeffs


## ------------------------------------------------------------------------------------

formula <- price ~ grade  + view  + condition + 
    sqft_lot15 + sqft_living15 + yr_renovated + sqft_basement + 
     waterfront + sqft_above

model <- lm(formula, data=train_data)
summary(model)


## ------------------------------------------------------------------------------------
plot(model)


## ------------------------------------------------------------------------------------
# Generiamo le previsioni sui dati originali
predictions <- exp(predict(model, newdata = test_data, type = "response"))
# Evaluate the model using R-squared
actual_values <- exp(test_data$price)  
rss <- sum((actual_values - predictions)^2)  
tss <- sum((actual_values - mean(actual_values))^2)  
rsquared <- 1 - (rss / tss)

print(paste("R2:", round(rsquared, 2) ))


## ------------------------------------------------------------------------------------
# Make a DataFrame
residuals_df <- data.frame(
  predictions = predictions,
  actual_values = actual_values,
  residuals = actual_values - predictions
)



## ------------------------------------------------------------------------------------
# Creare un data frame in formato long per ggplot
df_long <- data.frame(
  value = c(residuals_df$predictions, residuals_df$actual_values),
  type = rep(c("Predictions", "Actual Values"), each = length(predictions))
)

# Creare il grafico con ggplot
ggplot(df_long, aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +  # Densità con trasparenza
  scale_fill_manual(values = c("Residuals" = "green", "Predictions" = "orange", "Actual Values" = "blue")) +  # Colori per le densità
  labs(title = "Valori Osservati e Predetti", 
       x = "Valori", y = "Densità") +
  theme_minimal() +  # Tema minimal
  theme(legend.title = element_blank())  # Rimuovere il titolo della legenda


