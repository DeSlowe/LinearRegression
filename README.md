---
title: "Regressione Lineare - Caso Studio"
author: "Giovanni Del Piano"
date: "2025-03-07"
output:
  html_document:
    toc: true
    toc_float: true
    toc_depth: 3
    df_print: paged
    css: custom.css
  pdf_document:
    toc: true
    toc_depth: '3'
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  eval = TRUE,    # Ensure all chunks execute
  echo = TRUE,    # Show the code in the output
  warning = FALSE, # Suppress warnings
  message = FALSE  # Suppress messages
)
```

# **La Regressione Lineare**

## **Introduzione**

La regressione lineare è un metodo statistico utilizzato per modellare la relazione tra una variabile dipendente \( y \) e una o più variabili indipendenti \( x \). L'equazione della regressione lineare semplice è:

$$
y = \beta_0 + \beta_1 x + \varepsilon
$$

dove:  
- \( y \) è la variabile dipendente (output o risposta).  
- \( x \) è la variabile indipendente (predittore o input).  
- \( \beta_0 \) è l'intercetta, cioè il valore di \( y \) quando \( x = 0 \).  
- \( \beta_1 \) è il coefficiente di regressione, che rappresenta la variazione attesa di \( y \) per un'unità di incremento in \( x \).  
- \( \varepsilon \) è l'errore residuo, che rappresenta la parte di \( y \) non spiegata dal modello.

## **Ipotesi Classiche del Modello di Regressione Lineare**

Affinché un modello di **regressione lineare** sia valido e fornisca inferenze affidabili, deve rispettare alcune ipotesi fondamentali. Queste ipotesi sono alla base del modello statistico e dell'interpretazione dei risultati.

#### **1. Linearità**
La relazione tra la variabile dipendente \( y \) e le variabili indipendenti \( x_1, x_2, ..., x_n \) deve essere **lineare**. Questo significa che il modello assume la forma:

\[
y = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + ... + \beta_n x_n + \varepsilon
\]

Se la relazione non è lineare, il modello può essere inadeguato e potrebbero essere necessarie trasformazioni delle variabili.

---

#### **2. Indipendenza degli Errori (Assenza di Autocorrelazione)**
Gli errori (\(\varepsilon\)) devono essere **indipendenti tra loro**. In altre parole, non devono esserci correlazioni sistematiche tra gli errori associati a diverse osservazioni.

- Se gli errori sono correlati (ad esempio in dati temporali o spaziali), si parla di **autocorrelazione**, che può portare a stime distorte della significatività dei coefficienti.

---

#### **3. Normalità degli Errori**
Gli errori devono seguire una **distribuzione normale** con media zero:

\[
\varepsilon \sim N(0, \sigma^2)
\]

Questa ipotesi è importante per l'inferenza statistica, in particolare per i test di significatività dei coefficienti (\( p \)-value) e la costruzione di intervalli di confidenza.

- Se questa ipotesi non è rispettata, il test \( t \) e il test \( F \) potrebbero non essere validi.
- Si può verificare con un **QQ-plot** o un test di normalità come il **test di Shapiro-Wilk**.

---

#### **4. Omogeneità della Varianza degli Errori (Omoschedasticità)**
La varianza degli errori deve essere **costante** lungo tutti i valori della variabile indipendente:

\[
Var(\varepsilon_i) = \sigma^2, \quad \forall i
\]

- Se la varianza cambia con i valori di \( x \) (fenomeno chiamato **eteroschedasticità**), le stime dei coefficienti saranno ancora corrette, ma i loro **errori standard saranno distorti**, rendendo i test di significatività inaffidabili.
- L'eteroschedasticità si può rilevare con un **grafico dei residui**.

---

#### **5. Assenza di Collinearità Perfetta (per Modelli Multivariati)**
Nella **regressione multipla**, le variabili indipendenti non devono essere **fortemente correlate tra loro** (multicollinearità).

- Se due o più variabili sono altamente correlate, i coefficienti di regressione possono diventare instabili e difficili da interpretare.
- Per rilevare la collinearità, si può usare il **Variance Inflation Factor (VIF)**.

---

#### **Conclusioni**
Queste ipotesi sono essenziali per garantire che la regressione lineare fornisca risultati affidabili. Se una o più ipotesi non sono rispettate, potrebbero essere necessarie tecniche correttive come:


# **Dati**

Questo dataset contiene informazioni su 5000 proprietà, rendendolo una risorsa completa per esplorare le tendenze del mercato immobiliare e costruire modelli predittivi per i prezzi delle case. I dati includono diverse caratteristiche che catturano i dettagli della proprietà, la posizione e le condizioni di mercato, offrendo ampie opportunità per l'esplorazione dei dati, la visualizzazione e le applicazioni di machine learning.

### **Descrizione delle Variabili**  

#### Dettagli del Prezzo

- **price**: Prezzo di vendita della casa.  

#### Caratteristiche della Proprietà

- **bedrooms**: Numero di camere da letto.  
- **bathrooms**: Numero di bagni.  
- **sqft_living**: Superficie abitabile in piedi quadrati.  
- **sqft_lot**: Dimensione del lotto in piedi quadrati.  
- **floors**: Numero di piani.  
- **waterfront**: Indica se la proprietà ha una vista sul lungomare.  
- **view**: Valutazione della qualità della vista.  
- **condition**: Condizione generale della casa.  
- **grade**: Grado di costruzione e design (scala da 1 a 13).  

#### Metriche Aggiuntive

- **sqft_above**: Superficie della proprietà sopra il livello del suolo.  
- **sqft_basement**: Superficie del seminterrato in piedi quadrati.  
- **yr_built**: Anno di costruzione della proprietà.  
- **yr_renovated**: Anno dell'ultima ristrutturazione.  

#### Confronti con le Proprietà Vicine

- **sqft_living15**: Superficie abitabile media delle 15 proprietà più vicine.  
- **sqft_lot15**: Dimensione media del lotto delle 15 proprietà più vicine.


### **Data Exploration**

```{r}
library(tidyverse)
library(DescTools)
library(GGally)
library(caret)
library(kableExtra)
library(pander)
library(skimr)
library(ISLR)
library(MASS)
```


```{r}
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
```
Plot Distribution per variabili quantitative
```{r}
for (col in colnames(df)) {
  if (is.numeric(df[[col]])) {
    PlotFdist(df[[col]], main = col, xlab = col)  
  }
}
```

**Key Points della Visualizzazione:**

- Alcune variabili (es. bedrooms e bathrooms) per quanto numeriche non possono essere considerate tali, vanno quindi trasformate, rendendole categoriche

```{r}
df <- df %>% 
  mutate(
    across(c(bedrooms, bathrooms, floors, view, grade), as.factor),
    yr_renovated = as.factor(case_when(
      yr_renovated == 0 ~ "0",
      TRUE ~ "1"
    )))
  
```

Pair Plot delle variabili quantitative
```{r}
# Pair Plot for numerical variables
df %>% select_if(is.numeric) %>% ggpairs()
```

- Il risultato del Pair Plot mostra che la correlazione più forte esiste tra  Price e sqft_living, nel seguente grafico si mostra un dettaglio della relazione lineare esistente tra le due variabili
```{r}
ggplot(df, aes(x = price, y = sqft_living)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Price ~ Sq Feet", x = "Rating", y = "Price")
```

# **Regressione Lineare Semplice**

## **Model Training**
 **1) Dividere il dataset in train set e test set**

```{r}
set.seed(1234)
train_index <- createDataPartition(df$price, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]
```
 2) Stimare il modello sul train set
```{r}
# Apply linear regression model
model <-  lm(price ~ sqft_living, data = train_data)
summary(model)
```

## **Interpretazione dell'Intercetta e Trasformazione della Variabile Price**

Quando applichiamo un modello di **regressione lineare** al dataset **Credit**, potremmo ottenere un valore negativo per l'intercetta (\(\beta_0\)). Questo potrebbe sembrare strano, perché il prezzo delle case (`Price`) è sempre positivo nella realtà.

#### Perché otteniamo un'intercetta negativa?
L'intercetta rappresenta il valore previsto della variabile dipendente (`Price`) quando tutte le variabili indipendenti sono uguali a zero. In alcuni casi, questo non ha molto senso pratico, portando ad un’intercetta negativa che non ha un'interpretazione concreta.

#### Problema della Distribuzione di `Price`
Un altro motivo per cui il modello può comportarsi in modo anomalo è che la variabile `Price` è **strettamente positiva** e può avere una distribuzione asimmetrica. La regressione lineare funziona quando la variabile dipendente è distribuita più simmetricamente e su tutto l'asse dei numeri reali, compresi i valori negativi.
In sintesi, stiamo violando l'ipotesi di normalità 

#### Soluzione: Trasformazione Logaritmica o Traslazione
Per migliorare l’adattamento del modello, possiamo applicare una **trasformazione** a `Price`, ad esempio:

1. **Trasformazione logaritmica**  
   - Usare il **logaritmo naturale** può ridurre l'asimmetria e rendere la relazione più lineare:
   - \[
   Price' = \log(Price + 1)
   \]
   - Aggiungiamo 1 per evitare problemi con i valori pari a zero.

2. **Traslazione e trasformazione simmetrica**  
   - Possiamo trasformare `Price` sottraendo la media e dividendo per la deviazione standard (standardizzazione):
   - \[
   Price' = \frac{Price - \text{media}(Price)}{\text{deviazione standard}(Price)}
   \]
   - Questo rende `Price` distribuito intorno allo zero.



```{r}
# Trasformazione logaritmica delle delle variabili
df_log = df %>% mutate(across(is.numeric,log1p)) 

# Residuals Histogram
PlotFdist(df_log$price, main='log_price')
df_log %>% select_if(is.numeric) %>% ggpairs()

```
```{r}
train_index <- createDataPartition(df_log$price, p = 0.8, list = FALSE)
train_data <- df_log[train_index, ]
test_data <- df_log[-train_index, ]

# Apply linear regression model
model <-  lm(price ~ sqft_living, data = train_data)
summary(model)
```

#### **Vantaggi della Trasformazione**
- Migliora l’adattamento del modello ai dati.
- Rende più interpretabili i coefficienti della regressione.
- Evita problemi legati all’interpretazione dell’intercetta.

In conclusione, se notiamo un’intercetta negativa che non ha senso pratico, potremmo dover controllare la distribuzione della nostra variabile dipendente e applicare una trasformazione appropriata.


 **3) Applicare il modello sul test set**
```{r}
# Generiamo le previsioni sui dati originali
predictions <- exp(predict(model, newdata = test_data, type = "response"))
         # Trasformazione inversa
```

 **4) Valutare la bontà di adattamento del modello attraverso la statistica** $R^2$

  $R^2 = 1 - \frac{\sum (y_i - \hat{y}_i)^2}{\sum (y_i - \bar{y})^2}$

```{r}
# Evaluate the model using R-squared
actual_values <- exp(test_data$price)  
rss <- sum((actual_values - predictions)^2)  
tss <- sum((actual_values - mean(actual_values))^2)  
rsquared <- 1 - (rss / tss)

print(paste("R2:", round(rsquared, 2)))
```
## **Residual Analysis**

```{r}
# Make a DataFrame
residuals_df <- data.frame(
  predictions = predictions,
  actual_values = actual_values,
  residuals = actual_values - predictions
)

```
#### Plot Residual vs Fitted Scatterplot

Lo scopo del seguente grafico è assicurarsi che non ci sia un trend sistematico nei dati per assicurare omoschedasticità dei residui e indipendenza tra errori e predizioni

```{r}
# Residual vs Fitted
ggplot(residuals_df, aes(x = predictions, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values", y = "Residuals") +
  theme_minimal()
```

Il grafico mostra che i residui sono piuttosto centrati, tuttavia la presenza di outlier potrebbe inficiare le performance del modello. Nel prossimo grafico infatti si osservano delle code molto pesanti


```{r}
# Residuals Histogram
PlotFdist(residuals_df$residuals, main='Residuals')
```

## **Outlier's Detection**

Per individuare e rimuovere gli **outlier** in R, possiamo utilizzare diverse tecniche statistiche, tra cui la **Cook's Distance**, che misura l'influenza di un singolo punto sui parametri stimati del modello di regressione. Punti con un valore elevato di Cook's Distance sono considerati outlier influenti.

```{r}
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

```


```{r}
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

```

## **Conclusioni**
Dall' interpretazione dei residui emerge che esistono ancora informazioni sistematiche nei dati che non riusciamo a cogliere con la sola variabile 'sqft_living'. E' necessario quindi optare per l'inclusione di altre variabili. Vediamo come.


# **Regressione Lineare Multipla**

```{r}
factor_vars = df %>%
  select_if(is.factor) %>% names

# Reshape data: Gather all categorical variables into one column
df_long_cat <-  df %>%
  pivot_longer(cols = all_of(factor_vars), names_to = "Variable", values_to = "Category")
factor_vars
```

```{r}
# Create the boxplot
ggplot(df_long_cat, aes(x = Category, y = price, fill = Category)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_x")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
```
```{r}
set.seed(1234)
train_index <- createDataPartition(df_log$price, p = 0.8, list = FALSE)
train_data <- df_log[train_index, ]
test_data <- df_log[-train_index, ]

# Apply linear regression model
model <-  lm(price ~ ., data = train_data)
summary(model)
```

## **Introduzione al Concetto di Backward e Forward Modelling in R**
#### **1. Cos'è il Backward e Forward Modelling?**

Il Backward Modelling e il Forward Modelling sono due approcci di selezione automatica delle variabili per costruire modelli predittivi più efficienti. Entrambi i metodi cercano di selezionare le variabili indipendenti più significative, rimuovendo quelle che non contribuiscono in modo rilevante alla previsione del modello.

- **Backward Modelling** (eliminazione progressiva) parte con un modello completo e rimuove le variabili meno significative, una alla volta.

- **Forward Modelling** (selezione progressiva) parte con un modello vuoto e aggiunge le variabili più significative, una alla volta.

#### **2. Quando si applica e perché**
Questi metodi si applicano quando vogliamo costruire un modello di regressione con un numero significativo di variabili predittive, ma non siamo sicuri di quali siano le più rilevanti. La selezione automatica delle variabili aiuta a:

**Semplificare il modello**: rimuovendo variabili irrilevanti, rendendo il modello più interpretabile e facile da comprendere.
**Prevenire l'overfitting**: eliminando variabili che potrebbero far adattare il modello troppo ai dati di addestramento, migliorando così la generalizzazione.
**Ottimizzare il modello**: selezionando solo le variabili che migliorano la performance del modello.
Entrambi i metodi sono utili quando abbiamo a disposizione un grande numero di variabili, ma non vogliamo costruire un modello che sia troppo complesso e rischioso.

#### **3. Come funziona**

**Backward Modelling** (Eliminazione progressiva)

Il processo di Backward Modelling parte da un modello che include tutte le variabili predittive disponibili. Successivamente, il modello esamina ciascuna variabile per determinare se può essere eliminata senza compromettere significativamente la qualità del modello. In genere, la variabile che viene rimossa è quella che ha il valore di p-value più alto, indicante una scarsa significatività statistica.

Il processo continua finché tutte le variabili rimanenti sono significative (ovvero, i loro p-value sono sotto una certa soglia, di solito 0.05).

**Forward Modelling** (Selezione progressiva)

Il Forward Modelling, al contrario, inizia con un modello vuoto, che include solo l'intercetta. Successivamente, aggiunge le variabili una alla volta, selezionando quella che migliora maggiormente la bontà del modello, solitamente misurata tramite un criterio come l'AIC (Akaike Information Criterion). Il processo continua finché l'aggiunta di nuove variabili non porta a un miglioramento significativo.



```{r}
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
```


```{r}

formula <- price ~ grade  + view  + condition + 
    sqft_lot15 + sqft_living15 + yr_renovated + sqft_basement + 
     waterfront + sqft_above

model <- lm(formula, data=train_data)
summary(model)
```


```{r}
plot(model)
```



```{r}
# Generiamo le previsioni sui dati originali
predictions <- exp(predict(model, newdata = test_data, type = "response"))
# Evaluate the model using R-squared
actual_values <- exp(test_data$price)  
rss <- sum((actual_values - predictions)^2)  
tss <- sum((actual_values - mean(actual_values))^2)  
rsquared <- 1 - (rss / tss)

print(paste("R2:", round(rsquared, 2) ))
```


```{r}
# Make a DataFrame
residuals_df <- data.frame(
  predictions = predictions,
  actual_values = actual_values,
  residuals = actual_values - predictions
)

```

```{r}
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

```

# **Conclusioni**
**Qualità della casa (grade)** è un fattore cruciale per determinare il prezzo. Le case con un rating più alto hanno un prezzo molto più alto.

**Le caratteristiche della vista (view)** e la vicinanza al waterfront (waterfrontY) sono anche determinanti significativi per aumentare il prezzo.

**La superficie abitabile (sqft_living)** ha un effetto positivo sul prezzo, e più spazio tende a significare un prezzo più elevato.

**Il numero di bagni** è un altro importante indicatore: più bagni comportano un prezzo più elevato, con le case con più bagni che hanno un valore significativamente maggiore.

**Le condizioni della casa (condition)** sono importanti, con case in cattive condizioni che tendono ad avere un prezzo inferiore rispetto a quelle in ottime condizioni.

**La presenza di un seminterrato (sqft_basement)** ha un effetto positivo ma minore, mentre la superficie del lotto **sqft_lot15** sembra avere un effetto meno significativo.
