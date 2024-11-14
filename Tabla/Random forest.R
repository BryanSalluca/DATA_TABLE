# Cargar librerías necesarias
library(caret)
library(randomForest)
library(pROC)

# Crear un dataset sintético
set.seed(42)
n_samples <- 999
n_features <- 4
X <- as.data.frame(matrix(rnorm(n_samples * n_features), ncol = n_features))
names(X) <- c("var_37", "var_3389", "var_12", "Additional")
X$Female <- factor(sample(c("Male", "Female"), n_samples, replace = TRUE))
X$Class <- factor(sample(c(0, 1), n_samples, replace = TRUE))

# Dividir los datos en conjuntos de entrenamiento y prueba
trainIndex <- createDataPartition(X$Class, p = 0.7, list = FALSE)
trainData <- X[trainIndex,]
testData <- X[-trainIndex,]

# Entrenar el modelo Random Forest
rf_model <- randomForest(Class ~ ., data = trainData)
rf_probs <- as.numeric(predict(rf_model, testData, type = "prob")[,2])

# Calcular AUC y curva ROC para Random Forest
rf_roc <- roc(as.numeric(testData$Class), rf_probs)
rf_auc <- auc(rf_roc)
print(paste("Random Forest AUC:", rf_auc))

# Graficar la curva ROC para Random Forest
plot(rf_roc, col = "blue", main = "Curva ROC para Random Forest", lwd = 2)
legend("bottomright", legend = paste("Random Forest (AUC =", round(rf_auc, 2), ")"), 
       col = "blue", lwd = 2)
