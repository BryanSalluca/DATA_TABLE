# Cargar librerías necesarias
library(caret)
library(rpart)
library(pROC)

# Entrenar el modelo Árbol de Decisión
dt_model <- rpart(Class ~ ., data = trainData)
dt_probs <- as.numeric(predict(dt_model, testData, type = "prob")[,2])

# Calcular AUC y curva ROC para Árbol de Decisión
dt_roc <- roc(as.numeric(testData$Class), dt_probs)
dt_auc <- auc(dt_roc)
print(paste("Decision Tree AUC:", dt_auc))

# Graficar la curva ROC para Árbol de Decisión
plot(dt_roc, col = "green", main = "Curva ROC para árbol de decisión", lwd = 2)
legend("bottomright", legend = paste("Árbol de decisión (AUC =", round(dt_auc, 2), ")"), 
       col = "green", lwd = 2)
