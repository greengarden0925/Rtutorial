install.packages("smotefamily")
install.packages("caret")
install.packages("nnet")  # 用於多類別邏輯回歸模型
library(smotefamily)
library(caret)
library(nnet)

set.seed(123)

# 模擬 case 組（111 個樣本）
n_case <- 111
case_data <- data.frame(
  SNP1 = sample(0:2, n_case, replace = TRUE),
  SNP2 = sample(0:2, n_case, replace = TRUE),
  SNP3 = sample(0:2, n_case, replace = TRUE),
  SNP4 = sample(0:2, n_case, replace = TRUE),
  SNP5 = sample(0:2, n_case, replace = TRUE),
  age = rnorm(n_case, mean = 50, sd = 10),
  sex = sample(0:1, n_case, replace = TRUE),
  flow1 = rnorm(n_case),
  flow2 = rnorm(n_case),
  flow3 = rnorm(n_case),
  flow4 = rnorm(n_case),
  flow5 = rnorm(n_case),
  class = factor(rep("case", n_case))
)

# 模擬 control 組（10 個樣本）
n_control <- 10
control_data <- data.frame(
  SNP1 = sample(0:2, n_control, replace = TRUE),
  SNP2 = sample(0:2, n_control, replace = TRUE),
  SNP3 = sample(0:2, n_control, replace = TRUE),
  SNP4 = sample(0:2, n_control, replace = TRUE),
  SNP5 = sample(0:2, n_control, replace = TRUE),
  age = rnorm(n_control, mean = 50, sd = 10),
  sex = sample(0:1, n_control, replace = TRUE),
  flow1 = rnorm(n_control),
  flow2 = rnorm(n_control),
  flow3 = rnorm(n_control),
  flow4 = rnorm(n_control),
  flow5 = rnorm(n_control),
  class = factor(rep("control", n_control))
)

# 合併資料集
data <- rbind(case_data, control_data)



# 分離特徵與目標變數
X <- data[, -ncol(data)]  # 特徵
y <- data$class           # 目標變數

# 應用 SMOTE
smote_result <- SMOTE(X, y, K = 5, dup_size = 0)

# 取得過採樣後的資料集
data_smote <- smote_result$data
table(data_smote$class)
colnames(data_smote)[ncol(data_smote)] <- "class"
data_smote$class <- as.factor(data_smote$class)

#-------------------------------
# 分割資料集為訓練集和測試集
set.seed(456)
train_index <- createDataPartition(data_smote$class, p = 0.8, list = FALSE)
train_data <- data_smote[train_index, ]
test_data <- data_smote[-train_index, ]

# 訓練邏輯回歸模型
model <- train(class ~ ., data = train_data, method = "multinom")

# 預測測試集
predictions <- predict(model, newdata = test_data)

# 評估模型表現
confusionMatrix(predictions, test_data$class)
model
summary(model)



# 安裝並載入套件
install.packages("randomForest")   # caret 會自動載入，但仍建議明確安裝
install.packages("caret")
library(caret)
library(randomForest)

# 建立模型
set.seed(123)  # 為了重現性
model_rf <- train(
  class ~ ., 
  data = train_data, 
  method = "rf", 
  trControl = trainControl(method = "cv", number = 5),  # 5-fold cross validation
  importance = TRUE  # 計算變數重要性
)


# 預測測試集
predictions <- predict(model_rf, newdata = test_data)

# 評估模型表現
confusionMatrix(predictions, test_data$class)
model_rf
summary(model_rf)

# 假設您已建立好的模型為 model_rf
importance <- varImp(model_rf)
print(importance)

# caret 內建繪圖方法
plot(importance, top = 20)  # top = 顯示前幾個重要變數


library(ggplot2)

# 將變數重要性資料轉為 data frame 並排序
imp_df <- importance$importance
imp_df$Overall <- rowMeans(imp_df) 
imp_df$Variable <- rownames(imp_df)
imp_df <- imp_df[order(-imp_df$Overall), ]

# 畫圖
ggplot(imp_df[1:20, ], aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Variable Importance (Random Forest)",
       x = "Variables", y = "Importance") +
  theme_minimal()



install.packages("pROC")
library(pROC)

# 取得 class 為 "case" 的預測機率（正類）
pred_prob <- predict(model_rf, newdata = test_data, type = "prob")
head(pred_prob)

# 指定 "case" 為陽性類別（可依你的標籤改變）
roc_obj <- roc(response = test_data$class, predictor = pred_prob$case)

plot(roc_obj, print.auc = TRUE, col = "blue", lwd = 2,
     main = "ROC Curve for Random Forest Model")
