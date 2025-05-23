# 安裝所需套件（如尚未安裝）
# install.packages("ggplot2")
# install.packages("dplyr")

# 載入套件
library(ggplot2)
library(dplyr)

# 建立模擬資料
set.seed(123)  # 設定隨機種子以利重現

n <- 150  # 每個 timepoint 的樣本數
data <- data.frame(
  antiRo = c(rnorm(n, mean = 30, sd = 5),  # before
             rnorm(n, mean = 35, sd = 5)), # after
  timepoint = rep(c("before", "after"), each = n),
  genotype = sample(0:2, 2 * n, replace = TRUE)
)
str(data)

# 繪製 grouped violin plot（X 軸為 genotype，分組為 timepoint）
ggplot(data, aes(x = factor(genotype), y = antiRo, fill = timepoint)) +
  geom_violin(position = position_dodge(0.8), width = 0.7, alpha = 0.7) +
  stat_summary(fun = median, geom = "point",
               position = position_dodge(0.8), size = 2.5, color = "black") +
  labs(
    title = "Grouped Violin Plot of antiRo by Genotype and Timepoint",
    x = "Genotype",
    y = "antiRo",
    fill = "Timepoint"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")
