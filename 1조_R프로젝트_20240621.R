# 데이터 로드
data <- read.csv("산불면적예측데이터(수정).csv", header = TRUE, fileEncoding = "CP949")

# 데이터 표준화
data_scaled <- as.data.frame(scale(data))

# 로버스트 회귀 모델 학습
library(MASS)
robust_model <- rlm(면적 ~ ., data = data_scaled)

# 모델 요약
summary(robust_model)

# 잔차 분석
robust_residuals <- residuals(robust_model)
sd_res <- sd(robust_residuals, na.rm = TRUE)
sd_res

# 정규성 시각화
hist(robust_residuals, main = "Histogram of Robust Residuals")
qqnorm(robust_residuals)
qqline(robust_residuals)

shapiro.test(robust_residuals)  # 정규성 검정
acf(robust_residuals)  # 독립성 확인, 0을 제외하고 파란선만 안넘기면 됨

# 등분산성 확인
plot(robust_model, which = 1)

# 다중공선성 확인
vif(robust_model) > 10
variables <- data_scaled[, c("풍속","습도", "피해액")]
cor(variables)
data_scaled<- data_scaled[,c(-5,-6,-7,-8)] # 기온, 풍속, 습도, 피해액(-5,-6,-7,-8)

cor(data_scaled)
# 데이터에서 NA 값 제거
data_scaled <- na.omit(data_scaled)

# 학습 데이터와 테스트 데이터 분할
set.seed(123)
idx <- sample(1:nrow(data_scaled), 0.7 * nrow(data_scaled))
train <- data_scaled[idx, ]
test <- data_scaled[-idx, ]

# 로버스트 모델 재학습 및 예측
robust_lm_model <- rlm(면적 ~ ., data = train)
robust_pred <- predict(robust_lm_model, test)

# 예측값과 실제값의 상관계수 확인
cor(robust_pred, test$면적)

# 모델 진단 및 시각화
par(mfrow = c(2, 2))
plot(robust_lm_model)
summary(robust_lm_model)

# 상자그림(boxplot) 확인
boxplot(robust_lm_model$residuals, main = "로버스트 회귀 모델의 잔차 분포")
