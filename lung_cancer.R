#csv파일불러오기
cancers=read.csv("C:/Users/nkm11/Desktop/lab/대한의료정보학회/암임상_라이브러리_합성데이터_train_test_set(폐암)/학습데이터_폐암.csv", header = T, sep=",", fileEncoding = "euc-kr")

#사용할 열 가져오기 (병기stage, 흡연여부, 등등)
lung_cancer=cancers[, c(2:26, 29:34)]

#흡연자와 비흡연자 두 개로 구분
lung_cancer$Smoke = ifelse(lung_cancer$Smoke == 0, 0, 1)

# EGFR.mutation.Detection 열에서 99 값을 NA로 변환
#lung_cancer$EGFR.mutation.Detection[lung_cancer$EGFR.mutation.Detection == 99] <- NA

#음주여부 열에서 99 값을 NA로 변환
#lung_cancer$Type.of.Drink[lung_cancer$Type.of.Drink == 99] <- NA

#IV값이 NA가 나오지 않게 전처리
#lung_cancer = lung_cancer[complete.cases(lung_cancer), ]

View(lung_cancer)

#TNM변수 병합 및 정리
lung_cancer$T1 = ifelse(lung_cancer$T1 == 1 | lung_cancer$T1a == 1 | lung_cancer$T1b == 1 | lung_cancer$T1c == 1, 1, 0)
lung_cancer$T2 = ifelse(lung_cancer$T2 == 1 | lung_cancer$T2a == 1 | lung_cancer$T2b == 1, 1, 0)
lung_cancer$M = ifelse(lung_cancer$M1a == 1 | lung_cancer$M1b == 1 | lung_cancer$M1c == 1, 1, 0)
lung_cancer=subset(lung_cancer, select = -c(T0, T1a, T1b, T1c, T2a, T2b, M1a, M1b, M1c))
 
#I, II, III, IV열로 묶기
lung_cancer$I = ifelse(lung_cancer$M == 0 & lung_cancer$T1 == 1 & lung_cancer$T2 == 0 & lung_cancer$T3 == 0 & lung_cancer$T4 == 0 & lung_cancer$N1 == 1 & lung_cancer$N2 == 0 & lung_cancer$N3 == 0, 1,
               ifelse(lung_cancer$M == 0 &lung_cancer$T2 == 1 & lung_cancer$T3 == 0 & lung_cancer$T4 == 0 & lung_cancer$N1 == 0 & lung_cancer$N2 == 0 & lung_cancer$N3 == 0, 1, 0))

lung_cancer$II = ifelse(lung_cancer$M == 0 & lung_cancer$T1 == 1 & lung_cancer$T2 == 0 & lung_cancer$T3 == 0 & lung_cancer$T4 == 0 & lung_cancer$N1 == 0 & lung_cancer$N2 == 0 & lung_cancer$N3 == 0, 1,
                       ifelse(lung_cancer$M == 0 &lung_cancer$T2 == 1 & lung_cancer$T3 == 0 & lung_cancer$T4 == 0 & lung_cancer$N1 == 1 & lung_cancer$N2 == 0 & lung_cancer$N3 == 0, 1,
                              ifelse(lung_cancer$M == 0 &lung_cancer$T3 == 1 & lung_cancer$T4 == 0 & lung_cancer$N1 == 0 & lung_cancer$N2 == 0 & lung_cancer$N3 == 0, 1, 0)))

lung_cancer$III = ifelse(lung_cancer$M == 0 & lung_cancer$T1 == 1 & lung_cancer$T2 == 0 & lung_cancer$T3 == 0 & lung_cancer$T4 == 0 & lung_cancer$N2 == 1 & lung_cancer$N3 == 0, 1,
                        ifelse(lung_cancer$M == 0 & lung_cancer$T1 == 1 & lung_cancer$T2 == 0 & lung_cancer$T3 == 0 & lung_cancer$T4 == 0 & lung_cancer$N3 == 1, 1,
                               ifelse(lung_cancer$M == 0 & lung_cancer$T2 == 1 & lung_cancer$T3 == 0 & lung_cancer$T4 == 0 & lung_cancer$N2 == 1 & lung_cancer$N3 == 0, 1,
                                      ifelse(lung_cancer$M == 0 & lung_cancer$T2 == 1 & lung_cancer$T3 == 0 & lung_cancer$T4 == 0 & lung_cancer$N3 == 1, 1,
                                             ifelse(lung_cancer$M == 0 & lung_cancer$T3 == 1 & lung_cancer$T4 == 0 & lung_cancer$N1 == 1 & lung_cancer$N2 == 0 & lung_cancer$N3 == 0, 1,
                                                    ifelse(lung_cancer$M == 0 & lung_cancer$T3 == 1 & lung_cancer$T4 == 0 & lung_cancer$N2 == 1 & lung_cancer$N3 == 0, 1,
                                                           ifelse(lung_cancer$M == 0 & lung_cancer$T3 == 1 & lung_cancer$T4 == 0 & lung_cancer$N3 == 1, 1,
                                                                  ifelse(lung_cancer$M == 0 & lung_cancer$T3 == 1 & lung_cancer$T4 == 0 & lung_cancer$N2 == 1 & lung_cancer$N3 == 0, 1,
                                                                         ifelse(lung_cancer$M == 0 & lung_cancer$T4 == 1, 1, 0)))))))))

lung_cancer$IV = ifelse(lung_cancer$M == 1, 1, 0)
#count에 i ii iii iv값이 1인 경우 1로 입력
lung_cancer$count = apply(lung_cancer[, c("I", "II", "III", "IV")], 1, function(x) sum(x > 0))
#tumor가 발견되지 않은 값 삭제
lung_cancer=lung_cancer[!(lung_cancer$count == 0), ]


#흡연 여부(원-핫 인코딩 하면 IV값이 안나옴,아래의 코드를 사용하자.)
#lung_cancer = lung_cancer %>%
#  mutate(Smoke0 = ifelse(Smoke == 0, 1, 0),
#         Smoke1 = ifelse(Smoke == 1, 1, 0),
#         Smoke2 = ifelse(Smoke == 2, 1, 0)) %>% select(-Smoke)


#BMI 지수 열 생성, 신장, 체중 TX삭제
lung_cancer$BMI=lung_cancer$Weight / ((lung_cancer$Height/100) ^2 )
lung_cancer=subset(lung_cancer, select = -c(Height, Weight, TX, T1, T2, T3, T4, N1, N2, N3, M, count))

#data scaling
#lung_cancer = scale(lung_cancer[, c("AGE", "BMI")])
#lung_cancer = scale(lung_cancer[, c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy")])

#Cox회귀분석
install.packages("survival")
library(survival)
cox_mod=coxph(Surv(Survival.period, Death) ~ AGE + Adenocarcinoma + Large.cell.carcinoma + Squamous.cell.carcinoma + I + II + III + IV + Type.of.Drink + Smoke + BMI + EGFR.mutation.Detection + Operation + Chemotherapy + Radiation.Therapy, data = lung_cancer)
summary(cox_mod)

#시각화
# 히스토그램 (예시)
hist(lung_cancer$AGE, main = "Distribution of AGE", xlab = "Age")

# 박스 플롯 (예시)
boxplot(lung_cancer$BMI, main = "Boxplot of BMI", ylab = "BMI")

# 오즈 비율 그래프(실사용예시)
install.packages("ggplot2")
library(ggplot2)

ggplot(lung_cancer, aes(x = factor(Smoke), y = exp(coef(cox_mod)["Smoke"]), fill = factor(Smoke))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Smoking", y = "Odds Ratio", title = "Odds Ratio by Smoking")

# 결과 데이터 프레임 생성 (예시 데이터)
results <- data.frame(
  Variable = c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy"),
  HR = c(1.0001825, 0.9769715, 1.0456119, 1.0034341, 1.0139617, 0.9493408, 0.9416512, NA, 1.0001306, 0.8777377, 1.0043615, 0.9995443, 0.9551366, 1.0264522, 1.0199076),
  CI_Lower = c(0.9969, 0.8847, 0.9306, 0.9070, 0.8816, 0.8296, 0.8416, NA, 0.9992, 0.7857, 0.9968, 0.9985, 0.8706, 0.9363, 0.9188),
  CI_Upper = c(1.0035, 1.0788, 1.1748, 1.1101, 1.1662, 1.0863, 1.0536, NA, 1.0011, 0.9805, 1.0120, 1.0005, 1.0479, 1.1253, 1.1321)
)

# 포레스트 플롯 생성
ggplot(results, aes(x = HR, xmin = CI_Lower, xmax = CI_Upper, y = Variable)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.2) +
  scale_x_log10() +
  labs(x = "Hazard Ratio (HR)", y = "Variable", title = "Forest Plot") +
  theme_minimal()

#이건되겠지?
# 결과 데이터 프레임 생성
results <- data.frame(
  Variable = c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy"),
  HR = c(1.0001825, 0.9769715, 1.0456119, 1.0034341, 1.0139617, 0.9493408, 0.9416512, NA, 1.0001306, 0.8777377, 1.0043615, 0.9995443, 0.9551366, 1.0264522, 1.0199076),
  CI_Lower = c(0.9969, 0.8847, 0.9306, 0.9070, 0.8816, 0.8296, 0.8416, NA, 0.9992, 0.7857, 0.9968, 0.9985, 0.8706, 0.9363, 0.9188),
  CI_Upper = c(1.0035, 1.0788, 1.1748, 1.1101, 1.1662, 1.0863, 1.0536, NA, 1.0011, 0.9805, 1.0120, 1.0005, 1.0479, 1.1253, 1.1321)
)

# NA 값을 가지는 행 제거
results <- results[complete.cases(results), ]

# CI 오차 막대 생성
library(ggplot2)

ggplot(results, aes(x = HR, y = Variable)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  scale_x_log10() +
  labs(x = "Hazard Ratio (HR)", y = "Variable", title = "Confidence Interval (CI)") +
  theme_minimal()

#변수명 순서를 표와 같게
# 결과 데이터 프레임 생성
results <- data.frame(
  Variable = c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy"),
  HR = c(1.0001825, 0.9769715, 1.0456119, 1.0034341, 1.0139617, 0.9493408, 0.9416512, NA, 1.0001306, 0.8777377, 1.0043615, 0.9995443, 0.9551366, 1.0264522, 1.0199076),
  CI_Lower = c(0.9969, 0.8847, 0.9306, 0.9070, 0.8816, 0.8296, 0.8416, NA, 0.9992, 0.7857, 0.9968, 0.9985, 0.8706, 0.9363, 0.9188),
  CI_Upper = c(1.0035, 1.0788, 1.1748, 1.1101, 1.1662, 1.0863, 1.0536, NA, 1.0011, 0.9805, 1.0120, 1.0005, 1.0479, 1.1253, 1.1321)
)

# NA 값을 가지는 행 제거
results <- results[complete.cases(results), ]

# 변수명 순서 지정
variable_order <- rev(c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy"))

# 변수명 순서 반영하여 시각화
library(ggplot2)

ggplot(results, aes(x = HR, y = factor(Variable, levels = variable_order))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  scale_x_log10() +
  labs(x = "Hazard Ratio (HR)", y = "Variable", title = "Confidence Interval (CI)") +
  theme_minimal()









#검증데이터
cancers1=read.csv("C:/Users/nkm11/Desktop/대한의료정보학회/암임상_라이브러리_합성데이터_train_test_set(폐암)/검증데이터_폐암.csv", header = T, sep=",", fileEncoding = "euc-kr")

#사용할 열 가져오기 (병기stage, 흡연여부, 등등)
lung_cancer1=cancers1[, c(2:26, 29:34)]

#흡연자와 비흡연자 두 개로 구분
lung_cancer1$Smoke = ifelse(lung_cancer1$Smoke == 0, 0, 1)

# EGFR.mutation.Detection 열에서 99 값을 NA로 변환
#lung_cancer$EGFR.mutation.Detection[lung_cancer$EGFR.mutation.Detection == 99] <- NA

#음주여부 열에서 99 값을 NA로 변환
#lung_cancer$Type.of.Drink[lung_cancer$Type.of.Drink == 99] <- NA

#IV값이 NA가 나오지 않게 전처리
#lung_cancer = lung_cancer[complete.cases(lung_cancer), ]

View(lung_cancer1)

#TNM변수 병합 및 정리
lung_cancer1$T1 = ifelse(lung_cancer1$T1 == 1 | lung_cancer1$T1a == 1 | lung_cancer1$T1b == 1 | lung_cancer1$T1c == 1, 1, 0)
lung_cancer1$T2 = ifelse(lung_cancer1$T2 == 1 | lung_cancer1$T2a == 1 | lung_cancer1$T2b == 1, 1, 0)
lung_cancer1$M = ifelse(lung_cancer1$M1a == 1 | lung_cancer1$M1b == 1 | lung_cancer1$M1c == 1, 1, 0)
lung_cancer1=subset(lung_cancer1, select = -c(T0, T1a, T1b, T1c, T2a, T2b, M1a, M1b, M1c))

#I, II, III, IV열로 묶기
lung_cancer1$I = ifelse(lung_cancer1$M == 0 & lung_cancer1$T1 == 1 & lung_cancer1$T2 == 0 & lung_cancer1$T3 == 0 & lung_cancer1$T4 == 0 & lung_cancer1$N1 == 1 & lung_cancer1$N2 == 0 & lung_cancer1$N3 == 0, 1,
                       ifelse(lung_cancer1$M == 0 &lung_cancer1$T2 == 1 & lung_cancer1$T3 == 0 & lung_cancer1$T4 == 0 & lung_cancer1$N1 == 0 & lung_cancer1$N2 == 0 & lung_cancer1$N3 == 0, 1, 0))

lung_cancer1$II = ifelse(lung_cancer1$M == 0 & lung_cancer1$T1 == 1 & lung_cancer1$T2 == 0 & lung_cancer1$T3 == 0 & lung_cancer1$T4 == 0 & lung_cancer1$N1 == 0 & lung_cancer1$N2 == 0 & lung_cancer1$N3 == 0, 1,
                        ifelse(lung_cancer1$M == 0 &lung_cancer1$T2 == 1 & lung_cancer1$T3 == 0 & lung_cancer1$T4 == 0 & lung_cancer1$N1 == 1 & lung_cancer1$N2 == 0 & lung_cancer1$N3 == 0, 1,
                               ifelse(lung_cancer1$M == 0 &lung_cancer1$T3 == 1 & lung_cancer1$T4 == 0 & lung_cancer1$N1 == 0 & lung_cancer1$N2 == 0 & lung_cancer1$N3 == 0, 1, 0)))

lung_cancer1$III = ifelse(lung_cancer1$M == 0 & lung_cancer1$T1 == 1 & lung_cancer1$T2 == 0 & lung_cancer1$T3 == 0 & lung_cancer1$T4 == 0 & lung_cancer1$N2 == 1 & lung_cancer1$N3 == 0, 1,
                         ifelse(lung_cancer1$M == 0 & lung_cancer1$T1 == 1 & lung_cancer1$T2 == 0 & lung_cancer1$T3 == 0 & lung_cancer1$T4 == 0 & lung_cancer1$N3 == 1, 1,
                                ifelse(lung_cancer1$M == 0 & lung_cancer1$T2 == 1 & lung_cancer1$T3 == 0 & lung_cancer1$T4 == 0 & lung_cancer1$N2 == 1 & lung_cancer1$N3 == 0, 1,
                                       ifelse(lung_cancer1$M == 0 & lung_cancer1$T2 == 1 & lung_cancer1$T3 == 0 & lung_cancer1$T4 == 0 & lung_cancer1$N3 == 1, 1,
                                              ifelse(lung_cancer1$M == 0 & lung_cancer1$T3 == 1 & lung_cancer1$T4 == 0 & lung_cancer1$N1 == 1 & lung_cancer1$N2 == 0 & lung_cancer1$N3 == 0, 1,
                                                     ifelse(lung_cancer1$M == 0 & lung_cancer1$T3 == 1 & lung_cancer1$T4 == 0 & lung_cancer1$N2 == 1 & lung_cancer1$N3 == 0, 1,
                                                            ifelse(lung_cancer1$M == 0 & lung_cancer1$T3 == 1 & lung_cancer1$T4 == 0 & lung_cancer1$N3 == 1, 1,
                                                                   ifelse(lung_cancer1$M == 0 & lung_cancer1$T3 == 1 & lung_cancer1$T4 == 0 & lung_cancer1$N2 == 1 & lung_cancer1$N3 == 0, 1,
                                                                          ifelse(lung_cancer1$M == 0 & lung_cancer1$T4 == 1, 1, 0)))))))))

lung_cancer1$IV = ifelse(lung_cancer1$M == 1, 1, 0)
#count에 i ii iii iv값이 1인 경우 1로 입력
lung_cancer1$count = apply(lung_cancer1[, c("I", "II", "III", "IV")], 1, function(x) sum(x > 0))
#tumor가 발견되지 않은 값 삭제
lung_cancer1=lung_cancer1[!(lung_cancer1$count == 0), ]


#흡연 여부(원-핫 인코딩 하면 IV값이 안나옴,아래의 코드를 사용하자.)
#lung_cancer = lung_cancer %>%
#  mutate(Smoke0 = ifelse(Smoke == 0, 1, 0),
#         Smoke1 = ifelse(Smoke == 1, 1, 0),
#         Smoke2 = ifelse(Smoke == 2, 1, 0)) %>% select(-Smoke)


#BMI 지수 열 생성, 신장, 체중 TX삭제
lung_cancer1$BMI=lung_cancer1$Weight / ((lung_cancer1$Height/100) ^2 )
lung_cancer1=subset(lung_cancer1, select = -c(Height, Weight, TX, T1, T2, T3, T4, N1, N2, N3, M, count))

#data scaling
#lung_cancer = scale(lung_cancer[, c("AGE", "BMI")])
#lung_cancer = scale(lung_cancer[, c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy")])

#Cox회귀분석
install.packages("survival")
library(survival)
cox_mod1=coxph(Surv(Survival.period, Death) ~ AGE + Adenocarcinoma + Large.cell.carcinoma + Squamous.cell.carcinoma + I + II + III + IV + Type.of.Drink + Smoke + BMI + EGFR.mutation.Detection + Operation + Chemotherapy + Radiation.Therapy, data = lung_cancer1)
summary(cox_mod1)

#모델분석 코드?
predictions = predict(cox_mod, newdata = lung_cancer1)
head(predictions)

