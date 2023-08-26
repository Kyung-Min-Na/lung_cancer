#csv���Ϻҷ�����
cancers=read.csv("C:/Users/nkm11/Desktop/lab/�����Ƿ�������ȸ/���ӻ�_���̺귯��_�ռ�������_train_test_set(���)/�н�������_���.csv", header = T, sep=",", fileEncoding = "euc-kr")

#����� �� �������� (����stage, ��������, ���)
lung_cancer=cancers[, c(2:26, 29:34)]

#�����ڿ� �������� �� ���� ����
lung_cancer$Smoke = ifelse(lung_cancer$Smoke == 0, 0, 1)

# EGFR.mutation.Detection ������ 99 ���� NA�� ��ȯ
#lung_cancer$EGFR.mutation.Detection[lung_cancer$EGFR.mutation.Detection == 99] <- NA

#���ֿ��� ������ 99 ���� NA�� ��ȯ
#lung_cancer$Type.of.Drink[lung_cancer$Type.of.Drink == 99] <- NA

#IV���� NA�� ������ �ʰ� ��ó��
#lung_cancer = lung_cancer[complete.cases(lung_cancer), ]

View(lung_cancer)

#TNM���� ���� �� ����
lung_cancer$T1 = ifelse(lung_cancer$T1 == 1 | lung_cancer$T1a == 1 | lung_cancer$T1b == 1 | lung_cancer$T1c == 1, 1, 0)
lung_cancer$T2 = ifelse(lung_cancer$T2 == 1 | lung_cancer$T2a == 1 | lung_cancer$T2b == 1, 1, 0)
lung_cancer$M = ifelse(lung_cancer$M1a == 1 | lung_cancer$M1b == 1 | lung_cancer$M1c == 1, 1, 0)
lung_cancer=subset(lung_cancer, select = -c(T0, T1a, T1b, T1c, T2a, T2b, M1a, M1b, M1c))
 
#I, II, III, IV���� ����
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
#count�� i ii iii iv���� 1�� ��� 1�� �Է�
lung_cancer$count = apply(lung_cancer[, c("I", "II", "III", "IV")], 1, function(x) sum(x > 0))
#tumor�� �߰ߵ��� ���� �� ����
lung_cancer=lung_cancer[!(lung_cancer$count == 0), ]


#���� ����(��-�� ���ڵ� �ϸ� IV���� �ȳ���,�Ʒ��� �ڵ带 �������.)
#lung_cancer = lung_cancer %>%
#  mutate(Smoke0 = ifelse(Smoke == 0, 1, 0),
#         Smoke1 = ifelse(Smoke == 1, 1, 0),
#         Smoke2 = ifelse(Smoke == 2, 1, 0)) %>% select(-Smoke)


#BMI ���� �� ����, ����, ü�� TX����
lung_cancer$BMI=lung_cancer$Weight / ((lung_cancer$Height/100) ^2 )
lung_cancer=subset(lung_cancer, select = -c(Height, Weight, TX, T1, T2, T3, T4, N1, N2, N3, M, count))

#data scaling
#lung_cancer = scale(lung_cancer[, c("AGE", "BMI")])
#lung_cancer = scale(lung_cancer[, c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy")])

#Coxȸ�ͺм�
install.packages("survival")
library(survival)
cox_mod=coxph(Surv(Survival.period, Death) ~ AGE + Adenocarcinoma + Large.cell.carcinoma + Squamous.cell.carcinoma + I + II + III + IV + Type.of.Drink + Smoke + BMI + EGFR.mutation.Detection + Operation + Chemotherapy + Radiation.Therapy, data = lung_cancer)
summary(cox_mod)

#�ð�ȭ
# ������׷� (����)
hist(lung_cancer$AGE, main = "Distribution of AGE", xlab = "Age")

# �ڽ� �÷� (����)
boxplot(lung_cancer$BMI, main = "Boxplot of BMI", ylab = "BMI")

# ���� ���� �׷���(�ǻ�뿹��)
install.packages("ggplot2")
library(ggplot2)

ggplot(lung_cancer, aes(x = factor(Smoke), y = exp(coef(cox_mod)["Smoke"]), fill = factor(Smoke))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Smoking", y = "Odds Ratio", title = "Odds Ratio by Smoking")

# ��� ������ ������ ���� (���� ������)
results <- data.frame(
  Variable = c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy"),
  HR = c(1.0001825, 0.9769715, 1.0456119, 1.0034341, 1.0139617, 0.9493408, 0.9416512, NA, 1.0001306, 0.8777377, 1.0043615, 0.9995443, 0.9551366, 1.0264522, 1.0199076),
  CI_Lower = c(0.9969, 0.8847, 0.9306, 0.9070, 0.8816, 0.8296, 0.8416, NA, 0.9992, 0.7857, 0.9968, 0.9985, 0.8706, 0.9363, 0.9188),
  CI_Upper = c(1.0035, 1.0788, 1.1748, 1.1101, 1.1662, 1.0863, 1.0536, NA, 1.0011, 0.9805, 1.0120, 1.0005, 1.0479, 1.1253, 1.1321)
)

# ������Ʈ �÷� ����
ggplot(results, aes(x = HR, xmin = CI_Lower, xmax = CI_Upper, y = Variable)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.2) +
  scale_x_log10() +
  labs(x = "Hazard Ratio (HR)", y = "Variable", title = "Forest Plot") +
  theme_minimal()

#�̰ǵǰ���?
# ��� ������ ������ ����
results <- data.frame(
  Variable = c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy"),
  HR = c(1.0001825, 0.9769715, 1.0456119, 1.0034341, 1.0139617, 0.9493408, 0.9416512, NA, 1.0001306, 0.8777377, 1.0043615, 0.9995443, 0.9551366, 1.0264522, 1.0199076),
  CI_Lower = c(0.9969, 0.8847, 0.9306, 0.9070, 0.8816, 0.8296, 0.8416, NA, 0.9992, 0.7857, 0.9968, 0.9985, 0.8706, 0.9363, 0.9188),
  CI_Upper = c(1.0035, 1.0788, 1.1748, 1.1101, 1.1662, 1.0863, 1.0536, NA, 1.0011, 0.9805, 1.0120, 1.0005, 1.0479, 1.1253, 1.1321)
)

# NA ���� ������ �� ����
results <- results[complete.cases(results), ]

# CI ���� ���� ����
library(ggplot2)

ggplot(results, aes(x = HR, y = Variable)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  scale_x_log10() +
  labs(x = "Hazard Ratio (HR)", y = "Variable", title = "Confidence Interval (CI)") +
  theme_minimal()

#������ ������ ǥ�� ����
# ��� ������ ������ ����
results <- data.frame(
  Variable = c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy"),
  HR = c(1.0001825, 0.9769715, 1.0456119, 1.0034341, 1.0139617, 0.9493408, 0.9416512, NA, 1.0001306, 0.8777377, 1.0043615, 0.9995443, 0.9551366, 1.0264522, 1.0199076),
  CI_Lower = c(0.9969, 0.8847, 0.9306, 0.9070, 0.8816, 0.8296, 0.8416, NA, 0.9992, 0.7857, 0.9968, 0.9985, 0.8706, 0.9363, 0.9188),
  CI_Upper = c(1.0035, 1.0788, 1.1748, 1.1101, 1.1662, 1.0863, 1.0536, NA, 1.0011, 0.9805, 1.0120, 1.0005, 1.0479, 1.1253, 1.1321)
)

# NA ���� ������ �� ����
results <- results[complete.cases(results), ]

# ������ ���� ����
variable_order <- rev(c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy"))

# ������ ���� �ݿ��Ͽ� �ð�ȭ
library(ggplot2)

ggplot(results, aes(x = HR, y = factor(Variable, levels = variable_order))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  scale_x_log10() +
  labs(x = "Hazard Ratio (HR)", y = "Variable", title = "Confidence Interval (CI)") +
  theme_minimal()









#����������
cancers1=read.csv("C:/Users/nkm11/Desktop/�����Ƿ�������ȸ/���ӻ�_���̺귯��_�ռ�������_train_test_set(���)/����������_���.csv", header = T, sep=",", fileEncoding = "euc-kr")

#����� �� �������� (����stage, ��������, ���)
lung_cancer1=cancers1[, c(2:26, 29:34)]

#�����ڿ� �������� �� ���� ����
lung_cancer1$Smoke = ifelse(lung_cancer1$Smoke == 0, 0, 1)

# EGFR.mutation.Detection ������ 99 ���� NA�� ��ȯ
#lung_cancer$EGFR.mutation.Detection[lung_cancer$EGFR.mutation.Detection == 99] <- NA

#���ֿ��� ������ 99 ���� NA�� ��ȯ
#lung_cancer$Type.of.Drink[lung_cancer$Type.of.Drink == 99] <- NA

#IV���� NA�� ������ �ʰ� ��ó��
#lung_cancer = lung_cancer[complete.cases(lung_cancer), ]

View(lung_cancer1)

#TNM���� ���� �� ����
lung_cancer1$T1 = ifelse(lung_cancer1$T1 == 1 | lung_cancer1$T1a == 1 | lung_cancer1$T1b == 1 | lung_cancer1$T1c == 1, 1, 0)
lung_cancer1$T2 = ifelse(lung_cancer1$T2 == 1 | lung_cancer1$T2a == 1 | lung_cancer1$T2b == 1, 1, 0)
lung_cancer1$M = ifelse(lung_cancer1$M1a == 1 | lung_cancer1$M1b == 1 | lung_cancer1$M1c == 1, 1, 0)
lung_cancer1=subset(lung_cancer1, select = -c(T0, T1a, T1b, T1c, T2a, T2b, M1a, M1b, M1c))

#I, II, III, IV���� ����
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
#count�� i ii iii iv���� 1�� ��� 1�� �Է�
lung_cancer1$count = apply(lung_cancer1[, c("I", "II", "III", "IV")], 1, function(x) sum(x > 0))
#tumor�� �߰ߵ��� ���� �� ����
lung_cancer1=lung_cancer1[!(lung_cancer1$count == 0), ]


#���� ����(��-�� ���ڵ� �ϸ� IV���� �ȳ���,�Ʒ��� �ڵ带 �������.)
#lung_cancer = lung_cancer %>%
#  mutate(Smoke0 = ifelse(Smoke == 0, 1, 0),
#         Smoke1 = ifelse(Smoke == 1, 1, 0),
#         Smoke2 = ifelse(Smoke == 2, 1, 0)) %>% select(-Smoke)


#BMI ���� �� ����, ����, ü�� TX����
lung_cancer1$BMI=lung_cancer1$Weight / ((lung_cancer1$Height/100) ^2 )
lung_cancer1=subset(lung_cancer1, select = -c(Height, Weight, TX, T1, T2, T3, T4, N1, N2, N3, M, count))

#data scaling
#lung_cancer = scale(lung_cancer[, c("AGE", "BMI")])
#lung_cancer = scale(lung_cancer[, c("AGE", "Adenocarcinoma", "Large.cell.carcinoma", "Squamous.cell.carcinoma", "I", "II", "III", "IV", "Type.of.Drink", "Smoke", "BMI", "EGFR.mutation.Detection", "Operation", "Chemotherapy", "Radiation.Therapy")])

#Coxȸ�ͺм�
install.packages("survival")
library(survival)
cox_mod1=coxph(Surv(Survival.period, Death) ~ AGE + Adenocarcinoma + Large.cell.carcinoma + Squamous.cell.carcinoma + I + II + III + IV + Type.of.Drink + Smoke + BMI + EGFR.mutation.Detection + Operation + Chemotherapy + Radiation.Therapy, data = lung_cancer1)
summary(cox_mod1)

#�𵨺м� �ڵ�?
predictions = predict(cox_mod, newdata = lung_cancer1)
head(predictions)
