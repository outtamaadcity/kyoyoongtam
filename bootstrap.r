# 필요한 라이브러리 설치 및 로드
if (!require(dplyr)) install.packages("dplyr")
if (!require(readxl)) install.packages("readxl")
if (!require(boot)) install.packages("boot")

library(dplyr)
library(readxl)
library(boot)

# 데이터 로드
file_path <- "C:/Users/user/OneDrive/문서/플로어볼_경기_기록_모음.xlsx"
data <- read_excel(file_path, skip = 2)

# 데이터 확인
print("데이터 로드 완료")
print(head(data))

# 열 이름 지정
colnames(data) <- c('Index', 'Team', 'Rank_Points', 'Game_Participation', 'Game_1', 'Game_2', 'Game_3', 'MVP_1', 'MVP_2', 'MVP_3',
                    'Recorder_1', 'Recorder_2', 'Recorder_3', 'Goal_1', 'Goal_2', 'Goal_3', 'Assist_1', 'Assist_2', 'Assist_3',
                    'Pass_1', 'Pass_2', 'Pass_3', 'Shot_1', 'Shot_2', 'Shot_3', 'Foul_1', 'Foul_2', 'Foul_3', 'League_Points')

# 첫 번째 행 제거
data <- data[-1, ]

# 숫자형 변환
numeric_columns <- c('Rank_Points', 'Game_Participation', 'Game_1', 'Game_2', 'Game_3', 'MVP_1', 'MVP_2', 'MVP_3',
                     'Recorder_1', 'Recorder_2', 'Recorder_3', 'Goal_1', 'Goal_2', 'Goal_3', 'Assist_1', 'Assist_2',
                     'Assist_3', 'Pass_1', 'Pass_2', 'Pass_3', 'Shot_1', 'Shot_2', 'Shot_3', 'Foul_1', 'Foul_2',
                     'Foul_3', 'League_Points')
data[numeric_columns] <- lapply(data[numeric_columns], as.numeric)

# 결측값 있는 행 제거
data <- data[complete.cases(data), ]

# 각 선수의 총합 데이터 계산
data <- data %>%
  mutate(Total_Goals = Goal_1 + Goal_2 + Goal_3,
         Total_Assists = Assist_1 + Assist_2 + Assist_3,
         Total_Passes = Pass_1 + Pass_2 + Pass_3,
         Total_Shots = Shot_1 + Shot_2 + Shot_3,
         Total_Fouls = Foul_1 + Foul_2 + Foul_3,
         Total_MVP = MVP_1 + MVP_2 + MVP_3,
         Total_Recorder = Recorder_1 + Recorder_2 + Recorder_3)

# 분석에 사용할 데이터 선택
analysis_data <- data %>%
  select(Game_Participation, Total_Goals, Total_Assists, Total_Passes,
         Total_Shots, Total_Fouls, Total_Recorder, Total_MVP)

# 분석 데이터 확인
print("분석 데이터 준비 완료")
print(head(analysis_data))

# 최적의 가중치를 찾는 함수
find_optimal_weights <- function(pvalues_df) {
  best_weight_reg <- 0
  best_weight_spear <- 0
  min_discrepancy <- Inf

  for (weight_reg in seq(0, 1, by = 0.01)) {
    weight_spear <- 1 - weight_reg
    combined_pvalues <- weight_reg * pvalues_df$Regression_PValue + weight_spear * pvalues_df$Spearman_PValue
    discrepancy <- sum((combined_pvalues < 0.05) == ((pvalues_df$Regression_PValue < 0.05) | (pvalues_df$Spearman_PValue < 0.05)))

    if (discrepancy < min_discrepancy) {
      min_discrepancy <- discrepancy
      best_weight_reg <- weight_reg
      best_weight_spear <- weight_spear
    }
  }

  return(c(best_weight_reg, best_weight_spear))
}

# 부트스트랩 함수 정의
bootstrap_function <- function(data, indices) {
  sample_data <- data[indices, ]

  # 다중 회귀 분석
  lm_model <- lm(Total_MVP ~ Game_Participation + Total_Goals + Total_Assists + Total_Passes + Total_Shots + Total_Fouls + Total_Recorder, data = sample_data)
  regression_pvalues <- summary(lm_model)$coefficients[, 4][-1]

  # 회귀 분석 결과 확인
  print("회귀 분석 p-values:")
  print(regression_pvalues)

  # 스피어만 상관 분석
  spearman_corr <- cor(sample_data, method = "spearman")
  spearman_pvalues <- apply(sample_data, 2, function(x) cor.test(x, sample_data$Total_MVP, method = "spearman")$p.value)
  spearman_pvalues <- spearman_pvalues[-length(spearman_pvalues)]

  # 스피어만 상관 분석 결과 확인
  print("스피어만 상관 분석 p-values:")
  print(spearman_pvalues)

  # p-value 데이터프레임 생성
  pvalues_df <- data.frame(
    Variable = names(regression_pvalues),
    Regression_PValue = regression_pvalues,
    Spearman_PValue = spearman_pvalues
  )

  # p-values 데이터프레임 확인
  print("p-values 데이터프레임:")
  print(pvalues_df)

  # 최적의 가중치 계산
  optimal_weights <- find_optimal_weights(pvalues_df)

  return(optimal_weights)
}

# 부트스트랩 실행
set.seed(123)
bootstrap_results <- boot(data = analysis_data, statistic = bootstrap_function, R = 10)  # 디버깅을 위해 R을 10으로 설정

# 부트스트랩 결과 확인
print("부트스트랩 실행 완료")
print(bootstrap_results)

# 최적의 가중치 평균 계산
mean_weights <- colMeans(bootstrap_results$t)

# 최적의 가중치 출력
print("최적의 가중치")
print(mean_weights)

# 최적의 가중치를 원래 데이터에 적용
lm_model <- lm(Total_MVP ~ Game_Participation + Total_Goals + Total_Assists + Total_Passes + Total_Shots + Total_Fouls + Total_Recorder, data = analysis_data)
regression_pvalues <- summary(lm_model)$coefficients[, 4][-1]
print("최종 회귀 분석 p-values:")
print(regression_pvalues)

spearman_pvalues <- apply(analysis_data, 2, function(x) cor.test(x, analysis_data$Total_MVP, method = "spearman")$p.value)
spearman_pvalues <- spearman_pvalues[-length(spearman_pvalues)]
print("최종 스피어만 상관 분석 p-values:")
print(spearman_pvalues)

# p-value 데이터프레임 생성
pvalues_df <- data.frame(
  Variable = names(regression_pvalues),
  Regression_PValue = regression_pvalues,
  Spearman_PValue = spearman_pvalues
)

# p-value 데이터프레임 확인
print("p-value 데이터프레임 생성 완료")
print(pvalues_df)

# 결합된 p-value 계산
combined_pvalues <- mean_weights[1] * pvalues_df$Regression_PValue + mean_weights[2] * pvalues_df$Spearman_PValue

# 결합된 p-value 확인
print("결합된 p-values")
print(combined_pvalues)

# 결과 비교 데이터프레임 생성
comparison_df <- pvalues_df
comparison_df$Combined_PValue <- combined_pvalues

# 결과 출력
print("결과 비교 데이터프레임")
print(comparison_df)

# 결과를 CSV 파일로 저장
write.csv(comparison_df, 'bootstrap_p_value_comparison.csv', row.names = FALSE)
