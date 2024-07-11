# 필요한 패키지 설치 및 로드
install.packages("readxl")
install.packages("Hmisc")
library(readxl)
library(Hmisc)

# 엑셀 파일 불러오기
file_path <- "플로어볼 경기 개인 기록 모음.xlsx"
data <- read_excel(file_path, skip = 2)

# 컬럼 이름 지정
colnames(data) <- c('Index', 'Team', 'Rank_Points', 'Game_Participation', 'Game_1', 'Game_2', 'Game_3', 'MVP_1', 'MVP_2', 'MVP_3', 
                    'Recorder_1', 'Recorder_2', 'Recorder_3', 'Goal_1', 'Goal_2', 'Goal_3', 'Assist_1', 'Assist_2', 'Assist_3', 
                    'Pass_1', 'Pass_2', 'Pass_3', 'Shot_1', 'Shot_2', 'Shot_3', 'Foul_1', 'Foul_2', 'Foul_3', 'League_Points')

# 결측값 제거 및 총합 컬럼 생성
data <- na.omit(data)
data$Total_Goals <- rowSums(data[, c('Goal_1', 'Goal_2', 'Goal_3')], na.rm = TRUE)
data$Total_Assists <- rowSums(data[, c('Assist_1', 'Assist_2', 'Assist_3')], na.rm = TRUE)
data$Total_Passes <- rowSums(data[, c('Pass_1', 'Pass_2', 'Pass_3')], na.rm = TRUE)
data$Total_Shots <- rowSums(data[, c('Shot_1', 'Shot_2', 'Shot_3')], na.rm = TRUE)
data$Total_Fouls <- rowSums(data[, c('Foul_1', 'Foul_2', 'Foul_3')], na.rm = TRUE)
data$Total_MVP <- rowSums(data[, c('MVP_1', 'MVP_2', 'MVP_3')], na.rm = TRUE)
data$Total_Recorder <- rowSums(data[, c('Recorder_1', 'Recorder_2', 'Recorder_3')], na.rm = TRUE)

# 분석에 필요한 데이터 프레임 생성
analysis_data <- data[, c('Rank_Points', 'Game_Participation', 'Total_Goals', 'Total_Assists', 'Total_Passes', 
                          'Total_Shots', 'Total_Fouls', 'Total_MVP', 'Total_Recorder', 'League_Points')]

# 스피어만 상관 관계 및 유의수준 계산
corr_result <- rcorr(as.matrix(analysis_data), type="spearman")
corr_result$r # 상관 계수
corr_result$P # 유의수준 (p-value)
