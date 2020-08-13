df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df

is.na(df)               # 결측치 확인
table(is.na(df))        # 결측치 빈도 출력
table(is.na(df$sex))    # sex 결측치 빈도 출력
table(is.na(df$score))  # score 결측치 빈도 출력

mean(df$score)  # 평균 산출
sum(df$score)   # 합계 산출

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212),"hwy"] <- NA
mpg
is.na(mpg)
table(is.na(mpg))
table(is.na(mpg$hwy))


'''Q1. drv(구동방식)별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고 합니다. 분석을 하기 전에
우선 두 변수에 결측치가 있는지 확인해야 합니다. drv 변수와 hwy 변수에 결측치가 몇 개 있는지
알아보세요.
• Q2. filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 구동방식의 hwy 평균이 높은지 알아보세요.
하나의 dplyr 구문으로 만들어야 합니다.'''
mpg %>% 
  filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

library(dplyr) # dplyr 패키지 로드
df %>% filter(is.na(score))   # score가 NA인 데이터만 출력
df %>% filter(!is.na(score))  # score 결측치 제거

df_nomiss <- df %>% filter(!is.na(score))  # score 결측치 제거
mean(df_nomiss$score)                      # score 평균 산출
sum(df_nomiss$score)                       # score 합계 산출

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))  # score, sex 결측치 제거
df_nomiss                                                # 출력

df_nomiss2 <- na.omit(df)  # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2                 # 출력

exam[c(3, 8, 15),"math"] <- NA
exam
exam %>% summarise((mean_math = mean(math, na.rm = T)))



outlier <- data.frame(sex = c(1,2,3,1,2,3),
                      score = c(5,4,3,4,2,6))
outlier
table(outlier$sex)
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier #이상한 애들은 없애버린다.

outlier$score <- ifelse(outlier$score > 5, NA , outlier$score)
outlier

outlier %>% 
  filter(!is.na(sex)&!is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))


mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats

mpg$hwy <-  ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg%>%
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))

mean

mpg <- as.data.frame(ggplot2::mpg) # mpg 데이터 불러오기
mpg[c(10, 14, 58, 93), "drv"] <- "k" # drv 이상치 할당
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)


table(mpg$drv)

mpg$drv <- ifelse(!mpg$drv %in% c("4", "f", "r") ,NA,mpg$drv )
boxplot(!is.na(mpg$drv))
mpg %>% 
  filter(!is.na(drv)&!is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(sty))
