library(ggplot2)
library(dplyr)
#그래프 배경생성
ggplot(data = mpg, aes(x = displ, y = hwy))
#그래프 점(geom_point -> 점박이) 투사
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
#x 축 제한 3~6 (값을 포함함)
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3,6)
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3,6) + ylim(10,30)


ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)

ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point()

#ggplot2 < 패키지
#ggplot < ggplot2 안에있는 함수
ggplot(data = midwest, aes(x = poptotal, y = popasian))+
  geom_point()+
  xlim(0,500000)+
  ylim(0,10000)

#집단별 평균표 만들기
df_mpg <- mpg  %>% group_by(drv)  %>% summarise(mean_hwy = mean(hwy))
df_mpg
mpg
#geom_col 막대 그래프
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()

#재정렬 reorder 


ggplot(data = df_mpg,
       aes(x = reorder(drv, -mean_hwy),#-mean, 내림차순순
                          y = mean_hwy)) +
  geom_col()
#x 값만 넣어도 표현가능, y 는 각 값의 수치만 뜸
ggplot(data = mpg, aes(x = drv)) + geom_bar() +#ylim(0,200)
#x 축이 연속 변수일떄.
ggplot(data = mpg, aes(x = hwy)) + geom_bar()

#exercise

df <- mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class =="suv") %>% 
  mutate(tot = (cty+hwy)/2) %>% 
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)
  
ggplot(data = df, aes(x = reorder(manufacturer, -mean_cty)))

ggplot(data = mpg, aes(x = reorder(class, -mean_count)) + geom_bar()


       