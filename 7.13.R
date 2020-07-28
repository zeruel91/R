english <- c(90, 80, 60, 70)
english
math <- c(50, 60, 100, 20)
math
#<- : Alt -
df_midterm <- data.frame(english, math)
df_midterm


class <- c(1,1,2,2)
class

df_midterm <- data.frame(english, math, class)
df_midterm

mean(df_midterm$english) #평균
#한번에
df_midterm <- data.frame(english = c(90,80,60, 70),
math = c(50,60,100,20),class = (1,1,2,2))
df_midterm
mean(df_midterm$class)  # mean(대상 $ 속한 열)




practice <- data.frame(제품 = c('사과','딸기','수박'), 
                       가격=c(1800, 1500, 3000), 
                       판매량 = c (24, 38, 13))

practice

mean(practice$가격)