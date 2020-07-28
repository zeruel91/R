#### 07-1 ####

## -------------------------------------------------------------------- ##
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df

is.na(df)               # °áÃøÄ¡ È®ÀÎ
table(is.na(df))        # °áÃøÄ¡ ºóµµ Ãâ·Â
table(is.na(df$sex))    # sex °áÃøÄ¡ ºóµµ Ãâ·Â
table(is.na(df$score))  # score °áÃøÄ¡ ºóµµ Ãâ·Â

mean(df$score)  # Æò±Õ »êÃâ
sum(df$score)   # ÇÕ°è »êÃâ


## -------------------------------------------------------------------- ##
library(dplyr)                # dplyr ÆĞÅ°Áö ·Îµå
df %>% filter(is.na(score))   # score°¡ NAÀÎ µ¥ÀÌÅÍ¸¸ Ãâ·Â
df %>% filter(!is.na(score))  # score °áÃøÄ¡ Á¦°Å

df_nomiss <- df %>% filter(!is.na(score))  # score °áÃøÄ¡ Á¦°Å
mean(df_nomiss$score)                      # score Æò±Õ »êÃâ
sum(df_nomiss$score)                       # score ÇÕ°è »êÃâ

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))  # score, sex °áÃøÄ¡ Á¦°Å
df_nomiss                                                # Ãâ·Â

df_nomiss2 <- na.omit(df)  # ¸ğµç º¯¼ö¿¡ °áÃøÄ¡ ¾ø´Â µ¥ÀÌÅÍ ÃßÃâ
df_nomiss2                 # Ãâ·Â


## -------------------------------------------------------------------- ##
mean(df$score, na.rm = T)  # °áÃøÄ¡ Á¦¿ÜÇÏ°í Æò±Õ »êÃâ
sum(df$score, na.rm = T)   # °áÃøÄ¡ Á¦¿ÜÇÏ°í ÇÕ°è »êÃâ

exam <- read.csv("csv_exam.csv")  # µ¥ÀÌÅÍ ºÒ·¯¿À±â
exam[c(3, 8, 15), "math"] <- NA   # 3, 8, 15ÇàÀÇ math¿¡ NA ÇÒ´ç
exam

exam %>% summarise(mean_math = mean(math))  # math Æò±Õ »êÃâ

# math °áÃøÄ¡ Á¦¿ÜÇÏ°í Æò±Õ »êÃâ
exam %>% summarise(mean_math = mean(math, na.rm = T))  

exam %>% summarise(mean_math = mean(math, na.rm = T),      # Æò±Õ »êÃâ
                   sum_math = sum(math, na.rm = T),        # ÇÕ°è »êÃâ
                   median_math = median(math, na.rm = T))  # Áß¾Ó°ª »êÃâ


## -------------------------------------------------------------------- ##
mean(exam$math, na.rm = T) # °áÃøÄ¡ Á¦¿ÜÇÏ°í math Æò±Õ »êÃâ

exam$math <- ifelse(is.na(exam$math), 55, exam$math)  # math°¡ NA¸é 55·Î ´ëÃ¼
table(is.na(exam$math))                               # °áÃøÄ¡ ºóµµÇ¥ »ı¼º
exam                                                  # Ãâ·Â
mean(exam$math)                                       # math Æò±Õ »êÃâ


#### 07-2 ####

## -------------------------------------------------------------------- ##
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier

table(outlier$sex)
table(outlier$score)

# sex°¡ 3ÀÌ¸é NA ÇÒ´ç
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier

# score°¡ 5º¸´Ù Å©¸é NA ÇÒ´ç
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score))


## -------------------------------------------------------------------- ##
boxplot(mpg$hwy)

# »óÀÚ ±×¸² Åë°èÄ¡ Ãâ·Â
boxplot(mpg$hwy)$stats            

# 12~37 ¹ş¾î³ª¸é NA ÇÒ´ç
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)

# °áÃøÄ¡ È®ÀÎ
table(is.na(mpg$hwy))  

mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))


## -------------------------------------------------------------------- ##
## 1.°áÃøÄ¡ Á¤Á¦ÇÏ±â

# °áÃøÄ¡ È®ÀÎ
table(is.na(df$score))

# °áÃøÄ¡ Á¦°Å
df_nomiss <- df %>% filter(!is.na(score))

# ¿©·¯ º¯¼ö µ¿½Ã¿¡ °áÃøÄ¡ Á¦°Å
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))

# ÇÔ¼öÀÇ °áÃøÄ¡ Á¦¿Ü ±â´É ÀÌ¿ëÇÏ±â
mean(df$score, na.rm = T)
exam %>% summarise(mean_math = mean(math, na.rm = T))


## 2.ÀÌ»óÄ¡ Á¤Á¦ÇÏ±â

# ÀÌ»óÄ¡ È®ÀÎ
table(outlier$sex)

# °áÃø Ã³¸®
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)

# boxplotÀ¸·Î ±Ø´ÜÄ¡ ±âÁØ Ã£±â
boxplot(mpg$hwy)$stats

# ±Ø´ÜÄ¡ °áÃø Ã³¸®
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)



## -------------------------------------------------------------------- ##
boxplot(mpg$hwy)

# ?ƒ? ê·¸ë¦¼ ?†µê³„ì¹˜ ì¶œë ¥
boxplot(mpg$hwy)$stats            

# 12~37 ë²—ì–´?‚˜ë©? NA ?• ?‹¹
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)

# ê²°ì¸¡ì¹? ?™•?¸
table(is.na(mpg$hwy))  

mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))


## -------------------------------------------------------------------- ##
## 1.ê²°ì¸¡ì¹? ? •? œ?•˜ê¸?

# ê²°ì¸¡ì¹? ?™•?¸
table(is.na(df$score))

# ê²°ì¸¡ì¹? ? œê±?
df_nomiss <- df %>% filter(!is.na(score))

# ?—¬?Ÿ¬ ë³€?ˆ˜ ?™?‹œ?— ê²°ì¸¡ì¹? ? œê±?
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))

# ?•¨?ˆ˜?˜ ê²°ì¸¡ì¹? ? œ?™¸ ê¸°ëŠ¥ ?´?š©?•˜ê¸?
mean(df$score, na.rm = T)
exam %>% summarise(mean_math = mean(math, na.rm = T))


## 2.?´?ƒì¹? ? •? œ?•˜ê¸?

# ?´?ƒì¹? ?™•?¸
table(outlier$sex)

# ê²°ì¸¡ ì²˜ë¦¬
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)

# boxplot?œ¼ë¡? ê·¹ë‹¨ì¹? ê¸°ì?€ ì°¾ê¸°
boxplot(mpg$hwy)$stats

# ê·¹ë‹¨ì¹? ê²°ì¸¡ ì²˜ë¦¬
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)

