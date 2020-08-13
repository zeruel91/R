#R 에서 한글 쓰기.
KoNLP

'''
# windows 사용자라면!
# rtools 설치
# https://cran.r-project.org/bin/windows/Rtools/index.html
# Rtools35.exe (recommended) 다운로드 후 실행
# 경로 변경 없이 아래 경로(c:/Rtools)대로 설치해주세요.
# 혹시 경로를 변경하신다면 잘 기억해주세요!
# R 64bit 실행(rstudio 실행도 가능) 

# java, rJava 설치 install.packages("multilinguer")
# 이때 mac 사용자는 데스크탑 비밀번호를 물어봅니다. 입력해줘야 설치가 진행됩니다.
library(multilinguer)
install_jdk()
# 위 함수에서 에러가 발생하면 알려주세요
# https://github.com/mrchypark/multilinguer/issues

# 의존성 패키지 설치
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")

# github 버전 설치
install.packages("remotes")
# 64bit 에서만 동작합니다.
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))'''


install.packages("multilinguer")

library("multilinguer")

install_jdk()

rm(list =ls()) #변수창 삭제.

install.packages(c("stringr","hash","tau","Sejong","RSQLite","devtools"),type = "binary")

install.packages("remotes")

remotes::install_github("haven-jeon/KONLP",upgrade = "never", INSTALL_opts=c("--no-multiarch"))

install.packages("rvest")
library(rvest)



url <- "https://movie.daum.net/moviedb/grade?movieId=132861&type=netizen&page=1"

htxt <- read_html(url)
htxt

content <- html_nodes(htxt,".desc_review")
content

review <- html_text(content)
review
###################################


url2 <- "https://finance.naver.com/item/sise.nhn?code=005930&page="
all.price <- c()

for (page in 1:20){
  price <- paste(url2,page,sep="") %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    .[1] %>% 
    html_table()
  all.price <- c(all.price,price)  
}






url <- "http://www1.president.go.kr/articles/1827"
content <- html_nodes(htxt,'.cs_body')
content

speach <- html_text(content)
speach



#########################################
# 네이버 영화 리뷰 크롤링
#########################################


library("rvest")

library("R6")

url_base <- 'http://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=159037&target=after&page='



all.reviews <- c()

for(page in 1:20){
  
  url <- paste(url_base,page,sep='',encoding="euc-kr")
  
  # read_html 함수를 사용하여 html 페이지를 htxt 변수에 저장
  
  htxt <- read_html(url)
  
  # html_nodes 함수를 사용하여 list_netizen class에 해당하는 부분을 table 변수에 저자
  
  table <- html_nodes(htxt,'.list_netizen')
  
  # html_nodes 함수를 사용하여 title class에 해당하는 부분을 content 변수에 저자
  
  content <- html_nodes(table, '.title')
  
  # html_text 함수를 사용하여 text 부분을 reviews 변수에 저장
  
  reviews <- html_text(content)
  
  if(length(reviews)==0){break}
  
  all.reviews <- c(all.reviews, reviews)
  
  print(page)
  
}


all.reviews



#########################################
# 다음 영화 리뷰 크롤링
#########################################

# html 주소를 url에 저장

url <- 'http://movie.daum.net/moviedb/grade?movieId=112942&type=netizen&page=1'

# read_html 함수를 사용하여 html 페이지를 htxt 변수에 저장

htxt <- read_html(url)

# html_nodes 함수를 사용하여 desc_review class에 해당하는 부분을 content 변수에 저장

content <- html_nodes(htxt,'.desc_review')

# html_text 함수를 사용하여 text 부분을 reviews 변수에 저장

review <- html_text(content)

review








#########################################
# 삼성전자 주가 크롤링
#########################################



# 주소를 복사하여 url_base에 저장

url_base <- 'http://finance.naver.com/item/sise_day.nhn?code=005930&page='

all.price <- c()



# 20쪽까지 자료 가져오기

for (page in 1:20){
  
  price <- paste(url_base,page,sep='') %>%
    
    read_html() %>%
    
    html_nodes('table') %>%
    
    .[1] %>%
    
    html_table()
  
  all.price <- c(all.price,price)
  
}

# 크롤링한 자료 확인

all.price[[1]]









#########################################
# 워드 클라우드 
#########################################



# html 주소를 url에 저장

url <- 'https://www1.president.go.kr/articles/1827'


# read_html 함수를 사용하여 html 페이지를 htxt 변수에 저장

htxt <- read_html(url)


# html_nodes 함수를 사용하여 cs_body class에 해당하는 부분을 content 변수에 저장

# text left cb text_wrap motion fadeIn visible 클래스로는 안되드라구요. 왜 그럴까. ㅡ.ㅡ;;

content <- html_nodes(htxt,'.cs_body')

# html_text 함수를 사용하여 text 부분을 speach 변수에 저장

speach <- html_text(content)

speach


library(KoNLP)
install.packages("wordcloud")
library(wordcloud)

library(RColorBrewer)


## 세종사전 설치


useSejongDic()



## Step2. 데이터 정제

## 크롤링된 데이터에서 단어 추출하기

## 명사 추출 함수인 extracNoun 함수 사용

pword <- sapply(speach,extractNoun,USE.NAMES = F)

## 필터링을 위해 unlist 함수를 사용해서 저장

data <- unlist(pword)

## 글자수 2개 이상만 취급

data <- Filter(function(x){nchar(x)>=2},data)




## 불필요한 데이터 처리

## 한 번 이상의 숫자

data <- gsub("\\d+","",data)

## 새로운 라인

data <- gsub("\\n","",data)

## 줄 끝 문자를 제외한 모든 문자

data <- gsub("\\.","",data)

data <- gsub("\n","",data)

data <- gsub(" ","",data)

data <- gsub("-","",data)

data





## 빈도표 작성

data_cnt <- table(data)

## 내림차순 정렬 후 상위 20개 표시

head(sort(data_cnt, decreasing=T), 20)



## 그래픽 구현창(팝업창) 생성

palete <- brewer.pal(9, "Set1")

x11()

## 워드클라우드 실행

wordcloud(
  
  names(data_cnt),
  
  freq=data_cnt,
  
  scale=c(5,1),
  
  rot.per=0.5,
  
  min.freq=2,
  
  random.order=F,
  
  random.color=T,
  
  colors=palete
  
)
#___________________________________________


url <- 'https://terms.naver.com/entry.nhn?docId=1083492&cid=40942&categoryId=33384'


# read_html 함수를 사용하여 html 페이지를 htxt 변수에 저장

htxt <- read_html(url)


# html_nodes 함수를 사용하여 cs_body class에 해당하는 부분을 content 변수에 저장

# text left cb text_wrap motion fadeIn visible 클래스로는 안되드라구요. 왜 그럴까. ㅡ.ㅡ;;

content <- html_nodes(htxt,'.cs_body')

# html_text 함수를 사용하여 text 부분을 speach 변수에 저장

speach <- html_text(content)

speach


library(KoNLP)
install.packages("wordcloud")
library(wordcloud)

library(RColorBrewer)


## 세종사전 설치


useSejongDic()



## Step2. 데이터 정제

## 크롤링된 데이터에서 단어 추출하기

## 명사 추출 함수인 extracNoun 함수 사용

pword <- sapply(speach,extractNoun,USE.NAMES = F)

## 필터링을 위해 unlist 함수를 사용해서 저장

data <- unlist(pword)

## 글자수 2개 이상만 취급

data <- Filter(function(x){nchar(x)>=2},data)




## 불필요한 데이터 처리

## 한 번 이상의 숫자

data <- gsub("\\d+","",data)

## 새로운 라인

data <- gsub("\\n","",data)

## 줄 끝 문자를 제외한 모든 문자

data <- gsub("\\.","",data)

data <- gsub("\n","",data)

data <- gsub(" ","",data)

data <- gsub("-","",data)

data





## 빈도표 작성

data_cnt <- table(data)

## 내림차순 정렬 후 상위 20개 표시

head(sort(data_cnt, decreasing=T), 20)



## 그래픽 구현창(팝업창) 생성

palete <- brewer.pal(9, "Set1")

x11()

## 워드클라우드 실행

wordcloud(
  
  names(data_cnt),
  
  freq=data_cnt,
  
  scale=c(5,1),
  
  rot.per=0.5,
  
  min.freq=2,
  
  random.order=F,
  
  random.color=T,
  
  colors=palete
  
)
