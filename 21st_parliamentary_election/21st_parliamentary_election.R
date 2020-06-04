library(readxl) #readxl library
early.vote<-read_excel("~/R-Programming/data/제21대 국회의원선거_성별·연령별_사전투표자수(비례대표 기준).xlsx")
early.vote #사전투표를 early.vote이라고 하겠습니다
View(early.vote) #표 자세히 보기

names(early.vote) #열 이름 보기
names(early.vote)<-c("시도명","선거구명","구시군명","구분","계","age19세이하","age20대","age30대","age40대","age50대","age60대","age70대이상")
#열 이름에서 나이대 컬럼를 더욱 보기 편하게 이름을 변경하겠습니다.
names(early.vote) #나이대 컬럼의 이름이 제대로 바뀌었나 확인

str(early.vote) #사전투표 data.frame의 구조 확인인
early.vote$구분<-factor(early.vote$구분) #남자, 여자는 factor로 바꿔주기
summary(early.vote$구분) #남자 여자 summary로 확인하기->각 지역구별로 구분하기 때문에 당연히 남자 여자 수는 같아야함
early.vote$선거구명<-factor(early.vote$선거구명) #선거구명도 factor로 바꿔주기
summary(early.vote$선거구명) #선거구명 중에서 51개짜리의 '소계'가 있다. 나중에 분석할 때 소계는 제외해주기
early.vote$구시군명<-factor(early.vote$구시군명) #구시구명도 factor로 바꿔주기
summary(early.vote$구시군명) #구시군명 중에서 126개짜리의 '소계'가 있다. 그리고 결측치 값도 51개가 있다. 나중에 분석할 때 소계는 제외해주기

early.male<-subset(early.vote, early.vote$구분=="남" & early.vote$선거구명!="소계" & early.vote$구시군명!="소계")
#사전투표를 한 남자는 전체 데이터에서 구분이 '남'이고 선거구명과 구시군명에 '소계'가 없어야한다.
early.male #잘 걸러졌나 확인
early.male<-sum(early.male$계) #다 더해주기
early.male #남성의 총 사전투표 수

early.female<-subset(early.vote, early.vote$구분=="여" & early.vote$선거구명!="소계" & early.vote$구시군명!="소계")
#사전투표를 한 여자는 전체 데이터에서 구분이 '여'이고 선거구명과 구시군명에 '소계'가 없어야한다.
early.female #잘 걸러졌나 확인
early.female<-sum(early.female$계) #다 더해주기
early.female #여성의 총 사전투표 수

early.sex<-c(early.male, early.female) #남성과 여성의 총 사전투표 수
early.sex #남성과 여성의 총 사전투표 수 확인하기

early.sex.rate<-round((early.sex/sum(early.sex))*100, 2) #벡터를 전체 값으로 나누어서 % 비율 구하기
early.sex.rate #값 확인

pie(early.sex.rate, main="전국 사전투표 남여 비율", labels=c("남\n53.47%", "여\n46.53"), col=c("Blue", "Red"), radius=1.3)
#전국 사전투표 남여 비율을 pie chart로 나타내었다.

early.age10<-subset(early.vote$age19세이하, early.vote$선거구명!="소계" & early.vote$구시군명!="소계" & early.vote$구분!="계")
# 19세 이하인 컬럼에서 선거구명과 구시군명이 소계가 아니고 구분이 계(남+여)인 것을 골라내기
early.age10<-sum(early.age10) #값을 다 더하기->전국 19세 이하 사전 투표 수
early.age10 #값 확인하기

early.age20<-subset(early.vote$age20대, early.vote$선거구명!="소계" & early.vote$구시군명!="소계" & early.vote$구분!="계")
# 20대 컬럼에서 선거구명과 구시군명이 소계가 아니고 구분이 계(남+여)인 것을 골라내기
early.age20<-sum(early.age20) #값을 다 더하기->전국 20대 사전 투표 수
early.age20 #값 확인하기

early.age30<-subset(early.vote$age30대, early.vote$선거구명!="소계" & early.vote$구시군명!="소계" & early.vote$구분!="계")
# 30대 컬럼에서 선거구명과 구시군명이 소계가 아니고 구분이 계(남+여)인 것을 골라내기
early.age30<-sum(early.age30) #값을 다 더하기->전국 30대 사전 투표 수
early.age30 #값 확인하기

early.age40<-subset(early.vote$age40대, early.vote$선거구명!="소계" & early.vote$구시군명!="소계" & early.vote$구분!="계")
# 40대 컬럼에서 선거구명과 구시군명이 소계가 아니고 구분이 계(남+여)인 것을 골라내기
early.age40<-sum(early.age40) #값을 다 더하기->전국 40대 사전 투표 수
early.age40 #값 확인하기

early.age50<-subset(early.vote$age50대, early.vote$선거구명!="소계" & early.vote$구시군명!="소계" & early.vote$구분!="계")
# 50대 컬럼에서 선거구명과 구시군명이 소계가 아니고 구분이 계(남+여)인 것을 골라내기
early.age50<-sum(early.age50) #값을 다 더하기->전국 50대 사전 투표 수
early.age50 #값 확인하기

early.age60<-subset(early.vote$age60대, early.vote$선거구명!="소계" & early.vote$구시군명!="소계" & early.vote$구분!="계")
# 60대 컬럼에서 선거구명과 구시군명이 소계가 아니고 구분이 계(남+여)인 것을 골라내기
early.age60<-sum(early.age60) #값을 다 더하기->전국 60대 사전 투표 수
early.age60 #값 확인하기

early.age70<-subset(early.vote$age70대이상, early.vote$선거구명!="소계" & early.vote$구시군명!="소계" & early.vote$구분!="계")
# 70대 이상인 컬럼에서 선거구명과 구시군명이 소계가 아니고 구분이 계(남+여)인 것을 골라내기
early.age70<-sum(early.age70) #값을 다 더하기->전국 70대 이상인 사전 투표 수
early.age70 #값 확인하기

early.age<-c(early.age10,early.age20,early.age30,early.age40,early.age50,early.age60,early.age70) #하나의 벡터로 만들어 주기
early.age #나이대별 사전 투표 수 벡터값 확인

early.age.rate<-round((early.age/sum(early.age))*100, 2) #벡터를 전체 값으로 나누어서 % 비율 구하기
early.age.rate # 값 확인

pie(early.age.rate, main="전국 사전투표 나이대별 비율", labels=c("19세 이하\n2.24%", "20대\n14.65%", "30대\n12.73%", "40대\n17.67%", "50대\n21.94%", "60대\n18.33%", "70대 이상\n12.44%"), col=rainbow(length(early.age)), radius=1.3)
#전국 사전투표를 나이대별 비율로 나타내는 pie chart를 그려보았다.
#칼럼이 7개여서 무지개색도 7개니깐 무지개색으로 하면 이쁘게 더욱 시각화가 잘 될거같아 무지개색으로 했습니다.

early.sejong.univ<-subset(early.vote, early.vote$선거구명=="광진구을")
#세종대학교는 광진구에 위치해 있고 선거구 지역구는 광진을이다.
#그래서 세종대학교가 위치해 있는 광진을의 사전투표에 대한 분석을 하려고 한다.
#광진을 지역구의 데이터만 가져와 보았다.
early.sejong.univ #광진을 데이터 확인

early.sejong.univ.sex<-early.sejong.univ[-1,"계"] #남여 비율 확인 위해 '계'는 삭제한다
early.sejong.univ.sex #값 확인
early.sejong.univ.sex<-unlist(early.sejong.univ.sex) #unlist 처리를 해준다다
is.vector(early.sejong.univ.sex) #값이 vector인지 확인해준다

early.sejong.univ.sex<-round((early.sejong.univ.sex/sum(early.sejong.univ.sex))*100, 2) #벡터를 전체 값으로 나누어서 % 비율 구하기
early.sejong.univ.sex #값 확인

pie(early.sejong.univ.sex, main="광진을(세종대학교 위치) 사전투표 남여 비율", labels=c("남\n50.27%", "여\n49.73%"), col=c("Blue", "Red"), radius=1.3)
#세종대학교가 위치해있는 광진을 지역구의 사전투표 남여 비율을 pie chart로 그려보았다.

early.sejong.univ #광진을 데이터 확인
#세종대학교가 위치한 광진을 지역구의 나이대별 사전투표 비율을 분석해보려한다.
early.sejong.univ.age<-early.sejong.univ[1,] #계(남+여)는 삭제한다
early.sejong.univ.age #값 확인
early.sejong.univ.age<-early.sejong.univ.age[,6:12] #나이별 칼럼만 불러온다
early.sejong.univ.age #값 확인
early.sejong.univ.age<-unlist(early.sejong.univ.age) #unlist 처리해준다
is.vector(early.sejong.univ.age) #vector인지 확인

early.sejong.univ.age<-round((early.sejong.univ.age/sum(early.sejong.univ.age))*100, 2) #벡터를 전체 값으로 나누어서 % 비율 구하기
early.sejong.univ.age #나이대별 비율 확인인

pie(early.sejong.univ.age, main="광진을(세종대학교 위치) 사전투표 나이대별 비율", labels=c("19세 이하\n1.68%", "20대\n20.33%", "30대\n16.91%", "40대\n16.27%", "50대\n18.71%", "60대\n15.97%", "70대 이상\n10.13%"), col=rainbow(length(early.age)), radius=1.3)
#세종대학교가 위치한 광진을 지역구의 나이대별 비율을 나타내는 pe chart를 그려보았다.
#칼럼이 7개여서 무지개색도 7개니깐 무지개색으로 하면 이쁘게 더욱 시각화가 잘 될거같아 무지개색으로 했습니다.

early.vote #사전투표 데이터 확인

early.vote.area<-subset(early.vote, early.vote$선거구명!="소계"& early.vote$구시군명!="소계" & early.vote$구분=="계")
#각 지역별로 투표 지역구 수가 몇개씩 있는지 알아보고 싶었다.
#전체 데이터에서 선거구명과 구시군명이 '소계'가 아니고 구분이 '계(남+여)'인 것만 골라온다
early.vote.area # 값 확인

early.vote.area<-early.vote.area[,"시도명"] #시도명 데이터만 가져오기
early.vote.area #값 확인

early.vote.area<-unlist(early.vote.area) #unlist 처리
early.vote.area #값 확인

early.vote.area<-factor(early.vote.area) #fatcor로 처리해주기
early.vote.area #값 확인

summary(early.vote.area) #summary로 각 투표 지역구 수가 몇 개 있는지보기기

early.vote.area.freq<-table(early.vote.area) #도수 구하기
early.vote.area.propfreq<-prop.table(early.vote.area.freq) #상대 도수 구하기
early.vote.area.table<-rbind(early.vote.area.freq, early.vote.area.propfreq) #도수 분포표 구하기
early.vote.area.table<-addmargins(early.vote.area.table, margin=2) #합까지 있는 도수 분포표 구하기
early.vote.area.table #도수분포표 확인

early.vote.area.freq<-sort(early.vote.area.freq, decreasing=T) #내림차순으로 정렬

barplot(early.vote.area.freq, ylim=c(0,70), col="Spring Green", main="각 지역별 투표 지역구 수", xlab="지역", ylab="투표 지역구 수")
#각 지역별 투표 지역구 수를 bar chart로 나타내었다.
#색깔은 "Sprinf Green"으로 했는데 색도 이쁘기도 하고, 대한민국 정치에도 봄(Spring)이 오고 파릇파릇(Green)한 정치가 되었으면 하는 바람에서 색깔을 "Spring Green"으로 하였다.

library(readxl) #readxl library
vote<-read_excel("~/R-Programming/data/제21대 국선 지역구 및 비례대표 정당별 득표수 현황(홈페이지 게시).xlsx", sheet="지역구")
vote #데이터 확인

View(vote) #표 자세히 보기

all<-unlist(vote[1,"선거인수"]) #전국 선거인수만 가져오고 unlist 처리하기
actual<-unlist(vote[1,"투표수"]) #전국 투표수만 가져오고 unlist 처리하기
vote.yes<-actual #투표한 유권자 수
vote.no<-(all-actual) #투표하지 않은 유권자 수
vote.yes<-round((vote.yes/all)*100, 2) #투표한 유권자 비율
vote.no<-round((vote.no/all)*100, 2) #투표하지 않은 유권자 비율
vote.yes.no<-c(vote.yes, vote.no) #하나의 벡터로 만들기
vote.yes.no #값 확인
pie(vote.yes.no, main="제 21대 총선 투표 비율", label=c("투표o\n66.24%", "투표x\n33.76%"), col=c("Blue", "Red"), radius=1.3)
#제 21대 총선 투표 비율을 pie chart로 나타내었습니다.

vote #데이터 확인인
korea.city.label<-vote[c(3,11),1] #서울, 경기만 불러옴->서울, 경기가 다른 지역에 비해 인구가 많아 전체적으로 분석하면 서울, 경기를 제외한 지역의 시각화가 어려워 서울, 경기만 따로 분석합니다.
korea.city.label #값 확인
is.vector(korea.city.label) #벡터인지 확인
korea.city.label<-unlist(korea.city.label) #unlist 처리
is.vector(korea.city.label) #벡터인지 확인

korea.city<-vote[c(3,11),c(5:8)] #서울, 경기 지역의 지역구 투표에서 더불어민주당, 미래통합당, 민생당, 정의당의 투표 비율(정당 순서는 정당 기호 순)
korea.city #값 확인
korea.city=t(korea.city) #전치행렬로 저장
korea.city #값 확인

barplot(korea.city, names.arg=korea.city.label, xlim=c(0,30), width=10, main="서울 경기 정당별 지역구 투표 수", xlab="지역", ylab="투표 수", ylim=c(0,7000000), col=c("Blue", "Red", "Green", "Yellow"), legend.text=c("더불어민주당","미래통합당","민생당","정의당"), args.legend=list(x="topright"))
#서울, 경기 지역구 투표의 정당별 투표 수를 bar chart로 나타내었습니다.

vote #데이터 확인
korea.region.label<-vote[c(-1:-3,-11),1] #서울, 경기 지역 제외하고 불러옴
korea.region.label #값 확인
is.vector(korea.region.label) #벡터인지 확인
korea.region.label<-unlist(korea.region.label) #unlist 처리
is.vector(korea.region.label) #벡터인지 확인

korea.region<-vote[c(-1:-3,-11),c(5:8)] #서울, 경기 지역을 제외한 지역구 투표에서 더불어민주당, 미래통합당, 민생당, 정의당의 투표 비율(정당 순서는 정당 기호 순)
korea.region #값 확인
korea.region=t(korea.region) #전치행렬로 저장
korea.region #값 확인

barplot(korea.region, names.arg=korea.region.label, xlim=c(0,65), width=3, main="지역별 정당별 지역구 투표 수\n(서울 경기 제외 )", xlab="지역", ylab="투표 수", ylim=c(0,2000000), col=c("Blue", "Red", "Green", "Yellow"), legend.text=c("더불어민주당","미래통합당","민생당","정의당"), args.legend=list(x="topright"))
#서울, 경기 지역을 제외한 지역구 투표의 정당별 투표 수를 bar chart로 나타내었습니다.
