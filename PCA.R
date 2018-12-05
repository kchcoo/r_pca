#그래프 작성
#시계열

#각 변수의 합을 구해서 끝 컬럼으로 작성(ex. 이온합 등등)
#이온합
for(i in 1:613){
  df_pca$ion_sum[i]<-sum(df_pca[i,14:24])
}
#금속합
for(i in 1:613){
  df_pca$metal_sum[i]<-sum(df_pca[i,25:36])
}
#탄소는 TC자료 사용

df_pca$date<-as.Date(df_pca$date, tz="Asia/Seoul", origin="1970-01-01") #날짜형식으로 바꾸기

ggplot(data = df_pca, aes(x=date, y=ion_sum, colour=site, shape=site))+geom_point()
ggplot(data = df_pca, aes(x=date, y=metal_sum, colour=site, shape=site))+geom_point()
ggplot(data = df_pca, aes(x=date, y=PM, colour=site, shape=site))+geom_point()

#각 사이트별로 pairplot을 그릴 수 있나?
#====================================================================
#PCA(주성분분석)

df_pca<-read.csv("df_pca.csv", header = T)
ion_cor<-round(cor(df_pca[,14:26]), digits = 3)
metal_cor<-round(cor(df_pca[,27:38]), digits = 3)
carbon_cor<-round(cor(df_pca[,39:41]), digits = 3)
all_cor<-round(cor(df_pca[,14:39]), digits = 3)
weather_cor<-round(cor(df_pca[,c(3,10)]), digits = 3)


ion_prcomp<-prcomp(df_pca[,14:24], scale. = T)    #주성분 3개
metal_prcomp<-prcomp(df_pca[,25:36], scale. = T)  #주성분 2개
#Fe, Al, Ca, Mg의 주성분이 너무 큼 제외시켜서 분석해야 하는지? 스케일 표준화를 하지 않아서 생긴 문
carbon_prcomp<-prcomp(df_pca[,37:39], scale. = T) #주성분 1개
material_prcomp<-prcomp(df_pca[,14:39], scale. = T)          #주성분 3~4개
weather_prcomp<-prcomp(df_pca[,c(3,10)], scale. = T)   #주성분 1개
all_prcomp<-prcomp(df_pca[,c(3,10,14:39)], scale. = T) #주성분 3~4개

plot(prcomp(df_pca[,14:24], scale. = T), type="l", sub="Scree plot")
plot(prcomp(df_pca[,25:36], scale. = T), type="l", sub="Scree plot")
plot(prcomp(df_pca[,37:39], scale. = T), type="l", sub="Scree plot")
plot(prcomp(df_pca[,14:39], scale. = T), type="l", sub="Scree plot")
plot(prcomp(df_pca[,c(3,10)], scale. = T), type="l", sub="Scree plot")
plot(prcomp(df_pca[,c(3,10,14:39)], scale. = T), type="l", sub="Scree plot")

#what's the differences between PCA and PMF model? 요인분석의 일종?
#append=T 옵션으로 하나씩 추가해서 파일 작성가능 단, write.csv가 아닌 write.table로 작성해야함

all_prcomp_As<-prcomp(df_pca[df_pca$site=="Ansan",c(3,10,14:39)], scale. = T) #안산 전체 주성분 분석
all_prcomp_Pt<-prcomp(df_pca[df_pca$site=="Pyeongteak",c(3,10,14:39)], scale. = T) #평택 전체 주성분 분석
all_prcomp_Sw<-prcomp(df_pca[df_pca$site=="Suwon",c(3,10,14:39)], scale. = T) #수원 전체 주성분 분석
all_prcomp_Uw<-prcomp(df_pca[df_pca$site=="Uiwang",c(3,10,14:39)], scale. = T) #의왕 전체 주성분 분석
all_prcomp_Ujb<-prcomp(df_pca[df_pca$site=="Uijeongbu",c(3,10,14:39)], scale. = T) #의정부 전체 주성분 분석
all_prcomp_SU<-prcomp(df_pca[df_pca$site=="Suwon" | df_pca$site=="Uiwang", c(3,10,14:39)], scale. = T) # 수원 의왕 전체 주성분 분석

#자료를 잘랐더니 0만 있는 컬럼이 나와서 Li, PO4자료 삭제 
#주성분분석 결과가 사이트마다 조금씩 달라보이기는 하지만 주성분이 너무 많은 문제가 있음 변수의 숫자 자체를 조금 줄여볼 필요가 있음
#다중공선성의 문제로 최고온도, 최저온도, 최고풍속을 없앰

#여전히 주성분으로 설명될 수 있는 변수가 너무 많은 경향이 있음
#일단 이온, 중금속, 탄소, 각각의 주성분 분석과 하나 내지 둘의 조합을 통해 분석 범위를 좁은곳에서 넓은 곳으로 확장하고자 함 

metal_prcomp_As<-prcomp(df_pca[df_pca$site=="Ansan", c(27:38)], scale. = T)
metal_prcomp_Pt<-prcomp(df_pca[df_pca$site=="Pyeongteak", c(27:38)], scale. = T)

#전체 자료를 대상으로 주성분분석을 실행하여 변수를 site를 포함하여
#계절적, 요일적, 강우여부(NA처리 여부), 최대풍속 얼마 이상 이하, 풍향의 범위(이건 open air의 plot으로 구분해볼것) 등등의 이항변수화 시켜 분석할것
#주성분분석의 rotation 해석법 알아볼 것 