qqnorm(Cl_log, main = "Q-Q plot of loged Cl")
qqline(Cl_log)
qqnorm(Cl_sq, main = "Q-Q plot of sqrted Cl")
qqline(Cl_sq)
qqnorm(df$Cl_ion, main = "Q-Q plot of Cl")
qqline(df$Cl_ion)

NO2_log<-log(df$NO2_ion+1)
NO2_sq<-sqrt(df$NO2_ion)

qqnorm(NO2_log, main = "Q-Q plot of loged NO2")
qqline(NO2_log)
qqnorm(NO2_sq, main = "Q-Q plot of sqrted NO2")
qqline(NO2_sq)
qqnorm(df$NO2_ion, main = "Q-Q plot of NO2")
qqline(df$NO2_ion)

unique(df$site_No.)
for (i in 1:4){
  assign(paste0("no.",unique(df$site_No.)[i]), subset(df,df$site_No.==unique(df_$site_No.)[i]))
}

df2[df2$NO2_ion==0 & is.na(df2$NO2_ion)==F, "NO2_ion"]<-NA

df_$date<-as.Date(df_$date, tz="Asia/Seoul", origin="1970-01-01") #날짜형식으로 바꾸기
df<-df[, !names(df) %in% c("Pb")] #Pb라는 이름을 가진 컬럼을 제외하기

#11/3 이온및 중금속 데이터들의 표준화 시도 
#scale() 실행시 결과값이 행렬(matrix)로 도출됨 
#rm(df.,df.1,df1,m,new,b0,bf,dd) 여러 변수를 지울때는 쉼표로 이어주기만 하면 된다 

for(i in 29:41){
  a<-scale(df[i])
  b<-paste0("z.",colnames(df[i]))
  colnames(a)<-b
  df<-cbind(df,a)
}

#181111 의문점 또는 해야할 것
#NA를 0으로 만드는 법(강수량) & NA값은 어떻게 처리해야하는지 
#새로운 변수 만들어서 컬럼으로 합치기
#Date 형식은 요일도 구분할 수 있나?
#NA가 있는 행(11:15, 104, 245) 삭제 <- 처리됨


