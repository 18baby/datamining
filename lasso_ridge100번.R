# 데이터 분리 학습 실습(by HJ) [4/6]
#install.packages("Metrics")
#install.packages("glmnet")
library(dplyr)
library(Metrics)
library(glmnet)

# 데이터는 Rstudio가 github에 연동되면 파일 누르면 다운 가능!!
iFinal_df_1
summary(iFinal_df_1)

df = iFinal_df_1
df = df[, -1:-2]       # 인덱스, playerID 열 제거
summary(df)

# 결과 저장 테이블 (단순 선형 회귀 모델만 적용)
rst.tbl = matrix(NA, 3, 2)
colnames(rst.tbl) = c("lasso", "ridge")
rownames(rst.tbl) = c("mse", "mae", "R2")
rst.tbl

# 결과값 저장 행렬
mse.mat = mae.mat = R2.mat = matrix(NA, 100, 2)

# train, test 범위
n = nrow(df); n
train_n = as.integer(n*0.8); train_n   # train 분리 개수

# 모델 학습 진행
for(i in 1:100){
  # 시드 설정
  set.seed(i)
  set = sample(1:n, train_n)   # train idx 구분
  
  # 데이터 분리
  train_data = df[set, ]    # train 데이터
  test_data = df[-set, ]    # test 데이터
  
  train_x = as.matrix(train_data[, -ncol(train_data)])
  train_y = train_data[[ncol(train_data)]]
  test_x = as.matrix(test_data[, -ncol(test_data)])
  test_y = test_data[[ncol(test_data)]]
  
  # lasso 모델 학습
  lasso_model = glmnet(train_x, train_y, alpha=1)
  # ridge 모델 학습
  ridge_model = glmnet(train_x, train_y, alpha=0)
  
  # 예측값 생성
  lpred_y = predict(lasso_model, newx=test_x)   # lasso 예측값
  rpred_y = predict(ridge_model, newx=test_x)   # ridge 예측값
  real_y = test_y
  
  ltest_mse = mse(real_y, lpred_y)     # mse 계산
  ltest_mae = mae(real_y, lpred_y)     # mae 계산
  ltest_R2 = 1 - ltest_mse/var(real_y) # R^2 계산
  
  rtest_mse = mse(real_y, rpred_y)     # mse 계산
  rtest_mae = mae(real_y, rpred_y)     # mae 계산
  rtest_R2 = 1 - rtest_mse/var(real_y) # R^2 계산
  
  
  # 행렬에 값 추가
  mse.mat[i,1] = ltest_mse
  mae.mat[i,1] = ltest_mae
  R2.mat[i,1] = ltest_R2
  
  mse.mat[i,2] = rtest_mse
  mae.mat[i,2] = rtest_mae
  R2.mat[i,2] = rtest_R2
}

rst.tbl[1,] = apply(mse.mat, 2, median)
rst.tbl[2,] = apply(mae.mat, 2, median)
rst.tbl[3,] = apply(R2.mat, 2, median)

rst.tbl
