# lasso, ridge 함수화
library(dplyr)
library(Metrics)
library(glmnet)

# 확인할 DF들
df = iFinal_df_1     # 기본 DF
df = df[,-1:-2]
df1
df3
df4

# 결과 값 저장 테이블 생성
make_rst_tbl = function(){
  # 결과 저장 테이블
  rst.tbl = matrix(NA, 3, 2)
  colnames(rst.tbl) = c("lasso", "ridge")
  rownames(rst.tbl) = c("mse", "mae", "R2")
  return(rst.tbl)
}

# train, test 범위
n = nrow(df); n
train_n = as.integer(n*0.8); train_n   # train 분리 개수


get_result = function(df, n, train_n){
  # 결과 저장 테이블 생성
  rst.tbl = make_rst_tbl()
  # 결과값 저장 행렬
  mse.mat = mae.mat = R2.mat = matrix(NA, 100, 2)
  
  # 모델 학습 진행
  for(i in 1:100){
    # 시드 설정
    set.seed(i)
    set = sample(1:n, train_n)   # train idx 추출
    
    # 데이터 분리
    train_data = df[set, ]    
    test_data = df[-set, ]   
    
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
  
  # 결과 표 생성
  rst.tbl[1,] = apply(mse.mat, 2, median)
  rst.tbl[2,] = apply(mae.mat, 2, median)
  rst.tbl[3,] = apply(R2.mat, 2, median)
  
  return(rst.tbl)
}

# df별 전체 표 생성
system.time(get_result(df, n, train_n))      # 시간 측정 -> 6.12초
df.tbl = get_result(df, n, train_n); df.tbl
df1.tbl = get_result(df1, n, train_n); df1.tbl
df3.tbl = get_result(df3, n, train_n); df3.tbl
df4.tbl = get_result(df4, n, train_n); df4.tbl

