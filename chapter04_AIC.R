
# lambda = 8のポアソン分布(真の母集団)から50個サンプリング
set.seed(1)
y <- rpois(n = 50, lambda = 8) # beta = 2.08
hist(y)
d <- data.frame(y)

# 観測された50個のデータからモデルを組んでみる
model_null <- glm(y ~ 1, data = d, family = poisson) # beta = 2.114
logLik(model_null) # -119.4291


# 50個取ってくる操作を200回繰り返す
set.seed(2)
y2 <- vector()
for (i in 1:50){
  y_sub <- rpois(n = 50, lambda = 8)
  model_sub <- glm(y_sub ~ 1, data = d, family = poisson)
  
  y2 <- c(y2, logLik(model_sub))
}

mean(y2) # -122.6935 ... 最初の50個サンプルから作られたモデルは当てはまりの良さが過大評価されている

# 過大評価か過小評価かは場合による
# ここまでの工程を何回か繰り返してみる
# 1.真の母集団から50個サンプリング
# 2.サンプリング結果からモデルを作成、最大対数尤度を取る
# 3.真の母集団から50個サンプリングして最大対数尤度を計算、200回繰り返して平均を取る（=平均対数尤度）
# 4.2で作成したモデルのβと最大対数尤度,3で作成した平均対数尤度,1のデータ50個を結果データセットとする


evaluate_process <- function(){
  # 1
  y <- rpois(n = 50, lambda = 8)
  # 2
  model_sub <- glm(y ~ 1, data = data.frame(y), family = poisson)
  max_loglik <- logLik(model_sub)
  # 3
  y2 <- vector()
  for (i in 1:50){
    y_sub <- rpois(n = 50, lambda = 8)
    model_sub <- glm(y_sub ~ 1, data = data.frame(y_sub), family = poisson)
    
    y2 <- c(y2, logLik(model_sub))
  }
  ave_loglik <- mean(y2)
  
  result <- data.frame(beta = model_sub$coefficients[[1]],
                       max_loglik = max_loglik,
                       ave_loglik = ave_loglik,
                       y %>% t())
  return(result)
}

data <- data.frame()
for (i in 1:12){
  data <- rbind(data, evaluate_process())
}

data %>% View


