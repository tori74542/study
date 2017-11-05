library(dplyr)

d <- read.csv("./data3a.csv")

# 一定モデル(4.2) = null model
model_A <- glm(y ~ 1, data = d, family = poisson)
# 施肥効果が影響するモデル(3.5)
model_B <- glm(y ~ f, data = d, family = poisson)
# 体サイズが影響するモデル(3.4.2)
model_C <- glm(y ~ x, data = d, family = poisson)
# 施肥効果と体サイズの双方が影響するモデル(3.6)
model_D <- glm(y ~ x + f, data = d, family = poisson)

# 逸脱度(deviance)の算定, モデルCに着目

# 最大対数尤度は-235.4程度
loglik_C <- logLik(model_C) %>% as.numeric()

# 逸脱度は-2*最大対数尤度…470.8程度
deviance <- -2 * loglik_C

print(model_C)

# Null Deviance:	    89.51 
# Residual Deviance: 84.99 	AIC: 474.8

# 結果には470.8がいない

# 学習データに最も当てはまりの良いモデルとしてfull model,
# 学習データに最も当てはまりの悪いモデルとしてnull modelを考える

# full modelの逸脱度=最小逸脱度を考える
# まずはfull modelの最大対数尤度を計算
loglik_full <- dpois(d$y, lambda = d$y) %>% log %>% sum()

# -2*最大対数尤度が最小逸脱度(385.8)
min_deviance <- -2*loglik_full

# Residual Deviance=残差逸脱度...逸脱度 - full modelの逸脱度は84.99
# モデルのResidual Devianceと一致する
residual_deviance <- deviance - min_deviance

# 残差逸脱度は、最小逸脱度を基準とした当てはまりの悪さを示す


# null modelの逸脱度=残差逸脱度の最大値を考える

print(model_A)
# Null Deviance:	    89.51 
# Residual Deviance: 89.51 	AIC: 477.3

# null modelの最大対数尤度(-237.6432)
loglik_A <- logLik(model_A)
# null modelの逸脱度(475.2864)
null_deviance <- -2 * loglik_A
# 最小逸脱度との差が89.51、モデルのfull devianceと一致する
null_deviance - min_deviance


# 各モデルを使って表4.2を計算する手順まとめ
# 1.logLikで最大対数尤度を算出
# 2.-2*最大対数尤度で逸脱度を算出
# 3.導出した逸脱度と最小逸脱度（フルモデルから導出した逸脱度）の差が残差逸脱度
# 4.kはモデルの$rankで取得できる

calculate_residual_deviance <- function(model, min_deviance){
  # 1
  model_loglik <- logLik(model)
  # 2
  model_deviance <- -2 * model_loglik
  # 3
  model_redidual_deviance <- model_deviance - min_deviance
  # 4
  k <- model$rank
  
  # 各列に名前を付けて表（データフレーム）にする
  result <- data.frame(k        = k,
                       logL     = model_loglik %>% round(1),
                       deviance = model_deviance %>% round(1),
                       residual_deviance = model_redidual_deviance %>% round(1))
  
  return(result)
}


answer <- rbind(calculate_residual_deviance(model_A, min_deviance),
                calculate_residual_deviance(model_B, min_deviance),
                calculate_residual_deviance(model_C, min_deviance),
                calculate_residual_deviance(model_D, min_deviance))

answer %>% View


# ついでにAIC計算 逸脱度 + (2 * パラメータ数)
answer$AIC <- answer$deviance + (2 * answer$k)
