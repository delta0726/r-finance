# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 14 Probabilistic Utility
# Theme   : 3.2 Markov chains
# Date    : 2022/00/00
# Page    : P347 - P349
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 マルコフ連鎖の関数定義
# 2 マルコフ連鎖シミュレーション
# 3 推定値の比較確認


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)



# 1 マルコフ連鎖の関数定義 --------------------------------------------------------

# 関数定義
# --- マルコフ連鎖シミュレーション
MCsim <- function(x0 = 0, n, theta0, theta1){
    ans <- vector()
    length(ans) <- n + 1
    ans[1] <- x0
    for(i in 2:(n + 1)){
        ans[i] <- theta0 + theta1 * ans[i-1] + rnorm(1)
        }
    ans
}


# 2 マルコフ連鎖シミュレーション ---------------------------------------------------

# パラメータ設定
theta0 <- 2
theta1 <- 0.5
N <- 10000
x01 <- 14
x02 <- -10

# マルコフ連鎖シミュレーション
mc1 <- MCsim(x0 = x01, n = N, theta0 = theta0, theta1 = theta1)
mc2 <- MCsim(x0 = x02, n = N, theta0 = theta0, theta1 = theta1)

# プロット作成
mc1[1:100] %>%
  plot(type = "l", ylim = range(cbind(mc1, mc2)),
       xlab = "", ylab = "X", main = "Progression of first-order Markov Chain")
lines(mc2[1:100], col = "red")


# 3 推定値の比較確認 -------------------------------------------------------------

## Expected value of stationarity distribution and estimates
EfPop <- theta0 / (1 - theta1)
m <- trunc(N / 2) ## burn-in
EfEst1 <- mean(mc1[-c(1:m)])
c(EfPop, EfEst1)

## Standard error of estimate for first MC
ar1Est <- ar(mc1, order.max = 1)$ar
se1 <- sqrt(var(mc1) / N * (1 + ar1Est) / (1 - ar1Est))
c(EfEst1 - 2 * se1, EfEst1, EfEst1 + 2 * se1)

## Variance of stationarity distribution and estimate
VarfPop <- 1 / (1 - theta1^2)
VarfEst1 <- 1 / (1 - ar1Est^2)
c(VarfPop, VarfEst1)
