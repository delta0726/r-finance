# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 6 Suitable Distribution for returns
# Theme   : 7.1 VaR for a Single Stock
# Date    : 2022/11/12
# Page    : P82 - P84
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# - QCOM株式のVaRのバックテストを行う


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 ローリングでVaRの計算
# 3 データ整理
# 4 プロット作成


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(lmomco)
library(FRAPO)


# データロード
data(SP500)

# データ確認
# --- 週次の株価データ
SP500 %>% class()
SP500[, 1:5] %>% head()


# 1 データ準備 -----------------------------------------------------------------

# リターン系列の作成
# --- 週次リターン
L <-
  SP500 %>%
    rownames_to_column("Period") %>%
    select(Period, QCOM) %$%
    timeSeries(data = QCOM, charvec = Period) %>%
    returnseries(method = "discrete", trim = TRUE) %>%
    multiply_by(-1)


# 2 ローリングでVaRの計算 -------------------------------------------------------

# ＜ポイント＞
# - 104週(2年間)の週次リターンからVaRをローリングで計算
#   --- 一般化ラムダ分布(GLD)と正規分布


# パラメータ設定
# --- 信頼区間
# --- 終了点
# --- 開始点
level <- 0.99
ep <- 104:length(L)
sp <- 1:length(ep)

# 格納用オブジェクト
VaR <- matrix(NA, ncol = 2, nrow = length(ep))

# シミュレーション
# --- VaRの計算
i <- 1
for(i in seq_along(sp)){
  # リターン抽出
  x <- L[sp[i]:ep[i]]

  # モデル構築
  # --- 一般化ラムダ分布
  fit <- x %>% lmom.ub() %>% pargld()

  # VaRの算出
  # --- 一般化ラムダ分布
  # --- 正規分布
  VaRGld <- quagld(level, fit)
  VaRNor <- qnorm(level, mean(x), sd(x))
  VaR[i, ] <- c(VaRGld, VaRNor)

  # 結果出力
  print(paste("Result for", ep[i], ":", VaRGld, "and", VaRNor))
}

# 結果確認
VaR %>% head()
VaR %>% dim()


# 3 データ整理 ----------------------------------------------------------------

# プロット用データ作成
Res <-
  L[105:length(L)] %>%
    cbind(VaR[-nrow(VaR), ]) %>%
    set_colnames(c("Loss", "VaRGld", "VaRNor"))

# 確認
Res %>% head()


# 4 プロット作成 ---------------------------------------------------------------

# ＜ポイント＞
# - GLDモデルによるVaRの軌跡は正規分布の場合よりも不安定
#   --- バックテスト期間の終わりに向けて GLD VaR が急激に低下している
#   --- ボラティリティの低下と密接に関連していますが、外れ値に対するモーメント推定の感度も反映
#   --- 正規分布に従った VaR 測定値は保守的すぎる


# 散布図
Res[, "Loss"] %>%
  plot(type = "p", xlab = "Time Index",
       ylab = "Losses in percent", pch = 19, cex = 0.5,
       ylim = c(-15, max(Res)))

# 原点
abline(h = 0, col = "grey")

# ラインチャート
lines(Res[, "VaRGld"], col = "blue", lwd = 2)
lines(Res[, "VaRNor"], col = "red", lwd = 2)

# 凡例追加
legend("bottomleft", legend = c("Losses", "VaR GLD", "VaR Normal"),
       col = c("black", "blue", "red"),
       lty = c(NA, 1, 1), pch = c(19, NA, NA), bty = "n")
