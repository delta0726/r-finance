# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 6 Suitable Distribution for returns
# Theme   : 1 Fitting HPW return to the GHD
# Date    : 2022/11/10
# Page    : P74 - P77
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# - 正規分布は株式リターンの経験的分布よりも尖度低いため不適切（尖度をコントロールできない）
# - HP社の日次リターンをGHDとその特殊なケースであるHYPおよびNIGに適合させる
#   --- GHD：一般化双曲型分布
#   --- HYP：双曲型分布
#   --- NIG：正規逆ガウス分布


# ＜テールリスクとの関係＞
# - VaRやESなどのリスク指標は分布の左側に位置する分位点を用いてテール確率を定義する
#   --- ポートフォリオ構築においては収益分布全体をモデル化する必要がある


# ＜目次＞
# 0 準備
# 1 HP社のリターン系列を作成
# 2 分布モデルによるフィッティング
# 3 カーネル密度プロットによる比較
# 4 QQ-Plotによる比較
# 5 モデル診断による最良分布の確認
# 6 尤度比検定


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(ghyp)
library(timeSeries)
library(fBasics)
library(conflicted)

# コンフリクト解消
conflict_prefer("select", "dplyr")
conflict_prefer("legend", "graphics")


# データロード
data(DowJones30)

# データ確認
DowJones30 %>% class()
DowJones30 %>% glimpse()


# 1 HP社のリターン系列を作成 ----------------------------------------------------

# データ加工
# --- Period列を作成
DowJones30_Mod <-
  DowJones30 %>%
    mutate(Period = as.character(X.Y..m..d)) %>%
    as_tibble()

# 日次リターンの作成
# --- HP社のみを抽出
yret <-
  DowJones30_Mod %>%
    select(Period, HWP) %$%
    timeSeries(data = HWP, charvec = Period) %>%
    log() %>%
    diff() %>%
    multiply_by(100) %>%
    na.omit()

# データ確認
yret %>% length()
yret %>% hist()


# 2 分布モデルによるフィッティング -------------------------------------------------

# 各分布でフィッティング
#   --- GHD：一般化双曲型分布
#   --- HYP：双曲型分布
#   --- NIG：正規逆ガウス分布
ghdfit <- yret %>% fit.ghypuv(symmetric = FALSE, control = list(maxit = 1000), silent = TRUE)
hypfit <- yret %>% fit.hypuv(symmetric = FALSE, control = list(maxit = 1000), silent = TRUE)
nigfit <- yret %>% fit.NIGuv(symmetric = FALSE, control = list(maxit = 1000), silent = TRUE)


# 3 カーネル密度プロットによる比較 --------------------------------------------------

# ＜ポイント＞
# - 経験的分布のX値を元に各分布のY値を計算して分布を比較する
#   --- 経験的分布と正規分布は明らかに尖度が異なることが確認できる
#   --- 一般化双曲型分布(GHD)が最もフィッティングがよさそう


# カーネル密度
# --- HP社のリターン
ef <- yret %>% density()

# xの対応値の取得
#   --- GHD：一般化双曲型分布
#   --- HYP：双曲型分布
#   --- NIG：正規逆ガウス分布
#   --- 正規分布
ghddens <- ef$x %>% dghyp(ghdfit)
hypdens <- ef$x %>% dghyp(hypfit)
nigdens <- ef$x %>% dghyp(nigfit)
nordens <- ef$x %>% dnorm(mean = mean(yret), sd = sd(yret))

# プロット作成
col.def <- c("black", "red", "blue", "green", "orange")

ef %>% plot(xlab = "", ylab = expression(f(x)), ylim = c(0, 0.25))
ef$x %>% lines(ghddens, col = "red")
ef$x %>% lines(hypdens, col = "blue")
ef$x %>% lines(nigdens, col = "green")
ef$x %>% lines(nordens, col = "orange")
legend("topleft",
       legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col = col.def, lty = 1)


# 4 QQ-Plotによる比較 ----------------------------------------------------------

# ＜ポイント＞
# - 分布の裾が正規分布とどれくらい離れているかはQQプロットで確認することができる


ghdfit %>%
  qqghyp(line = TRUE, ghyp.col = "red", plot.legend = FALSE,
         gaussian = FALSE, main = "", cex = 0.8)

hypfit %>%
  qqghyp(add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
         gaussian = FALSE, line = FALSE, cex = 0.8)

nigfit %>%
  qqghyp(add = TRUE, ghyp.pch = 3, ghyp.col = "green",
         gaussian = FALSE, line = FALSE, cex = 0.8)


legend("topleft", legend = c("GHD", "HYP", "NIG"),
       col = col.def[-c(1,5)], pch = 1:3)


# 5 モデル診断による最良分布の確認 --------------------------------------------------

# ＜ポイント＞
# - stepAIC.ghyp()を使うと最良分布をシミュレーションにより評価することができる


# 分布診断
# --- 分布を最も良く説明できる分布を探す
AIC <- yret %>%
  stepAIC.ghyp(dist = c("ghyp", "hyp", "NIG"),
               symmetric = FALSE,
               control = list(maxit = 1000))

# 確認
AIC %>% print()

# 表出力
# --- ghypのAICが最も低い
AIC$fit.table %>%
  mutate_if(is.numeric, round, 3) %>%
  select(model, aic, llh, lambda, alpha.bar, mu, sigma, gamma)


# 6 尤度比検定 --------------------------------------------------------------------

LRghdnig <- ghdfit %>% lik.ratio.test(nigfit)
LRghdhyp <- ghdfit %>% lik.ratio.test(hypfit)
