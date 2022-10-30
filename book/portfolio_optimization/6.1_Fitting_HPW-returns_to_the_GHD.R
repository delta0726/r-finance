# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 6 Suitable Distribution for returns
# Theme   : 1 Fitting HPW return to the GHD
# Date    : 2022/10/31
# Page    : P74 - P77
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# - 正規分布は金融市場の過剰尖度を捉えることができないという課題がある
# - HP社の日次リターンをGHDとその特殊なケースであるHYPおよびNIGに適合させます
#   --- GHD：一般化双曲型分布
#   --- HYP：双曲型分布
#   --- NIG：正規逆ガウス分布


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
  DowJones30_Mod$HWP %>%
    timeSeries(charvec = DowJones30_Mod$Period) %>%
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

# カーネル密度
# --- HP社のリターン
ef <- yret %>% density()

# 各分布
#   --- GHD：一般化双曲型分布
#   --- HYP：双曲型分布
#   --- NIG：正規逆ガウス分布
ghddens <- ef$x %>% dghyp(ghdfit)
hypdens <- ef$x %>% dghyp(hypfit)
nigdens <- ef$x %>% dghyp(nigfit)

# 正規分布
nordens <- ef$x %>% dnorm(mean = mean(yret), sd = sd(yret))


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

# 分布診断
# --- 分布を最も良く説明できる分布を探す
AIC <- yret %>%
  stepAIC.ghyp(dist = c("ghyp", "hyp", "NIG"),
               symmetric = FALSE,
               control = list(maxit = 1000))

# 確認
AIC %>% print()


# 6 尤度比検定 --------------------------------------------------------------------

LRghdnig <- ghdfit %>% lik.ratio.test(nigfit)
LRghdhyp <- ghdfit %>% lik.ratio.test(hypfit)
