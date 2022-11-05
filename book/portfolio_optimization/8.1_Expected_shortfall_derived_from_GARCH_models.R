# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 8 Modeling Volatility
# Theme   : 4 Empirical application of volatility models
# Date    : 2022/11/05
# Page    : P128 - P130
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 GARCHモデルの実行
# 3 結果確認


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(timeSeries)
library(AER)
library(fGarch)
library(tseries)

# データロード
data(NYSESW)

# データ確認
NYSESW %>% class()
NYSESW %>% head()
NYSESW %>% length()


# 1 データ準備 -----------------------------------------------------------------

# リターン計算
NYSELOSS <-
  NYSESW %>%
    log() %>%
    diff() %>%
    multiply_by(-100) %>%
    timeSeries(char.vec = time(NYSESW))

# プロット確認
NYSELOSS %>% ts.plot()


# 2 GARCHモデルの実行 ----------------------------------------------------------

# ＜ポイント＞
# - スチューデントのt分布の過程プロセスを持つGARCH(1, 1)モデルを推定
# - 条件付き標準偏差の1ステップ先の予測が計算され、自由度パラメーターの近似値がオブジェクト df に割り当てる
# - 次に、デフォルトの信頼水準 p = 0.99 で予想不足額が計算されます


# 関数定義
# ---
ESgarch <- function(y, p = 0.99){
  gfit <- garchFit(formula = ~garch(1, 1), data = y,
                   cond.dist = "std", trace = FALSE)
  sigma <-  predict(gfit, n.ahead = 1)[3]
  df <- coef(gfit)["shape"]
  ES <- sigma * (dt(qt(p, df), df)/(1 - p)) *
        ((df + (qt(p, df))^2)/(df - 1))
  return(ES)
}


# 期間設定
from <- NYSELOSS %>% time() %>% .[-c((nrow(NYSELOSS) - 999) : nrow(NYSELOSS))]
to <- NYSELOSS %>% time() %>% .[-c(1:1000)]

# 関数実行
NYSEESL1 <-
  NYSELOSS %>%
    fapply(from = from, to = to, FUN = ESgarch) %>%
    lag(k = 1)

# データ確認
NYSEESL1 %>% head()


# 3 結果確認 -----------------------------------------------------------------

# データ整理
res <-
  NYSELOSS %>%
    cbind(NYSEESL1) %>%
    na.omit() %>%
    set_colnames(c("NYSELOSS", "ES99"))

# プロット作成
res[, 2] %>%
  plot(col = "red", ylim = range(res),
       main = "NYSE: t-GARCH(1,1) ES 99%",
       ylab = "percentages", xlab = "")
points(res[, 1], type = "p", cex = 0.2, pch = 19, col = "blue")
legend("topleft", legend = c("Loss", "ES"),
       col = c("blue", "red"), lty = c(NA, 1), pch = c(19, NA))
