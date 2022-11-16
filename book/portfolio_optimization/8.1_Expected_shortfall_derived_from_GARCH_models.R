# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 8 Modeling Volatility
# Theme   : 4 Empirical application of volatility models
# Date    : 2022/11/17
# Page    : P128 - P130
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# - 金融市場はボラティリティ・クラスタリングの特性を持つ（Stylized Fact）
#   --- ARMAなどの時系列モデルはマーケットの分散が不偏であることことを仮定（金融市場には不向き）
#   --- GARCHは分散にも時系列構造を持たせた予測モデル


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 GARCHによるモデリング
# 3 結果確認


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(lubridate)
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

# プロット確認
NYSESW %>% ts.plot()


# 1 データ準備 -----------------------------------------------------------------

# ＜ポイント＞
# - GARCHはボラティリティを扱うため、株価データはリターンに変換しておく


# 日次リターン計算
NYSELOSS <-
  NYSESW %>%
    log() %>%
    diff() %>%
    multiply_by(-100) %>%
    timeSeries(char.vec = time(NYSESW))

# プロット確認
NYSELOSS %>% ts.plot()


# 2 GARCHによるモデリング ------------------------------------------------------

# ＜ポイント＞
# - スチューデントのt分布の過程プロセスを持つGARCH(1, 1)モデルを推定
# - 条件付き標準偏差の1ステップ先の予測が計算され、自由度パラメーターの近似値がオブジェクト df に割り当てる
# - 次に、デフォルトの信頼水準 p = 0.99 で予想不足額が計算されます


# 関数定義
# --- GARCHモデルで1期先のボラティリティを予測
# --- 予測ボラティリティから期待ショートフォールを算出
# --- 期待ショートフォールを出力
ESgarch <- function(y, p = 0.99){
  gfit <- garchFit(formula = ~garch(1, 1), data = y,
                   cond.dist = "std", trace = FALSE)
  sigma <-  predict(gfit, n.ahead = 1)[3]
  df <- coef(gfit)["shape"]
  ES <- sigma * (dt(qt(p, df), df)/(1 - p)) * ((df + (qt(p, df))^2)/(df - 1))
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

# ＜ポイント＞
# - ボラティリティ・クラスタリングの発生時期に期待ショートフォールが大きくなっている


# データ整理
X_Plot <-
  NYSELOSS %>%
    cbind(NYSEESL1) %>%
    na.omit() %>%
    set_colnames(c("NYSELOSS", "ES99")) %>%
    as.data.frame() %>%
    rownames_to_column("Period") %>%
    mutate(Period = ymd(Period))

# プロット作成
X_Plot %>%
  ggplot(aes(x = Period)) +
  geom_line(aes(y = ES99), color = "red") +
  geom_col(aes(y = NYSELOSS), color = "gray", alpha = 0.75) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  theme_bw()

