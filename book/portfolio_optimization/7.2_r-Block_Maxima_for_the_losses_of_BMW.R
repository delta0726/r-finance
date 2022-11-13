# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 7 Extreme value theory
# Theme   : 4.3 r-block maxima for BMW
# Date    : 2022/11/13
# Page    : P107 - P110
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# - Block maximaでは最大値のみを抽出したが、問題となるのはサンプル数
#   --- 特にファイナンス系のデータの場合、数十年単位で最大値を確保できないことが多い
# - サンプル数への対処法はr-block maximaモデルを検討する
#   --- r-block maximaは上からr個の値を抽出します。


# ＜参考＞
# - Rを使って極値理論をファイナンスへ応用してみた（その２）
# https://qiita.com/hrkz_szk/items/2c966aab9342f61a5b59


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 最大損失データの抽出
# 3 上位2つの損失をプロット
# 4 フィッティングとモデル診断


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(timeSeries)
library(evir)
library(ismev)


# データロード
data(bmw)

# データ確認
bmw %>% class()
bmw %>% as.data.frame() %>% head()


# 1 データ準備 --------------------------------------------------------------------

# 日付データ
BmwDates <-
  bmw %>%
    attr("times") %>%
    as.POSIXct() %>%
    format( "%Y-%m-%d") %>%
    as.character()

# リターン作成
# --- パーセント表示
BmwRet <-
  bmw %>%
    timeSeries(charvec = BmwDates) %>%
    multiply_by(100)

# プロット
BmwRet %>% plot(ylab="Log-return of BMW")

# データ変換
# --- 小数をパーセントに変換して正負を逆転（損失を正の値として扱う）
BmwLoss <- bmw * 100 * -1.0

# 属性追加
attr(BmwLoss, "years") <- BmwLoss %>% attr( "time") %>% format("%Y")
BmwLoss %>% glimpse()


# 2 最大損失データの抽出 ---------------------------------------------------------------

# ＜ポイント＞
# - 年ごとの損失値の上位2つを抽出してデータセットとしてまとめる


# パーツ作成
# --- 年データからインデックス作成
Yearu <- BmwLoss %>% attr("time") %>% format( "%Y") %>% unique()
idx <- 1:length(Yearu)

# 関数定義
# --- 最大損失の上位2つを抽出
test_f <- function(x){
  BmwLoss[attr(BmwLoss, "year") == Yearu[x]] %>%
    sort(decreasing = TRUE) %>%
    head(2)
}

# データ作成
BmwOrder <-
  idx %>%
    sapply(test_f) %>%
    t() %>%
    set_colnames(c("r1", "r2")) %>%
    set_rownames(Yearu)

# 確認
BmwOrder %>% print()


# 3 上位2つの損失をプロット -----------------------------------------------------------

Yearu %>%
  plot(BmwOrder[, 1], col = "black", ylim = range(BmwOrder),
       ylab = "Losses BMW (percentages)", xlab = "",
       pch = 21, bg = "black")

Yearu %>%
  points(BmwOrder[, 2], col = "grey", pch = 23, bg = "grey")


# 4 フィッティングとモデル診断 ---------------------------------------------------------

# フィッティング
# --- convのゼロはうまく推定されたことを示している
BmwOrderFit <- BmwOrder %>% ismev::rlarg.fit()

# 確認
BmwOrderFit %>% print()

# 信頼区間
lower <- BmwOrderFit$mle - 1.96 * BmwOrderFit$se
upper <- BmwOrderFit$mle + 1.96 * BmwOrderFit$se
data.frame(lower = lower, upper = upper, row.names = c("mu", "sigma", "xi"))


# モデル診断
BmwOrderFit %>% rlarg.diag()
