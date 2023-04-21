# ***********************************************************************************************
# Library : ichimoku
# Theme   : Quantitative Strategy
# Date    : 2023/04/17
# URL     : https://cran.r-project.org/web/packages/ichimoku/vignettes/strategies.html
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 単一の投資戦略の作成
# 2 単一投資戦略の結合
# 3 複雑な投資戦略
# 4 投資戦略のパフォーマンス評価
# 5 高度な戦略から機械学習へ


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(ichimoku)
library(lubridate)
library(timetk)
library(PerformanceAnalytics)


# データロード
TKR <- sample_ohlc_data
cloud <- ichimoku(TKR)

# データ確認
TKR %>% dim()
TKR %>% head()


# 1 単一の投資戦略の作成 -----------------------------------------------

# データ作成
cloud <- TKR %>% ichimoku()

# 確認
cloud %>% print()


# トレーディング戦略の作成
strat <- strat(cloud, c1 = "cloudB", c2 = "kijun")
print(strat[100:105, ], plot = FALSE)

# サマリー
# --- 表示される項目のみで、特別なデータの格納はない
strat %>% summary()

# プロット作成
strat %>% plot(theme = "dark")
strat %>% iplot(theme = "dark")


# 2 単一投資戦略の結合 ------------------------------------------------

# 個別投資戦略の作成
strat1 <- cloud %>% strat(c1 = "cloudB", c2 = "kijun")
strat2 <- cloud %>% strat(c1 = "kijun", c2 = "tenkan")

# 投資戦略の結合
newstrat <- stratcombine(strat1, strat2)
summary(newstrat)


# 3 複雑な投資戦略 ----------------------------------------------------

# ＜ポイント＞
# - 以下の2つは同じ結果となる

# 複雑な投資戦略の作成
strat12 <- cloud %>% strat(c1 = "cloudB", c2 = "kijun", c3 = "kijun", c4 = "tenkan")

# 単一投資戦略の結合
strat1 <- cloud %>% strat(c1 = "cloudB", c2 = "kijun")
strat2 <- cloud %>% strat(c1 = "kijun", c2 = "tenkan")
newstrat <- stratcombine(strat1, strat2)

# 比較
all.equal(strat12, newstrat)


# 4 投資戦略のパフォーマンス評価 -------------------------------------------

# ＜ポイント＞
# - xtsオブジェクトで出力されるので、PerformanceAnalyticsとの相性が良い


# データ確認
# --- 戦略リターンと銘柄リターン
strat[, c("sret", "ret")] %>% na.omit()

data(edhec)
chart.CumReturns(edhec[,"Funds of Funds"],main="Cumulative Returns")

# パフォーマンス
# --- Pycharmだと表示されない（Rstudioでは出ている）
strat[, c("sret", "ret")] %>% chart.CumReturns()

# ドローダウン
strat[, "sret"] %>% table.Drawdowns()
strat[, c("sret", "ret")] %>% table.AnnualizedReturns()


# 5 高度な戦略から機械学習へ ---------------------------------------------

