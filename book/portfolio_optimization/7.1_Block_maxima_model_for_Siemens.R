# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 7 Extreme value theory
# Theme   : 4.2 Block maxima model for Siemens
# Date    : 2022/11/13
# Page    : P103 - P107
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜参考＞
# - Rを使って極値理論をファイナンスへ応用してみた（その１）
# https://qiita.com/hrkz_szk/items/43debffda9697d9dd7a9


# ＜目次＞
# 0 準備
# 1 データ加工
# 2 一般化極値分布によるモデリング
# 3 診断プロットの作成


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(conflicted)
library(evir)
library(ismev)
library(timeSeries)
library(fExtremes)

# コンフリクト解消
conflict_prefer("pgev", "fExtremes")


# データロード
data(siemens)

# データ確認
siemens %>% class()
siemens %>% as.data.frame() %>% head()
siemens %>% glimpse()


# 1 データ加工 ----------------------------------------------------------------

# データ変換
# --- 小数をパーセントに変換して正負を逆転（損失を正の値として扱う）
SieLoss <- siemens * 100 * -1

# データ確認
SieLoss %>% ts.plot()


# 2 一般化極値分布によるモデリング --------------------------------------

# ＜ポイント＞
# - gev()のblock引数でsemester(半年)を指定したので半年ごとの最大値が抽出される
# - サンプル期間の後半にボラティリティが上昇したという発見はこのグラフに反映されている
#   --- したがって、同一に分散されたブロックの最大値の仮定に違反する可能性がある
#   --- ただし、当面はこの違反の可能性を無視し、共変量としての傾向がモデルに含まれる場合に再度対処する


# ＜参考＞
# 生態学のデータ解析 - 極値分布
# https://kuboweb.github.io/-kubo/ce/ExtremeValue.html


# 一般化極値分布(GEV)に適合
# --- 半年間(semester)の最大値を自動的に抽出し、それらのデータをGEVに当てはめる
SieGEV <- SieLoss %>% evir::gev(block = "semester")

# 結果確認
SieGEV %>% print()

# 最大値のプロット
SieGEV$data %>%
  plot(type = "h", col = "blue", xlab = "", ylab = "Block Maxima",
       main = "Maximum Biannual Losses of Siemens")

# パラメータ
rbind(SieGEV$par.ests, SieGEV$par.ses) %>%
  set_rownames(c("Estimate", "Standard Error"))


# 3 診断プロットの作成 -------------------------------------------------

# 一般化極値分布(GEV)に適合
SieGEV2 <- SieGEV$data %>% ismev::gev.fit(type = "GEV")

# 結果確認
SieGEV2 %>% summary()

# 診断データの作成
SieGEV2 %>% gev.diag()

# 診断プロットの作成
par(mfrow = c(2, 1))
SieGEV2 %>% gev.prof(m = 20, xlow = 5, xup = 16, conf = 0.95)
SieGEV2 %>% gev.profxi(xlow = 0.0, xup = 0.7, conf = 0.95)



#

# ＜ポイント＞
# - 観察された最大の12.01%の損失は「10年に1 回」のイベントとしてカバーされていない
#   --- むしろ、この損失は42年に 回程度しか発生しません

mLoss <- SieGEV$data %>% max()
mYears <- 1 / (1 - pgev(mLoss, mu = SieGEV2$mle[1], beta = SieGEV2$mle[2], xi = SieGEV2$mle[3])) / 2


SieGEV3 <-  SieGEV$data %>% gevFit(type = "pwm")
SieGEV3
