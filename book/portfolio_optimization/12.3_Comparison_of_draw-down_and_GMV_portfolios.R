# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 12 Risk-Optimal Portfolio
# Theme   : 6.2 Drawdown constrainted portfolios
# Date    : 2022/11/03
# Page    : P254 - P260
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 最小分散ポートフォリオの構築
# 3 最大DDの計算
# 4 最大DDをコントロールしたポートフォリオ構築
# 6 リスク指標の確認


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(fPortfolio)
library(FRAPO)
library(PerformanceAnalytics)

# データロード
data(MultiAsset)

# データ確認
MultiAsset %>% head()


# 1 データ準備 ---------------------------------------------------------------

# リターン作成
Rets <-
  MultiAsset %>%
    returnseries(method = "discrete", percentage = FALSE, trim = TRUE) %>%
    timeSeries(charvec = rownames(.))

# データ確認
Rets %>% head()
Rets %>% dim()


# 2 最小分散ポートフォリオの構築 --------------------------------------------------

# ベース設定
gmvspec <- portfolioSpec()

# ポートフォリオ最適化
GMV <-
  Rets %>%
    minvariancePortfolio(spec = gmvspec,
                         constraints = "LongOnly")

# ウエイト
WGT_GMV <- GMV %>% getWeights()


# 3 最大DDの計算 -------------------------------------------------------------

# ポートフォリオリターン
GMVret <- timeSeries(Rets %*% WGT_GMV, charvec = time(Rets))

# ドローダウン
GMVDD <- GMVret %>% Drawdowns()

# プロット作成
GMVDD %>%
  multiply_by(100) %>%
  ts.plot(ylab = "Draw Downs (percentage)",
          main = "Draw Downs of Global Minimum Variance", ylim = c(-6, 0))

# 最大ドローダウン
GMVMaxDD <- max(-1.0 * GMVDD)


# 4 最大DDをコントロールしたポートフォリオ構築 -------------------------------------

# ポートフォリオ構築
# --- Portfolio optimisation with conditional draw down at risk constraint
# ---
# --- 条件付CDaR制約
# --- 条件付CDaR最小化
MaxDD <- MultiAsset %>% PMaxDD(MaxDD = GMVMaxDD)
AveDD <- MultiAsset %>% PAveDD(AveDD = GMVMaxDD)
CDaR95 <- MultiAsset %>% PCDaR(alpha = 0.95, bound = GMVMaxDD)
CDaRMin95 <- MultiAsset %>% PCDaR(alpha = 0.95)

# プロット比較
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
AveDD %>% plot(main = "(a) AveDD")
MaxDD %>% plot(ylim = ylims, main = "(b) MaxDD")
CDaR95 %>% plot(ylim = ylims, main = "(c) CDaR")
CDaRMin95 %>% plot(ylim = ylims, main = "(d) Minimum CDaR")
par(oldpar)


# 5 リスク指標の確認 ----------------------------------------------------------

# ポートフォリオ名称
Pnames <- c("GMV", "MaxDD", "AveDD", "CDaR95", "CDaRMin95")

# ウエイト結合
WeightMatrix <-
  cbind(getWeights(GMV),
        Weights(MaxDD),
        Weights(AveDD),
        Weights(CDaR95),
        Weights(CDaRMin95)) %>%
  set_colnames(Pnames)

# 確認
WeightMatrix %>% print()


# 6 リスク指標の確認 ----------------------------------------------------------

tmp <-
  WeightMatrix %>%
    apply(2, function(x) ES(Rets, weights = x, method = "gaussian", portfolio_method = "component"))

## ES 95%
PES <-
  tmp %>%
    lapply(function(x) x[[1]]) %>%
    unlist() %>%
    multiply_by(100)

## Marginal Contributions to ES
PMES <-
  tmp %>%
    lapply(function(x) x[[3]]) %>%
    unlist() %>%
    matrix(nrow = ncol(Rets)) %>%
    multiply_by(100) %>%
    set_colnames(Pnames) %>%
    set_rownames(colnames(Rets))

## Marginal Contributions to StdDev
V <- Rets %>% cov()

PMRC <-
  WeightMatrix %>%
    apply( 2, mrc, Sigma = V) %>%
    set_rownames(names(Rets))

## Diversification ratio
PDR <- WeightMatrix %>% apply( 2, dr, Sigma = V)
