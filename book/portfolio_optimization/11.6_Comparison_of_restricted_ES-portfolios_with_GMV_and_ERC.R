# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 11 Diversification roconsidered
# Theme   : 6.3 Limiting contribution to expected shortfall
# Date    : 2022/11/03
# Page    : P221 - P225
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 最適化のベース設定
# 3 ESを計算する関数の定義
# 4 期待ESの寄与に関する予算制約ポートフォリオ
# 5 最小CVaRの最小集中ポートフォリオ
# 6 その他のポートフォリオ
# 7 結果比較
# 8 リスク指標の確認


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(FRAPO)
library(PortfolioAnalytics)
library(DEoptim)

# データロード
data(MultiAsset)

# データ確認
MultiAsset %>% head()
MultiAsset %>% dim()
MultiAsset %>% class()


# 1 データ準備 ----------------------------------------------------------------

# リターン作成
R <- MultiAsset %>% returnseries(percentage = FALSE, trim = TRUE)

# アセット数
N <- R %>% ncol()


# 2 最適化のベース設定 ---------------------------------------------------------

# ベース設定
port_spec <- portfolio.spec(assets = colnames(R))

# 制約条件
# --- ロングオンリー
# --- フルインベストメント
C1 <-
  port_spec %>%
    add.constraint(type = "weight_sum",
                   min_sum = 1,
                   max_sum = 1) %>%
    add.constraint(type="box",
                   min = rep(0, N),
                   max = rep(1, N))

# 目的関数
# --- 95% の信頼水準で最小の CVaR を特徴とする資産配分を見つける
ObjCVaR <-
  C1 %>%
    add.objective(type = "risk",
                  name = "ES",
                  arguments = list(p = 0.95),
                  enabled = TRUE)


# 3 ESを計算する関数の定義-------------------------------------------------

calc_es <- function(R, weights){
  ES(R, weights = WCVaRMinCon, p = 0.95,
     portfolio_method = "component")
}



# 4 ESの寄与に関する予算制約ポートフォリオ -----------------------------------

# 目的関数の追加
# --- 資産の下振れリスクの寄与に関する予算の制約を定義（最大20%）
ObjCVaRBudget <-
  ObjCVaR %>%
    add.objective(type = "risk_budget",
                  name = "ES",
                  max_prisk = 0.2,
                  arguments = list(p = 0.95),
                  enabled = TRUE)

# 最適化
SolCVaRBudget <-
  R %>%
    optimize.portfolio(portfolio = ObjCVaRBudget,
                       optimize_method = "DEoptim",
                       itermax = 50,
                       search_size = 20000,
                       trace = TRUE)

# ウエイト計算
WCVaRBudget <- SolCVaRBudget$weights

# 期待ショートフォールの計算
CVaRBudget <- R %>% calc_es(weights = WCVaRBudget)


# 5 最小CVaRの最小集中ポートフォリオ -----------------------------------------

## Minimum CVaR concentration portfolio
ObjCVaRMinCon <-
  ObjCVaR %>%
    add.objective(type = "risk_budget",
                  name = "ES",
                  min_concentration= TRUE,
                  arguments = list(p = 0.95),
                  enabled = TRUE)

SolCVaRMinCon <-
  R %>%
    optimize.portfolio(portfolio = ObjCVaRMinCon,
                       optimize_method = "DEoptim",
                       itermax = 50,
                       search_size = 20000,
                       trace = TRUE)

WCVaRMinCon <- SolCVaRMinCon$weights

# 期待ショートフォール
CVaRMinCon <- R %>% calc_es(weights = WCVaRMinCon)


# 6 その他のポートフォリオ -------------------------------------------------

# ポートフォリオ構築
# --- 最小分散ポートフォリオ
# --- リスクパリティポートフォリオ
WGMV <- R %>% PGMV(percentage = FALSE) %>% Weights()
WERC <- R %>% cov() %>% PERC(percentage = FALSE) %>% Weights()

# 期待ショートフォール
CVaRGMV <- R %>% calc_es(weights = WGMV)
CVaRERC <- R %>% calc_es(weights = WERC)


# 7 結果比較 -------------------------------------------------

strategy_names <- c("CVaR-Min", "CVaR-Div", "Min-Vol", "Risk_Parity")


# ウエイト
W <-
  cbind(WCVaRBudget, WCVaRMinCon, WGMV, WERC) %>%
    set_colnames(strategy_names) %>%
    multiply_by(100)

# 寄与度
cbind(CVaRBudget$contribution,
      CVaRMinCon$contribution,
      CVaRGMV$contribution,
      CVaRERC$contribution) %>%
  set_colnames(strategy_names) %>%
  multiply_by(100)

# 寄与度パーセント
cbind(CVaRBudget$pct_contrib_MES,
      CVaRMinCon$pct_contrib_MES,
      CVaRGMV$pct_contrib_MES,
      CVaRERC$pct_contrib_MES) %>%
  set_colnames(strategy_names)



# 8 リスク指標の確認 ------------------------------------------------------------

# ポートフォリオリターン
Rdec <- as.matrix(R / 100)
Pret <- W %>% apply( 2, function(x) Rdec %*% x / 100)

# ボラティリティ
SD_raw <- Pret %>% apply(MARGIN = 2, sd) * 100
SD_ann <- Pret %>% apply(MARGIN = 2, function(x) sd(x) * sqrt(260)) * 100

# 期待ショートフォール
ES95 <- Pret %>% apply(MARGIN = 2, function(x) abs(ES(R = x, method = "modified") * 100))

# 分散レシオ
# Diversification Ratio
# concentration Ratio
DR <- W %>% apply(MARGIN = 2, FUN = dr, Sigma = V)
CR <- W %>% apply(MARGIN = 2, FUN = cr, Sigma = V)

# 確認
rbind(SD_raw, SD_ann, ES95, DR, CR)
