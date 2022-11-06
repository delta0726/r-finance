# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 11 Diversification roconsidered
# Theme   : 6.2 Optimal tail-dependent portfolio against benchmark
# Date    : 2022/11/06
# Page    : P216 - P221
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 コピュラの計算
# 3 アウトサンプルのパフォーマンス測定
# 4 パフォーマンス統計量1
# 5 パフォーマンス統計量2
# 6 時系列リターンのプロット


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(FRAPO)
library(copula)
library(PerformanceAnalytics)

# データロード
data(INDTRACK6)

# データ確認
# --- インデックスと個別銘柄の株価
INDTRACK6 %>% as_tibble()


# 1 データ準備 -----------------------------------------------------------------

# リターン作成
# --- マーケット
# --- 個別銘柄
RM <- INDTRACK6[1:260, 1] %>% returnseries(method = "discrete", trim = TRUE)
RA <- INDTRACK6[1:260, -1] %>% returnseries(method = "discrete", trim = TRUE)

# 個別銘柄の統計量
# --- ベータ
# --- 相関係数
Beta <- RA %>% apply( 2, function(x) cov(x, RM) / var(RM))
Tau <- RA %>% apply( 2, function(x) cor(x, RM, method = "kendall"))


# 2 コピュラの計算 ---------------------------------------------------------------

# クレイトン・コピュラ
# --- Lower Tail Dependence
ThetaC <- copClayton@iTau(Tau)
LambdaL <- copClayton@lambdaL(ThetaC)

# ウエイト算出
# --- ベータが中央値以下を選択
# --- 1以下の対数はマイナスとなるため、-1を乗じてプラスにする
IdxBeta <- Beta < median(Beta)
WBeta <- -1 * log(abs(Beta[IdxBeta]))
WBeta <- WBeta / sum(WBeta) * 100

# ウエイト算出
# --- LambdaLが中央値以下を選択
IdxTD <- LambdaL < median(LambdaL)
WTD <- -1 * log(LambdaL[IdxTD])
WTD <- WTD / sum(WTD) * 100


Intersection <- sum(names(WTD) %in% names(WBeta)) / length(WBeta) * 100


# 3 アウトサンプルのパフォーマンス測定 -----------------------------------------------

RMo <-
  INDTRACK6[260:290, 1] %>%
    returnseries(method = "discrete",percentage = FALSE) %>%
    add(1)

RAo <-
  INDTRACK6[260:290, -1] %>%
    returnseries(method = "discrete", percentage = FALSE) %>%
    add(1)

# ベンチマーク
RMo[1] <- 100
RMEquity <- RMo %>% cumprod()

# ローベータ
LBEquity <- RAo[, IdxBeta]
LBEquity[1, ] <- WBeta
LBEquity <- LBEquity %>% apply( 2, cumprod) %>% rowSums()

# TD
TDEquity <- RAo[, IdxTD]
TDEquity[1, ] <- WTD
TDEquity <- TDEquity %>% apply( 2, cumprod) %>% rowSums()

# 結果結合
y <- cbind(RMEquity, LBEquity, TDEquity)


# 4 パフォーマンス統計量1 -----------------------------------------------------------

# リターン変換
# --- パーセントから小数に変換
RAdec <- RA / 100

# リターン系列の分割
RALB <- RAdec[, names(WBeta)]
RATD <- RAdec[, names(WTD)]

# 標準偏差
LbStd <- rowSums(RALB * WBeta / 100) %>% StdDev() %>% multiply_by(100)
TdStd <- rowSums(RATD * WTD / 100) %>% StdDev() %>% multiply_by(100)

# 期待ショートフォール
LbES95 <- rowSums(RALB * WBeta / 100) %>% ES(method = "gaussian") %>% abs() %>% multiply_by(100)
TdES95 <- rowSums(RATD * WTD / 100) %>% ES(method = "gaussian") %>% abs() %>% multiply_by(100)

# 分散レシオ
LbDr <- WBeta %>% dr(Sigma = cov(RALB))
TdDr <- WTD %>% dr(Sigma = cov(RATD))
LbCr <- WBeta %>% cr(Sigma = cov(RALB))
TdCr <- WTD %>% cr(Sigma = cov(RATD))


# 5 パフォーマンス統計量2 -----------------------------------------------------------

## Key measure (ex ante)
LbRetO <-
  LBEquity %>%
    returnseries(percent = FALSE, trim = TRUE) %>%
    timeSeries(charvec = 1:30)

TdRetO <-
  TDEquity %>%
    returnseries(percent = FALSE, trim = TRUE) %>%
    timeSeries(charvec = 1:30)

BmRetO <- timeSeries(RMo[-1] - 1, charvec = 1:30)

km <- function(pf, bm, scale){
  ra <- Return.annualized(pf, scale = scale) * 100
  ir <- InformationRatio(pf, bm, scale = scale)
  upr <- UpDownRatios(pf, bm, method = "Capture", side = "Up")
  dnr <- UpDownRatios(pf, bm, method = "Capture", side = "Down")
  res <- c(ra, ir, upr, dnr)
  names(res) <- c("Return", "IR", "UpRatio", "DownRatio")
  return(res)
}

LbKM <- LbRetO %>% km(BmRetO, scale = 52)
TdKM <- TdRetO %>% km(BmRetO, scale = 52)


# 6 時系列リターンのプロット -------------------------------------------------

# リターン推移
RMEquity %>%
  plot(type = "l", ylim = range(y), ylab = "Equity Index",
       xlab = "Out-of-Sample Periods")

lines(LBEquity, lty = 2)
lines(TDEquity, lty = 3)
legend("topleft",
       legend = c("S&P 500", "Low Beta", "Lower Tail Dep."),
       lty = 1:3)

# バープロット
# --- 相対リターン
RelOut <- rbind((LBEquity / RMEquity - 1) * 100,
                (TDEquity / RMEquity - 1) * 100)
RelOut <- RelOut[, -1]
barplot(RelOut, beside = TRUE, ylim = c(-5, 17),
        names.arg = 1:ncol(RelOut),
        legend.text = c("Low Beta", "Lower Tail Dep."),
        args.legend = list(x = "topleft"))
abline(h = 0)
box()
