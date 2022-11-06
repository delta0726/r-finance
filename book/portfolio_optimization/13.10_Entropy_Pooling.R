# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 13 Tactical asset allocation
# Theme   : 6.3 Entropy Pooling
# Date    : 2022/00/00
# Page    : P318 - P324
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 バックテスト準備
# 3 バックテストの実行
# 4 バックテスト結果の評価
# 5 パフォーマンス統計量


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(FRAPO)
library(fGarch)
library(fMultivar)
library(sn)
library(fPortfolio)
library(PerformanceAnalytics)

# データロード
data(ESCBFX)

# データ確認
# --- 為替データ
ESCBFX %>% head()
ESCBFX %>% dim()


# 1 データ準備 ------------------------------------------------------------------

# オブジェクト変更
FXDaily <- ESCBFX %>% timeSeries(charvec = rownames(ESCBFX))

# 日付取得
DDates <- FXDaily %>% time()

# 分析期間
FirstWed <- FXDaily %>% time() %>% isWeekday(wday = 3) %>% which(arr.ind = TRUE) %>% head(1)
LastWed <- FXDaily %>% time() %>% isWeekday(wday = 3) %>% which(arr.ind = TRUE) %>% tail(1)

# 日付出力
AllWedDays <-
  timeSequence(from = DDates[FirstWed],
               to = DDates[LastWed],
               by = "week")

# ダミー日付
DumWed <- rep(1, length(AllWedDays)) %>% timeSeries(charvec = AllWedDays)
FXWeekly <- cbind(DumWed, FXDaily) %>% interpNA(method = "before") %>% .[AllWedDays, -1]


FXWeekly <-
  rep(1, length(AllWedDays)) %>%
    timeSeries(charvec = AllWedDays) %>%
    cbind(FXDaily) %>%
    interpNA(method = "before") %>%
    .[AllWedDays, -1]

# オブジェクト準備
assetsNames <- FXWeekly %>% colnames()
Anames <- FXWeekly %>% colnames()
FxNames <- FXWeekly %>% colnames()


FxPrice <- 1 / FXWeekly
FxRet <- FxPrice %>% returns(percentage = TRUE, type = "discrete", trim = TRUE)
FxRetSub <- window(FxRet, start = start(FxRet), end = time(FxRet)[520])


# 2 バックテスト準備 --------------------------------------------------------

# パラメータ設定
J <- 1000
N <- ncol(FxPrice)
Eperiods <- time(FxRet)[-c(1:519)]
Speriods <- time(FxRet)[1:length(Eperiods)]
LengthBack <- length(Speriods)
WEP <- WMD <- WND <- matrix(NA, nrow = LengthBack, ncol = N)
x0 <- rep(0, N + 1)
pprior <- matrix(rep(1 / J, J), ncol = 1)


# 関数定義
# --- GARCH-forecast
CondVolaFcst <- function(x){
    m <- garchFit(formula = ~ garch(1,1),
                  data = x, trace = FALSE)
    mp <- as.numeric(predict(m, n.ahead = 1))[3]
    mp
}

# 関数定義
# --- Entropy Pooling
f0 <- function(v, p, Aeq, beq){
    x <- exp(log(p) - 1 - crossprod(Aeq, v))
        x = apply(cbind(x, 0), 1, max)
        L = t(x) %*% (log(x) - log(p) + crossprod(Aeq, v)) -
            crossprod(beq, v)
       -L
}

gf <- function(v, p, Aeq, beq){
    x <- exp(log(p) - 1 - crossprod(Aeq, v))
    beq - Aeq %*% x
}


ep <- function(x0, Aeq, beq, pprior){
    vopt <- try(optim(par = x0, fn = f0, gr = gf,
              Aeq = Aeq, beq = beq, p = pprior, method = "BFGS"))
    if(class(vopt) == "try-error"){
        return(pprior)
    } else {
        v <- vopt$par
        pbar <- exp(log(pprior) - 1 - crossprod(Aeq, v))
        return(pbar / sum(pbar))
    }
}



# 3 バックテストの実行 -----------------------------------------------------

for(i in 1:LengthBack){
  # 進捗
  cat(paste("Backtestperiod:", Eperiods[i], "\n"))

  # 乱数シード
  set.seed(i + 12345)

  fp <- FxRet %>% window(start = Speriods[i], end = Eperiods[i])

  ## Update Market Model
  MarketMod <- fp %>% series() %>% mstFit()

  par <- MarketMod@fit$estimated
  M <-
    J %>%
      rmst(xi = par$beta, Omega = par$Omega,
           alpha = par$alpha, nu = par$nu) %>%
      set_colnames(Anames)

  mu2 <- colMeans(M)^2
  Vt <- t(M)
  Aeq <- rbind(Vt^2, rep(1, J))

  ## GARCH-model
  mfcst <- fp %>% lapply(CondVolaFcst)
  fcst <- mfcst %>% unlist() %>% matrix(ncol = 1, byrow = TRUE)
  beq <- matrix(c(mu2 + fcst[, 1]^2, 1), ncol = 1)

  ## EP-optimization
  Ep <- ep(x0, Aeq, beq, pprior)

  ## EP for fixed tau = 0.5
  EpH <- 0.5 * Ep + 0.5 * pprior

  EPspec <- portfolioSpec()
  EPmom <- function(x, spec = NULL, ...){
      m <- cov.wt(x, drop(EpH))
      list("mu" = m$center, "Sigma" = m$cov)
  }
  setEstimator(EPspec) <- "EPmom"

  WEP[i, ] <-
    M %>% as.timeSeries() %>%
      tangencyPortfolio(spec = EPspec) %>%
      getWeights()

  ## Portfolio based on market distribution
  WMD[i, ] <-
    M %>%
      as.timeSeries() %>%
      tangencyPortfolio(spec = portfolioSpec()) %>%
      getWeights()

  ## Portfolio based on normality assumption
  WND[i, ] <-
    fp %>% tangencyPortfolio(spec = portfolioSpec()) %>%
      getWeights()
}



# 4 バックテスト結果の評価 --------------------------------------------------------

# ライブラリ
## Evaluation of backtest

# リスト格納
# --- バックテスト結果のウエイト
W <- list(WEP, WMD, WND)


Equity <- function(x, Eperiods){
    WTsL1 <- x %>% timeSeries(charvec = Eperiods) %>% lag(k = 1)
    RetFacPR <- rowSums(FxRet[Eperiods, ] / 100 * WTsL1) + 1
    RetFacPR[1] <- 100
    RetFacPR %>% cumprod() %>% timeSeries(Eperiods)
}


WealthBack <- W %>% lapply(Equity, Eperiods = Eperiods)


# プロット範囲
ylims <-
  WealthBack %>%
    lapply(function(x) pretty(range(x))) %>%
    unlist() %>%
    range()

# プロット作成
WealthBack[[1]] %>% plot(lwd = 2, xlab = "", ylab = "Index", main = "", ylim = ylims)
lines(WealthBack[[2]], lty = 2, lwd = 2, col = "blue")
lines(WealthBack[[3]], lty = 3, lwd = 2, col = "red")
legend("topleft", legend = c("EP", "Market", "Normal"), lty = 1:3,
       lwd = 2, col = c("black", "blue", "red"))



# 5 パフォーマンス統計量 -----------------------------------------------------------

# 関数定義
# --- パフォーマンス統計量の計算
PerfM <- function(x){
  EPRet <- returns(x, method = "discrete", percentage = FALSE, trim = TRUE)
  EPRA <- EPRet[, 1] %>% Return.annualized(scale = 52) %>% multiply_by(100)
  EPSD <- EPRet[, 1] %>% StdDev.annualized(scale = 52) %>% multiply_by(100)
  EPSRA <- EPRet[, 1] %>% SharpeRatio.annualized(scale = 52)
  EPES <- EPRet[, 1] %>% ES(p = 0.95, method = "modified", clean = "boudt") %>% multiply_by(-100)
  EPMDD <- EPRet[, 1] %>% maxDrawdown() %>% multiply_by(100)

  # データ結合
  EPPerfW <-
    rbind(EPRA, EPSD, EPSRA, EPES, EPMDD) %>%
      set_rownames(c("Return (annual)", "Risk (annual, SD)",
                     "Sharpe Ratio", "CVaR (modified, 95%)", "Max Draw Down"))

  # 出力
  return(EPPerfW)
}


# 結果整理
PerfM <-
  WealthBack %>%
    lapply(PerfM) %>%
    unlist() %>%
    matrix(ncol = 3) %>%
    set_colnames(c("EP", "Market", "Normal")) %>%
    set_rownames(c("Return (annual)", "Risk (annual, SD)",
                   "Sharpe Ratio", "CVaR (modified, 95%)", "Maximum Draw Down"))

# 確認
PerfM

