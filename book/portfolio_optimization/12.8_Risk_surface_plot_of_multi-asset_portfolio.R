# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 12 Risk-Optimal Portfolio
# Theme   : 6.4 Risk surface plot
# Date    : 2022/11/04
# Page    : P265 - P271
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 Risk-Surfaceプロットの作成


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(FRAPO)
library(fPortfolio)

# データロード
data(MultiAsset)

# データ確認
MultiAsset %>% head()
MultiAsset %>% dim()


# 1 データ準備 ------------------------------------------------------------------

# 株価データ
pr <- MultiAsset %>% timeSeries(charvec = rownames(.))

# リターンデータ
rtn <- pr %>% returns(method = "discrete", percentages = TRUE, trim = TRUE)

# アセット情報
# --- アセット数
# --- アセット名
NAssets <- pr %>% ncol()
ANames <- pr %>% colnames()

# 分散共分散行列
Sigma <- rtn %>% cov()

# 平均リターン
mu <- rtn %>% colMeans()


# 2 Risk-Surfaceプロットの作成 ---------------------------------------------------

# 関数定義
# --- リスク寄与度
mrc.sd <- function(rtn, weights){
    Sigma <- cov(rtn)
    a <- mrc(weights, Sigma)
    sd(a)
}


divers <-
  rtn %>%
    markowitzHull(nFrontierPoints = 50) %>%
    feasibleGrid(trace = FALSE) %>%
    bestDiversification(trace = FALSE)

surf <-
  divers %>%
    riskSurface(FUN = "mrc.sd")

# データ確認
divers %>% glimpse()


allWeights <- divers %>% attr("weights")
idx <- allWeights[, 1] %>% unique() %>% sort()


dropt <- matrix(0, nrow = length(idx), ncol = 2)
idxRow <- seq_along(idx)

for(j in idx){
    w <- matrix(allWeights[allWeights[, 1] == j, -c(1, 2)], ncol = NAssets)
    divm <- vector()
    length(divm) <- nrow(w)
    for(i in seq_len(nrow(w))){
        divm[i] <- dr(w[i, ], Sigma)
    }
    divmidx <- which.max(divm)
    wopt <- w[divmidx, ]
    dropt[idxRow[j], ] <- c(crossprod(wopt, mu),
                            sqrt(crossprod(wopt, Sigma) %*% wopt))
}



# ライブラリ
## Surface plot with superimposed mdp solutions
## per standard deviation risk level
surf %>%
  surfacePlot(type = "filled.contour",
              palette = gray.colors, addHull = TRUE, addGrid = FALSE,
              addAssets = FALSE, xlab = "Target Risk", ylab = "Target Return",
              main = "Convex Hull with Risk Surface:\nStd.Dev. of MRC and MDP-line")
lines(x = dropt[, 2], y = dropt[, 1], col = "blue", lwd = 2)
box()

## Computing special points and plotting
frontier <- rtn %>% portfolioFrontier()
MVP <- frontier %>% minvariancePoints()
TGP <- frontier %>% tangencyPoints()
sap <- frontier %>% singleAssetPoints()

wewp <- rep(1/NAssets, NAssets)
mewp <- crossprod(mu, wewp)
sewp <- sqrt(crossprod(wewp, Sigma) %*% wewp)
ERC <- PERC(Sigma)
werc <- Weights(ERC) / 100.0
merc <- crossprod(mu, werc)
serc <- sqrt(crossprod(werc, Sigma) %*% werc)
points(sap, col = "darkgreen", pch = 19, cex = 0.8)
text(sap, ANames, col = "darkred", cex = 0.6, pos = 4)
points(TGP, col = "tan", pch = 19, cex = 2.5)
text(TGP[1], TGP[2], "TGP", col = "purple", cex = 0.5)
points(x = sewp, y = mewp, col = "tan", pch = 19, cex = 2.5)
text(sewp, mewp, "EWP", col="purple", cex = 0.5)
points(x = serc, y = merc, col = "tan", pch = 19, cex = 2.5)
text(serc, merc, "ERC", col="purple", cex = 0.5)
points(MVP, col = "tan", pch = 19, cex = 2.5)
text(MVP[1], MVP[2], "MVP", col = "purple", cex = 0.5)




# ライブラリ
sdmrc <- surf$z
c104 <- which((sdmrc >= 10.35) & (sdmrc <= 10.45), arr.ind = TRUE)
w104 <- matrix(NA, nrow = nrow(c104), ncol = NAssets) %>% set_colnames(ANames)

for(i in seq_len(nrow(c104))){
    gidx <- which((allWeights[, 1] == c104[i, 1]) &
                  (allWeights[, 2] == c104[i, 2]), arr.ind = TRUE)
    w104[i, ] <- allWeights[gidx, -c(1, 2)]
}
## Computing standard deviations of mrc and standard deviation risk
sdmrc104 <- w104 %>% apply( 1, function(x) sd(mrc(x, Sigma = Sigma)))
sdr104 <- w104 %>% apply( 1, function(x) sqrt(crossprod(x, Sigma) %*% x)) * 100

## Grouping by asset class
wEquity <- w104[, 1:6] %>% rowSums()
wBonds <- w104[, 7:9] %>% rowSums()
wGold <- w104[, 10]

# 結合
wAsset <- cbind(wEquity, wBonds, wGold) * 100

# 出力データ作成
ans <-
  cbind(wAsset, sdmrc104, sdr104) %>%
    set_colnames(c("Equity", "Bonds", "Gold", "StdDev. of MRC", "StdDev. Risk")) %>%
    set_rownames(seq_len(nrow(ans)))

# データ確認
print(ans)
