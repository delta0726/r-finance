# ***********************************************************************************************
# Library : ichimoku
# Theme   : Auxiliary Functions
# Date    : 2023/04/17
# URL     : https://cran.r-project.org/web/packages/ichimoku/vignettes/utilities.html
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 tradingDays()
# 2 look()
# 3 xts_df()
# 4 df_merge()
# 5 df_append()


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(ichimoku)

# データロード
TKR <- sample_ohlc_data

# データ確認
TKR %>% dim()
TKR %>% head()


# 1 tradingDays() --------------------------------------------------------

# ＜ポイント＞
# - カレンダーを管理する


# 日付ベクトル
# --- 7days calendar
dates <- seq(from = as.POSIXct("2020-01-01"), by = "1 day", length.out = 7)
dates

# 取引日付ベクトル
# --- デフォルトでは1/1と12/24と土日は休日とみなす
dates %>% tradingDays()

# 休日指定
dates %>% tradingDays(holidays = c("2020-01-02", "2020-01-03"))

# 全てをTrading Dayとみなす
dates %>% tradingDays(holidays = NULL)


# 2 look() ------------------------------------------------------------------

# ＜ポイント＞
# - 一目均衡データのパラメータを確認する


# パラメータ確認
# --- 一目均衡データの作成
sample_ohlc_data %>% ichimoku(ticker = "TKR") %>% look()

# パラメータ確認
# --- 一目均衡トレーディング戦略
cloud %>% strat() %>% look()

# パラメータ確認
# --- 一目均衡トレーディング戦略
cloud %>% mlgrid() %>% look()


# 3 xts_df() -----------------------------------------------------------------

# 一目均衡データの作成
cloud <- sample_ohlc_data %>% ichimoku()

# データ確認
cloud %>% class()
cloud %>% str()


# データ変換
df <- cloud %>% xts_df()
df %>% str()


# データ変換
# --- 属性情報を維持
df2 <- cloud %>% xts_df(keep.attrs = TRUE)
df2 %>% str()


# 4 df_merge() -----------------------------------------------------------------

# ＜ポイント＞
# - OHLCデータのデータフレームを時系列方向に結合
#   --- 重複も


# データ確認
sample_ohlc_data %>% str()

# データ抽出1
data1 <- sample_ohlc_data[1:6, ]
data1

# データ抽出2
data2 <- sample_ohlc_data[4:10, ]
data2

# データ結合
df_merge(data1, data2)
df_merge(data1, data2) %>% str()


# 5 df_append() -----------------------------------------------------------------

# ＜ポイント＞
# - OHLCデータのデータフレームを時系列方向に結合


# データ確認
sample_ohlc_data %>% str()

# データ抽出1
data1 <- sample_ohlc_data[1:8, ]
data1

# データ抽出2
data2 <- sample_ohlc_data[7:10, ]
data2

# データ結合
df_merge(data1, data2)
df_merge(data1, data2) %>% str()
