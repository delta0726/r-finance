# ***********************************************************************************************
# Library : ichimoku
# Theme   : Cloud Charts - The Reference Manual
# Date    : 2023/04/16
# URL     : https://cran.r-project.org/web/packages/ichimoku/vignettes/reference.html
# ***********************************************************************************************


# ＜一目均衡表の構成要素＞
# 転換線      ：過去9期間の高値と安値の中間点（当期を含む）
# 基準線      ： 過去26期間の最高値と安値の中間点 (現在の期間を含む)
# 先行スパン1 ： 26 期間 (現在の期間を含む) 先にプロットされた転換線と基順線の中間点
# 先行スパン2 ： 過去 52 期間 (当期を含む) の最高値と安値の中間点を、26 期間 (当期を含む) 先にプロットします
# 遅行スパン  ： 遅行スパン：26期（当期を含む）の後ろにプロットした当期の終値
# 雲         ： は、先行スパン A と先行スパン B によって囲まれた領域です (通常、チャート上で陰になっています)


# ＜目次＞
# 0 準備
# 1 一目均衡表の作成
# 2 一目均衡データの不要データの扱い
# 3 カスタム休日の定義
# 4 一目均衡データの変換
# 5 一目均衡表のプロット作成
# 6 インタラクティブプロット
# 7 オブジェクトの構造


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(ichimoku)

# データロード
TKR <- sample_ohlc_data

# データ確認
TKR %>% dim()
TKR %>% head()


# 1 一目均衡表の作成 -----------------------------------------------------

# ＜関数概要＞
# ichimoku(x, ticker, periods = c(9L, 26L, 52L), keep.data, ...)


# 一目均衡表データ作成
# --- プロットも同時に作成される（プロットはxtsのもの）
cloud <- TKR %>% ichimoku()

# データ確認
cloud %>% print(plot = FALSE)
cloud %>% print(plot = TRUE)

# データ構造
cloud %>% class()
cloud %>% glimpse()

# サマリー
cloud %>% summary()


# 2 一目均衡データの不要データの扱い --------------------------------------------------

# ＜概要＞
# - 入力オブジェクトには、ボリュームなど、一目の分析に不要な追加データが含まれている場合があります
# - ichimoku() を呼び出すときに keep.data = TRUE を指定します。

# データ保存
kumo_true  <- TKR %>% ichimoku(keep.data = TRUE)
kumo_false <- TKR %>% ichimoku(keep.data = FALSE)
kumo_true %>% as.data.frame() %>% colnames()
kumo_false %>% as.data.frame() %>% colnames()


# 3 カスタム休日の定義 -------------------------------------------------------------

# ＜概要＞
# - holidays引数を使うと休日を指定することができる


# ベクトル指定
TKR %>% ichimoku(holidays = c("2020-01-13", "2020-02-11", "2020-02-24"))

# 休日データ
timeDate::holidayLONDON()
timeDate::holidayLONDON() %>% class()

# Or via a functions that returns a vector of dates (e.g. from the 'timeDate' package):
TKR %>% ichimoku(holidays = timeDate::holidayLONDON())
TKR %>% ichimoku(holidays = timeDate::holidayNYSE())


# 4 一目均衡データの変換 -------------------------------------------------------------

# 一目均衡表データ作成
cloud <- TKR %>% ichimoku()

# インデックスの取得
index <- cloud %>% index()
index %>% print()
index %>% class()

# メインデータの取得
core <- cloud %>% coredata()
core %>% print()
core %>% class()


cloud2 <- ichimoku(ichimoku(cbind(index, core), ticker = attr(cloud, "ticker")))

# データ検証
identical(cloud, cloud2)


# 5 一目均衡表のプロット作成 ---------------------------------------------------

# 一目均衡表データ作成
cloud <- TKR %>% ichimoku()

# プロット作成
# --- タイトルなどの装飾あり
# --- ticker引数はサブタイトルを指定するために使用される
cloud %>%
  plot(window = "2020-05/", ticker = "SYM (JSE)", subtitle = "Sample Data Series")

# プロット作成
# --- type引数に"r"か"s"を指定すると、オシレーターが主要な一目の雲チャートの下に表示される
# --- r： RSI  s：stochastic oscillator
cloud %>% plot(type = "r")
cloud %>% plot(type = "s")

# テーマ変更
cloud %>% plot(window = "2020-04-01/2020-12-01", theme = "solarized", type = "s")

# カスタム・サブプロット
kumo %>% plot(window = "2020-04/2020-11", theme = "mono", type = "bar", custom = "volume")


# 6 インタラクティブ・プロット ---------------------------------------------------

# ＜ポイント＞
# - ichimokuオブジェクトの構造が非常に分かりやすくなるので一目の価値あり

# プロット
cloud %>% iplot()

# オブジェクト
# ---  "shiny.appobj"
obj_iplot <- cloud %>% iplot()
obj_iplot %>% class()


# 7 オブジェクトの構造 ----------------------------------------------------------

# Index:
# index(object) - date-time index [POSIXct]
# Columns [numeric]:
# object$open - opening price
# $high - high price
# $low - low price
# $close - closing price
# $cd - candle direction (-1 = down, 0 = flat, 1 = up)
# $tenkan - Tenkan-sen
# $kijun - Kijun-sen
# $senkouA - Senkou span A
# $senkouB - Senkou span B
# $chikou - Chikou span
# $cloudT - cloud Top (max of senkouA, senkouB)
# $cloudB - cloud Base (min of senkouA, senkouB)
# Attributes:
# attributes(object)$periods - parameters used to calculate the cloud [integer vector of length 3]
# $periodicity - periodicity of the data in seconds [numeric]
# $ticker - instrument identifier [character]


