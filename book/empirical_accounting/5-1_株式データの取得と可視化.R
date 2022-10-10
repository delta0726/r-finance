# ***********************************************************************************************
# Title   : 実証会計・ファイナンス（Rによる財務・株式データの取得）
# Chapter : 5-1 株式データの取得と可視化
# Date    : 2022/10/09
# Page    : P187 - P214
# URL     : https://www2.econ.osaka-u.ac.jp/~eaafinr/sect-3.html
# ***********************************************************************************************


# ＜概要＞
# - Rのデータフレームを用いた株式リターンの演算方法を確認する


# ＜目次＞
# 0 準備
# 1 データ概要
# 2 時価総額とリターンの計算
# 3 トータルリターンと超過リターンの計算
# 4 株式データの探索的データ分析
# 5 リターンの累積
# 6 株式データと財務データの結合
# 7 企業サイズと時価総額によるバブルチャート可視化


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(scales)
library(psych)


# データロード
stock_data <- read_csv("data/ch05_stock_data.csv")
financial_data <- read_csv("data/ch04_output.csv")


# 1 データ概要 -------------------------------------------------------------------

# データ確認
# --- 株価データ
stock_data %>% print()
stock_data %>% glimpse()

# コーポレートイベントの確認
# --- 配当支払や株式分割は株価調整が必要なので要注意
stock_data %>%
  filter(firm_ID == 1 & month_ID %in% 27:30)

# 株式分割
# --- 発行済株式数と調整係数の連動を確認
stock_data %>%
  filter(firm_ID == 74 & month_ID %in% 29:32)


# 2 時価総額とリターンの計算 ----------------------------------------------------------

# 時価総額の追加
stock_data <-
  stock_data %>%
    mutate(ME = stock_price * shares_outstanding)

# ヒストグラム作成
# --- x軸の上限を95%点に設定
# --- 単位を100万円にした上で桁区切りのカンマを追加
stock_data %>%
  ggplot() +
  geom_histogram(aes(x = ME)) +
  labs(x = "Market Equity", y = "Count") +
  scale_x_continuous(limits = c(0, quantile(stock_data$ME, 0.95)),
                     labels = label_comma(scale = 1e-6)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()


# 3 トータルリターンと超過リターンの計算 -------------------------------------------------

# 前月株価の取得
# --- firm_IDに関してグループ化
# --- ラグ系列の作成
stock_data %>%
  group_by(firm_ID) %>%
  mutate(lagged_stock_price = lag(stock_price)) %>%
  ungroup() %>%
  select(year, month, firm_ID, stock_price, lagged_stock_price)

# 月次リターンの追加
# --- 株式分割と配当を調整した系列を作成（stock_price_adjusted）
# --- 無リスク金利に対する超過リターンも作成（Re）
stock_data <-
  stock_data %>%
    group_by(firm_ID) %>%
    mutate(lagged_stock_price = lag(stock_price)) %>%
    ungroup() %>%
    mutate(stock_price_adj = (stock_price + DPS) * adjustment_coefficient,
           R = (stock_price_adj - lagged_stock_price) / lagged_stock_price,
           Re = R - R_F) %>%
    select(year, month, month_ID, firm_ID, stock_price, stock_price_adj,
           lagged_stock_price, adjustment_coefficient, DPS, R, R_F, Re, ME)

# データ確認
# --- 株式分割と配当を調整したリターン計算
stock_data %>%
  filter(firm_ID == 74 & month_ID %in% 29:32)

# データ確認
# --- 超過リターンの計算
stock_data %>%
  mutate(year, month, month_ID, firm_ID, R, R_F, Re)


# 4 株式データの探索的データ分析 -------------------------------------------------

# 月次リターンの要約統計量
stock_data$R %>% summary()

# 月次リターンの標準偏差と分散
stock_data$R %>% sd(na.rm = TRUE)
stock_data$R %>% var(na.rm = TRUE)

# 月次リターンの歪度
skewness <- function(x) (1 / length(x)) * sum(((x - mean(x)) / sd(x))^3)
stock_data$R %>% na.omit() %>% skewness()
stock_data$R %>% skew()

# 月次リターンの尖度
# --- 定義に-3を追加
kurtosis <- function(x) (1 / length(x)) * sum(((x - mean(x)) / sd(x))^4) - 3
stock_data$R %>% na.omit() %>% kurtosis()
stock_data$R %>% kurtosi()

# ヒストグラム
# --- 月次リターン
stock_data %>%
  ggplot(aes(x = R)) +
    geom_histogram() +
    labs(x = "Monthly Stock Return", y = "Count") +
    scale_y_continuous(expand = c(0, 0)) +
    theme_classic()


# 5 リターンの累積 ---------------------------------------------------------

# 累積リターン
# --- summarise()でprod()を使って月次に集約
# --- firm_IDとyearのペアでグループ化
# --- バイ・アンド・ホールドの年次リターン
annual_stock_data <-
  stock_data %>%
    group_by(firm_ID, year) %>%
    summarize(R = prod(1 + R) - 1,
              R_F = prod(1 + R_F) - 1,
              .groups = "drop") %>%
    mutate(Re = R - R_F) %>%
    select(firm_ID, year, R, Re, R_F) %>%
    ungroup()

# 累和リターン
# --- 元本が一定となるよう毎月リバランスした場合の年次リターン
stock_data %>%
  group_by(firm_ID, year) %>%
  summarize(simple_cumulative_R = sum(R)) %>%
  ungroup()


# 6 株式データと財務データの結合 ----------------------------------------------

# 財務データの確認
financial_data %>% glimpse()

# レコード数の確認
financial_data %>% nrow()
annual_stock_data %>% nrow()
stock_data %>% nrow()

# データ結合
annual_data <-
  annual_stock_data %>%
    full_join(financial_data, by = c("firm_ID", "year"))

monthly_data  <-
  stock_data %>%
    full_join(financial_data, by = c("firm_ID", "year"))

# 確認
annual_data %>% nrow()
monthly_data %>% nrow()


# 参考：full_join()関数の動作
A <- tibble(firm_ID = c(1, 2), stock_price = c(120, 500))
B <- tibble(firm_ID = c(1, 3), DPS = c(5, 10))
A %>% full_join(B, by = "firm_ID")
A %>% inner_join(B, by = "firm_ID")
A %>% left_join(B, by = "firm_ID")
A %>% right_join(B, by = "firm_ID")


# 7 企業サイズと時価総額によるバブルチャート可視化 ------------------------

# ＜ポイント＞
# - 売上高と純利益で散布図を作成した上で、時価総額によりバブルチャート化


# データ作成
# --- 12月のデータのみを抽出
# --- 年次データと結合
annual_data <-
  stock_data %>%
    filter(month == 12) %>%
    select(year, firm_ID, ME) %>%
    full_join(annual_data, ., by = c("year", "firm_ID")) %>%
    mutate(ME = ME / 1e6)

# プロット作成
# --- firm_IDが2から20のデータを抽出
# --- 対数を取るため当期純利益（X）が正のデータのみ抽出
annual_data %>%
  filter(year == 2015, firm_ID %in% 2:20, X > 0) %>%
  select(sales, X, ME) %>%
  ggplot(aes(x = log(sales), y = log(X), size = ME)) +
  geom_point(alpha = 0.4) +
  scale_size(range = c(1, 20), name = "Market Equity") +
  scale_x_continuous(limits = c(8, 14)) +
  scale_y_continuous(limits = c(2, 11)) +
  theme_classic()

# データ保存
# annual_data %>% write_csv("data/ch05_annual_data.csv")
