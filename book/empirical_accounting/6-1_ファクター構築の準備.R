# ***********************************************************************************************
# Title   : 実証会計・ファイナンス（Rによる財務・株式データの取得）
# Chapter : 6 ファクターモデルの準備
# Theme   : 1 ファクター構築の準備
# Date    : 2022/10/14
# Page    : P225 - P236
# URL     : https://www2.econ.osaka-u.ac.jp/~eaafinr/sect-3.html
# ***********************************************************************************************


# ＜概要＞
# - CAPMやFF3などのモデルで使用するデータセットを作成する


# ＜目次＞
# 0 準備
# 1 年次データの加工
# 2 月次データの加工
# 3 ファクターデータの作成
# 4 市場ポートフォリオの累積リターンの可視化
# 5 年次データに時価総額の分位を追加
# 6 月次超過リターンの平均値を計算
# 7 分位ごとの期間平均リターンの比較
# 8 BPにより分位ポートフォリオを作成
# 9 BPにより加重ウエイトで分位ポートフォリオを作成


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(broom)
library(gridExtra)


# データロード
# --- リターンと財務データを結合した月次データ
# --- リターンと財務データを結合した年次データ
monthly_data <- read_csv("data/ch05_output1.csv")
annual_data  <- read_csv("data/ch05_output2.csv")

# データ確認
# --- 含まれる列は異なる点に注意
monthly_data %>% names()
annual_data %>% names()


# 1 年次データの加工 -------------------------------------------------------------

# ＜ポイント＞
# - 年次データに期初ウエイトを追加する

# データ加工
# --- 期初の時価総額(lagged_ME)を作成
# --- 年ごとにグループ化して期初ウエイト(w_M)の作成
# --- 期初ウエイト(w_M)のうち2016年以降でNAなものは0に置換
annual_data <-
  annual_data %>%
    group_by(firm_ID) %>%
    mutate(lagged_ME = lag(ME)) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(w_M = lagged_ME / sum(lagged_ME, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(w_M = replace(w_M, year >= 2016 & is.na(w_M), 0))

# データ確認
# --- 年ごとのウエイト合計が1（ポートフォリオを作成）
annual_data %>%
  group_by(year) %>%
  summarize(weight_sum = sum(w_M)) %>%
  ungroup()


# 2 月次データの加工 ------------------------------------------------------------

# 結合用データの作成
# --- 個別銘柄の年初ウエイトを取得
annual_weight <-
  annual_data %>%
    select(year, firm_ID, w_M)

# データ加工
# --- 月次データに年初ウエイトを追加
monthly_data <-
  monthly_data %>%
    full_join(annual_weight, by = c("year", "firm_ID")) %>%
    select(-w_M, w_M)

# データ保存
# monthly_data %>% write_csv("data/ch06_monthly_data.csv")


# 3 ファクターデータの作成 -------------------------------------------------------

# ＜ポイント＞
# - 月次のリターンデータを作成する
# --- リスクフリーレート/マーケットリターン/リスクプレミアムは月ごとに一意に定義される


# ファクターデータの作成
# --- 2016年以降のデータを抽出
# --- 各銘柄の月次リターンの加重平均を計算（年次ウエイトを使うのはおかしいが許容）
# --- ｢R_F｣はリスクフリーレート、｢R_M｣はマーケットリターン、｢R_Me｣はリスクプレミアム
factor_data <-
  monthly_data %>%
    filter(year >= 2016) %>%
    group_by(month_ID) %>%
    summarize(R_F = R_F[1],
              R_M = sum(w_M * R, na.rm = TRUE)) %>%
    mutate(R_Me = R_M - R_F)

# データ保存
# factor_data %>% write_csv("data/ch06_factor_data.csv")


# 4 市場ポートフォリオの累積リターンの可視化 -------------------------------------

# ＜ポイント＞
# - 市場ポートフォリオのリターンはベンチマークとしても扱われる重要な要素


# プロットデータの作成
# --- 累積リターンの追加
plot_data <-
  factor_data %>%
    mutate(gross_R_M = 1 + R_M,
           cumulative_gross_R_M = cumprod(gross_R_M))

# プロット作成
# --- 累積リターンの推移
# --- 期初が1になっていない
plot_data %>%
  ggplot(aes(x = month_ID, y = cumulative_gross_R_M)) +
  geom_line() +
  geom_point() +
  labs(x = "Month ID", y = "Cumulative Gross Return") +
  theme_tq()

# プロット作成
# --- 折れ線グラフの期初の1を追加
# --- 元本の水準を点線で図示
plot_data %>%
  select(month_ID, cumulative_gross_R_M) %>%
  add_row(month_ID = 12, cumulative_gross_R_M = 1, .before = 1) %>%
  ggplot(aes(x = month_ID, y = cumulative_gross_R_M)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  labs(x = "Month ID", y = "Cumulative Gross Return") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_tq()


# 5 年次データに時価総額の分位を追加 ----------------------------------------------

# ＜ポイント＞
# - 時価総額に基づいて10分位の分位情報を追加する


# 分位の追加
# --- 時価総額に基づいて10分位化
annual_data <-
  annual_data %>%
    group_by(year) %>%
    mutate(ME_rank10 = as.factor(ntile(lagged_ME, 10))) %>%
    ungroup()

# 確認
annual_data %>%
  select(year, firm_ID, ME_rank10) %>%
  drop_na() %>%
  group_by(year, ME_rank10) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(names_from = ME_rank10, values_from = n)


# 6 月次超過リターンの平均値を計算 --------------------------------------------------

# 分位ごとの月次超過リターンの平均値を計算
# --- Reはリスクフリーレートに対する超過リターン
ME_sorted_portfolio <-
  annual_data %>%
    select(year, firm_ID, ME_rank10) %>%
    full_join(monthly_data, by = c("year", "firm_ID")) %>%
    drop_na() %>%
    group_by(month_ID, ME_rank10) %>%
    summarize(Re = mean(Re)) %>%
    ungroup()

# データ保存
# ME_sorted_portfolio %>% write_csv("data/ch06_ME_sorted_portfolio.csv")


# 7 分位ごとの期間平均リターンの比較 -------------------------------------------------

# 平均リターンの計算
# --- 分位別の全銘柄かつ全期間の平均リターン
ME_cross_sectional_return <-
  ME_sorted_portfolio %>%
    group_by(ME_rank10) %>%
    summarize(mean_Re = mean(Re)) %>%
    ungroup()

# プロット作成
# --- サイズによる分位別の平均リターン
ME_cross_sectional_return %>%
  ggplot(aes(x = ME_rank10, y = mean_Re)) +
    geom_col() +
    labs(x = "ME Rank", y = "Mean Monthly Excess Return") +
    scale_y_continuous(expand = c(0, 0)) +
    theme_tq()

# データ保存
ME_cross_sectional_return %>% write_csv("data/ch06_ME_cross_sectional_return.csv")


# 8 BPにより分位ポートフォリオを作成 ---------------------------------------------------

# 列の追加
# --- BPとBPによる分位
annual_data <-
  annual_data %>%
    mutate(lagged_BEME = lagged_BE / lagged_ME) %>%
    group_by(year) %>%
    mutate(BEME_rank10 = as.factor(ntile(lagged_BEME, 10))) %>%
    ungroup()

# 分位ごとの月次超過リターンの平均値を計算
# --- Reはリスクフリーレートに対する超過リターン
BEME_sorted_portfolio <-
  annual_data %>%
    select(year, firm_ID, BEME_rank10, lagged_ME) %>%
    full_join(monthly_data, by = c("year", "firm_ID")) %>%
    drop_na() %>%
    group_by(month_ID, BEME_rank10) %>%
    summarize(Re = mean(Re)) %>%
    ungroup()

# プロット作成
# --- BPによる分位別の平均リターン
p1 <-
  BEME_sorted_portfolio %>%
    group_by(BEME_rank10) %>%
    summarize(mean_Re = mean(Re)) %>%
    ggplot() +
    geom_col(aes(x = BEME_rank10, y = mean_Re)) +
    geom_hline(yintercept = 0) + # y = 0の直線を追加
    labs(x = "BE/ME Rank", y = "Mean Monthly Excess Return") +
    scale_y_continuous(limits = c(-0.005, 0.02)) +
    theme_tq()

# プロット表示
print(p1)

# データ保存
# annual_data %>% write_csv("data/ch06_annual_data.csv")


# 9 BPにより加重ウエイトで分位ポートフォリオを作成 ---------------------------------------

# 分位ごとの月次超過リターンの平均値を計算
# --- Reはリスクフリーレートに対する超過リターン
# --- リターン集計をウエイト加重に変更
BEME_sorted_portfolio_w_mcap <-
  annual_data %>%
    select(year, firm_ID, BEME_rank10, lagged_ME) %>%
    full_join(monthly_data, by = c("year", "firm_ID")) %>%
    drop_na() %>%
    group_by(month_ID, BEME_rank10) %>%
    mutate(w = lagged_ME / sum(lagged_ME)) %>% # 各ポートフォリオで保有比率を計算
    summarize(Re = sum(w * Re)) %>% # 時価総額加重の月次超過リターンを計算
    ungroup()

# プロット作成
# --- BPによる分位別の平均リターン
p2 <-
  BEME_sorted_portfolio_w_mcap %>%
    group_by(BEME_rank10) %>%
    summarize(mean_Re = mean(Re)) %>%
    ungroup() %>%
    ggplot(aes(x = BEME_rank10, y = mean_Re)) +
    geom_col() +
    geom_hline(yintercept = 0) +
    labs(x = "BE/ME Rank", y = "Mean Monthly Excess Return") +
    scale_y_continuous(limits = c(-0.005, 0.02)) +
    theme_tq()


# プロット表示
print(p2)
grid.arrange(p1, p2)
