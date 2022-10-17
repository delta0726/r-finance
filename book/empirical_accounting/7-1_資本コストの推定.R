# ***********************************************************************************************
# Title   : 実証会計・ファイナンス（Rによる財務・株式データの取得）
# Chapter : 7 ファクター・モデルの応用
# Theme   : 1 資本コストの推定
# Date    : 2022/10/12
# Page    : P296 - P319
# URL     : https://www2.econ.osaka-u.ac.jp/~eaafinr/sect-3.html
# ***********************************************************************************************


# ＜概要＞
# - 資本コストとは企業が資金調達を行う際に資金提供者に約束する期待リターンを指す
#   --- 期待リターンをFF3モデルから算出する
#   --- FF3モデルの回帰係数を個別銘柄ごとに推定するのがポイント


# ＜目次＞
# 0 準備
# 1 使用するデータの整理
# 2 企業ごとにFF3モデルを実行
# 3 ファクターローディングの算出
# 4 ファクターリターンの算出
# 5 株式資本コストの推定
# 6 サイズ別に株式資本コストの推定値を可視化
# 7 潜在配当成長率の推定


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(magrittr)
library(broom)

# データロード
monthly_data <- read_csv("data/ch05_output1.csv")
annual_data  <- read_csv("data/ch05_output2.csv")
factor_data <- read_csv("data/ch06_output.csv")


# 1 使用するデータの整理 --------------------------------------------------------

# ＜ポイント＞
# - 以降で企業ごとに回帰モデルを構築するため、サンプル数の少ない企業は排除する


# データ加工
# --- 2020年に存在する企業を抽出
# --- 36ヶ月以上のデータがあるfirm_IDを抽出
monthly_tidy_data <-
  monthly_data %>%
    group_by(firm_ID) %>%
    mutate(is_public_2020 = (max(month_ID) == 72),
           N_observations = n()) %>%
    ungroup() %>%
    filter(is_public_2020 == TRUE,
           N_observations >= 36) %>%
    select(-N_observations, -R_F) %>%
    full_join(factor_data, by = "month_ID") %>%
    select(firm_ID, year, month_ID, Re, R_Me, SMB, HML, DPS, shares_outstanding, ME)


# 2 企業ごとにFF3モデルを実行 --------- ------------------------------------

# ＜ポイント＞
# - 企業ごとにFF3モデルを構築して回帰モデルの結果を取得する
#   --- 個別銘柄に絞って回帰しているので時系列回帰となっている
#   --- 分析期間の推定値が一意に定義される


# 銘柄数
monthly_tidy_data$firm_ID %>% unique() %>% length()

# FF3の実行
# --- 各企業ごとの回帰係数の抽出
# --- 定数項なしでモデル化
FF3_results <-
  monthly_tidy_data %>%
    select(firm_ID, Re, R_Me, SMB, HML) %>%
    group_by(firm_ID) %>%
    nest() %>%
    mutate(FF3_regression = map(data, ~ lm(Re ~ 0 + R_Me + SMB + HML, data = .)),
           FF3_summary = map(FF3_regression, tidy)) %>%
    select(-c(data, FF3_regression)) %>%
    unnest(cols = FF3_summary) %>%
    ungroup()

# 確認
FF3_results %>% print()


# 3 ファクターローディングの算出 ------------------------------------

# ＜ポイント＞
# - 回帰モデルの結果から回帰係数を取得してファクターローディングとする


# ファクターローディングの作成
# --- 回帰係数をワイド型に整形
FF3_loadings <-
  FF3_results %>%
    pivot_wider(id_cols = "firm_ID", names_from = "term", values_from = "estimate") %>%
    rename(beta_M = R_Me, beta_SMB = SMB, beta_HML = HML)

# 確認
FF3_loadings %>% print()


# 4 ファクターリターンの算出 ------------------------------------------------

# ＜ポイント＞
# - ファクターリターンの平均値を期待リターンとして使用する


# リスクプレミアムの計算
factor_risk_premium <-
  factor_data %>%
    select(R_Me, SMB, HML) %>%
    apply(MARGIN = 2, mean) %>%
    multiply_by(12)


# 5 株式資本コストの推定 ------------------------------------------------------------

# ＜ポイント＞
# - ファクターの期待リターンとローディングから銘柄ごとの期待リターンを算出する


# ファクター・ローディングを行列変換
factor_loadings <-
  FF3_loadings %>%
    select(beta_M, beta_SMB, beta_HML) %>%
    as.matrix()

# 期待リターンを計算
expected_R_FF3 <- factor_loadings %*% factor_risk_premium

# テーブル作成
FF3_cost_of_capital <-
  tibble(firm_ID = FF3_loadings$firm_ID,
         cost_of_capital = as.vector(expected_R_FF3))

# 確認
FF3_cost_of_capital %>% print()

# プロット作成
# --- 株式資本コストの推定値をヒストグラムで可視化
FF3_cost_of_capital %>%
  ggplot(aes(x = cost_of_capital)) +
    geom_histogram() +
    labs(x = "FF3 Cost of Capital", y = "Count") +
    scale_y_discrete(expand = c(0, 0)) +
    theme_tq()

# データ保存
# FF3_cost_of_capital %>% write_csv("data/ch06_FF3_cost_of_capital.csv")


# 6 サイズ別に株式資本コストの推定値を可視化 ----------------------------------------------

# 対象企業の抽出
target_firms <-
  monthly_tidy_data %>%
      select(year, firm_ID) %>%
      distinct()

# プロットデータの作成
# --- 2020年のデータのみ抽出
# --- 時価総額に基づき二分割
annual_data_2020 <-
  annual_data %>%
    inner_join(target_firms, by = c("year", "firm_ID")) %>%
    group_by(firm_ID) %>%
    mutate(lagged_ME = lag(ME)) %>%
    ungroup() %>%
    filter(year == 2020) %>%
    mutate(ME_rank2 = as.factor(ntile(lagged_ME, 2)),
           ME_rank2 = fct_recode(ME_rank2, Small = "1", Large = "2")) %>%
    full_join(FF3_cost_of_capital, by = "firm_ID") %>%
    drop_na()

# 密度プロットの作成
# --- 資本コストの分布がsmall/largeで異なることを確認
annual_data_2020 %>%
  ggplot(aes(x = cost_of_capital, group = ME_rank2, color = ME_rank2)) +
    geom_density() +
    labs(x = "FF3 Cost of Capital", y = "Estimated Density", linetype = "Firm Type")  +
    scale_y_continuous(expand = c(0, 0), breaks = NULL) +
    theme_tq()


# 7 潜在配当成長率の推定 ------------------------------------------------------------

# 配当利回りの計算
dividend_yield_2020 <-
  monthly_tidy_data %>%
    select(firm_ID, year, month_ID, DPS, shares_outstanding, ME) %>%
    filter(year == 2020) %>%
    group_by(firm_ID) %>% #
    summarize(annual_dividend = sum(DPS * shares_outstanding),
              latest_ME = ME[which.max(month_ID)]) %>%
    mutate(dividend_yield = annual_dividend / latest_ME) %>%
    filter(dividend_yield > 0)

# 潜在配当成長率の推定
# --- 株式資本コストの推定値を追加
# --- 配当割引モデルから潜在配当成長率を逆算
dividend_yield_2020 %>%
  left_join(FF3_cost_of_capital, by = "firm_ID") %>%
  mutate(implied_dividend_growth = (cost_of_capital - dividend_yield) / (1 + dividend_yield))
