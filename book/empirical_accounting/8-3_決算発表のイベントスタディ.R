# ***********************************************************************************************
# Title   : 実証会計・ファイナンス（Rによる財務・株式データの取得）
# Chapter : 8 イベント・スタディ
# Theme   : 3 決算発表のイベントスタディ
# Date    : 2022/10/20
# Page    : P354 - P373
# URL     : https://www2.econ.osaka-u.ac.jp/~eaafinr/sect-3.html
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 決算サプライズの計算
# 3 イベント分析テーブルの作成
# 4 マーケットモデルの推定
# 5 累積異常リターン(CAR)の計算
# 6 イベントの平均CARの推移を可視化
# 7 イベントリターンの統計的検定


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)


# データロード
return_data <- read_csv("data/ch08_return_data.csv")
market_return_data <- read_csv("data/ch08_market_return_data.csv")
event_data <- read_csv("data/ch08_event_data.csv")


# 1 データ準備 ----------------------------------------------------------------

# 市場リターンデータ
market_return_data <-
  market_return_data %>%
    mutate(date_ID = row_number()) %>%
    select(date, date_ID, R_M )

# 日付IDテーブル
data_ID_table <-
  market_return_data %>%
    select(date, date_ID)

# 個別銘柄リターンデータ
# --- 市場リターンと日付IDを結合
return_data <-
  return_data %>%
    full_join(data_ID_table, by = "date") %>%
    select(-date) %>%
    full_join(market_return_data, by = "date_ID")

# データ確認
# --- 市場リターンの日数をカウント
# --- 企業数をカウント
market_return_data %>% nrow()
return_data$firm_ID %>% unique() %>% length()


# 2 決算サプライズの計算 ----------------------------------------------------

# 決算イベントデータ
# --- 予想利益サプライズを計算
# --- 各年度ごとにイベントの強弱を5段階に区分
# --- 日付データから年度を抽出
event_data <-
  event_data %>%
    mutate(event_ID = row_number(),
           surprise = (earnings_forecast - realized_earnings) / lagged_ME,
           year = as.integer(substr(event_date, 1, 4))) %>%
    group_by(year) %>%
    mutate(event_strength = as.factor(ntile(surprise, 5))) %>%
    ungroup() %>%
    left_join(data_ID_table, by = c("event_date" = "date")) %>%
    select(event_ID, date_ID, firm_ID, event_strength) %>%
    rename(event_date_ID = date_ID)


# 3 イベント分析テーブルの作成 ----------------------------------------------------

# パラメータ
# --- 決算イベント数をカウント
# --- モデルの推定期間
# --- イベント前の分析期間
# --- イベント後の分析期間
N_events <- event_data %>% nrow()
N1 <- 100
N2 <- 30
N3 <- 30

# イベント分析テーブルの作成
# --- イベント分析パターンを列挙したテーブルを作成
# --- イベントデータと結合
# --- リターンデータと結合
full_sample_data <-
  tibble(event_ID = sort(rep(1:N_events, N1 + N2 + N3 + 1)),
         relative_days = rep(-(N1 + N2):N3, N_events)) %>%
    full_join(event_data, by = "event_ID") %>%
    mutate(date_ID = event_date_ID + relative_days + 1) %>%
    left_join(return_data, by = c("firm_ID", "date_ID")) %>%
    select(event_ID, event_strength, relative_days, R, R_M)


# 4 マーケットモデルの推定 --------------------------------------------------------

# 推定期間のデータのみ抽出
estimation_window_data <-
  full_sample_data %>%
  filter(relative_days < -N2)

# 推定結果を保存するために空のリストを準備
market_model_results <- list(NA)

i <- 1
for (i in 1:N_events) {

  # マーケット・モデルの推定
  lm_results <-
    estimation_window_data %>%
      filter(event_ID == i) %>%
      lm(R ~ R_M, data = .)

  # 結果
  market_model_results[[i]] <-
    lm_results %>%
      tidy() %>%
      mutate(event_ID = i,
             sigma_AR = glance(lm_results)$sigma) %>%
      select(event_ID, everything())
}


# 5 累積異常リターン(CAR)の計算 --------------------------------------------------------

# データ整理
full_sample_data <-
  market_model_results %>%
    bind_rows() %>%
    pivot_wider(id_cols = c("event_ID", "sigma_AR"), names_from = "term", values_from = "estimate") %>%
    rename(alpha = "(Intercept)", beta = R_M) %>%
    full_join(full_sample_data, by = "event_ID") %>%
    select(-c("alpha", "beta", "sigma_AR"), c("alpha", "beta", "sigma_AR"))


# 累積異常リターン(CAR)の計算
event_window_data <-
  full_sample_data %>%
    filter(relative_days >= -N2) %>%
    mutate(R_normal = alpha + beta * R_M,
           AR = R - R_normal) %>%
    group_by(event_ID) %>%
    mutate(CAR = cumsum(AR)) %>%
    ungroup()


# 6 イベントの平均CARの推移を可視化 ------------------------------------------------

# 平均CARの計算
mean_CAR_by_event_strength <-
  event_window_data %>%
    group_by(relative_days, event_strength) %>%
    summarize(mean_CAR = mean(CAR)) %>%
    ungroup()

# プロット作成
mean_CAR_by_event_strength %>%
  ggplot(aes(x = relative_days, y = mean_CAR, linetype = event_strength)) +
  geom_line() +
  scale_linetype_manual(values = c("dotted", "dotdash", "dashed", "longdash", "solid")) +
  annotate("rect", xmin = -1, xmax = 0, ymin = -Inf, ymax = Inf, alpha = 0.1) +
  labs(x = "Relative Days", y = "Mean CAR", linetype = "Event Strength") +
  scale_x_continuous(expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_classic()


# 7 イベントリターンの統計的検定 ----------------------------------------------------------

output_table <-
  event_window_data %>%
    filter(event_strength == 5) %>%
    group_by(relative_days) %>%
    summarize(mean_AR = mean(AR),
              mean_CAR = mean(CAR),
              sigma_mean_AR = sqrt(sum(sigma_AR^2)) / n()) %>%
    ungroup() %>%
    mutate(t_value = mean_AR / sigma_mean_AR,
           p_value = (1 - pnorm(abs(t_value))) * 2) %>%
    mutate(significance = cut(p_value,
                              breaks = c(0, 0.01, 0.05, 0.1, 1),
                              labels = c("***", "**", "*", ""),
                              include.lowest = TRUE)) %>%
    select(relative_days, mean_AR, t_value, p_value, significance, mean_CAR) %>%
    mutate(mean_AR = round(mean_AR, 5) * 100,
           t_value = round(t_value, 2),
           p_value = round(p_value, 2),
           mean_CAR = round(mean_CAR, 5) * 100)


# 結果確認
output_table %>% print()
