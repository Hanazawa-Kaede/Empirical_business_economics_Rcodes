pacman::p_load(tidyverse, broom, estimatr, fixest, summarytools)

#データの読み込み
#自動車データ
data <- read_csv(file = "source/CleanData_20180222.csv", locale = locale(encoding = "shift-jis"))

#家計数データ（潜在的な市場規模）
dataHH <- read_csv(file = "source/HHsize.csv")

#消費者物価指数の読み込み
dataCPI <- read_csv(file = "source/zni2015s.csv", locale = locale(encoding = "shift-jis"))

dataCPI <- dataCPI[6:56,　] #特定の行を抜き出し
dataCPI <- 
  dataCPI %>% 
  rename( year = '類・品目',
          cpi = '総合') %>% 
  select(year, cpi) %>% 
  mutate( year = as.numeric(year), 
          cpi = as.numeric(cpi)) #列の値を数値型に変換


#データクリーニング
# 必要な変数のみをキープ。
data <-
  data %>%
  dplyr::select(
    Maker, Type, Name, Year, Sales, Model, price, kata,
    weight, FuelEfficiency,　HorsePower,
    overall_length, overall_width, overall_height
  ) %>%
  dplyr::rename(year = Year)

# 家計サイズをマージする。
data <-
  data %>%
  dplyr::left_join(dataHH)

# CPIをマージする
data <- 
  data %>% 
  dplyr::left_join(dataCPI) 


# 燃費が欠損しているデータがある。今回は簡便な処理として観測から落とす。
data <-
  data %>%
  dplyr::filter(is.na(FuelEfficiency) == 0)

# 価格の実質化を行う。ここでは、2016年を基準年とする。
# また、価格の単位を100万円にする。元のデータは1万円


cpi2016 <- 
  dataCPI %>% 
  filter(year == 2016) %>% 
  select(cpi) %>% 
  as.double() #dataCPIデータフレームから2016年のCPI（消費者物価指数）の値を抽出し、
              #浮動小数点数としてcpi2016変数に格納

data <- 
  data %>% 
  mutate( price = price / (cpi / cpi2016) ) %>% 
  mutate( price = price / 100) %>%
  select(-cpi) 

# サイズ(高さ＊幅＊長さ)、燃費の重量に対する比率を定義する。
data <-
  data %>%
  dplyr::mutate(size = (overall_length / 1000) * (overall_width / 1000) * (overall_height / 1000)) %>%
  dplyr::mutate(hppw = HorsePower / weight) %>%
  dplyr::select(-HorsePower, -weight) %>% 
  dplyr::select(-starts_with("overall")) 

# 自動車の車種IDを作成する。
data <-
  data %>%
  dplyr::group_by(Name) %>%
  dplyr::mutate(NameID = dplyr::cur_group_id()) %>%
  dplyr::ungroup() %>%
  dplyr::relocate(NameID, year)


# マーケットシェアとOutside option shareを定義する。
data <-
  data %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(inside_total = sum(Sales)) %>%
  dplyr::ungroup() %>%　#データフレームから行グループ化を解除する
  dplyr::mutate(outside_total = HH - inside_total) %>%
  dplyr::mutate(share = Sales / HH) %>%
  dplyr::mutate(share0 = outside_total / HH) %>%
  dplyr::select(-inside_total, -outside_total)



# まず、マーケット・企業レベルにおける、各製品属性の和と自乗和を計算する。
# ここでacross関数は、最初に文字列ベクトルで指定した変数について、後ろにリスト内で定義した操作を適用している。
data <-
  data %>%
  dplyr::group_by(year, Maker) %>%
  dplyr::mutate(
    dplyr::across( c("hppw", "FuelEfficiency", "size"),
                   list( sum_own = ~ sum(.x, na.rm = TRUE) )   ),
    dplyr::across( c("hppw", "FuelEfficiency", "size"),
                   list( sqr_sum_own = ~ sum(.x^2, na.rm = TRUE) ) ),
    group_n = n()
  ) %>%
  dplyr::ungroup()

# 次に、マーケットレベルでの、各製品属性の和を計算する。
data <- 
  data %>% 
  dplyr::group_by(year) %>%
  dplyr::mutate( 
    dplyr::across( c("hppw", "FuelEfficiency", "size"),
                   list( sum_mkt = ~ sum(.x, na.rm = TRUE) )  ),
    dplyr::across( c("hppw", "FuelEfficiency", "size"),
                   list( sqr_sum_mkt = ~ sum(.x^2, na.rm = TRUE) )    ),
    mkt_n = n()
  ) %>%
  dplyr::ungroup() 

# 以上で定義した変数を利用して、まずBLP操作変数を構築する。
data <- 
  data %>% 
  dplyr::mutate(
    iv_BLP_own_hppw = hppw_sum_own - hppw,
    iv_BLP_own_FuelEfficiency = FuelEfficiency_sum_own - FuelEfficiency,
    iv_BLP_own_size = size_sum_own - size,
    iv_BLP_other_hppw = hppw_sum_mkt - hppw_sum_own,
    iv_BLP_other_FuelEfficiency = FuelEfficiency_sum_mkt - FuelEfficiency_sum_own,
    iv_BLP_other_size = size_sum_mkt - size_sum_own ) 

# 続いて、Differentiation IVを構築する。
data <- 
  data %>% 
  mutate(
    iv_GH_own_hppw = (group_n - 1) * hppw^2 + (hppw_sqr_sum_own - hppw^2) - 2 * hppw * (hppw_sum_own - hppw),
    iv_GH_own_FuelEfficiency = (group_n - 1) * FuelEfficiency^2 + (FuelEfficiency_sqr_sum_own - FuelEfficiency^2) - 2 * FuelEfficiency * (FuelEfficiency_sum_own - FuelEfficiency),
    iv_GH_own_size = (group_n - 1) * size^2 + (size_sqr_sum_own - size^2) - 2 * size * (size_sum_own - size),
    iv_GH_other_hppw = (mkt_n - group_n) * hppw^2 + (hppw_sqr_sum_mkt - hppw_sqr_sum_own) - 2 * hppw * (hppw_sum_mkt - hppw_sum_own),
    iv_GH_other_FuelEfficiency = (mkt_n - group_n) * FuelEfficiency^2 + (FuelEfficiency_sqr_sum_mkt - FuelEfficiency_sqr_sum_own) - 2 * FuelEfficiency * (FuelEfficiency_sum_mkt - FuelEfficiency_sum_own),
    iv_GH_other_size = (mkt_n - group_n) * size^2 + (size_sqr_sum_mkt - size_sqr_sum_own) - 2 * size * (size_sum_mkt - size_sum_own),
  ) %>%
  dplyr::select(
    -starts_with("sum_own"),
    -starts_with("sum_mkt"),
    -starts_with("sqr_sum_own"),
    -starts_with("sqr_sum_mkt"),
    -mkt_n,
    -group_n
  )　


# 車種のID
IDvec <- sort(unique(data$NameID))
J <- length(IDvec) #alternativeの数

# 乱数のシードを固定する。
set.seed(125)

# sample関数を使って、先に用意した車種IDベクトルから、重複無しで30車種を取得する。
# なお、sample関数は内部で乱数を利用しているため、先の乱数シード固定が重要となる。
NIPPYOautoIDvec <- sample(IDvec, size = 30, replace = FALSE)
NIPPYOautoIDvec <- sort(NIPPYOautoIDvec)

# 日評自動車のデータセットを作成する。
data_NIPPYO <-
  data %>% 
  filter( NameID %in% NIPPYOautoIDvec ) %>% 
  select( Sales, price, hppw, FuelEfficiency, size ) %>% 
  mutate( log_sales = log(Sales),
          log_price = log(price) ) 

ols_intro <-
  fixest::feols(log_sales ~ log_price + hppw + FuelEfficiency + size, data = data_NIPPYO)

# 結果のアウトプット
fixest::etable( ols_intro, 
                se = "hetero",
                signifCode = NA, 
                fitstat = c("r2", "n" ) , 
                dict = c(log_price = "log(価格)",
                         hppw = "馬力／重量",
                         FuelEfficiency = "燃費(キロメートル／ 1 リットル)",
                         size = "サイズ",
                         `(Intercept)` = "定数項"),
                digits = 2,
                digits.stats = 2,
                depvar = FALSE)


#日評自動車のデータのみ取り出す。
data_graph <-
  data_NIPPYO %>% dplyr::select(price, Sales)

g1 <-
  ggplot2::ggplot(data_graph, aes(price, Sales)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

plot(g1)


#データセット全体の記述統計
data %>% 
  dplyr::select( Sales, price, FuelEfficiency, size, hppw ) %>% 
  summarytools::descr( transpose = TRUE, stats = c("mean", "sd", "q1", "med", "q3"), order = "preserve")


# まず被説明変数を定義する。
data <-
  data %>%
  dplyr::mutate(logit_share = log(share) - log(share0))

# OLSの結果
ols <-
  fixest::feols(logit_share ~ price + hppw + FuelEfficiency + size, data = data)

# BLP操作変数を用いた結果
iv_BLP <-
  fixest::feols(
    logit_share ~ hppw + FuelEfficiency + size | 0 |
      price ~ iv_BLP_own_hppw + iv_BLP_own_FuelEfficiency + iv_BLP_own_size +
      iv_BLP_other_hppw + iv_BLP_other_FuelEfficiency + iv_BLP_other_size,
    data = data
  )

# Differentiation IVを用いた結果
iv_GH <-
  fixest::feols(
    logit_share ~ hppw + FuelEfficiency + size | 0 |
      price ~ iv_GH_own_hppw + iv_GH_own_FuelEfficiency + iv_GH_own_size +
      iv_GH_other_hppw + iv_GH_other_FuelEfficiency + iv_GH_other_size,
    data = data
  )

# 推定結果をレポートする。
fixest::etable( list(ols, iv_BLP, iv_GH),  
                se = "hetero",
                fitstat = c("r2", "n", "ivf" ) , 
                signifCode = NA, 
                dict = c(price = "自動車価格",
                         hppw = "馬力／重量",
                         FuelEfficiency = "燃費(キロメートル／ 1 リットル)",
                         size = "サイズ",
                         `(Intercept)` = "定数項"),
                digits = 2,
                digits.stats = 2,
                depvar = FALSE)


# BLP操作変数を用いた結果
iv1st_BLP <-
  fixest::feols(
    price ~ hppw + FuelEfficiency + size +
      iv_BLP_own_hppw + iv_BLP_own_FuelEfficiency + iv_BLP_own_size +
      iv_BLP_other_hppw + iv_BLP_other_FuelEfficiency + iv_BLP_other_size,
    data = data
  )

# Differentiation IVを用いた結果
iv1st_GH <-
  fixest::feols(
    price ~ hppw + FuelEfficiency + size + 
      iv_GH_own_hppw + iv_GH_own_FuelEfficiency + iv_GH_own_size +
      iv_GH_other_hppw + iv_GH_other_FuelEfficiency + iv_GH_other_size,
    data = data
  )

# 推定結果をレポートする。
fixest::etable( list( iv1st_BLP, iv1st_GH),  
                se = "hetero",
                fitstat = c("r2", "n") , 
                signifCode = NA, 
                dict = c(price = "自動車価格",
                         hppw = "馬力／重量",
                         FuelEfficiency = "燃費(キロメートル／ 1 リットル)",
                         size = "サイズ",
                         `(Intercept)` = "定数項"),
                digits = 2,
                digits.stats = 2,
                depvar = FALSE)


#自己価格弾力性の計算
data %>% 
  dplyr::mutate( own_elas_ols = ols$coefficients["price"]*price*(1-share), 
                 own_elas_ivblp = iv_BLP$coefficients["fit_price"]*price*(1-share), 
                 own_elas_ivgh  = iv_GH$coefficients["fit_price"]*price*(1-share) ) -> data_elas

data_elas %>% 
  dplyr::select( starts_with("own_elas") ) %>% 
  summarytools::descr( transpose = TRUE, 
                       stats = c("mean", "sd", "med", "min", "max") )

data %>% 
  select( NameID, year, Sales, price, FuelEfficiency, size, hppw, HH, share ) %>% 
  mutate( xi_fit = resid(iv_GH)) -> dt_application



NameID_target <- 87

dt_application %>% 
  filter( NameID == NameID_target & year == 2016) %>% 
  head()

#価格を引数にして，販売台数を返す関数（需要曲線）を考える
f_share <- function(price_cand, year, modelID_target, dt, estparam){
  
  dt %>% 
    filter( year == 2016) %>% 
    mutate( temp_price = ifelse( NameID == modelID_target, price_cand, price) ) %>% 
    mutate( delta = estparam[1] + estparam[2]*temp_price + estparam[3]*hppw + 
              estparam[4]*FuelEfficiency + estparam[5]*size + xi_fit ) %>% 
    mutate( denom = 1 + sum(exp(delta))) %>% 
    mutate( pred_sales = exp(delta)/denom*HH) %>% 
    filter( NameID == modelID_target ) -> dt_result
  
  return(dt_result$pred_sales)  
  
}


estparam <- iv_GH$coefficients
NameID_target <- 87

# 価格の範囲として、0.5(50万円)から5(500万円)を考える。なお、もとの価格は3.198 (319.8万円)
pricevec <- seq(from = 0.3, to = 5,　by = 0.05)
quantvec <- numeric(length(pricevec))
revenuevec <- numeric(length(pricevec))

for ( i in 1:length(pricevec)){
  
  quantvec[i] <- f_share(price_cand = pricevec[i], year = 2016, modelID_target = NameID_target, dt = dt_application, estparam = estparam )
  revenuevec[i] <- pricevec[i]*quantvec[i]
  
}


# フォントの設定。
windowsFonts(
  `Yu Mincho` = windowsFont("Yu Mincho"),
  `Yu Gothic` = windowsFont("Yu Gothic")
)
theme_set(theme(text = element_text(family = "Yu Gothic")))

# 既にテーマに設定したフォントファミリをデフォルトに設定
update_geom_defaults("text", list(family = theme_get()$text$family))
update_geom_defaults("label", list(family = theme_get()$text$family))

# fig_demand <- qplot(quantvec, pricevec*100, xlab = "Sales", ylab = "Price(Unit:10,000JPY)", geom = "line" )
fig_demand <- qplot(quantvec, pricevec*100, xlab = "販売台数", ylab = "価格(単位：万円)", geom = "line" )
plot(fig_demand)

ggsave(file = "demand.png", plot = fig_demand, width = 9, height = 6)
ggsave(file = "demand.pdf", plot = fig_demand, width = 9, height = 6, device = cairo_pdf )

# pricevecの価格の単位を1万円にする。
fig_rev <- qplot(pricevec*100, revenuevec*100/10000, ylab = "収入(単位：億円)", xlab = "価格(単位：万円)", geom = c("line"), ylim = c(500,1500) )
plot(fig_rev)

ggsave(file = "revenue.png", plot = fig_rev, width = 9, height = 6)
ggsave(file = "revenue.pdf", plot = fig_rev, width = 9, height = 6, device = cairo_pdf)

# 最適化で利用する関数を定義する。
f_revenue <- function(price_cand, year , modelID_target, dt , estparam){
  
  quantity <- f_share(price_cand, year , modelID_target, dt , estparam  )
  revenue <- price_cand*quantity
  return(revenue)
  
}

# 最適化アルゴリズムを使って、収入を最大にする価格を求める。
result <- optimise( f_revenue, interval = c(0.3, 3), maximum = TRUE, 
                    year = 2016, 
                    modelID_target = NameID_target, dt = dt_application, estparam = estparam )

print(result)


