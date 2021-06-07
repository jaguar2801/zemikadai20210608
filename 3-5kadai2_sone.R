#ライブラリのインポート
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 分析対象のデータ
data <- read.csv("3-2-1-beer-sales-2.csv")

simple_lm_brms <- brm(
  formula = sales ~ temperature,         # modelの構造を指定
  family = gaussian(link = "identity"),  # 正規分布を使う
  data = data,              # データ
  seed = 1                               # 乱数の種
)


simple_lm_brms

# 参考：MCMCサンプルの取得

new_data_20<- data.frame(temperature = 20)

fitted(simple_lm_brms,new_data_20)   #[66.89337   73.72776]
　　　　　　　　　　　　　　　　　 
new_data_30<-data.frame(temperature=30)  
fitted(simple_lm_brms,new_data_30)      ##[88.45818   101.5605]

eff<-marginal_effects(simple_lm_brms)
plot(eff,points=TRUE)
