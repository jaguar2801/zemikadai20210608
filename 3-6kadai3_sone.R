library(rstan)
library(brms)

rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

sales_weather <- read.csv("3-6-1-beer-sales-3.csv")
head(sales_weather, 3)

# データの要約
summary(sales_weather)

# 図示
ggplot(data = sales_weather, mapping = aes(x = weather, y = sales)) +
  geom_violin() +
  geom_point(aes(color = weather)) +
  labs(title = "ビールの売り上げと天気の関係")

sales_weather_dummies<-read.csv("3-6-1-beer-sales-3_dummies.csv")
head(sales_weather_dummies)
anova_brms <- brm(
  formula = sales ~ cloudy + rainy + sunny,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = sales_weather_dummies,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

anova_brms
#=======================================\
#opulation-Level Effects: 
#           Estimate Est.Error  l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept -14668.64  12661.98 -38546.59  4578.18 1.83        6       22
#cloudy     14731.90  12662.39  -4514.24 38611.71 1.83        6       22
#rainy      14731.62  12662.34  -4515.40 38609.76 1.83        6       22
#sunny      14752.01  12662.38  -4493.90 38634.10 1.83        6       22


# 推定された天気別の平均売り上げのグラフ
eff <- marginal_effects(anova_brms)
plot(eff, points = FALSE)
