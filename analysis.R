##============================================
## info
##============================================

## result
##============================================

## setting
##============================================
library(ggplot2)
library(dplyr)

first_data <- read.csv(file = "/Users/akifumi.tominaga/Desktop/課題2_macbookの価格モデリング/data/mac_price_150717.csv", fileEncoding = "utf-8") # import

## first task
##============================================
# df
df <- first_data %>%
  select(product, price) %>%
  filter(product == "macbook" | product =="macbook_air" | product == "macbook_pro")

# add product_detail and product_category to df
price_df<-transform(df,product_detail=c(
  "01_macbook_low",
  "02_macbook_high",
  "03_macbook_air_11inch_low",
  "04_macbook_air_11inch_high",
  "05_macbook_air_13inch_low",
  "06_macbook_air_13inch_high",
  "07_macbook_pro_13inch",
  "08_macbook_pro_retina_13inch_low",
  "09_macbook_pro_retina_13inch_middle",
  "10_macbook_pro_retina_13inch_high",
  "11_macbook_pro_retina_15inch_low",
  "12_macbook_pro_retina_15inch_high"
))

price_df<-transform(price_df,product_category=c(
  "macbook",
  "macbook",
  "macbook_air_11inch",
  "macbook_air_11inch",
  "macbook_air_13inch",
  "macbook_air_13inch",
  "macbook_pro_13inch",
  "macbook_pro_retina_13inch",
  "macbook_pro_retina_13inch",
  "macbook_pro_retina_13inch",
  "macbook_pro_retina_15inch",
  "macbook_pro_retina_15inch"
))

# ggplot create data frame
g <- ggplot(
  price_df,                  # ggplot dataframe
  aes (                      # ggplot set option
    x = product_detail,
    y = price,
    fill = product_category  # color by product_category
    )
)

# graph_design
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1)) # setting angle of x-axis text
g <- g + geom_bar(
  width = 0.8,               # bar width
  stat = "identity"
)

g <- g + xlab("product_detail")                # name of x-axis
g <- g + ylab("Price(yen)")                    # name of y-axis
g <- g + ggtitle("Compare prices of MacBook")  # name of graph

plot(g)

## task2
##============================================
install.packages("MuMIn")
library(MuMIn)

y<- first_data["price"]
# set explanatory variable
x1<-first_data["display_type"]
x2<-first_data["display_size"]
x3<-first_data["size"]
x4<-first_data["weight"]
x5<-first_data["memory_size"]
x6<-first_data["gpu"]
x7<-first_data["gpu-cnt"]
x8<-first_data["cpu"]
x9<-first_data["cpu_clock"]
x10<-first_data["strage_type"]
x11<-first_data["strage_volume"]

model1<-lm(y~x1)      # モデル1　x1だけを使ってyを計算
model2<-lm(y~x1+x2)   # モデル2　x1とx2の両方を使ってモデリング
model3<-lm(y~x1*x2)   # モデル3　x1とx2の交互作用も入れてモデリング

summary(model3)

anova(model2,model3) # Pr(>F) 誤差の大きさ
anova(model1,model2)

AIC(model1)
AIC(model2)
AIC(model3)
# AICが一番小さいものが良いモデル

model.best1<-step(model3)
model.best1

library(MuMIn)
options(na.action = "na.fail")   #  prevent fitting models to different datasets
kekka.AIC<-dredge(model3,rank="AICc")
kekka.AIC

# AIC 最小モデルを引っ張ってくるには
all.model <- get.models(kekka.AIC)
best.model<-all.model[1]
best.model

# Example from Burnham and Anderson (2002), page 100:
# data(Cement) 
# options(na.action = "na.fail")   #  prevent fitting models to different datasets
# fm1 <- lm(y ~ ., data = Cement)
# dd <- dredge(fm1)



avg.model<-model.avg(get.models(kekka.AIC, subset = delta < 4))
avg.model
summary(avg.model)


library(MuMIn)
set.seed(0)
N<-100
Intercept<-5
B1<-10
B2<-5
x1<-sort(rnorm(N,sd=2))
x2<-rnorm(N,sd=2)
e<-rnorm(n=N,sd=3)
y<-Intercept+B1*x1+B2*x2+e
model3<-lm(y~x1*x2)
kekka.AIC<-dredge(model3,rank="AIC")
all.model <- get.models(kekka.AIC)
best.model<-all.model[1]
best.model


# Mixed models:

# if(require(nlme)) {
#   fm2 <- lme(distance ~ age + Sex, data = Orthodont,
#              random = ~ 1 | Subject, method = "ML")
#   ms2 <- dredge(fm2)

# Get top-most models, but fitted by REML:
#   (confset.d4 <- get.models(ms2, subset = delta < 4, method = "REML"))
# }