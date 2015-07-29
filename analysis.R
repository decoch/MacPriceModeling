##============================================
## info
##============================================

## result
##============================================

## setting
##============================================
library(ggplot2)
library(dplyr)

first_data <- read.csv(file = "/Users/akifumi.tominaga/DataScienceProject/MacPriceModeling/data/mac_price_150717.csv", fileEncoding = "utf-8") # import

##============================================
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

##============================================
## task2
##============================================

## info
##============================================
# 参考サイト(http://logics-of-blue.com/%E3%83%A2%E3%83%87%E3%83%AB%E9%81%B8%E6%8A%9E_%E5%AE%9F%E8%B7%B5%E7%B7%A8/)
# install.packages("MuMIn") # require only first
library(MuMIn)

## setting data
##============================================
tmp_df <- first_data %>%
  select(product, price, display_type, display_size, memory_size, cpu_core_cnt, cpu_clock, strage_type, strage_volume) %>%
  filter(product == "macbook" | product =="macbook_air" | product == "macbook_pro")

tmp_df$display_type <- ifelse(tmp_df$display_type=="retina", 1, 0)
tmp_df$strage_type <- ifelse(tmp_df$strage_type=="flash", 1, 0)
tmp_df <- transform(tmp_df,gpu_bentchmark=c(
  372,
  372,
  778,
  778,
  778,
  778,
  454,
  947,
  947,
  947,
  1191,
  1191
))

task2_df <- tmp_df

## modeling
##============================================
model <- lm(price~display_size + memory_size + cpu_clock + strage_type + strage_volume, data=task2_df)
summary(model)
AIC(model)
kekka.AIC<-dredge(model,rank="AIC")
kekka.AIC

ans_model <- lm(price ~ display_size + memory_size + cpu_clock + 
                 strage_type + strage_volume, data = task2_df)
ans_model
summary(ans_model)
# best.model <- confset.95p[1]
# best.model
