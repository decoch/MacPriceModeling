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