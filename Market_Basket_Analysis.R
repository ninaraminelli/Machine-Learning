library(dplyr)
library(data.table)
install.packages("arules", dependencies=TRUE)
library(arules)

# I have installed the exploratory package to use do_apriori function. This function will help us to find the association rules using the date we have
devtools::install_github("exploratory-io/exploratory_func")
library(exploratory)

orders <- fread('order_products__prior.csv')
#head(orders)

orders$reordered <- NULL
orders$add_to_cart_order <- as.character(orders$add_to_cart_order)

products <- fread('products.csv')

#head(products)

# Let's joing the datasets to get products' names
orders <- merge(orders, products, by="product_id")

orders$product_id <- NULL
orders$aisle_id <- NULL
orders$department_id <-NULL

orders <- arrange(orders, order_id)

market_basket <- orders %>%
    do_apriori(product_name, order_id, min_support = 0.000003) 

# support shows how many transactions support this conditions
# confidence is the level of confidence of this condition, the how many percent of times this condition was true
# lift is the number of times that this condition should happen compared to reality

market_basket <- group_by(market_basket, rhs) %>%
                  top_n(3, lift) %>%
                    arrange(lhs)

fwrite(market_basket, file="market_basket.csv", sep=",")





