setwd("C:/Users/zc2j/Downloads/instacart_2017_05_01")
library(dplyr)
library(data.table)

# Instalei o pacote exploratory para usar a função do_apriori para achar as regras de associação com o formato de dados que temos
#devtools::install_github("exploratory-io/exploratory_func")
library(exploratory)

orders <- fread('order_products__prior.csv')
#head(orders)

orders$reordered <- NULL
orders$add_to_cart_order <- as.character(orders$add_to_cart_order)

products <- fread('products.csv')

#head(products)

#vamos juntar os datasets para pegar o nome dos produtos
orders <- merge(orders, products, by="product_id")

#head(orders)

orders$product_id <- NULL
orders$aisle_id <- NULL
orders$department_id <-NULL

#head(orders)

orders <- arrange(orders, order_id)

market_basket <- orders %>%
    do_apriori(product_name, order_id, min_support = 0.000003) 

# support é a quantidade de transações que suportam essa condição
# confidence é o grau de confiaça dessa condição, ou seja, o percentual de vezes que aquela condição foi verdadeira
# lift é o número de vezes que essa condição deveria acontecer comparada com a realidade

market_basket <- group_by(market_basket, rhs) %>%
                  top_n(3, lift) %>%
                    arrange(lhs)

fwrite(market_basket, file="market_basket.csv", sep=",")





