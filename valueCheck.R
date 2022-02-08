
country <- unique( impu_clean[[1]]$iso_o[ idx ])

check <- rep( NA, length( country ))

for( i in 1:length(country)){
   orig <- impu_clean[[1]] %>% filter( iso_o == country[i] ) %>% select( ends_with( "_o" )) %>% distinct()
   host <- impu_clean[[1]] %>% filter( iso_d == country[i] ) %>% select( ends_with( "_d" )) %>% distinct()
   check[i] <- mean( orig == host )
}
