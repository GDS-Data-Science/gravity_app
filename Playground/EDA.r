################################################################################
################################################################################
##                                                                            ##
##                 Playground for Gravity Model - EDA                         ##
##                                                                            ##
################################################################################
################################################################################

################################################################################
#                                    EDA                                       #
################################################################################

# structure of the data 
str( dat )
# summary of the variables
summary( dat )
## structure of the data
# length of time series (balanced panel)
table( table( dat$Id ))
# number of unique country of origin 
n_o <- length( unique( dat$iso_o ))
# number of unique country of asylum
n_d <- length( unique( dat$iso_d ))
# number of unique country pairs 
n_p <- length( unique( dat$Id ))
# equality of countries in 'o' and 'd' category
mean( unique( dat$iso_o ) %in% unique( dat$iso_d ))

## visualization 
# distribution of dependent variable 
hist( dat$newarrival, main = "Distribution of 'newarrival' " )
# share of zeros
mean( dat$newarrival == 0 )
# plot time series 
dat %>% group_by( iso_o, year ) %>% 
        summarise( total = sum( newarrival )) %>% 
        filter( total > 100 ) %>% 
        ggplot( aes( x = year, y = total, color = iso_o )) +
        geom_line( )
# impact of PR and CL on newarrival
ggplot( data = dat, mapping = aes( x = log( newarrival ), 
                                   y = ..density..,
                                   colour = as.factor( PR_o ))) +
   geom_freqpoly(  ) +
   ggtitle( "Impact of political rights on the distribution of newarrival" )
   
ggplot( data = dat, mapping = aes( x = log( newarrival ),
                                   y = ..density..,
                                   colour = as.factor( CL_o ))) +
   geom_freqpoly(  ) +
   ggtitle( "Impact of civil liberty on the distribution of newarrival" )
# impact of violence type on newarrival
ggplot( data = dat, mapping = aes( x = log( newarrival ), 
                                   y = ..density..,
                                   colour = as.factor( typeOfViolence_o ))) +
   geom_freqpoly(  ) +
   ggtitle( "Impact of violence type on the distribution of newarrival" )
# impact of island status 
ggplot( data = dat, mapping = aes( x = as.factor( island_o ), y = log( newarrival ))) +
   geom_boxplot()
ggplot( data = dat, mapping = aes( x = as.factor( island_d ), y = log( newarrival ))) +
   geom_boxplot()
# impact of landlocked status 
ggplot( data = dat, mapping = aes( x = as.factor( landlocked_o ), y = log( newarrival ))) +
   geom_boxplot()
# impact of prima facie 
ggplot( data = dat, mapping = aes( x = as.factor( index0asylum ), y = log( newarrival ))) +
   geom_boxplot()
# correlation between GDP_PP and newarrival
ggplot( data = dat ) +
   geom_hex( mapping = aes( x = GDP_PP_o, y = log( newarrival )))
ggplot( data = dat ) +
   geom_hex( mapping = aes( x = GDP_PP_d, y = log( newarrival )))
# correlation between best_est_o and newarrival
ggplot( data = dat ) +
   geom_hex( mapping = aes( x = best_est_o, y = newarrival ))
# correlation between distance and newarrival
ggplot( data = dat, mapping = aes( x = dist, y = log( newarrival ))) + 
   geom_boxplot( mapping = aes( group = cut_number( dist, 10 )))






             
             