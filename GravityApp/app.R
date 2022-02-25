################################################################################
################################################################################
##                                                                            ##
##                              SIMUGRAV App                                  ##
##                                                                            ##
################################################################################
################################################################################

################################ Preparation ###################################


#============================== load packages ==================================

library( chorddiag )
library( dplyr )
library( FENmlm )
library( haven )
library( networkD3 )
library( plotly )
library( purrr )
library( readr )
library( readxl )
library( shiny )
library( shinythemes )
library( shinyWidgets )
library( tidyr )
library( utils )

#============================== read in data ===================================

dat <- read.csv( "input_data/CountriesRegions.csv", fileEncoding = "ISO-8859-1" )
load( "input_data/impuData22.Rdata" )
load( "input_data/impuData17.Rdata" )
load( "input_data/estimations.Rdata" )
load( "input_data/stock_calc.Rdata" )

#============================ generate data set ================================

# removing non-clustered countries
impu22 <- lapply( impu22, 
                  function( x ) filter( x, !( iso_o %in% c( "ABW", "UVK", "MHL", "PLW", "PRI" )) &
                                           !( iso_d %in% c( "ATG", "BTN", "BRN", "CPV", "GNQ", "FSM", "MMR",
                                                            "NRU", "ERI", "KIR", "UVK", "MAC", "MDV", "MHL", 
                                                            "PRI", "WSM", "SMR", "STP", "SYC", "SLE", "SGP",
                                                            "LCA", "TWN", "TLS", "TON", "TKM", "TUV", "UZB" ))))


# generate subset of dat for country of origin and host country 
dat_o <- subset( dat, iso3 %in% impu22[[1]]$iso_o )
dat_d <- subset( dat, iso3 %in% impu22[[1]]$iso_d )

# # generate factor variables 
# for( i in 1:5 ){
#    impu22[[i]] <- within( impu22[[i]], {
#       CL_d_factor <- cut( CL_d, c( 1, 3, 6, 8 ), right = FALSE, 
#                           labels = c( "free", "partfree", "nofree" ))
#       CL_o_factor <- cut( CL_o, c( 1, 3, 6, 8 ), right = FALSE, 
#                           labels = c( "free", "partfree", "nofree" ))
#       PR_d_factor <- cut( PR_d, c( 1, 3, 6, 8 ), right = FALSE, 
#                           labels = c( "free", "partfree", "nofree" ))
#       PR_o_factor <- cut( PR_o, c( 1, 3, 6, 8 ), right = FALSE, 
#                           labels = c( "free", "partfree", "nofree" ))
#    })
# }

# country library drop down menu
d_americas <- dat_d$gis_name[ dat_d$main_office == "Americas " ]
o_americas <- dat_o$gis_name[ dat_o$main_office == "Americas " ]
d_asia <- dat_d$gis_name[ dat_d$main_office == "Asia and the Pacific" ]
o_asia <- dat_o$gis_name[ dat_o$main_office == "Asia and the Pacific" ]
d_e_africa <- dat_d$gis_name[ dat_d$main_office == "East and Horn of Africa, and Great Lakes" ]
o_e_africa <- dat_o$gis_name[ dat_o$main_office == "East and Horn of Africa, and Great Lakes" ]
d_w_africa <- dat_d$gis_name[ dat_d$main_office == "West and Central Africa" ]
o_w_africa <- dat_o$gis_name[ dat_o$main_office == "West and Central Africa" ]
d_s_africa <- dat_d$gis_name[ dat_d$main_office == "Southern Africa" ]
o_s_africa <- dat_o$gis_name[ dat_o$main_office == "Southern Africa" ]
d_europe <- dat_d$gis_name[ dat_d$main_office == "Europe" ]
o_europe <- dat_o$gis_name[ dat_o$main_office == "Europe" ]
d_mena <- dat_d$gis_name[ dat_d$main_office == "Middle East and North Africa" ]
o_mena <- dat_o$gis_name[ dat_o$main_office == "Middle East and North Africa" ]


################################################################################
#                                 functions                                    #
################################################################################

#========================== colors Sankey diagram ==============================

# color scheme for Sankey diagram
ColourScal ='d3.scaleOrdinal() .range([ "#000000", "#faeb00", "#338ec9", "#e73451", "#00ab92", "#2871a0", "#0072bc", "#66CCBD", "#fefbcc", "#FBF14C", "#C1DDEE", "#007766"])'

#============================ function bar plot ================================

# function for bar chart
bar_plot <- function( data ){
   g <- ggplot( data ) +
         geom_bar( aes( x = year, y = var ), 
                   stat = "identity", fill = "#057fab" ) +
         geom_text( aes( x = year, y = var, label = var ),
                    position = position_stack( vjust = 0.5 ),
                    color = "white" ) +
         labs( y = "", x = "" ) +
         theme_classic() +
         theme( axis.line.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                axis.line.y = element_blank(), 
                axis.ticks.y = element_blank(), 
                axis.text.y = element_blank()) +
         theme( plot.background = element_rect( fill = "#F5F5F5", color = "#F5F5F5" ),
                panel.background = element_rect( fill = "#F5F5F5", color = "#F5F5F5" ))
   
   ggplotly( g, tooltip = c( "text" ))
}

#=========================== function line plot ================================

# function line plot 
line_plot <- function( data ){
   g <- ggplot( data, aes( x = year, y = var )) +
        geom_line( color = "#057fab", size = 1 ) +
        #geom_ribbon( aes( ymin = low, ymax = high ), alpha = 0.1 ) + 
        geom_vline( xintercept = 2021, color = "red", size = 1.3 ) +
        labs( y = "", x = "" ) +
        theme_minimal() +
        theme( axis.line.x = element_blank(), 
               axis.ticks.x = element_blank(), 
               axis.line.y = element_blank(), 
               axis.ticks.y = element_blank()) +
        theme( plot.background = element_rect( fill = "#F5F5F5", color = "#F5F5F5" ),
               panel.background = element_rect( fill = "#F5F5F5", color = "#F5F5F5" ))
   
   ggplotly( g, dynamicTicks = TRUE) %>% layout(hovermode = "x")
}


################################################################################
################################################################################

################################################################################
##                                Shiny UI                                    ##
################################################################################

ui <- navbarPage(
         theme = shinytheme( "yeti" ),
         setBackgroundColor( "WhiteSmoke" ),
         collapsible = TRUE,
         title = "SIMUGRAV",
         

############################### tab 'Simulation' ###############################

         tabPanel( "Simulation Flow Data",
                   
                   fluidRow(
                      column( 3,
                              h3( "Country of Origin" ),
                              br(), 
                              selectInput( "orig", "Choose country", 
                                           list( "Americas" = o_americas, 
                                                 "Asia and the Pacific" = o_asia, 
                                                 "East and Horn of Africa, and Great Lakes" = o_e_africa, 
                                                 "Europe" = o_europe, 
                                                 "Middle East and North Africa" = o_mena, 
                                                 "Southern Africa" = o_s_africa, 
                                                 "West and Central Africa" = o_w_africa )), 
                              br(),
                              sliderInput( "fatalO", "Total number of fatalities", 
                                           min = 0, max = 500, value = 20, round = TRUE ),
                              br(),
                              sliderTextInput( "gdpO", "Percentage change in GDP per capita", 
                                               choices = c( -25, -15, -10, -7, -5, -4, -3, -2, -1.5, -1, -0.5, -0.25, 0, 
                                                            0.25, 0.5, 1, 1.5, 2, 3, 4, 5, 7, 10, 15, 25 ), 
                                               selected = 0, grid = FALSE ),
                              selectInput( "civlibO", "Civil liberty on 1-7 scale", 
                                           choices = c( "1 Very free" = 1, 
                                                        "2 Free" = 2, 
                                                        "3 Less free" = 3, 
                                                        "4 Moderately free" = 4,
                                                        "5 Partly free" = 5, 
                                                        "6 Unfree" = 6, 
                                                        "7 Very unfree" = 7 )), 
                                     
                               selectInput( "polRO", "Political rights on 1-7 scale", 
                                           choices = c( "1 Very free" = 1, 
                                                        "2 Free" = 2, 
                                                        "3 Less free" = 3, 
                                                        "4 Moderately free" = 4,
                                                        "5 Partly free" = 5, 
                                                        "6 Unfree" = 6, 
                                                        "7 Very unfree" = 7 )), 
                              pickerInput( "year", "Select year", 
                                           choices = c( 2022, 2023, 2024 ), 
                                           selected = 2022,
                                           options = list( `actions-box` = TRUE, 
                                                           `none-selected-text` = "Please make a selection!" ),
                                           multiple = TRUE )
                              ),
                      column( 6,
                              tabsetPanel( type = "pills", 
                                 tabPanel( "Summary", 
                                           plotlyOutput( "line_plot", height = "550px" )), 
                                 tabPanel( "2022", 
                                           sankeyNetworkOutput( "sankey22", height = "600px" )), 
                                 tabPanel( "2023", 
                                           sankeyNetworkOutput( "sankey23", height = "600px" )),
                                 tabPanel( "2024", 
                                           sankeyNetworkOutput( "sankey24", height = "600px" ))
                              )
                      ),
                      column( 3, 
                              h3( "Country of Asylum" ),
                              br(), 
                              selectInput( "host", "Choose country", 
                                           list( "Americas" = d_americas, 
                                                 "Asia and the Pacific" = d_asia, 
                                                 "East and Horn of Africa, and Great Lakes" = d_e_africa, 
                                                 "Europe" = d_europe, 
                                                 "Middle East and North Africa" = d_mena, 
                                                 "Southern Africa" = d_s_africa, 
                                                 "West and Central Africa" = d_w_africa )),
                              br(),
                              sliderInput( "fatalH", "Total number of fatalities", 
                                           min = 0, max = 100, value = 5, round = TRUE ),
                              br(),
                              sliderTextInput( "gdpH", "Percentage change in GDP per capita", 
                                               choices = c( -25, -15, -10, -7, -5, -4, -3, -2, -1.5, -1, -0.5, -0.25, 0, 
                                                            0.25, 0.5, 1, 1.5, 2, 3, 4, 5, 7, 10, 15, 25 ), 
                                               selected = 0, grid = FALSE ), 
                              selectInput( "civlibH", "Civil liberty on 1-7 scale", 
                                             choices = c( "1 Very free" = 1, 
                                                          "2 Free" = 2, 
                                                          "3 Less free" = 3, 
                                                          "4 Moderately free" = 4,
                                                          "5 Partly free" = 5, 
                                                          "6 Unfree" = 6, 
                                                          "7 Very unfree" = 7 )), 
                              selectInput( "polRH", "Political rights on 1-7 scale", 
                                             choices = c( "1 Very free" = 1, 
                                                          "2 Free" = 2, 
                                                          "3 Less free" = 3, 
                                                          "4 Moderately free" = 4,
                                                          "5 Partly free" = 5, 
                                                          "6 Unfree" = 6, 
                                                          "7 Very unfree" = 7 )), 
                              checkboxInput( "temAsyl", "Temporary Protection" ), 
                              actionButton( "go", "Run" )
                         ) # end column
                   ), # end fluidrow   
                   
               div( absolutePanel( bottom = 20, right = 60, width = 80,
                                   fixed=TRUE, draggable = FALSE, height = "auto",
                                   tags$img( src = 'unhcr.png', height = '120', width = '120')))),

############################### tab 'Data Table' ############################### 
         
         tabPanel( "Data Download",
                   selectInput( "top3", "Choose ranking variable", 
                                choices = c( "Refugee Stocks" = "refugee_stocks",
                                             "Asylum Seeker Stocks" = "asylum_stocks" ),
                                selected = "refugee_stocks" ),
                   verbatimTextOutput( "rawtable" ),
                   downloadButton( "downloadCsv", "Download as CSV" ),
                   tags$br()
         ), # end tabPanel 'Data Download'


################################ tab 'Variables' ############################### 

         tabPanel( "Variables",
                   div( class = "text",
                        tags$head( includeCSS( "www/bootstrap.css" )),
                        h2( "Explanation of Variables" ),
                        p( "The following variables are used in the simulation: "),
                        includeHTML( "www/table.html" )
                        ),
                   
                   div( absolutePanel( bottom = 20, right = 60, width = 80,
                                       fixed=TRUE, draggable = FALSE, height = "auto",
                                       tags$img( src = 'unhcr.png', height = '120', width = '120')))),

############################### tab 'Background' ############################### 

         tabPanel( "Background", 
                   div( class = "text",
                        tags$head( includeCSS( "www/bootstrap.css" )),
                        h2( "Information on the Gravity Model for Refugee and Asylum Seeker Flows" ),
                        br(),
                        h4( "Model development"),
                        p( "This simulation is based on a gravity model of refugee flows between 
                            countries. These refugee flows between country pairs are modelled based
                            on historical data. An overview of the variables that are used in the 
                            estimation can be found under tab 'Variables'."),
                        p( "The simulation uses the empirical model as a basis to pedict future refugee 
                            flows between country pairs. These predicted refugee flows 
                            are then used to re-calculate expected stock figures, 
                            which can be downloaded under tab 'Data Download'."),
                        br(),
                        h4( "Running the simulation" ), 
                        p( "A user can run the simulation in the folowing way: "), 
                        tags$ol(
                           tags$li( "Choose the country of origin and the country of asylum in the drop down menu."),
                           tags$li( "Choose the years for your modifications. 
                                     You can select seleveral years by ticking the boxes." ),
                           tags$li( "Choosing the countries, will automatically install the expected default 
                                     values for all variables of a given country of origin and country od asylum.
                                     Please note, that because the default values are predicted values, 
                                     these values will come with some uncertainty and will not necessarily 
                                     reflect the newest real world developments. 
                                     As a user, you can now apply your expert knowledge to 
                                     modify these values according to your expectations or interests.
                                     For example, you can increase the number of conflict fatalities 
                                     in the country of origin for the next year and reduce the 
                                     per capita GDP of the country of asylum by 2% to see what an impact 
                                     these changes would have on the expected refugee flows between 
                                     your chosen country pair and country of asylum." ), 
                           tags$li( "Once all parameters are set, click on the button 'Run'.
                                     Clicking this button will update the prediction of future refugee 
                                     flows and stock figures as they could be expected as a consequence of the 
                                     chosen modifications.
                                     Please bear in mind, that these are predictive values which 
                                     have a certain amount of uncertainty." ), 
                           tags$li( "The predicted stock figures for your chosen country of asylum
                                     can be seen and downloaded as a .CSV file under tab 'Data Download'.")
                        )),
                   div( class = "text",
                        tags$h4( "Development and Maintenance" ),
                       "Data Science Team, Statistics and Demographics Section (SDS), GDS", tags$br(),
                        tags$br(), tags$h4( "Contact" ),
                       "pellandr@unhcr.org", tags$br(),
                       "huang@unhcr.org", tags$br(),
                       "hennings@unhcr.org", tags$br(),
                       "delpanta@unhcr.org", tags$br()),
                       
                   
                   div( absolutePanel( bottom = 20, right = 60, width = 80,
                                       fixed=TRUE, draggable = FALSE, height = "auto",
                                       tags$img( src = 'unhcr.png', height = '120', width = '120'))))
  )

  
################################################################################
################################################################################

################################################################################
##                                Shiny server                                ##
################################################################################

  
server <- function( input, output, session ) {
   
################################ Simulation tab ################################  

   ## interactive filter function      
   new_data <- function( nDat ){
      # modify dead and dead_log
      nDat$dead_d[ nDat$year %in% input$year & nDat$iso_d == iso_host() ] <-
                                              ifelse( input$fatalH > 100, 0, 1 )
      nDat$dead_o[ nDat$year %in% input$year & nDat$iso_o == iso_orig() ] <-
                                              ifelse( input$fatalO > 100, 0, 1 )
      nDat$dead_log_d[ nDat$year %in% input$year & nDat$iso_d == iso_host() ] <-
                              ifelse( input$fatalH == 0, 0, log( input$fatalH ))
      nDat$dead_log_o[ nDat$year %in% input$year & nDat$iso_o == iso_orig() ] <-
                              ifelse( input$fatalO == 0, 0, log( input$fatalO ))
      # modify Nyear_conf and Nyear_log
      nDat$Nyear_conf_d[ nDat$year %in% input$year & nDat$iso_d == iso_host()] <-
                                               ifelse( input$fatalH > 50, 1, 0 )
      nDat$Nyear_conf_o[ nDat$year %in% input$year & nDat$iso_o == iso_orig() ] <-
                                               ifelse( input$fatalO > 50, 1, 0 )
      # nDat$Nyear_log_d[ nDat$year == input$year & nDat$iso_d == iso_host()] <- 
      #        if( input$fatalH < 50 ){ 
      #           0 } else{ 
      #               nDat$Nyear_log_d[ nDat$year == input$year & nDat$iso_d == iso_host()]}
      # nDat$Nyear_log_o[ nDat$year == input$year & nDat$iso_o == iso_orig()] <- 
      #    if( input$fatalH < 50 ){ 
      #       0 } else{ 
      #           nDat$Nyear_log_o[ nDat$year == input$year & nDat$iso_o == iso_orig()]}
      # modify GDP_PP_d and GDP_PPP_d
      if( input$gdpH != 0 ){
         nDat$GDP_PP_d[ nDat$year %in% input$year & nDat$iso_d == iso_host()] <-
             nDat$GDP_PP_d[ nDat$year %in% input$year & nDat$iso_d == iso_host()] * ( 1 + input$gdpH/100 )
         nDat$GDP_PPP_d[ nDat$year %in% input$year & nDat$iso_d == iso_host()] <-
             nDat$GDP_PPP_d[ nDat$year %in% input$year & nDat$iso_d == iso_host()] * ( 1 + input$gdpH/100 )
      }
      # modify GDP_PP_o and GDP_PPP_o
      if( input$gdpO != 0 ){
         nDat$GDP_PP_o[ nDat$year %in% input$year & nDat$iso_o == iso_orig() ] <-
            nDat$GDP_PP_o[ nDat$year %in% input$year & nDat$iso_o == iso_orig() ] * ( 1 + input$gdpO/100 )
         nDat$GDP_PPP_o[ nDat$year %in% input$year & nDat$iso_o == iso_orig() ] <-
            nDat$GDP_PPP_o[ nDat$year %in% input$year & nDat$iso_o == iso_orig() ] * ( 1 + input$gdpO/100 )
      }
      # modify CL_d
      nDat$CL_d[ nDat$year %in% input$year & nDat$iso_d == iso_host()] <- as.numeric( input$civlibH )

      # modify CL_o
      nDat$CL_o[ nDat$year %in% input$year & nDat$iso_o == iso_orig()] <- as.numeric( input$civlibO )

      # modify PR_d
      nDat$PR_d[ nDat$year %in% input$year & nDat$iso_d == iso_host()] <- as.numeric( input$polRH )

      # modify PR_o
      nDat$PR_o[ nDat$year %in% input$year & nDat$iso_o == iso_orig()] <- as.numeric( input$polRO )

      # modify index0asylum
      if( input$temAsyl == TRUE ){
         nDat$index0asylum[ nDat$year %in% input$year &
                            nDat$iso_o == iso_orig() &
                            nDat$iso_d == iso_host()] <- 1
      } else{
         nDat$index0asylum[ nDat$year %in% input$year &
                            nDat$iso_o == iso_orig() &
                            nDat$iso_d == iso_host()] <- 0
      }
      nDat
   }

   isoOrig <- reactive({ dat$iso3[ dat$gis_name == input$orig ]})
   isoHost <- reactive({ dat$iso3[ dat$gis_name == input$host ]}) 
   
   ### update buttons and sliders for country of origin   
   observeEvent( c( input$orig, input$year ), {
      default_values <- impu22[[1]] %>% 
                        filter( iso_o == isoOrig() & year == input$year[1] ) %>%
                        select( best_est_o, CL_o, GDP_PP_o, PR_o ) %>% 
                        distinct()
      updateSliderInput( inputId = "fatalO", 
                         value = default_values$best_est_o, 
                         max = ifelse(( default_values$best_est_o + default_values$best_est_o * 1.25 ) < 5000, 5000, 
                                        round(( default_values$best_est_o + default_values$best_est_o * 1.25 )/1000 ) * 1000 ))
      updateSliderTextInput( session = session, inputId = "gdpO", 
                             label = paste0( "GDP per capita $", 
                                     round( default_values$GDP_PP_o, 0 ), 
                                     ". Percentage change:"))
      updateSelectInput( inputId = "civlibO", selected = default_values$CL_o )
      updateSelectInput( inputId = "polRO", selected = default_values$PR_o )
   })

   ### update buttons and sliders for host country
   observeEvent( c( input$host, input$orig, input$year ), {
      default_values <- impu22[[1]] %>% 
                        filter( iso_d == isoHost() & year == input$year[1] ) %>%
                        select( best_est_d, CL_d, GDP_PP_d, PR_d ) %>% 
                        distinct()
      updateSliderInput( inputId = "fatalH", 
                         value = default_values$best_est_d, 
                         max = ifelse(( default_values$best_est_d + default_values$best_est_d * 1.25 ) < 5000, 5000, 
                                        round(( default_values$best_est_d + default_values$best_est_d * 1.25 )/1000 ) * 1000 ))
      updateSliderTextInput( session = session, inputId = "gdpH", 
                             label = paste0( "GDP per capita $", 
                                             round( default_values$GDP_PP_d, 0 ), 
                                             ". Percentage change:"))
      updateSelectInput( inputId = "civlibH", selected = default_values$CL_d )
      updateSelectInput( inputId = "polRH", selected = default_values$PR_d )
      default_click <- impu22[[1]] %>% 
                       filter( iso_d == isoHost() & iso_o == isoOrig(), year == input$year[1] ) %>%
                       select( index0asylum ) %>% 
                       distinct()
      updateCheckboxInput( inputId = "temAsyl", value = as.logical( default_click$index0asylum ))
   })
   
   
   # observeEvent( input$civlibH, {
   #    print(paste0("You have chosen: ", class( input$civlibH )))
   # })
   
   ### get iso codes 
   iso_orig <- eventReactive( input$go, { dat$iso3[ dat$gis_name == input$orig ]})
   iso_host <- eventReactive( input$go, { dat$iso3[ dat$gis_name == input$host ]})  
 
   ## reactive newdata
   newdata <- eventReactive( input$go, {
        newdata <- lapply( impu22, new_data )
   })
   
   ### predictions
   predictions <- reactive({
      #browser()
        flow_predictions <- mapply( function( x, y ) 
                                    predict( x, newdata = y, type = "response" ), 
                                             #se.fit = TRUE, interval="confidence" ),
                                    x = est_models, y = newdata())
        #round( rowMeans( flow_predictions ), 0 )
        pre_newarrival <- data.frame( iso_o = impu22[[1]]$iso_o, 
                                      iso_d = impu22[[1]]$iso_d, 
                                      year = impu22[[1]]$year,
                                      var = round( rowMeans( flow_predictions ), 0 ))
        pre_newarrival
   })
   
   ## calculate year totals for iso_d
   iso_d_totals <- reactive({
       predictions() %>% 
       group_by( iso_d, year ) %>% 
       summarise( total = sum( var ))
   })
   
   ## create line plot
   output$line_plot <- renderPlotly({
      data_pred <- predictions() %>% 
                   filter( iso_o == iso_orig() & iso_d == iso_host() & year %in% c( 2022:2024 ))  
                  
      if( nrow( data_pred ) == 0 ){
          shiny::validate( "No data available for this country pair. Please select another country pair." )
      }
      
      data_line <- impu17[[1]] %>% 
                   select( iso_o, iso_d, year, newarrival ) %>% 
                   filter( iso_o == iso_orig() & iso_d == iso_host()) %>%
                   rename( var = newarrival ) %>% 
                   bind_rows( data_pred )
      
      line_plot( data_line )
   })

   ### data Sankey plot
   sankey_dat <- reactive({
      data_sankey <- predictions() %>% 
                     filter( iso_d == iso_host() & year %in% c( 2022:2024 ))
   })
   
   ### create Sankey plot 2022
   output$sankey22 <- renderSankeyNetwork({
      # 22 data
      data_sankey_22_max  <- sankey_dat() %>%
                             filter( year == 2022 ) %>%
                             top_n( 4 ) %>%
                             select( -year )
      
      iso_d_totals <- iso_d_totals() %>%
                      filter( iso_d == iso_host() & year == 2022 )
      
      if( !( iso_orig() %in% data_sankey_22_max$iso_o )){
         data_sankey_22 <- sankey_dat() %>%
                           filter( year == 2022 & iso_o == iso_orig()) %>%
                           select( -year ) %>%
                           bind_rows( data_sankey_22_max )  
      } else{
         data_sankey_22 <- data_sankey_22_max 
      }
      
      tmp_total <- iso_d_totals$total - sum( data_sankey_22$var ) 
      
      data_sankey_22 <- data_sankey_22 %>%
                        add_row( iso_o = "REST",
                                 iso_d = iso_host(),
                                 var =  tmp_total )
      
      # nodes data frame
      nodes <- data.frame( name = c( as.character( data_sankey_22$iso_o ),
                                     as.character( data_sankey_22$iso_d ))
                           %>% unique())
      # id connections
      data_sankey_22$IDsource = match( data_sankey_22$iso_o, nodes$name ) - 1
      data_sankey_22$IDtarget = match( data_sankey_22$iso_d, nodes$name ) - 1

      # plot network
      sankeyNetwork( Links = data_sankey_22, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "var", NodeID = "name",
                     sinksRight = FALSE, colourScale = ColourScal,
                     nodeWidth = 40, fontSize = 13, nodePadding = 20 )
   })
   
   ### create Sankey plot 2023
   output$sankey23 <- renderSankeyNetwork({
      # 23 data
      data_sankey_23_max  <- sankey_dat() %>%
                             filter( year == 2023 ) %>%
                             top_n( 4 ) %>%
                             select( -year )
      
      iso_d_totals <- iso_d_totals() %>%
         filter( iso_d == iso_host() & year == 2023 )
      
      if( !( iso_orig() %in% data_sankey_23_max$iso_o )){
         data_sankey_23 <- sankey_dat() %>%
            filter( year == 2023 & iso_o == iso_orig()) %>%
            select( -year ) %>%
            bind_rows( data_sankey_23_max )  
      } else{
         data_sankey_23 <- data_sankey_23_max 
      }
      
      tmp_total <- iso_d_totals$total - sum( data_sankey_23$var ) 
      
      data_sankey_23 <- data_sankey_23 %>%
         add_row( iso_o = "REST",
                  iso_d = iso_host(),
                  var =  tmp_total )
      
      # nodes data frame
      nodes <- data.frame( name = c( as.character( data_sankey_23$iso_o ),
                                     as.character( data_sankey_23$iso_d ))
                           %>% unique())
      # id connections
      data_sankey_23$IDsource = match( data_sankey_23$iso_o, nodes$name ) - 1
      data_sankey_23$IDtarget = match( data_sankey_23$iso_d, nodes$name ) - 1
      
      # plot network
      sankeyNetwork( Links = data_sankey_23, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "var", NodeID = "name",
                     sinksRight = FALSE, colourScale = ColourScal,
                     nodeWidth = 40, fontSize = 13, nodePadding = 20 )
   })
   
   ### create Sankey plot 2024
   output$sankey24 <- renderSankeyNetwork({
      # 24 data
      data_sankey_24_max  <- sankey_dat() %>%
         filter( year == 2024 ) %>%
         top_n( 4 ) %>%
         select( -year )
      
      iso_d_totals <- iso_d_totals() %>%
         filter( iso_d == iso_host() & year == 2024 )
      
      if( !( iso_orig() %in% data_sankey_24_max$iso_o )){
         data_sankey_24 <- sankey_dat() %>%
            filter( year == 2024 & iso_o == iso_orig()) %>%
            select( -year ) %>%
            bind_rows( data_sankey_24_max )  
      } else{
         data_sankey_24 <- data_sankey_24_max 
      }
      
      tmp_total <- iso_d_totals$total - sum( data_sankey_24$var ) 
      
      data_sankey_24 <- data_sankey_24 %>%
                        add_row( iso_o = "REST",
                                 iso_d = iso_host(),
                                 var =  tmp_total )
      
      # nodes data frame
      nodes <- data.frame( name = c( as.character( data_sankey_24$iso_o ),
                                     as.character( data_sankey_24$iso_d ))
                           %>% unique())
      # id connections
      data_sankey_24$IDsource = match( data_sankey_24$iso_o, nodes$name ) - 1
      data_sankey_24$IDtarget = match( data_sankey_24$iso_d, nodes$name ) - 1
      
      # plot network
      sankeyNetwork( Links = data_sankey_24, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "var", NodeID = "name",
                     sinksRight = FALSE, colourScale = ColourScal,
                     nodeWidth = 40, fontSize = 13, nodePadding = 20 )
   })
   
   

################################ download tab ##################################
   
   ### calculate stock data 
   stock <- reactive({
      #browser()
      # create host country sub set and nest grouped data frame 
      dat_stock <- dat_stock %>% 
                   left_join( predictions(), by = c( "iso_o", "iso_d", "year" )) %>% 
                   filter( iso_d == iso_host() & year %in% c( 2021:2024 )) %>% 
                   rename( predarrival = var ) %>% 
                   group_by( iso_o ) %>% 
                   mutate( deci_rate_d = replace( deci_rate_d, is.na( deci_rate_d ), 0 ),
                           deci_posi_rate_o = replace( deci_posi_rate_o, is.na( deci_posi_rate_o ), deci_rate_d ), 
                           index0asylum = replace( index0asylum, is.na( index0asylum ), 0 )) %>%
                   nest()
      
      # function to calculate new refugee, asylum seeker and vda stocks
      stock_calc <- function( df ){        
         within( df, {
               for( i in 2:4 ){
               # refugees 
               ref[i] <- round( ref[i-1] + predarrival[i] * index0asylum[i] +
                       ( predarrival[i] * ( 1 - index0asylum[i]) * ( 1 - percVDA[i-1]) +
                         asy[i-1]) * deci_rate_d[i] * deci_posi_rate_o[i], 0 )
               # asylum seekers 
               asy[i] <- round( ( asy[i-1] + predarrival[i] * ( 1 - index0asylum[i]) * ( 1 - percVDA[i-1] )) *
                         ( 1 - deci_rate_d[i]), 0 )
               # venezuelans
               vda[i] <- round( vda[i-1] + predarrival[i] * percVDA[i-1], 0 )
            }
         })
      }
      
      # calculate stocks 
      dat_stock$data <- lapply( dat_stock$data, stock_calc ) 
      # unlist  
      dat_stock <- unnest( dat_stock, cols = c( data ))
      # select variables and rename 
      dat_stock %>% select( iso_o, iso_d, year, ref, asy, vda ) %>% 
                    filter( iso_o != "UKN" ) %>%
                    mutate( vda = replace( vda, iso_o != "VEN", 0 )) %>% 
                    rename( country_origin = iso_o, 
                            country_asylum = iso_d, 
                            refugee_stocks = ref, 
                            asylum_stocks = asy, 
                            venezuelans_stocks = vda ) %>% 
                    as.data.frame()
      
   })
   
   ### make display table 
   display <- reactive({
      totals <- stock() %>% 
                group_by( country_asylum, year ) %>% 
                summarise( refugee_stocks = sum( refugee_stocks, na.rm = TRUE ), 
                           asylum_stocks = sum( asylum_stocks, na.rm = TRUE ), 
                           venezuelans_stocks = sum( venezuelans_stocks, na.rm = TRUE )) %>% 
                ungroup() %>%
                mutate( country_origin = "REST" ) %>% 
                select( country_origin, country_asylum, year,
                        refugee_stocks, asylum_stocks, venezuelans_stocks ) 
      
      top3 <- stock() %>% 
              group_by( year ) %>% 
              top_n( 3, wt = get( input$top3 )) %>% 
              ungroup() 
              
      top3_totals <- top3 %>% 
                     group_by( year ) %>% 
                     summarise( refugee_stocks = sum( refugee_stocks, na.rm = TRUE ), 
                                asylum_stocks = sum( asylum_stocks, na.rm = TRUE ), 
                                venezuelans_stocks = sum( venezuelans_stocks, na.rm = TRUE )) %>%
                     ungroup()
      
      
      totals[ , 4:6 ] <- totals[ , 4:6 ] - top3_totals[ , 2:4 ]
      
      view_table <- top3 %>% bind_rows( totals ) %>% 
                             arrange( year )
      as.data.frame( view_table )
   })
   
   ### download stock data country of asylum   
   output$downloadCsvhost <- downloadHandler(
      filename = function() {
         paste( "stock_data_country_asylum", ".csv", sep = "" )
      },
      content = function(file) {
         write.csv( stock(), file )
      }) 
   
   ### table display risk data  
   output$rawtable <- renderPrint({
      orig <- options( width = 1000 )
      print( display(), row.names = FALSE)
      options( orig )
   })
   
} # end Shiny server function

################################################################################
################################################################################

##### Run the application 
shinyApp( ui = ui, server = server )

################################################################################
################################################################################
################################################################################
