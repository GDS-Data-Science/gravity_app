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
library( readr )
library( readxl )
library( shiny )
library( shinythemes )
library( shinyWidgets )
library( utils )

#============================== read in data ===================================

dat <- read.csv( "input_data/CountriesRegions.csv" )
load( "input_data/impuData21.Rdata" )
load( "input_data/estimations.Rdata" )

#============================ generate data set ================================

# generate subset of dat for country of origin and host country 
dat_o <- subset( dat, iso3 %in% impu21[[1]]$iso_o )
dat_d <- subset( dat, iso3 %in% impu21[[1]]$iso_d )

# generate factor variables 
for( i in 1:5 ){
   impu21[[i]] <- within( impu21[[i]], {
      CL_d_factor <- cut( CL_d, c( 1, 3, 6, 8 ), right = FALSE, 
                          labels = c( "free", "partfree", "nofree" ))
      CL_o_factor <- cut( CL_o, c( 1, 3, 6, 8 ), right = FALSE, 
                          labels = c( "free", "partfree", "nofree" ))
      PR_d_factor <- cut( PR_d, c( 1, 3, 6, 8 ), right = FALSE, 
                          labels = c( "free", "partfree", "nofree" ))
      PR_o_factor <- cut( PR_o, c( 1, 3, 6, 8 ), right = FALSE, 
                          labels = c( "free", "partfree", "nofree" ))
   })
}

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
         geom_bar( aes( x = years, y = var ), 
                   stat = "identity", fill = "#057fab" ) +
         geom_text( aes( x = years, y = var, label = var ),
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
                              br(), 
                              fluidRow( 
                                    column( 6,  
                                         radioButtons( "civlibO", "Civil Liberty", 
                                                       choices = c( "Free" = "free", 
                                                                    "Partly free" = "partfree", 
                                                                    "Not free" = "nofree" ), 
                                                       selected = "free" )), 
                                    column( 6, 
                                         radioButtons( "polRO", "Political Rights", 
                                                       choices = c( "Free" = "free", 
                                                                    "Partly free" = "partfree", 
                                                                    "Not free" = "nofree" ), 
                                                       selected = "free" ))), 
                              br(),
                              selectInput( "year", "Select year", 
                                           choices = c( 2021, 2022, 2023 ))
                              ),
                      column( 6,
                              tabsetPanel( type = "pills", 
                                 tabPanel( "Summary", 
                                           plotlyOutput( "bar_plot", height = "550px" )), 
                                 tabPanel( "2021", 
                                           sankeyNetworkOutput( "sankey21", height = "600px" )), 
                                 tabPanel( "2022", tableOutput("checker")),
                                 tabPanel( "2023" )
                              )
                      ),
                      column( 3, 
                              h3( "Host Country" ),
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
                              br(), 
                              fluidRow( 
                                 column( 6,  
                                         radioButtons( "civlibH", "Civil Liberty", 
                                                       choices = c( "Free" = "free", 
                                                                    "Partly free" = "partfree", 
                                                                    "Not free" = "nofree" ), 
                                                       selected = "free" )), 
                                 column( 6, 
                                         radioButtons( "polRH", "Political Rights", 
                                                       choices = c( "Free" = "free", 
                                                                    "Partly free" = "partfree", 
                                                                    "Not free" = "nofree" ), 
                                                       selected = "free" ))
                              ), # end fluidrow2
                              checkboxInput( "temAsyl", "Temporary Asylum" ),
                         ) # end column
                   ), # end fluidrow   
                   
               div( absolutePanel( bottom = 20, right = 60, width = 80,
                                   fixed=TRUE, draggable = FALSE, height = "auto",
                                   tags$img( src = 'unhcr.png', height = '120', width = '120')))),

############################### tab 'Data Table' ############################### 
         
         tabPanel( "Data Download",
                   numericInput( "maxrows", "Rows to show", 25),
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
                           tags$li( "Choose the country of origin and the host country in the drop down menu."),
                           tags$li( "Choose the year of the modification." ),
                           tags$li( "Choosing country and year, will automatically install the expected default 
                                     values for all variables of a given country of origin and host country.
                                     Please note, that because these are predicted values, 
                                     these values will come with some uncertainty and will not necessarily 
                                     reflect the newest developments. 
                                     As a user, you can now use your expert knowledge to 
                                     modify these values according to your expectations or interests.
                                     For example, you can increase the number of conflict fatalities 
                                     in the country of origin for the next year and reduce the 
                                     per capita GDP of the host country by 2% to see what an impact 
                                     these changes would have on the expected refugee flows between 
                                     your chosen country pair." ), 
                           tags$li( "Once all parameters are set, click on the button 'Run Simulation'.
                                     Clicking this button will update the prediction of future refugee 
                                     flows and stock figures as they could be expected as a consequence of the 
                                     chosen modifications." ), 
                           tags$li( "The updated results can be downloaded as a CSV file under tab 
                                    'Data Download'.")
                        )),
                   div( class = "text",
                        tags$h4( "Development and Maintenance" ),
                       "Data Science Team, Statistics and Demographics Section (SDS), GDS", tags$br(),
                        tags$br(), tags$h4( "Contact" ),
                       "pellandr@unhcr.org", tags$br(),
                       "hennings@unhcr.org", tags$br(),
                       "delpanta@unhcr.org", tags$br(),
                       "huang@unhcr.org", tags$br()),
                   
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
   new_data <- function( dt ){
      nDat <- filter( dt, iso_d == iso_host() )
      # modify dead and dead_log
      nDat$dead_d[ nDat$year >= input$year ] <- ifelse( input$fatalH > 100, 0, 1 )
      nDat$dead_o[ nDat$year >= input$year & nDat$iso_o == iso_orig() ] <- ifelse( input$fatalO > 100, 0, 1 )
      nDat$dead_log_d[ nDat$year >= input$year ] <- ifelse( input$fatalH == 0, 0, log( input$fatalH ))
      nDat$dead_log_o[ nDat$year >= input$year & nDat$iso_o == iso_orig() ] <- ifelse( input$fatalO == 0, 0, log( input$fatalO ))
      # modify Nyear_conf and Nyear_log
      nDat$Nyear_conf_d[ nDat$year == input$year ] <- ifelse( input$fatalH > 50, 1, 0 )
      nDat$Nyear_conf_o[ nDat$year == input$year & nDat$iso_o == iso_orig() ] <- ifelse( input$fatalO > 50, 1, 0 )
      nDat$Nyear_log_d <- ifelse( input$fatalH < 50, 0, nDat$Nyear_log_d  )
      nDat$Nyear_log_o <- ifelse( input$fatalO < 50, 0, nDat$Nyear_log_o  )
      # modify GDP_PP_d and GDP_PPP_d
      if( input$gdpH != 0 ){
         nDat$GDP_PP_d[ nDat$year >= input$year ] <-
            nDat$GDP_PP_d[ nDat$year >= input$year ] * ( 1 + input$gdpH/100 )
         nDat$GDP_PPP_d[ nDat$year >= input$year ] <-
            nDat$GDP_PPP_d[ nDat$year >= input$year ] * ( 1 + input$gdpH/100 )
      }
      # modify GDP_PP_o and GDP_PPP_o
      if( input$gdpO != 0 ){
         nDat$GDP_PP_o[ nDat$year >= input$year & nDat$iso_o == iso_orig() ] <-
            nDat$GDP_PP_o[ nDat$year >= input$year & nDat$iso_o == iso_orig() ] * ( 1 + input$gdpO/100 )
         nDat$GDP_PPP_o[ nDat$year >= input$year & nDat$iso_o == iso_orig() ] <-
            nDat$GDP_PPP_o[ nDat$year >= input$year & nDat$iso_o == iso_orig() ] * ( 1 + input$gdpO/100 )
      }
      # modify CL_d
      nDat$CL_d[ nDat$year >= input$year ] <- ifelse( input$civlibH == "free", 2,
                                                 ifelse( input$civlibH == "partfree", 5, 7 ))
      # modify CL_o
      nDat$CL_o[ nDat$year >= input$year & nDat$iso_o == iso_orig() ] <-
                                              ifelse( input$civlibO == "free", 2,
                                                 ifelse( input$civlibO == "partfree", 5, 7 ))
      # modify PR_d
      nDat$PR_d[ nDat$year >= input$year ] <- ifelse( input$polRH == "free", 2,
                                                 ifelse( input$polRH == "partfree", 5, 7 ))
      # modify CL_d
      nDat$PR_o[ nDat$year >= input$year & nDat$iso_o == iso_orig() ] <-
                                              ifelse( input$polRO == "free", 2,
                                                 ifelse( input$polRO == "partfree", 5, 7 ))
      # modify index0asylum
      if( input$temAsyl == TRUE ){
         nDat$index0asylum[ nDat$year >= input$year & nDat$iso_o == iso_orig() ] <- 1
      } else{
         nDat$index0asylum[ nDat$year >= input$year & nDat$iso_o == iso_orig() ] <- 0
      }
      nDat
   }

   ### get iso codes 
   iso_orig <- reactive({ dat$iso3[ dat$gis_name == input$orig ]})
   iso_host <- reactive({ dat$iso3[ dat$gis_name == input$host ]})   
   
   ### update buttons and sliders for country of origin   
   observeEvent( c( input$orig, input$year ), {
      default_values <- impu21[[1]] %>% 
                        filter( iso_o == iso_orig() & year == input$year ) %>%
                        select( best_o, CL_o_factor, GDP_PP_o, PR_o_factor ) %>% 
                        distinct()
      updateSliderInput( inputId = "fatalO", 
                         value = default_values$best_o, 
                         max = ifelse(( default_values$best_o + default_values$best_o * 1.25 ) < 5000, 5000, 
                                        round(( default_values$best_o + default_values$best_o * 1.25 )/1000 ) * 1000 ))
      updateSliderTextInput( session = session, inputId = "gdpO", 
                             label = paste0( "GDP per capita $", 
                                     round( default_values$GDP_PP_o, 0 ), 
                                     ". Percentage change:"))
      updateRadioButtons( inputId = "civlibO", selected = default_values$CL_o_factor )
      updateRadioButtons( inputId = "polRO", selected = default_values$PR_o_factor )
   })

   ### update buttons and sliders for host country
   observeEvent( c( input$host, input$orig, input$year ), {
      default_values <- impu21[[1]] %>% 
                        filter( iso_d == iso_host() & year == input$year ) %>%
                        select( best_d, CL_d_factor, GDP_PP_d, PR_d_factor ) %>% 
                        distinct()
      updateSliderInput( inputId = "fatalH", 
                         value = default_values$best_d, 
                         max = ifelse(( default_values$best_d + default_values$best_d * 1.25 ) < 5000, 5000, 
                                        round(( default_values$best_d + default_values$best_d * 1.25 )/1000 ) * 1000 ))
      updateSliderTextInput( session = session, inputId = "gdpH", 
                             label = paste0( "GDP per capita $", 
                                             round( default_values$GDP_PP_d, 0 ), 
                                             ". Percentage change:"))
      updateRadioButtons( inputId = "civlibH", selected = default_values$CL_d_factor )
      updateRadioButtons( inputId = "polRH", selected = default_values$PR_d_factor )
      default_click <- impu21[[1]] %>% 
                      filter( iso_d == iso_host() & iso_o == iso_orig(), year == input$year ) %>%
                      select( index0asylum ) %>% 
                      distinct()
      updateCheckboxInput( inputId = "temAsyl", value = as.logical( default_click$index0asylum ))
   })
   
   
   ### reactive newdata
   newdata <- reactive({
        newdata <- lapply( impu21, new_data )
   })
   
                                 
   ### predictions
   predictions <- reactive({
        flow_predictions <- mapply( function( x, y ) predict( x, newdata = y, type = "response" ),
                                    x = est_models, y = newdata())
        round( rowMeans( flow_predictions ), 0 )
   })
   

   ## create bar plot
   output$bar_plot <- renderPlotly({
       if( mean( names( predictions()) == iso_orig()) == 0 ){
           shiny::validate( "No data available for this country pair. Please select another country pair." )
      }

      # create data set for bar plot
      data_bar <- data.frame( years = c( 2021:2024 ),
                              var = predictions()[ names( predictions()) == iso_orig()])
      data_bar <- data_bar[ data_bar$years %in% c( 2021:2023), ]
      # plot data
      bar_plot( data_bar )
   })

   ### data Sankey plot
   sankey_dat <- reactive({
      data_sankey <- data.frame( years = rep( 2021:2024, n = length( predictions()/4)),
                                 iso_o = names( predictions()),
                                 iso_d = iso_host(),
                                 var = predictions())
   })

   ### create Sankey plot 2021
   output$sankey21 <- renderSankeyNetwork({
      # 21 data
      data_sankey_21 <- sankey_dat() %>%
                        filter( years == 2021 ) %>%
                        arrange( desc( var)) %>%
                        top_n( 5 ) %>%
                        select( -years )
      # nodes data frame
      nodes <- data.frame( name = c( as.character( data_sankey_21$iso_o ),
                                     as.character( data_sankey_21$iso_d ))
                           %>% unique())
      # id connections
      data_sankey_21$IDsource = match( data_sankey_21$iso_o, nodes$name ) - 1
      data_sankey_21$IDtarget = match( data_sankey_21$iso_d, nodes$name ) - 1

      # plot network
      sankeyNetwork( Links = data_sankey_21, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "var", NodeID = "name",
                     sinksRight = FALSE, colourScale = ColourScal,
                     nodeWidth = 40, fontSize = 13, nodePadding = 20 )
   })
 
################################ download tab ##################################
   
   ### calculate stock data 
   
   

   ### download stock data    
   output$downloadCsv <- downloadHandler(
      filename = function() {
         paste( "stock_data", ".csv", sep = "" )
      },
      content = function(file) {
         write.csv( stock, file )
      }) 
   
   ### table display risk data  
   output$rawtable <- renderPrint({
      orig <- options( width = 1000 )
      print( tail( stock, input$maxrows ), row.names = FALSE)
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
