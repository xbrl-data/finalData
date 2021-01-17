library(shiny)
library(shinymaterial)
library(shinyMobile)
library(shinyWidgets)
library(highcharter)
library(lubridate)
library(leaflet)
library(shinyjs)
library(polished)
library(glue)

cols <- c('#0D1540', '#06357a', '#00a4e3', '#adafb2', '#a31c37', '#d26400', '#eaa814', '#5c1848', '#786592', '#ff4e50', '#027971', '#008542', '#5c6d00')

df1 <- readRDS('./data/har.rds')# %>% mutate(Price.Sq.Ft.List = as.integer(Price.Sq.Ft.List),
#                                             Price.Sq.Ft.Sold = as.integer(Price.Sq.Ft.Sold)) %>%
#   
#   mutate(popup = 
#                                               
#                                               glue::glue(
#                                                 '<b>Address:</b> {Address}, {City.Location} {Zip.Code}<br>
#                                                  <b>Year Built:</b> {Year.Built}<br>
#                                                 <b>SqFt/Bedrooms/Bathrooms/Stories:</b> {Building.SqFt}/{Bedrooms}/{Baths.Total}/{Stories}<br>
#                                                 <b>Pool?:</b> {Pool.Private}<br>
#                                                 <b>List Date:</b> {List.Date}<br>
#                                                  <b>List Price:</b> ${List.Price}<br>
#                                                 <b>List Price Per Sq Ft:</b> ${Price.Sq.Ft.List}/ft<br>
#                                                 <b>Sold Date:</b> {Close.Date}<br>
#                                                  <b>Sold Price:</b> ${Close.Price}<br>
#                                                 <b>Sold Price Per Sq Ft:</b> ${Price.Sq.Ft.Sold}/ft
#                                                 
#                                                 '
#                                                 
#                                                 
#                                               )
#                                               
#                                               )

ui <- material_page(
  title ="",
  #nav_bar_fixed = T,
  #nav_bar_color = "white",
  include_fonts = TRUE,
  include_icons = TRUE,
  background_color =  "grey lighten-5",
  primary_theme_color = "#424242",
  include_nav_bar = FALSE,
  material_tabs(
    tabs = c(
      "Home" = "home",
      "Analytics" = "analytics"
    ),
    color = "dark gray"
  ),
  
  #     HTML('<center><img src="logo.png"></center>'),
  # tags$hr(style="border-color: #eeeeee;"),
  material_tab_content(
    tab = 'home',

    HTML('<center><h4><b>Welcome to Linh Davis Homes</b></h4></center>'),
    material_row(
      material_column(width = 12, align = 'center',
        HTML('<center><p>This is a long paragraph describing my passion for real estate, my goals, your goals, and anything else I can think of.</p></center>')
      )
    ),
    br(),
    material_row(
      #class = "text-center",
      material_column(width = 12, offset = 0, align = 'center',
                      material_modal(
                        modal_id = "about",
                        button_text = "About",
                        button_depth = 5,
                        button_color = "grey darken-3",
                        button_icon = "info",
                        title = "",
                        p('Help')
                      )
      )
    ),
    br(),
    HTML('<center><a href = "https://www.kwmemorial.com" target="_blank"><img src="https://pics.harstatic.com/office/395001.png" width = "15%"></a></center>'),
    br(),
    br(),
    
    material_row(
      #class = "text-center",
      material_column(width = 4, offset = 0, align = 'center',
        material_modal(
          modal_id = "buyHome",
          button_text = "Buy",
          button_depth = 5,
          
          button_color = "red darken-4",
          button_icon = "home",
          title = "",
          f7Block(
            
            h6("In today's data-driven world, finding a home in Houston is
               easy.  Finding the perfect home is difficult.  With our dedicated staff
               and resources, we can easily help you find your forever home.", style = "color:#06357a;"),
            br(),
            tags$a(img(src = 'https://www.kwmemorial.com/wp-content/uploads/2018/11/home_scr_2_1.jpg')),
            br(),
            
            h6("Find a realtor", style = "color:#06357a;"), style="text-align: center;",
            
            
            f7Link(label = icon("piggy-bank", "fa-3x"),
                   src = "https://www.consumerfinance.gov/owning-a-home/prepare/",
                   external = TRUE),
            
            h6("Assess finances", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("university", "fa-3x"),
                   src = "https://www.bankrate.com/calculators/mortgages/how-much-money-can-i-borrow.aspx",
                   external = TRUE),
            
            h6("Qualify for mortgage", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("check-double", "fa-3x"),
                   src = "https://www.moneyunder30.com/home-buying-checklist-wants-vs-needs",
                   external = TRUE),
            
            h6("Assess wants and needs", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("home", "fa-3x"),
                   src = "https://www.kwmemorial.com/advanced-search-property/",
                   external = TRUE),
            
            h6("Find your perfect home", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("handshake", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/buyers/",
                   external = TRUE),
            
            h6("Make an offer", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("info-circle", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/buyers/",
                   external = TRUE),
            
            h6("Home inspection", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("thumbs-up", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/buyers/",
                   external = TRUE),
            
            h6("Mortgage approved", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("tasks", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/buyers/",
                   external = TRUE),
            
            h6("Prepare for closing", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("truck-moving", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/buyers/",
                   external = TRUE),
            
            h6("Move In!", style = "color:#06357a;"), style="text-align: center;"
            
          )
        )
      ),
      material_column(
        width = 4, offset = 0,align = 'center',
        material_modal(
          modal_id = "sellHome",
          button_text = "Sell",
          button_depth = 5,
          
          button_color = "grey darken-3",
          button_icon = "swap_horiz",
          title = "",
          f7Block(
            
            h6("Our dedicated professionals, unparalleled brand, and innovative tools
          can help you to understand the best ways to maximize the value of your home.", 
               style = "color:#06357a;"),
            br(),
            tags$a(img(src = 'https://www.kwmemorial.com/wp-content/uploads/2018/08/index-our-services-pic-1.jpg', width ='35%')),
            br(),
            
            f7Link(label = icon("search", "fa-3x"),
                   src = "https://www.kwmemorial.com",
                   external = TRUE),
            h6("Find a realtor", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("hand-holding-usd", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/sellers/",
                   external = TRUE),
            
            h6("Pricing Strategy", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("bullhorn", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/sellers/",
                   external = TRUE),
            
            
            h6("Marketing", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("paint-roller", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/sellers/",
                   external = TRUE),
            
            h6("Prepare home", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("ad", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/sellers/",
                   external = TRUE),
            
            
            h6("List Home", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("shoe-prints", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/sellers/",
                   external = TRUE),
            
            h6("Showings/Open House", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("exchange-alt", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/sellers/",
                   external = TRUE),
            
            h6("Negotiation", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("handshake", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/sellers/",
                   external = TRUE),
            
            h6("Under Contract", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("file-contract", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/sellers/",
                   external = TRUE),
            
            h6("Final Details", style = "color:#06357a;"), style="text-align: center;",
            
            f7Link(label = icon("truck-moving", "fa-3x"),
                   src = "https://www.kwmemorial.com/services/sellers/",
                   external = TRUE),
            
            h6("Moving Day", style = "color:#06357a;"), style="text-align: center;",
            tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/zj7Y1zxfJk4",
                        frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
                        allowfullscreen=NA), style="text-align: center;"
            
          )
      )
      ),
      material_column(width = 4, offset = 0,align = 'center',
                      
                      material_modal(
                        modal_id = "rentHome",
                        button_text = "Rent",
                        button_color = "grey",
                        button_depth = 5,
                        
                        button_icon = "touch_app",
                        title = "",
                        f7Block(
                          h6("We also provide assistance in finding the perfect rental
                      for you.  The options in Houston are numerous and
         can be adapted to what's most imporant to you.", style = "color:#06357a;"),
                          br(),
                          tags$a(img(src = 'https://www.kwmemorial.com/wp-content/uploads/2018/08/index-about-us-left-pic.png', width = '35%')),
                          br(),
                          icon("traffic-light", "fa-3x"),
                          h6("Short Commute", style = "color:#06357a;"), style="text-align: center;",
                          icon("swimmer", "fa-3x"),
                          h6("Pool", style = "color:#06357a;"), style="text-align: center;",
                          icon("building", "fa-3x"),
                          h6("High-Rise", style = "color:#06357a;"), style="text-align: center;",
                          icon("binoculars", "fa-3x"),
                          h6("Great Views", style = "color:#06357a;"), style="text-align: center;",
                          icon("wine-glass", "fa-3x"),
                          h6("Nightlife", style = "color:#06357a;"), style="text-align: center;",
                          icon("home", "fa-3x"),
                          h6("Traditional Home", style = "color:#06357a;"), style="text-align: center;"
                        )
                      
                     )
      )
    ), 
    br(),
    material_parallax(image_source = 'https://upload.wikimedia.org/wikipedia/commons/4/4d/Houston_Skyline_%285374518048%29.jpg'),
    br(),
    material_row(
      material_column(width = 6, align = 'center',
    
      uiOutput("trec")
    ),
    material_column(width = 6, align = 'center',
                    
      uiOutput('protect')
    )
    
    )
  ),
  material_tab_content(
    tab = 'analytics',
    
   useShinyjs(),
    material_row(
      material_column(width = 3,# align = 'center',
        material_card(
          title = strong('Regional Filters'),
          depth = 3,
          uiOutput('homes'),
          uiOutput("TT"),
          
          material_card(
            title = strong('Property Filters'),
            br(),
            #uiOutput('homes'),
            #depth = 3,
            

            
            sliderInput("rooms", label = h5("Bedrooms"), min = 0,
                        max = 6, value = c(2, 4), step = 0.5),

            sliderInput("bathrooms", label = h5("Bathrooms"), min = 0, 
                        max = 6, value = c(1, 3), step = 0.5),
            
            
            sliderInput("sqft", label = h5("Square Feet"), min = 0, 
                        max = 8000, value = c(1000, 4000), step = 250, ticks = FALSE),
            
            sliderInput("year", label = h5("Year Built"), min = 1970, 
                        max = 2020, value = c(2010, 2020), step = 5, sep = ''), 
            # material_row(
            #   material_column(width = 6,
            #     
                awesomeRadio(
                  inputId = "pool",
                  label = h5("Pool?"),
                  choices = c('Y', 'N', 'Both'),
                  selected = 'Both',
                  inline = T
                ),
              #   ),
              # material_column(width = 6,
                              
                awesomeRadio(
                  inputId = "fireplace",
                  label = h5("Fireplace?"),
                  choices = c('Y', 'N', 'Both'),
                  selected = 'Both',
                  inline = T
                )
              #)
            #)
          )
          
        )
      ),
      material_column(width = 9, align = 'center',
                      
                      email_input('email',label = tagList(icon("envelope"), "Please Enter Email to Access Market Analytics")),
                      shinymaterial::material_button('push', label = 'Submit', depth = 3, color = 'red darken-4'),
                      
                      hidden(div(
                        id = 'dataHide',
                      
                      highchartOutput('homesSold'),
                      highchartOutput('homeMetrics'),
                      #highchartOutput('homeDisc'),
                      highchartOutput('plot5'),
                      highchartOutput('plot6'),
                      br(),
                      hidden(div(
                        id = 'mapHide',
                        material_modal(
                          modal_id = "mapHome",
                          button_text = "Map",
                          button_color = "grey",
                          button_depth = 5,
                          
                          button_icon = "map",
                          title = "",
                          leafletOutput('map')
                        ))
                        
                      )
                      
                      ))
      )
    )
  )
   
     
   
      
    
  

)


server <- function(input, output, session){
  
  output$protect <- renderUI({
    # url <- a(strong("TREC Consumer Notice"), href="http://docs.wixstatic.com/ugd/07f529_2661e82ee694419f90ff6e5faa493c46.pdf", target = "_blank")
    # tagList( url)
    f7Link(label = strong("TREC Consumer Notice"),
           src = "http://docs.wixstatic.com/ugd/07f529_2661e82ee694419f90ff6e5faa493c46.pdf",
           external = TRUE)
  })

  
  
  output$trec <- renderUI({
    # url <- a(strong("TREC Consumer Notice"), href="http://docs.wixstatic.com/ugd/07f529_2661e82ee694419f90ff6e5faa493c46.pdf", target = "_blank")
    # tagList( url)
    f7Link(label = strong("TREC Information"),
           src = "trec.pdf",
           external = TRUE)
  })
  
  output$TT <- renderUI({
    
    
                      
          selectizeGroupUI(
            id = "my-filters",
            params = list(
              propType = list(inputId = "propType", title = "Property Type:"),
              city = list(inputId = "city", title = "City:"),
              market = list(inputId = "market", title = "Market Area:"),
              Subdivision = list(inputId = "Subdivision", title = "Subdivision:"),
              # Style = list(inputId = "Style", title = "Style:"),
              # Stories = list(inputId = "Stories", title = "Stories:"),
              elementary = list(inputId = "elementary", title = "Elementary:"),
              middle = list(inputId = "middle", title = "Middle School:"),
              high = list(inputId = "high", title = "High School:"),
              zip = list(inputId = "zip", title = "Zip Code:")
            ),
            inline = TRUE
          
    )
    
  })
  
  observeEvent(input$push, {
    if(input$email == ''){
      shinyjs::hide('dataHide')
    } else {
      shinyjs::show('dataHide')
      shinyjs::hide('email')
      shinyjs::hide('push')
    }
  })
  
  harData1 <- reactive(
    df1 %>% rename(propType = Property.Type, city = City.Location, market = Market.Area,
                  elementary = School.Elementary, middle = School.Middle,
                   high = School.High, zip = Zip.Code)
  )
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = harData1,
    vars = c("propType", "city", "market", "Subdivision", "elementary", "middle", "high", "zip")
  )
  
  harData3 <- reactive({
    
    check1 <- as.numeric(max(input$sqft))
    if(check1 == 8000){
      check1 <- 1000000
    }
    
    check2 <- as.numeric(max(input$rooms))
    if(check2 == 6){
      check2 <- 100
    }
    
    check3 <- as.numeric(max(input$bathrooms))
    if(check3 == 6){
      check3 <- 100
    }
    
    check4 <- as.numeric(min(input$year))
    if(check4 == 1970){
      check4 <- 1900
    }
    
    dfx <- res_mod() %>%
      filter(Bedrooms >= as.numeric(min(input$rooms)) & Bedrooms <= check2) %>%
      filter(Baths.Total >= as.numeric(min(input$bathrooms)) & Baths.Total <= check3) %>%
      filter(Building.SqFt >= as.numeric(min(input$sqft)) & Building.SqFt <= check1) %>%
      filter(Year.Built >= check4 & Year.Built <= as.numeric(max(input$year)))
    
    if(input$fireplace == 'Both'){
      NULL
    } else {
      dfx <- dfx %>% filter(Fireplace %in% input$fireplace)
    }
    
    if(input$pool == 'Both'){
      NULL
    } else {
      dfx <- dfx %>% filter(Pool.Private %in% input$pool)
    }
    dfx
  })
  
  output$homes <- renderUI({
    paste0('Total Sold Homes: ', nrow(harData3()))
    
  })
  
  

  output$homesSold <- renderHighchart({
    
    
      
      
      highchart() %>%
        hc_colors(cols[c(3,5)]) %>%
        hc_add_series(data.frame(date = as.Date('2020-07-01'), count = 1), type = 'column', hcaes(x = date, y = count),  
                      name = 'Homes Sold', showInLegend = T, id = 'column') %>%
      hc_add_series(data.frame(date = as.Date('2020-07-01'), price = 1), type = 'line', hcaes(x = date, y = as.integer(price)),
                    name = 'Sales Price, $', showInLegend = T, yAxis = 1,
                    marker = list(enabled = F), id = 'line') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_yAxis_multiples(list(title = list(text = 'Homes Sold During Month',
                                             style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])),
                                gridLineColor = 'transparent'),
                           list(title = list(text = 'Average Sales Price', 
                                             style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])),
                                gridLineColor = 'transparent', opposite = T, labels = list(format = '${value}'))              
                           ) %>%
        hc_title(text = 'Home Sales Metrics', align = 'left', style = list(fontFamily = 'Arial', fontSize = '18px', color = cols[2])) %>%
      hc_subtitle(text = 'Total Sales and Average Price', align = 'left', style = list(fontFamily = 'Arial', fontSize = '16px', color = cols[3])) %>%
        hc_tooltip(shared = T)
      
   # }
    
  })
  
  observe({
    if(nrow(harData3()) == 0){
      NULL
    } else {
      
   tryCatch({
      
      
        dfx <- harData3() %>% group_by(month1 = month(Close.Date), year1 = year(Close.Date)) %>% summarise(count = n(), price = mean(Close.Price)) %>%
          ungroup() %>% mutate(date = as.Date(paste0(year1, '-', month1, '-01'))) %>% arrange(date)
  
      highchartProxy('homesSold') %>%
          hcpxy_remove_series(id = "column")%>%
          hcpxy_remove_series(id = "line")%>%
        hcpxy_add_series(dfx, type = 'column', hcaes(x = date, y = count), name = 'Homes Sold', showInLegend = T, id = 'column') %>%
        hcpxy_add_series(dfx, type = 'line', hcaes(x = date, y = as.integer(price)), name = 'Sales Price, $', showInLegend = T, yAxis = 1,
                      marker = list(enabled = F), id = 'line')
      
   },
   error = function(e) {
     e
     NULL
   })

    }

  })
  
  
  
  output$homeMetrics <- renderHighchart({
    
    
    
    
    highchart() %>%
      hc_colors(cols[c(6,8)]) %>%
      hc_add_series(data.frame(date = as.Date('2020-07-01'), days = 1), type = 'column', hcaes(x = date, y = days),  
                    name = 'Days on Market', showInLegend = T, id = 'column') %>%
      hc_add_series(data.frame(date = as.Date('2020-07-01'), discount = 1), type = 'line', hcaes(x = date, y = round(discount*100,1)),
                    name = 'Discount to List, %', showInLegend = T, yAxis = 1,
                    marker = list(enabled = F), id = 'line') %>%
      hc_xAxis(type = 'datetime') %>%
      hc_yAxis_multiples(list(title = list(text = 'Days on Market',
                                           style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])),
                              gridLineColor = 'transparent'),
                         list(title = list(text = 'Average Price Discount', 
                                           style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])),
                              gridLineColor = 'transparent', opposite = T, labels = list(format = '{value}%'))              
      ) %>%
      hc_subtitle(text = 'Days on Market and Discount to List', align = 'left', style = list(fontFamily = 'Arial', fontSize = '16px', color = cols[3])) %>%
      hc_tooltip(shared = T)
    
    # }
    
  })
  
  observe({
    if(nrow(harData3()) == 0){
      NULL
    } else {
      
      tryCatch({
      
        dfx <- harData3() %>%  filter(abs(1-Close.Price/List.Price) < 1) %>%
          group_by(month1 = month(Close.Date), year1 = year(Close.Date)) %>%
          summarise(days = mean(Market.Days, na.rm=T), discount = 1-mean(Close.Price/List.Price, na.rm=T)) %>%
          ungroup() %>% mutate(date = as.Date(paste0(year1, '-', month1, '-01'))) %>% arrange(date)
        
        highchartProxy('homeMetrics') %>%
          hcpxy_remove_series(id = "column")%>%
          hcpxy_remove_series(id = "line")%>%
          hcpxy_add_series(dfx, type = 'column', hcaes(x = date, y = as.integer(days)), name = 'Days on Market', showInLegend = T, id = 'column') %>%
          hcpxy_add_series(dfx, type = 'line', hcaes(x = date, y = round(discount*100, 1)), name = 'Discount to List, %', showInLegend = T, yAxis = 1,
                           marker = list(enabled = F), id = 'line')
        
      },
      error = function(e) {
        e
        NULL
      })
      
    }
    
  })
  
  # output$homeDisc <- renderHighchart({
  #   
  #   
  #   
  #   
  #   highchart() %>%
  #     hc_colors(cols[c(3)]) %>%
  #     hc_add_series(data.frame(priceList = 10, discount = .1), type = 'scatter', hcaes(x = priceList, y = discount),  
  #                   name = 'Discount vs List Price per Sq Ft', showInLegend = F, id = 'column') %>%
  #     hc_xAxis(title = list(text = 'List Price per Sq Ft', 
  #                           style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2]))) %>%
  #     hc_yAxis(title = list(text = 'Average Price Discountt',
  #                                          style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])),
  #                             gridLineColor = 'transparent', labels = list(format = '{value}%')) %>%
  #     hc_subtitle(text = 'List Price per Sq Ft vs Discount', align = 'left', style = list(fontFamily = 'Arial', fontSize = '16px', color = cols[3])) %>%
  #     hc_tooltip(crosshairs = TRUE, #shared = TRUE, 
  #                pointFormat = "<b>List Price Per Sq Ft: </b> {point.x}<br>
  #                               <b>Sales Price Discount to List (%): </b> {point.y}"
  #     )
  #   
  #   # }
  #   
  # })
  # 
  # observe({
  #   if(nrow(harData3()) == 0){
  #     NULL
  #   } else {
  #     
  #     tryCatch({
  #       
  #       dfx <- harData3() %>% mutate(priceList = round(Price.Sq.Ft.List, -1)) %>% filter(abs(1-Close.Price/List.Price) < 1) %>%
  #         group_by(priceList) %>% summarise( discount = 1-mean(Close.Price/List.Price, na.rm=T), count = n()) %>% 
  #         filter(abs(discount) < 200) %>% filter(count > 1)
  #       
  #       highchartProxy('homeDisc') %>%
  #         hcpxy_remove_series(id = "column")%>%
  #         #hcpxy_remove_series(id = "line")%>%
  #         hcpxy_add_series(dfx, type = 'scatter', hcaes(x = priceList, y = round(discount*100, 1)),
  #                          name = 'Discount vs List Price per Sq Ft', showInLegend = F, id = 'column') 
  #       
  #     },
  #     error = function(e) {
  #       e
  #       NULL
  #     })
  #     
  #   }
  #   
  # })
  # 
  
  
  
  output$plot5 <- renderHighchart({
   
    dfx <- data.frame(Date = as.Date('2020-07-01'), PricePerSqFt = 0,
                      p90 = 0, maxPrice = 0, minPrice = 0, p10=0)  
    
      highchart() %>%
        hc_colors(cols) %>%

       # hc_xAxis(type = 'datetime')%>% 
        hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             <b> {point.y}</b><br/>",
                   shared = FALSE) %>%
       # hc_title(text = 'Total Price', align = 'left') %>%
        hc_subtitle(text = 'Monthly Price Per Sq Ft Metrics', align = 'left',
                    style = list(fontFamily = 'Arial', fontSize = '16px', color = cols[3])) %>% 
        hc_legend(align = "right", verticalAlign = "top",
                  layout = "horizontal")%>%
        hc_yAxis(gridLineColor = 'transparent', 
                 title = list(text = 'US$ Per SqFt', style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])),
                 labels = list(format = '${value}/sqFt',style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])))%>%
        hc_xAxis(type = 'datetime', title = list(text = '<b></b>',style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])),
                 labels = list(style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2]))) %>% 
        hc_credits(enabled = TRUE, text = 'Source: HAR', href = "http://www.har.com")%>%
      hc_add_series(dfx, type = 'scatter', hcaes(x = (Date), y = as.integer(PricePerSqFt)),
                       name = 'Price', showInLegend = TRUE,marker=list(symbol='circle', radius=5), id = 'line1') %>%
        hc_add_series(dfx, type = 'spline', hcaes(x = (Date), y = as.integer(p90)), 
                       name = '10th %',
                       marker = list(enabled = FALSE), id = 'line2') %>%
        hc_add_series(dfx, type = 'spline', hcaes(x = (Date), y = as.integer(maxPrice)), 
                       name = '25th %',
                       marker = list(enabled = FALSE), id = 'line3') %>%
        hc_add_series(dfx, type = 'spline', hcaes(x = (Date), y = as.integer(minPrice)),
                       name = '75th %',
                       marker = list(enabled = FALSE), id = 'line4') %>%
        hc_add_series(dfx, type = 'spline', hcaes(x = (Date), y = as.integer(p10)),
                       name = '90th %',
                       marker = list(enabled = FALSE), id = 'line5') 
    
    
  })
  
  observe({
    if(nrow(harData3()) == 0){
      NULL
    } else {
      
      tryCatch({
        dfx <- harData3() %>% mutate(MONTH = month(Close.Date), YEAR = year(Close.Date)) %>%
          group_by(MONTH, YEAR) %>%
          summarise(Volume = sum(Close.Price),Homes = n(), PricePerSqFt = mean(Price.Sq.Ft.Sold),
                    maxPrice = quantile(Price.Sq.Ft.Sold, 0.75), minPrice = quantile(Price.Sq.Ft.Sold, 0.25),
                    p90 = quantile(Price.Sq.Ft.Sold, 0.9), p10 = quantile(Price.Sq.Ft.Sold, 0.1)) %>%
          ungroup() %>% mutate(Date = as.POSIXct(paste0(MONTH, '/01/', YEAR), format = '%m/%d/%Y')) %>%
          arrange( Date) %>%
          mutate(Date = as.Date(Date))
        
        
    
        
        highchartProxy('plot5') %>%
          hcpxy_remove_series(id = "line1")%>%
          hcpxy_remove_series(id = "line2")%>%
          hcpxy_remove_series(id = "line3")%>%
          hcpxy_remove_series(id = "line4")%>%
          hcpxy_remove_series(id = "line5")%>%
          hcpxy_add_series(dfx, type = 'scatter', hcaes(x = (Date), y = as.integer(PricePerSqFt)),
                        name = 'Price', showInLegend = TRUE,marker=list(symbol='circle', radius=5), id = 'line1') %>%
          hcpxy_add_series(dfx, type = 'spline', hcaes(x = (Date), y = as.integer(p90)), 
                        name = '10th %',
                        marker = list(enabled = FALSE), id = 'line2') %>%
          hcpxy_add_series(dfx, type = 'spline', hcaes(x = (Date), y = as.integer(maxPrice)), 
                        name = '25th %',
                        marker = list(enabled = FALSE), id = 'line3') %>%
          hcpxy_add_series(dfx, type = 'spline', hcaes(x = (Date), y = as.integer(minPrice)),
                        name = '75th %',
                        marker = list(enabled = FALSE), id = 'line4') %>%
          hcpxy_add_series(dfx, type = 'spline', hcaes(x = (Date), y = as.integer(p10)),
                        name = '90th %',
                        marker = list(enabled = FALSE), id = 'line5') 
        
      },
      error = function(e) {
        e
        NULL
      })
      
    }
    
  })
  
  output$plot6 <- renderHighchart({

    if(nrow(harData3()) == 0){
      NULL
    } else {
      
    date1 <- max(df1$Close.Date) %m+% months(-6)
    
    dfx <- harData3() %>% filter(Close.Date >= date1) %>%
      group_by(propType) %>%
      summarise(Volume = sum(Close.Price),Homes = n(), PricePerSqFt = mean(Price.Sq.Ft.Sold),
                maxPrice = quantile(Price.Sq.Ft.Sold, 0.75), minPrice = quantile(Price.Sq.Ft.Sold, 0.25),
                p90 = quantile(Price.Sq.Ft.Sold, 0.9), p10 = quantile(Price.Sq.Ft.Sold, 0.1)) %>%
      ungroup() 
    if(nrow(dfx) == 0){
      NULL
    } else {
        tryCatch({
        highchart() %>%
          hc_colors(cols) %>%
          hc_add_series(dfx, type = 'scatter', hcaes(x = propType, y = as.integer(PricePerSqFt)),
                        name = 'Average', showInLegend = TRUE,marker=list(symbol='circle', radius=10),
                        id = 'line1') %>%
          hc_add_series(dfx, type = 'scatter', hcaes(x = (propType), y = as.integer(p90)), 
                        name = '10th %',
                        marker = list(symbol='circle', radius=10),
                        id = 'line2') %>%
          hc_add_series(dfx, type = 'scatter', hcaes(x = (propType), y = as.integer(maxPrice)), 
                        name = '25th %',
                        marker = list(symbol='circle', radius=10),
                        id = 'line3') %>%
          hc_add_series(dfx, type = 'scatter', hcaes(x = (propType), y = as.integer(minPrice)),
                        name = '75th %',
                        marker = list(symbol='circle', radius=10),
                        id = 'line4') %>%
          hc_add_series(dfx, type = 'scatter', hcaes(x = (propType), y = as.integer(p10)),
                        name = '90th %',
                        marker = list(symbol='circle', radius=10),
                        id = 'line5') %>%
          hc_xAxis(categories = dfx$propType)%>% 
          hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
               <b> {point.y}</b><br/>",
                     shared = TRUE) %>%
            hc_subtitle(text = 'Last 6 Months Price Per Sq Ft Metrics', align = 'left',
                        style = list(fontFamily = 'Arial', fontSize = '16px', color = cols[3])) %>% 
          hc_legend(align = "right", verticalAlign = "top",
                    layout = "horizontal")%>%
          hc_credits(enabled = TRUE, text = 'Source: HAR', href = "http://www.har.com")%>%
            hc_yAxis(gridLineColor = 'transparent', 
                     title = list(text = 'US$ Per SqFt', style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])),
                     labels = list(format = '${value}/sqFt',style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])))%>%
            hc_xAxis(title = list(text = '<b></b>',style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2])),
                     labels = list(rotation = -45,style = list(fontFamily = 'Arial', fontSize = '14px', color = cols[2]))) 
      },
      error = function(e) {
        e
        NULL
      })
    
    }
    }
  })
  
  observe({
    if(nrow(harData3() %>% filter(!is.na(lat)) %>% filter(Close.Date >= max(df1$Close.Date) %m+% months(-6))) > 600){
      shinyjs::hide('mapHide')
    } else {
      shinyjs::show('mapHide')
    }
  })
  
  
  output$map <- renderLeaflet({
    
    if(nrow(harData3() %>% filter(!is.na(lat)) %>% filter(Close.Date >= max(df1$Close.Date) %m+% months(-6))) > 600){
      NULL
    } else {
      
      dfx <- harData3() %>% filter(!is.na(lat)) %>% filter(Close.Date >= max(df1$Close.Date) %m+% months(-6))
      
      icons <- awesomeIcons(
        icon = 'home',
        iconColor = cols[3],
        library = 'fa',
        markerColor = cols[3]
      )
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>%
        addAwesomeMarkers(lng = dfx$long, lat = dfx$lat,  icon = icons, popup = dfx$popup)
      
      
    }
    
    
  })
  
  
}


shiny::shinyApp(ui, server)
