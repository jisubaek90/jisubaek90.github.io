ui <- tagList(
  
  useShinyjs(),
  useShinyalert(),
  
  # ***********************----
  # navbarpage layout----
  navbarPage(
    id = 'navBar',
    # hyperlink to a website of your choice
    title = tags$a(
      href = 'https://english.visitkorea.or.kr/enu/index.kto',
      target = '_blank',
      img(src = 'h_icon_6.png', height = '25px')
    ),
    windowTitle = 'Seoul', #<--what the tab on browser will state
    position = 'fixed-top', #<--fixed-bottom if you want menu bar on bottom
    collapsible = TRUE, #<--collapse navbar tabs when window is shrunk
    theme = shinytheme('simplex'), #<--select shinytheme https://rstudio.github.io/shinythemes/
    # cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper
    # readable, sandstone, simplex, slate, spacelab, superhero, united, yeti
    
    # ***********************----
    # home page----
    tabPanel(
      value = '0',
      title = div(
        img(src = 'slogo.png', height = 25) #<--image for the home page
      ),
      # ui home rendered on server
      uiOutput('home'),
      
      # _hotel section----
      fluidRow(
        style = 'background-image:url(pg1.png); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg1Bttn',
          label = h1('Hotels'),
          style = 'simple'
        )
      ),
      # _restaurants section----
      fluidRow(
        style = 'background-image:url(pg3.png); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg3Bttn',
          label = h1('Restaurants'),
          style = 'simple'
        )
      ),
      # _attractions section----
      fluidRow(
        style = 'background-image:url(pg4.png); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg2Bttn',
          label = h1('Attractions'),
          style = 'simple'
        )
      ),
      # _personal_insights section----
      fluidRow(
        style = 'background-image:url(pg2.png); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg4Bttn',
          label = h1('Traveling Ideas'),
          style = 'simple'
        )
      ),
      # _business_insights section----
      fluidRow(
        style = 'background-image:url(pg5.png); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg5Bttn',
          label = h1('Business Ideas'),
          style = 'simple'
        )
      ),
      # _footer section----
      fluidRow(
        wellPanel(
          style = 'padding:20px; background-color:#ffffff;',
          align = 'center',
          h4('Our Partners:'),
          hr(),
          tags$span(
            # __marriott
            tags$a(
              href = 'https://marriott.com/',
              target = '_blank',
              img(src = 'marriott.png', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __hyatt
            tags$a(
              href = 'https://www.hyatt.com/',
              target = '_blank',
              img(src = 'hyatt.png', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __pan pacific
            tags$a(
              href = 'https://panpacific.com/',
              target = '_blank',
              img(src = 'panpacific.png', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __four seasons
            tags$a(
              href = 'https://fourseasons.com/',
              target = '_blank',
              img(src = 'fourseasons.png', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __rosewood
            tags$a(
              href = 'https://rosewood.com/',
              target = '_blank',
              img(src = 'rosewood.png', height = '30px')
            )
          )
        )
      )
    ),
    # ***********************----
    # tabs----
    # _hotels tab----
    tabPanel(
      value = 'a',
      title = div(
        img(src = 'h_icon_1.png', height = '25px'), 
        HTML('&nbsp;'), 'Hotels'
      ),
      h3(style = 'padding-top:70px;', 'Seoul Hotels'),
      hr(),
      uiOutput('hote')
    ),
    # _restaurants tab----
    tabPanel(
      value = 'c',
      title = div(
        img(src = 'h_icon_3.png', height = '25px'), 
        HTML('&nbsp;'), 'Restaurants'
      ),
      h3(style = 'padding-top:70px;', 'Seoul Restaurants'),
      hr(),
      uiOutput('rest')
    ),
    # _attractions tab----
    tabPanel(
      value = 'b',
      title = div(
        img(src = 'h_icon_2.png', height = '25px'), 
        HTML('&nbsp;'), 'Attractions'
      ),
      h3(style = 'padding-top:70px;', 'Seoul Attractions'),
      hr(),
      uiOutput('attr')
    ),
    # _travelers tab----
    tabPanel(
      value = 'd',
      title = div(
        img(src = 'h_icon_4.png', height = '25px'), 
        HTML('&nbsp;'), 'Traveling Ideas'
      ),
      fluidRow(
        style = 'padding-top:70px; padding-left:10px; padding-right:10px;',
        radioGroupButtons(
          inputId = 'insight',
          label = 'Select a Travel Idea to View:',
          choices = c(paste0('Insight ', 1:4)),
          selected = character(0),
          justified = TRUE
        )
      ),
      uiOutput('insi')
    ),
    
    # _business tab----
    tabPanel(
      value = 'e',
      title = div(
        img(src = 'h_icon_5.png', height = '25px'), 
        HTML('&nbsp;'), 'Business Ideas'
      ),
      fluidRow(
        style = 'padding-top:70px; padding-left:10px; padding-right:10px;',
        radioGroupButtons(
          inputId = 'idea',
          label = 'Select a Travel Idea to View:',
          choices = c(paste0('Idea ', 1:3)),
          selected = character(0),
          justified = TRUE
        )
      ),
      uiOutput('idea')
    )
  )
)



