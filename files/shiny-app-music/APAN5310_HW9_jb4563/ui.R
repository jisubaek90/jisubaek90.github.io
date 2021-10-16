
ui <- fluidPage(
  theme = shinytheme('flatly'),
  titlePanel(
    title = NULL,
    windowTitle = 'Music Playlist Generator'
  ),
  h1(
    style = 'text-align:center;',
    'Music Playlist Generator'
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      align = 'center',
      width = 5,
      style = 'padding-top:120px;',
      h4('Select the number of songs to be chosen from each genre:'),
      hr(),
      radioGroupButtons(
        inputId = 'genre1',
        label = 'Dance',
        choices = c(0:5),
        selected = 0,
        status = 'primary',
        justified = TRUE,
        checkIcon = list(
          yes = icon(
            name = 'ok', 
            lib = 'glyphicon'
          )
        )
      ),
      radioGroupButtons(
        inputId = 'genre2',
        label = 'K-Pop',
        choices = c(0:5),
        selected = 0,
        status = 'success',
        justified = TRUE,
        checkIcon = list(
          yes = icon(
            name = 'ok', 
            lib = 'glyphicon'
          )
        )
      ),
      radioGroupButtons(
        inputId = 'genre3',
        label = 'Pop',
        choices = c(0:5),
        selected = 0,
        status = 'warning',
        justified = TRUE,
        checkIcon = list(
          yes = icon(
            name = 'ok', 
            lib = 'glyphicon'
          )
        )
      ),
      radioGroupButtons(
        inputId = 'genre4',
        label = 'Rock',
        choices = c(0:5),
        selected = 0,
        status = 'danger',
        justified = TRUE,
        checkIcon = list(
          yes = icon(
            name = 'ok', 
            lib = 'glyphicon'
          )
        )
      ),
      hr(),
      actionBttn(
        inputId = 'run',
        label = 'Generate Playlist'
      )
    ),
    mainPanel(
      align = 'center',
      width = 7,
      uiOutput(
        outputId = 'info'
      ), # selected song information (song name, artist, views)
      uiOutput(
        outputId = 'vid'
      ) # embedded YouTube player
    )
  )

  
)

